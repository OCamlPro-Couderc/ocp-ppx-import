
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Types

type kind = Value | Type

type mod_import = {
  mod_kind : kind;
  mod_name : Longident.t loc;
  mod_alias : Longident.t loc option;
}

type val_import = {
  val_kind : kind;
  val_name : string loc;
  val_alias : string loc option;
}

type import_config = {
    namespace : Longident.t loc;
    modules : mod_import list;
    values : val_import list;
}

let namespace = ref None
let used = Hashtbl.create 17
(* let imported_pers = Hashtbl.create 17 *)

let rec lid_of_path = function
    Path.Pident id -> Lident (Ident.name id)
  | Path.Pdot (p, str, _) -> Ldot (lid_of_path p, str)
  | Path.Papply (p1, p2) -> Lapply (lid_of_path p1, lid_of_path p2)

let find_cmi loc ns =
  let ns_name = match ns.txt with
      Lident n -> n
    | _ -> raise Syntaxerr.(
        Error (Expecting (loc, "only compilation units can be imported"))) in
  let path = Misc.find_in_path_uncap !(Config.load_path) (ns_name ^ ".cmi") in
  let cmi = Cmi_format.read_cmi path in
  cmi.Cmi_format.cmi_sign

let find_type_decl loc ns sg tname =
  (* Trick: adding the signature into an empty environment and extract the type
     from it will result in a typedecl with every type constructor correclty
     prefixed by the module name.
     It avoid doing the strengthening by hand. *)
  let ns = match ns with
    Lident s -> s | _ -> assert false in
  let nsid = Ident.create_persistent ns in
  let tmp_env =
    Env.add_module nsid (Mty_signature sg) @@ Compmisc.initial_env () in
  let td = Env.find_type (Path.(Pdot(Pident nsid, tname, 0))) tmp_env in
  td

let rec gen_core_type loc ty =
  match (Btype.repr ty).desc with
    Tvar (Some v) | Tunivar (Some v) -> Typ.var v
  | Tvar None | Tunivar None -> Typ.var "_"
  | Tarrow (l, ty, ty', _) ->
    Typ.arrow l (gen_core_type loc ty) (gen_core_type loc ty')
  | Ttuple tys -> Typ.tuple @@ List.map (gen_core_type loc) tys
  | Tconstr (p, tys, _) ->
    Typ.constr (Location.mkloc (lid_of_path p) loc) @@
    List.map (gen_core_type loc) tys
  | Tobject (ty, _) -> gen_object loc ty
  | Tvariant rd ->
    gen_variant loc rd
  | Tpackage(p, lids, tys) ->
    gen_package loc (p, lids, tys)
  | Tpoly (ty, params) ->
    gen_poly loc params ty
  | Tnil | Tlink _ | Tsubst _ | Tfield (_, _, _, _) -> assert false

and gen_poly loc params ty =
  let gen_param ty =
    match (Btype.repr ty).desc with
      Tunivar (Some v) -> v
    | _ -> assert false in
  gen_core_type loc ty |>
  Typ.poly (List.map gen_param params)

and gen_object loc ty =
  let rec step acc ty =
    match (Btype.repr ty).desc with
      Tfield (l, (Fpresent | Fvar { contents = Some Fpresent }), ty, rem) ->
      let ty = gen_core_type loc ty in
      step ((l, [], ty) :: acc) rem
    | Tfield (_, _, _, rem) -> step acc rem
    | Tvar _ | Tunivar _ -> Typ.object_ (List.rev acc) Open
    | Tnil -> Typ.object_ (List.rev acc) Closed
    | _ -> assert false
  in
  step [] ty

and gen_variant loc rd =
  let closed = if rd.row_closed then Closed else Open in
  let is_closed = rd.row_closed in
  let gen_fields (labels, present) (l, rf) =
    match rf with
      Rpresent None ->
      let present = if is_closed then l :: present else present in
      Rtag (l, [], true, []) :: labels, present
    | Rpresent (Some ty) ->
      let present = if is_closed then l :: present else present in
      Rtag (l, [], false, [gen_core_type loc ty]) :: labels, present
    | Reither (cst, tys, _, _) ->
      Rtag (l, [], cst, List.map (gen_core_type loc) tys) :: labels, present
    | Rabsent -> labels, present
  in
  let labels, present = List.fold_left gen_fields ([], []) rd.row_fields in
  let present =
    if List.length labels = List.length present then Some (List.rev present)
    else None in
  Typ.variant (List.rev labels) closed present

and gen_package loc (p, lids, tys) =
  let lid p = Location.mkloc (lid_of_path p) loc in
  let substs = List.map2 (fun l ty ->
      Location.mkloc l loc, gen_core_type loc ty) lids tys in
  Typ.package (lid p) substs

let gen_constructor loc cd =
  let args = List.map (gen_core_type loc) cd.cd_args in
  let res = match cd.cd_res with
      None -> None
    | Some ty -> Some (gen_core_type loc ty) in
  let name = Location.mkloc (Ident.name cd.cd_id) loc in
  Type.constructor ~args ?res name

let gen_field loc ld =
  let mut = ld.ld_mutable in
  let name = Location.mkloc (Ident.name ld.ld_id) loc in
  Type.field ~mut name (gen_core_type loc ld.ld_type)

let gen_type_kind loc = function
    Type_abstract -> Ptype_abstract
  | Type_open -> Ptype_open
  | Type_record (lds, _) ->
    Ptype_record (List.map (gen_field loc) lds)
  | Type_variant cds ->
    Ptype_variant (List.map (gen_constructor loc) cds)

let gen_type_params loc params =
  let i = ref 0 in
  let gen_param p =
    match p.desc with
      Tvar _ ->
      (gen_core_type loc p, Invariant), None
    | _ ->
      let param =
        Typ.var ("a" ^ (string_of_int !i)) in
      incr i;
      (param, Invariant), Some (param, gen_core_type loc p, loc) in
  let params, cstrs =
    List.map gen_param params |> List.split in
  let cstrs =
    List.fold_left (fun l opt ->
         match opt with
           Some cstr -> cstr :: l
         | None -> l) [] cstrs
    |> List.rev in
  params, cstrs

let gen_typedecl_from_sig loc ty_name orig_name decl =
  let kind = gen_type_kind loc decl.type_kind in
  let params, cstrs = gen_type_params loc decl.type_params in
  let manifest = Typ.constr orig_name (fst @@ List.split params) in
  Type.mk ~kind ~params ~cstrs ~manifest ty_name

let print_lid fmt lid =
  let rec step fmt = function
      Lident id -> Format.fprintf fmt "%s" id
    | Ldot (lid, s) ->
      Format.fprintf fmt "%a.%s" step lid s
    | Lapply (l1, l2) ->
      Format.fprintf fmt "%a(%a)" step l1 step l2 in
  step fmt lid

let parse_kind elt =
  if List.exists (fun (str, _) -> str.txt = "type") elt.pexp_attributes
  then Type
  else Value

let parse_element (acc_vals, acc_mods) elt =
  let kind = parse_kind elt in
  match elt.pexp_desc with
  | Pexp_apply ( { pexp_desc = Pexp_ident {txt = Lident "=>" }},
      [ "", { pexp_desc = Pexp_construct (m, None)};
        "", ({ pexp_desc = Pexp_construct (m', None)} as e);
      ]) ->
    acc_vals, { mod_kind = parse_kind e;
                mod_name = m;
                mod_alias = Some m' } :: acc_mods
  | Pexp_construct (m', None) ->
    acc_vals, { mod_kind = kind; mod_name = m'; mod_alias = None } :: acc_mods

  | Pexp_apply ( { pexp_desc = Pexp_ident {txt = Lident "=>" }},
      [ "", { pexp_desc = Pexp_ident {txt = Lident v; loc}};
        "", ({ pexp_desc = (Pexp_ident {txt = Lident v'; loc = loc'})} as e);
      ]) ->
    { val_kind = parse_kind e;
      val_name = Location.mkloc v loc;
      val_alias = Some (Location.mkloc v' loc') } :: acc_vals, acc_mods
  | Pexp_ident { txt = Lident v; loc } ->
    { val_kind = kind;
      val_name = Location.mkloc v loc;
      val_alias = None } :: acc_vals, acc_mods
  | _ -> raise Syntaxerr.(
      Error (Expecting (elt.pexp_loc, "value or module identifier")))

let parse_import loc p =
  match p.pexp_desc with
  | Pexp_open (ovf, ns, {pexp_desc = Pexp_tuple elts}) ->
    let values, modules =
      if elts = [] then
        raise Syntaxerr.(
            Error (Expecting (p.pexp_loc, "module(s) and/or value(s)")))
      else
          List.fold_left parse_element ([], []) elts in
    if Hashtbl.mem used ns.txt then
      raise Syntaxerr.(
          Error (Ill_formed_ast (loc, "namespace already imported")))
    else
      { namespace = ns; modules; values }
  | Pexp_construct ({ txt = Ldot (ns, md); loc }, None) ->
    if Hashtbl.mem used ns then
      raise Syntaxerr.(
          Error (Ill_formed_ast (loc, "namespace already imported")))
    else
      { namespace = Location.mkloc ns loc;
        modules = [{
            mod_kind = parse_kind p;
            mod_name = Location.mkloc (Lident md) loc;
            mod_alias = None
          }];
        values = [] }
  | Pexp_ident { txt = Ldot (ns, md); loc } ->
    if Hashtbl.mem used ns then
      raise Syntaxerr.(
          Error (Ill_formed_ast (loc, "namespace already imported")))
    else
      { namespace = Location.mkloc ns loc;
        values = [{
            val_kind = parse_kind p;
            val_name = Location.mkloc md loc;
            val_alias = None
          }];
        modules = [] }
  | _ ->
    raise Syntaxerr.(Error (Expecting (p.pexp_loc, "module imports")))

let parse_payload loc p =
  match p with
    PStr [{ pstr_desc = Pstr_eval (p, _)}] ->
    parse_import loc p
  | _ ->
    Format.eprintf "Error: %a.\n%!" (Printast.payload 0) p;
    raise Syntaxerr.(Error (Ill_formed_ast (loc, "expecting a module with \
                                                  direct submodules")))

let concat_lids loc ns md =
  let rec concat md =
    match md with
      Lident m -> Ldot (ns, m)
    | Ldot (md', m) -> Ldot (concat md', m)
    | Lapply (_, _) ->
      raise Syntaxerr.(Error (Ill_formed_ast (loc, "functor application is not \
                                                    a valid import")))
  in
  concat md

let gen_mod_binding ns mi =
  let modident, modloc =
    match mi.mod_alias with
      Some { txt = Lident modname; loc } ->
      Location.mkloc modname loc, loc
    | Some _ -> assert false
    | None ->
      match mi.mod_name with
        { txt = Lident modname; loc } -> Location.mkloc modname loc, loc
      | _ ->
        raise Syntaxerr.(
            Error
              (Variable_in_scope (mi.mod_name.loc,
                                  "only toplevel modules can be imported")))
  in
  let loc = mi.mod_name.loc in
  Hashtbl.add used ns.txt ();
  let imported = concat_lids loc ns.txt mi.mod_name.txt in
  match mi.mod_kind with
    Value ->
    let modexpr = Mod.ident ~loc (Location.mkloc imported loc) in
    Str.module_ ~loc (Mb.mk ~loc:modloc modident modexpr)
  | Type ->
    let modtype_expr = Mty.ident ~loc (Location.mkloc imported loc) in
    Str.modtype ~loc (Mtd.mk ~loc:modloc modident ~typ:modtype_expr)

let gen_val_binding ns vi =
  let valident, valloc =
    match vi.val_alias with
      Some v -> v, v.loc
    | None -> vi.val_name, vi.val_name.loc
  in
  let loc = vi.val_name.loc in
  Hashtbl.add used ns.txt ();
  let imported = concat_lids loc ns.txt (Lident vi.val_name.txt) in
  let expr = Exp.ident ~loc (Location.mkloc imported loc) in
  let pat = Pat.var valident in
  Str.value Nonrecursive ~loc [Vb.mk ~loc:valloc pat expr]

let gen_typedecl ns vi =
  let tyident, tyloc =
    match vi.val_alias with
      Some v -> v, v.loc
    | None -> vi.val_name, vi.val_name.loc
  in
  let loc = vi.val_name.loc in
  Hashtbl.add used ns.txt ();
  let imported = concat_lids loc ns.txt (Lident vi.val_name.txt) in
  let sg = find_cmi loc ns in
  let td_sig = find_type_decl loc ns.txt sg vi.val_name.txt in
  let td =
    gen_typedecl_from_sig loc tyident (Location.mkloc imported loc) td_sig in
  Str.type_ ~loc [td]

let gen_fake_module loc (conf : import_config) parsed =
  let items =
    let mbs =
      List.map (gen_mod_binding conf.namespace) conf.modules in
    let vbs =
      List.map (fun vi ->
          match vi.val_kind with
            Value -> gen_val_binding conf.namespace vi
          | Type -> gen_typedecl conf.namespace vi) conf.values in
    List.rev_append mbs [] |> List.rev_append vbs in
  let md_loc = conf.namespace.loc in
  let curr_mod_name =
    let unit_name =
      md_loc.Location.loc_start.Lexing.pos_fname |>
      Filename.basename |>
      Filename.chop_extension |>
      String.capitalize in
    match Clflags.for_package with
      { contents = Some p } ->
      Format.sprintf "%s.%s" p unit_name
    | _ -> unit_name in
  let md_name =
    Format.asprintf "*namespace*-%a-%s"
      print_lid conf.namespace.txt curr_mod_name in
  let md_lid =
    Location.mkloc (Lident md_name) md_loc in
  let mb = Mod.structure ~loc items in
  let item =
    Str.module_ (Mb.mk ~loc (Location.mkloc md_name md_loc) mb) in
  (Str.open_ ~loc @@ Opn.mk ~loc md_lid) :: item :: parsed

let extract_namespace loc = function
    PStr [{ pstr_desc =
              Pstr_eval ({ pexp_desc = Pexp_construct (lid, None) }, _)}] -> lid
  | _ -> raise Syntaxerr.(
      Error (Expecting (loc, "module identifier")))

let gen_namespace_item ns parsed =
    if Ast_mapper.tool_name () = "ocamldep" then parsed
    else
      let loc = ns.loc in
      (Str.open_ ~loc @@ Opn.mk ~loc ns) :: parsed

let gen_imports argv =
  { default_mapper with
    structure = fun mapper str ->
      let rev_strc =
        List.fold_left (fun parsed strc ->
            match strc.pstr_desc with
              Pstr_extension (({ txt = "import"; loc }, payload), attrs) ->
              let imports = parse_payload strc.pstr_loc payload in
              gen_fake_module loc imports parsed
            | Pstr_extension (({txt = "namespace"; loc}, payload), attrs) ->
              let ns =
                match !namespace with
                  Some ns ->
                  raise Syntaxerr.(
                      Error (Ill_formed_ast (loc, "namespace already defined")))
                | None ->
                  let ns = extract_namespace loc payload in
                  namespace := Some ns;
                  ns
              in
              gen_namespace_item ns parsed
            | _ -> default_mapper.structure_item mapper strc :: parsed)
          [] str in
      List.rev rev_strc
  }

let _ =
  register "namespace" gen_imports
