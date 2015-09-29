
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

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

let print_lid fmt lid =
  let rec step fmt = function
      Lident id -> Format.fprintf fmt "%s" id
    | Ldot (lid, s) ->
      Format.fprintf fmt "%a.%s" step lid s
    | Lapply (l1, l2) ->
      Format.fprintf fmt "%a(%a)" step l1 step l2 in
  step fmt lid

(* let rec lid_length = function *)
(*     Lident id -> String.length id *)
(*   | Ldot (lid, s) -> String.length lid + 1 + lid_length lid *)
(*   | Lapply (l1, l2) -> lid_leng *)
let parse_element kind (acc_vals, acc_mods) elt =
  match elt.pexp_desc with
  | Pexp_apply ( { pexp_desc = Pexp_ident {txt = Lident "=>" }},
      [ "", { pexp_desc = Pexp_construct (m, None)};
        "", { pexp_desc = Pexp_construct (m', None)}; ]) ->
    acc_vals, { mod_kind = kind; mod_name = m; mod_alias = Some m' } :: acc_mods
  | Pexp_construct (m', None) ->
    acc_vals, { mod_kind = kind; mod_name = m'; mod_alias = None } :: acc_mods
(*   | Pexp_ident { txt = Lident ".." } -> assert false *)
(*   | _ -> raise Syntaxerr.(Error (Expecting (m.pexp_loc, "module longident"))) *)

(* let parse_value kind v = *)
(*   match v.pexp_desc with *)
  | Pexp_apply ( { pexp_desc = Pexp_ident {txt = Lident "=>" }},
      [ "", { pexp_desc = Pexp_ident {txt = Lident v; loc}};
        "", { pexp_desc = (Pexp_ident {txt = Lident v'; loc = loc'})}; ]) ->
    { val_kind = kind;
      val_name = Location.mkloc v loc;
      val_alias = Some (Location.mkloc v' loc') } :: acc_vals, acc_mods
  | Pexp_ident { txt = Lident v; loc } ->
    { val_kind = kind;
      val_name = Location.mkloc v loc;
      val_alias = None } :: acc_vals, acc_mods
  | _ -> raise Syntaxerr.(
      Error (Expecting (elt.pexp_loc, "value or module identifier")))


let parse_import loc p =
  let kind = Value in
  match p.pexp_desc with
  | Pexp_open (ovf, ns, {pexp_desc = Pexp_tuple elts}) ->
    let values, modules =
      if elts = [] then
        raise Syntaxerr.(
            Error (Expecting (p.pexp_loc, "module(s) and/or value(s)")))
      else
          List.fold_left (parse_element Value) ([], []) elts in
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
            mod_kind = kind;
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
            val_kind = kind;
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
  let modexpr = Mod.ident ~loc (Location.mkloc imported loc) in
  Str.module_ ~loc (Mb.mk ~loc:modloc modident modexpr)

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


let gen_fake_module loc (conf : import_config) parsed =
  let items =
    let mbs =
      List.map (gen_mod_binding conf.namespace) conf.modules in
    let vbs =
      List.map (gen_val_binding conf.namespace) conf.values in
    List.rev_append mbs [] |> List.rev_append vbs in
  let md_loc = conf.namespace.loc in
  let md_name =
    Format.asprintf "*namespace*-%a" print_lid conf.namespace.txt in
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
