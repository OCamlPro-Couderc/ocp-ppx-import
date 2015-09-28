
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

type mod_import = {
  modname : Longident.t loc;
  alias : Longident.t loc option;
}

type import_config = {
    namespace : Longident.t loc;
    modules : mod_import list;
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



let parse_module m =
  match m.pexp_desc with
  | Pexp_apply ( { pexp_desc = Pexp_ident {txt = Lident "=>" }},
      [ "", { pexp_desc = Pexp_construct (m, None)};
        "", { pexp_desc = Pexp_construct (m', None)}; ]) ->
    { modname = m; alias = Some m' }
  | Pexp_construct (m', None) -> { modname = m'; alias = None }
  (* | Ppat_any *)
  | _ -> raise Syntaxerr.(Error (Expecting (m.pexp_loc, "module longident")))


let parse_import p =
  match p.pexp_desc with
  | Pexp_open (ovf, ns, {pexp_desc = Pexp_tuple mods}) ->
    let modules =
      if mods = [] then
        raise Syntaxerr.(Error (Expecting (p.pexp_loc, "module(s)")))
      else
        List.map parse_module mods in
    { namespace = ns; modules }
  | _ ->
    raise Syntaxerr.(Error (Expecting (p.pexp_loc, "module imports")))

let parse_payload loc p =
  match p with
    PStr [{ pstr_desc = Pstr_eval (p, _)}] ->
    parse_import p
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

let gen_binding ns mi =
  let modident, modloc =
    match mi.alias with
      Some { txt = Lident modname; loc } ->
      Location.mkloc modname loc, loc
    | Some _ -> assert false
    | None ->
      match mi.modname with
        { txt = Lident modname; loc } -> Location.mkloc modname loc, loc
      | _ ->
        raise Syntaxerr.(
            Error
              (Ill_formed_ast (mi.modname.loc, "only toplevel modules can \
                                                be imported")))
  in
  let loc = mi.modname.loc in
  Hashtbl.add used () ns.txt;
  let imported = concat_lids loc ns.txt mi.modname.txt in
  let modexpr = Mod.ident ~loc (Location.mkloc imported loc) in
  Str.module_ ~loc (Mb.mk ~loc:modloc modident modexpr)

let gen_fake_module loc (conf : import_config) parsed =
  let items =
    let mbs =
      List.map (gen_binding conf.namespace) conf.modules in
    List.rev_append mbs [] in
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
