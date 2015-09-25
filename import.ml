
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

type mod_import = {
  modname : Longident.t loc;
  alias : Longident.t loc option;
}

type import = {
    namespace : Longident.t loc;
    modules : mod_import list;
}



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
  let imported = concat_lids loc ns.txt mi.modname.txt in
  let modexpr = Mod.ident ~loc (Location.mkloc imported loc) in
  Str.module_ ~loc (Mb.mk ~loc:modloc modident modexpr)

let gen_imports argv =
  { default_mapper with
    structure = fun mapper str ->
      let rev_strc =
        List.fold_left (fun parsed strc ->
            match strc.pstr_desc with
              Pstr_extension (({ txt = "import"; loc }, payload), attrs) ->
              let imports = parse_payload strc.pstr_loc payload in
              let mbs =
                List.map (gen_binding imports.namespace) imports.modules in
              List.rev_append mbs parsed
            | _ -> default_mapper.structure_item mapper strc :: parsed)
          [] str in
      List.rev rev_strc
  }

let _ =
  register "import" gen_imports
