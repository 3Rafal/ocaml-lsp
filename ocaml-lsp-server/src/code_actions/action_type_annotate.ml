open Import

let action_kind = "type-annotate"

let check_typeable_context pipeline pos_start =
  let open Typedtree in
  let pos_start = Mpipeline.get_lexing_pos pipeline pos_start in
  let typer = Mpipeline.typer_result pipeline in
  let browse = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
  let is_exp_constrained = function
    | Texp_constraint _, _, _ -> true
    | Texp_coerce (Some _, _), _, _ -> true
    | _ -> false
  in
  let is_pat_constrained = function
    | Tpat_constraint _, _, _ -> true
    | _ -> false
  in
  let pat_constraint_loc = function
    | Typedtree.Tpat_constraint _, loc, _ -> Some loc
    | _ -> None
  in
  let is_valid env typ loc p extras =
    if List.exists ~f:p extras then `Invalid else `Valid (env, typ, loc)
  in
  let rec trav_cases = function
    | { c_lhs = { pat_desc = Tpat_var _; _ }
      ; c_rhs = { exp_desc = Texp_function { cases; _ }; _ }
      ; _
      }
      :: _ -> trav_cases cases
    | { c_lhs = { pat_desc = Tpat_var _; pat_loc; _ }
      ; c_rhs = { exp_extra; exp_type; exp_env; _ }
      ; _
      }
      :: _ ->
      if List.exists ~f:is_exp_constrained exp_extra then `Invalid
      else `Valid_fun (exp_env, exp_type, pat_loc)
    | { c_lhs = { pat_desc = Tpat_alias _; pat_loc; pat_extra; _ }
      ; c_rhs = { exp_extra; exp_type; exp_env; _ }
      ; _
      }
      :: _ -> (
      if List.exists ~f:is_exp_constrained exp_extra then `Invalid
      else
        match pat_extra |> List.rev |> List.find_map ~f:pat_constraint_loc with
        | Some loc -> `Valid_fun (exp_env, exp_type, Loc.union pat_loc loc)
        | None -> `Valid_fun (exp_env, exp_type, pat_loc))
    | _ -> `Invalid
  in
  match Mbrowse.enclosing pos_start [ browse ] with
  | (_, Pattern { pat_desc = Tpat_var _; _ })
    :: ( _
       , Value_binding
           { vb_expr = { exp_desc = Texp_function { cases; _ }; _ }; _ } )
    :: _ -> trav_cases cases
  | (_, Expression e) :: _ ->
    is_valid e.exp_env e.exp_type e.exp_loc is_exp_constrained e.exp_extra
  | (_, Pattern { pat_desc = Tpat_any; pat_loc; pat_env; pat_type; _ })
    :: (_, Pattern { pat_desc = Tpat_alias _; pat_extra; _ })
    :: _ -> is_valid pat_env pat_type pat_loc is_pat_constrained pat_extra
  | (_, Pattern p) :: _ ->
    is_valid p.pat_env p.pat_type p.pat_loc is_pat_constrained p.pat_extra
  | _ :: _ | [] -> `Invalid

let get_source_text doc (loc : Loc.t) =
  let open Option.O in
  let source = Document.source doc in
  let* start = Position.of_lexical_position loc.loc_start in
  let+ end_ = Position.of_lexical_position loc.loc_end in
  let (`Offset start) = Msource.get_offset source (Position.logical start) in
  let (`Offset end_) = Msource.get_offset source (Position.logical end_) in
  String.sub (Msource.text source) ~pos:start ~len:(end_ - start)

let code_action uri doc str_fmt (env, typ, loc) =
  let open Option.O in
  let+ original_text = get_source_text doc loc in
  let typ_str =
    let buffer = Buffer.create 16 in
    let ppf = Format.formatter_of_buffer buffer in
    let pp_type env ppf ty =
      let open Merlin_analysis in
      let module Printtyp = Type_utils.Printtyp in
      Printtyp.wrap_printing_env env ~verbosity:(Lvl 0) (fun () ->
          Printtyp.shared_type_scheme ppf ty)
    in
    Format.fprintf ppf "%a%!" (pp_type env) typ;
    Buffer.contents buffer
  in
  let newText = Printf.sprintf str_fmt original_text typ_str in
  let edit : WorkspaceEdit.t =
    let textedit : TextEdit.t = { range = Range.of_loc loc; newText } in
    let version = Document.version doc in
    let textDocument =
      OptionalVersionedTextDocumentIdentifier.create ~uri ~version ()
    in
    let edit =
      TextDocumentEdit.create ~textDocument ~edits:[ `TextEdit textedit ]
    in
    WorkspaceEdit.create ~documentChanges:[ `TextDocumentEdit edit ] ()
  in
  let title = String.capitalize_ascii action_kind in
  CodeAction.create
    ~title
    ~kind:(CodeActionKind.Other action_kind)
    ~edit
    ~isPreferred:false
    ()

let code_action doc (params : CodeActionParams.t) =
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    let pos_start = Position.logical params.range.start in
    let action fmt_str data =
      code_action params.textDocument.uri doc fmt_str data
    in
    Document.Merlin.with_pipeline_exn merlin (fun pipeline ->
        match check_typeable_context pipeline pos_start with
        | `Invalid -> None
        | `Valid_fun x -> action "%s : %s" x
        | `Valid x -> action "(%s : %s)" x)

let t =
  { Code_action.kind = CodeActionKind.Other action_kind; run = code_action }
