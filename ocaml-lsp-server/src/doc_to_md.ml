open Import
open Cmarkit

type t =
  | Raw of string
  | Markdown of string

let loc_to_meta _loc = Meta.none

let style_inline ~meta (style : Odoc_parser.Ast.style) inline =
  match style with
  | `Bold -> Inline.Strong_emphasis (Inline.Emphasis.make inline, meta)
  | `Italic -> Inline.Emphasis (Inline.Emphasis.make inline, meta)
  | `Emphasis -> Inline.Emphasis (Inline.Emphasis.make inline, meta)
  | `Superscript -> inline
  | `Subscript -> inline

let rec inline_element_to_inline
    (inline : Odoc_parser.Ast.inline_element Odoc_parser.Loc.with_location) :
    Inline.t =
  match inline with
  | Odoc_parser.Loc.{ value = `Space _; location } ->
    let meta = loc_to_meta location in
    Inline.Text (" ", meta)
  | Odoc_parser.Loc.{ value = `Word w; location } ->
    let meta = loc_to_meta location in
    Inline.Text (w, meta)
  | Odoc_parser.Loc.{ value = `Code_span c; location } ->
    let meta = loc_to_meta location in
    Inline.Code_span (Inline.Code_span.of_string c, meta)
  | Odoc_parser.Loc.{ value = `Raw_markup (Some "html", text); location } ->
    let meta = loc_to_meta location in
    Inline.Raw_html (Block_line.tight_list_of_string text, meta)
  | Odoc_parser.Loc.{ value = `Raw_markup (_, text); location } ->
    (* Cmarkit doesn't have constructors for backend other than HTML for inline
       raw markups, only for blocks. *)
    let meta = loc_to_meta location in
    Inline.Text (text, meta)
  | Odoc_parser.Loc.{ value = `Styled (style, inlines); location } ->
    let text = inline_element_list_to_inlines inlines in
    let meta = loc_to_meta location in
    style_inline ~meta style text
  | Odoc_parser.Loc.
      { value = `Reference (_kind, _ref, _inlines); location = _location } ->
    (* TODO: add support for references *)
    Inline.Break (Inline.Break.make `Hard, Meta.none)
  | Odoc_parser.Loc.{ value = `Link (link, inlines); location } ->
    let text = inline_element_list_to_inlines inlines in
    let ref =
      `Inline (Link_definition.make ~dest:(link, Meta.none) (), Meta.none)
    in
    let link = Inline.Link.make text ref in
    let meta = loc_to_meta location in
    Inline.Link (link, meta)
  | Odoc_parser.Loc.{ value = `Math_span text; location } ->
    let meta = loc_to_meta location in
    Inline.Ext_math_span
      ( Inline.Math_span.make
          ~display:false
          (Block_line.tight_list_of_string text)
      , meta )

and inline_element_list_to_inlines inlines =
  let inlines = List.map ~f:inline_element_to_inline inlines in
  Inline.Inlines (inlines, Meta.none)

let rec nestable_block_element_to_block
    (nestable :
      Odoc_parser.Ast.nestable_block_element Odoc_parser.Loc.with_location) =
  match nestable with
  | Odoc_parser.Loc.{ value = `Paragraph text; location } ->
    let inline = inline_element_list_to_inlines text in
    let paragraph = Block.Paragraph.make inline in
    let meta = loc_to_meta location in
    Block.Paragraph (paragraph, meta)
  | Odoc_parser.Loc.{ value = `List (kind, style, xs); location } ->
    let type' =
      match kind with
      | `Unordered -> `Unordered '*'
      | `Ordered -> `Ordered (1, '*')
    in
    let tight =
      match style with
      | `Heavy -> false
      | `Light -> true
    in
    let list_items =
      List.map
        ~f:(fun n ->
          let block = nestable_block_element_list_to_block n in
          (Block.List_item.make block, Meta.none))
        xs
    in
    let l = Block.List'.make ~tight type' list_items in
    let meta = loc_to_meta location in
    Block.List (l, meta)
  | Odoc_parser.Loc.{ value = `Modules modules; location } ->
    let type' = `Unordered '*' in
    let tight = false in
    let list_items =
      List.map
        ~f:(fun Odoc_parser.Loc.{ value = m; location } ->
          let inline = Inline.Text (m, Meta.none) in
          let paragraph = Block.Paragraph.make inline in
          let block = Block.Paragraph (paragraph, Meta.none) in
          let meta = loc_to_meta location in
          let marker = Layout.string "!modules:" in
          (Block.List_item.make ~marker block, meta))
        modules
    in
    let l = Block.List'.make ~tight type' list_items in
    let meta = loc_to_meta location in
    Block.List (l, meta)
  | Odoc_parser.Loc.
      { value = `Code_block (metadata, { value = code; location = _code_loc })
      ; location
      } ->
    let info_string =
      match metadata with
      | None -> None
      | Some ({ value = lang; location = lang_log }, _env) ->
        Some (lang, loc_to_meta lang_log)
    in
    let block_line = Block_line.list_of_string code in
    let code_block = Block.Code_block.make ?info_string block_line in
    let meta = loc_to_meta location in
    Block.Code_block (code_block, meta)
  | Odoc_parser.Loc.{ value = `Verbatim code; location } ->
    let info_string = Some ("verb", Meta.none) in
    let block_line = Block_line.list_of_string code in
    let code_block = Block.Code_block.make ?info_string block_line in
    let meta = loc_to_meta location in
    Block.Code_block (code_block, meta)
  | Odoc_parser.Loc.{ value = `Math_block code; location } ->
    let block_line = Block_line.list_of_string code in
    let code_block = Block.Code_block.make block_line in
    let meta = loc_to_meta location in
    Block.Ext_math_block (code_block, meta)

and nestable_block_element_list_to_block nestables =
  let blocks = List.map ~f:nestable_block_element_to_block nestables in
  Block.Blocks (blocks, Meta.none)

let tag_to_paragraph (tag : Odoc_parser.Ast.tag) =
  let format_tag tag text =
    Inline.Inlines
      ( [ Inline.Strong_emphasis
            (Inline.Emphasis.make (Inline.Text (tag, Meta.none)), Meta.none)
        ; Inline.Text (text, Meta.none)
        ]
      , Meta.none )
  in
  let tag, text =
    (* TODO: add support for tags *)
    match[@warning "-27"] tag with
    | `Author s -> ("@author", s)
    | `Deprecated text -> ("@deprecated", "")
    | `Param (id, text) -> ("@param", "")
    | `Raise (exc, text) -> ("@raise", "")
    | `Return text -> ("@return", "")
    | `See (`Url, url, text) -> ("@see", "")
    | `See (`File, filename, text) -> ("@see", "")
    | `See (`Document, document, text) -> ("@see", "")
    | `Since s -> ("@since", "")
    | `Before (version, text) -> ("@before", "")
    | `Version s -> ("@version", "")
    | `Canonical s -> ("@canonical", "")
    | `Inline -> ("@inline", "")
    | `Open -> ("@open", "")
    | `Closed -> ("@closed", "")
  in
  let inline = format_tag tag text in
  Block.Paragraph.make inline

let rec block_element_to_block
    (block_element :
      Odoc_parser.Ast.block_element Odoc_parser.Loc.with_location) =
  match block_element with
  | Odoc_parser.Loc.{ value = `Heading (level, _, content); location } ->
    let text = inline_element_list_to_inlines content in
    let heading = Block.Heading.make ~level text in
    let meta = loc_to_meta location in
    Block.Heading (heading, meta)
  | Odoc_parser.Loc.{ value = `Tag t; location } ->
    let paragraph = tag_to_paragraph t in
    let meta = loc_to_meta location in
    Block.Paragraph (paragraph, meta)
  | Odoc_parser.Loc.
      { value =
          ( `Paragraph _
          | `List _
          | `Modules _
          | `Code_block _
          | `Verbatim _
          | `Math_block _ )
      ; location = _
      } as nestable -> nestable_block_element_to_block nestable

and block_element_list_to_block l =
  let blocks = List.map ~f:block_element_to_block l in
  Block.Blocks (blocks, Meta.none)

let translate doc : t =
  let location = Lexing.dummy_pos in
  let v = Odoc_parser.parse_comment ~location ~text:doc in
  match Odoc_parser.warnings v with
  | [] ->
    let ast = Odoc_parser.ast v in
    let block = block_element_list_to_block ast in
    let doc = Doc.make block in
    let cmark = Cmarkit_commonmark.of_doc doc in
    Markdown cmark
  | warnings ->
    let messages =
      List.map
        ~f:(fun warn -> ("msg", `String (Odoc_parser.Warning.to_string warn)))
        warnings
    in
    Log.log ~section:"debug" (fun () ->
        Log.msg "Invalid documentation comment" messages);
    Raw doc
