open Misc
open Cmm
open Mach
open Selectgen

let collapse_machtype = function
  | [||] -> Int
  | [| x |] -> x
  | _ -> failwith "got many machtypes"

open Llvm
open Llvm_debuginfo

let () = install_fatal_error_handler Compenv.fatal
let const_of_int64_sext ty x = const_of_int64 ty x true

let emit_data m data =
  let c = module_context m in
  match data with
  | [] -> ()
  | contents ->
    let skip_offset = ref 0 in
    let aliases = ref [] in
    let ty mk_type mk_const const_val =
      let ty = mk_type c in
      Some (mk_const ty const_val)
    in
    let contents =
      ListLabels.mapi contents ~f:(fun offset data_item ->
          match data_item with
          | Csymbol_address s ->
            Some (declare_global (i64_type c) s m : llvalue)
          | Cint i -> ty i64_type const_of_int64_sext (Int64.of_nativeint i)
          | Csingle f -> ty float_type const_float f
          | Cdouble f -> ty double_type const_float f
          | Cskip i ->
            incr skip_offset;
            Some (const_string c (String.make i '\x00'))
          | Cstring s -> Some (const_string c s)
          | Cint8 i -> ty i8_type const_int i
          | Cint16 i -> ty i16_type const_int i
          | Cint32 i -> ty i32_type const_of_int64_sext (Int64.of_nativeint i)
          | Calign _ ->
            incr skip_offset;
            failwith ("not implemented " ^ __LOC__)
          | Cdefine_symbol name ->
            aliases := (name, offset - !skip_offset, false) :: !aliases;
            incr skip_offset;
            None
          | Cglobal_symbol name ->
            aliases := (name, offset - !skip_offset, true) :: !aliases;
            incr skip_offset;
            None)
      |> List.filter_map (fun x -> x)
      |> Array.of_list
    in
    let g = define_global "" (const_struct c contents) m in
    ListLabels.iter !aliases ~f:(fun (name, offset, is_global) ->
        let value = const_element g offset in
        let alias = add_alias m (i64_type c) value name in
        let linkage : Linkage.t = if is_global then External else Internal in
        set_linkage linkage alias)

let implementation phrases ~ppf_dump:(_ : Format.formatter) =
  (* TODO: use ppf_dump... *)
  let c = global_context () in
  let m =
    create_module c (Compilenv.current_unit_infos ()).Cmx_format.ui_name
  in
  let di = dibuilder m in
  let di_file = dibuild_create_file di ~filename:"file" ~directory:"dir" in
  let _cu =
    dibuild_create_compile_unit di OCaml ~file_ref:di_file
      ~producer:"<unknown producer>" ~is_optimized:false
      ~flags:(StringLabels.concat (Array.to_list Sys.argv) ~sep:" ")
      ~dwoid:0 ~di_inlining:false ~di_profiling:false
      ~sys_root:Config.standard_library ~sdk:Config.version
  in
  ListLabels.iter phrases ~f:(function
    | Cfunction _ -> failwith "not implemented"
    | Cdata data -> emit_data m data);
  Llvm_analysis.assert_valid_module m
