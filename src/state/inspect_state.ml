module File = struct
  type t = { name : string; path : string; code : string option }

  type location =
    | Direct_location of string * string
    | Cil_location of Cil.location

  let open_direct name path = { name; path; code = None }

  let open_cil l p =
    let files =
      Parse.get_files p
      |> List.filter (fun f -> Parse.file_to_name f = l.Cil.file)
    in
    match files with
    | f :: _ ->
        let name = Parse.file_to_name f in
        let path = Parse.file_to_path f in
        Some { name; path; code = None }
    | _ -> None

  let get_name f = f.name

  let get_path f = f.path

  let get_code f = f.code

  let name = get_name

  [@@@ocaml.deprecated "Use get_name"]

  let path = get_path

  [@@@ocaml.deprecated "Use get_path"]

  let code f = Option.get f.code

  [@@@ocaml.deprecated "Don't use"]

  let code_opt = get_code

  [@@@ocaml.deprecated "Use get_code"]
end

module Func = struct
  type t = {
    name : string;
    file_name : string;
    file_path : string;
    dot : string option;
  }

  let get_name f = f.name

  let get_file_name f = f.file_name

  let get_file_path f = f.file_path

  let get_dot f = f.dot

  let name = get_name

  [@@@ocaml.deprecated "Use get_name"]

  let file_name = get_file_name

  [@@@ocaml.deprecated "Use get_file_name"]

  let file_path = get_file_path

  [@@@ocaml.deprecated "Use get_file_path"]

  let dot f = Option.get f.dot

  [@@@ocaml.deprecated "Don't use"]

  let dot_opt = get_dot

  [@@@ocaml.deprecated "Use get_dot"]
end

type file = File.t

type func = Func.t

type t = File of file | Func of func

let open_file_direct n p = File (File.open_direct n p)

let open_file_cil l p = File.open_cil l p |> Option.map (fun f -> File f)