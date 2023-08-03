open Batteries
open Goblint_lib.Messages

module Message = struct
  include Message

  let location { multipiece; _ } =
    let loc (p : Piece.t) = Option.map Location.to_cil p.loc in
    match multipiece with
    | Single p -> loc p
    | Group { pieces; _ } -> pieces |> List.filter_map loc |> List.enum |> Enum.get

  let severity_to_hash m = m.severity |> Severity.hash
  let severity_to_string m = match m.severity with 
    | Error -> "Error"
    | Warning -> "Warning"
    | Info -> "Info"
    | Debug -> "Debug"
    | Success -> "Success"
  let severity_to_bs_alert m = Severity.to_bootstrap_class m.severity

  let to_string msg =
    let out = IO.output_string () in
    let fmt = Format.formatter_of_output out in
    print ~ppf:fmt msg;
    IO.close_out out
    let equal a b = a.multipiece == b.multipiece
end

type t = Message.t list

(* TODO: unused *)
let filter_by_line (file, line) =
  let pred msg =
    match Message.location msg with
    | Some loc when loc.file = file && loc.line = line -> true
    | _ -> false
  in
  List.filter pred
