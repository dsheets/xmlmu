open Cmdliner

(* TODO: doc *)
(* TODO: URI? *)
let in_file = Arg.(
  value & pos 0 (some string) None & info []
    ~docv:"INPUT_FILE"
    ~doc:"The file to parse")

(* TODO: mod the options? *)
let mu ?sdocs ?man ?docs ?doc ?version ?(name="mu") exec =
  Term.(pure exec $ in_file),
  Term.info ?sdocs ?man ?docs ?doc ?version name

let input_of_file_option = function
  | None -> Xmlmu.input_of_in_channel stdin
  | Some filename -> Xmlmu.input_of_file filename

(*
mu subcommand
-i xml/bin
-o xml/bin
-ocaml pipe src
anon arg file

prov filter is filter
template render is filter
prov render is filter
html output is filter
*)
