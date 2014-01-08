val mu :
  ?sdocs:string ->
  ?man:Cmdliner.Manpage.block list ->
  ?docs:string ->
  ?doc:string ->
  ?version:string ->
  ?name:string ->
  (string option -> 'a) -> 'a Cmdliner.Term.t * Cmdliner.Term.info

val input_of_file_option : string option -> Xmlmu.input
