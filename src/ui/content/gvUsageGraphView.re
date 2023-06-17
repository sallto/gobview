open Js_of_ocaml
open GoblintCil
let generate_dot_file = (matches,name) =>{
  let inner_graph = (matches |> List.map(((_,loc,_,_))=>"\""++loc.file ++ "\" -- \""++name ++"\"; ") |> List.fold_right((a,b)=>a++b,_,""));
  let result = "graph {" ++ name ++ ";" ++ 
  inner_graph ++ "}";
  result;
};
[@react.component]
let make = (~search: Search.t, ~funcdec:fundec) =>
{ 
    let dot = switch(search.matches){
      | Done(results) => generate_dot_file(results, funcdec.svar.vname)
      | _ => "graph{\""++funcdec.svar.vname++"\";}"
    } |> Js.string;

    <ErrorBoundary
      message={
        "Cannot display the function graph. The generated DOT file is probably too large."
        |> Js.string
      }>
      <Graphviz
        dot={dot}
        options={Js.Unsafe.obj([|
          ("height", Js.Unsafe.inject("100%")),
          ("width", Js.Unsafe.inject("100%")),
          ("zoom", Js.Unsafe.inject(true)),
        |])}
        className={"fun-cfg" |> Js.string}
      />
    </ErrorBoundary>}