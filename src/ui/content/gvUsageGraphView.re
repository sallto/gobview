open Js_of_ocaml;
open GoblintCil;
let generate_dot_file = (matches, name) => {
  let inner_graph =
    matches
    |> List.map(((_, loc, _, _)) =>
         "\"" ++ loc.file ++ "\" -- \"" ++ name ++ "\"; "
       )
    |> List.fold_right((a, b) => a ++ b, _, "");
  let result = "graph {" ++ name ++ ";" ++ inner_graph ++ "}";
  result;
};
let rec pp = (rep)=>{
     switch(rep){
      |`Nothing => print_string("Nothing");print_newline();
      |`Value(s) => print_string("Value: "++s);print_newline();
      |`List(l)=>print_string("List");print_newline();List.iter(pp,l);
      |`Assoc([])=>print_string("EmptyASSOC");print_newline();
      |`Assoc(l) => List.iter(((str,t))=>{print_string("ASSOC: "++str);print_newline();pp(t);},l);
      |`Tagged(str,r)=> print_string("Tagged: "++str);print_newline();pp(r);
      |`Pair(r1,r2)=>print_string("Pair");print_newline();pp(r1);pp(r2);
    };
};
[@react.component]
let make = (~graph, ~ctx: option(Representation.t)) => {
  /*Hashtbl.iter((_,(_,ctx))=>
    switch(ctx){
      |`Nothing => print_string("Nothing");print_newline();
      |`Value(s) => print_string("Value: "++s);print_newline();
      |`List(_)=>print_string("List");print_newline();
      |`Assoc([(str,_),..._]) => print_string("ASSOC: "++str);print_newline();
      |`Assoc([])=>print_string("EmptyASSOC");print_newline();
      |`Tagged(str,_)=> print_string("Tagged: "++str);print_newline();
      |`Pair(_,_)=>print_string("Pair");print_newline();
    },graph);*/
  let map_to_graph  =(map) => Hashtbl.fold( (x, (y, _), carry) =>
    carry
    ++ "\n\""
    ++ y.svar.vname
    ++ "\""
    ++ " -> \""
    ++ x.svar.vname
    ++ "\";",map,"digraph{") ++ "}";
  let listOfNames =
    switch (ctx) {
    | None =>
      print_string("None\n");
      map_to_graph(graph);
    | Some(c) =>
          print_string(Yojson.Safe.pretty_to_string(Representation.to_yojson(c)));print_newline();
      let copy = Hashtbl.copy(graph);
      Hashtbl.filter_map_inplace(
        (_, (_, f_c) as a) =>{
          print_string(Yojson.Safe.pretty_to_string(Representation.to_yojson(f_c)));print_newline();
          if (f_c == c) {
            Some(a);
          } else {
            None;
          }},
        copy,
      );
      map_to_graph(copy);
    };
  print_string(listOfNames);
  print_newline();

  <ErrorBoundary
    message={
      "Cannot display the function graph. The generated DOT file is probably too large."
      |> Js.string
    }>
    <Graphviz
      dot={listOfNames |> Js.string}
      options={Js.Unsafe.obj([|
        ("height", Js.Unsafe.inject("100%")),
        ("width", Js.Unsafe.inject("100%")),
        ("zoom", Js.Unsafe.inject(true)),
      |])}
      className={"fun-cfg" |> Js.string}
    />
  </ErrorBoundary>;
};
