open Batteries;
open Str;
module SelectedSidebar = State.SelectedSidebar;
open GoblintCil;

[@react.component]
let make = (~results: list((string, Representation.t)),~dispatch, ~cil) => {
  //let on_click = () => dispatch @@ `StartPerformSearch;
  <ul className="list-group list-group-flush">
    {results
     |> List.mapi((i, (n, r)) => {
          <li key={string_of_int(i)} className="list-group-item">
    
            <div style={React.Dom.Style.make(~fontWeight="bold", ())}>
              {n |> React.string}  {if(string_match(regexp("fundec:\(.*\)"),n,0)){
        List.find_map(
          (e)=>{switch(e){
            | GFun(fundec,_)=> Some(<FindUsageButton dispatch fundec />)
            | _ => None
          }},cil)
      }else{
        {React.string("")}
      }}
            </div>
            <GvRepresentationView represent=r />
     
          </li>
        })
     |> React.list}
  </ul>;
};
