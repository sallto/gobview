open Batteries;
open Str;
module SelectedSidebar = State.SelectedSidebar;


[@react.component]
let make = (~results: list((string, Representation.t)),~dispatch) => {
  //let on_click = () => dispatch @@ `StartPerformSearch;
  <ul className="list-group list-group-flush">
    {results
     |> List.mapi((i, (n, r)) => {
          <li key={string_of_int(i)} className="list-group-item">
    
            <div style={React.Dom.Style.make(~fontWeight="bold", ())}>
              {n |> React.string}  {if(string_match(regexp("fundec:\(.*\)"),n,0)){
        let name= matched_group(1,n);
        let set_sidebar= 
        ()=> dispatch@@`SwitchSidebarLeft(SelectedSidebar.Search);
        let set_search_mode=
        () => dispatch@@`UpdateSearchMode(Search.Json);
        let set_query=
          ()=>dispatch@@`ParseSearchQuery("{\"kind\":[\"fun\"],\"target\":[\"name\",\""++name++"\"],\"find\":[\"uses\"],\"expression\":\"\",\"mode\":[\"Must\"]}");
          let perform_search=
          () =>{ dispatch @@`StartPerformSearch;
          dispatch@@`PerformSearch;
          };

        <span className="link-like text-muted" onClick={_=>{
          set_sidebar();
          set_search_mode();
          set_query();
          perform_search()
        }}
        >
          {React.string(" Find Usages")}
        </span>
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
