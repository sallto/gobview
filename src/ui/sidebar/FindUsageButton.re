open Batteries;
module SelectedSidebar = State.SelectedSidebar;
open GoblintCil;
[@react.component]
let make = (~dispatch, ~fundec: fundec) => {
        let set_sidebar= 
        ()=> dispatch@@`SwitchSidebarLeft(SelectedSidebar.Search);
        let set_search_mode=
        () => dispatch@@`UpdateSearchMode(Search.Json);
        let set_query=
          ()=>dispatch@@`ParseSearchQuery("{\"kind\":[\"fun\"],\"target\":[\"name\",\""++fundec.svar.vname++"\"],\"find\":[\"uses\"],\"expression\":\"\",\"mode\":[\"Must\"]}");
          let perform_search=
          () =>{ dispatch @@`StartPerformSearch;
          dispatch@@`PerformSearch;
          };
        <span className="link-like text-muted" onClick={_=>{
          set_sidebar();
          set_search_mode();
          set_query();
          perform_search();
        }}
        >
          {React.string(" Find Usages")}
        </span>;
}