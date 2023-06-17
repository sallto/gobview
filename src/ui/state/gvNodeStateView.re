open Batteries;
open GoblintCil;
open Str;

let make_single = (ctx, path) => {
  <CollapsibleList collapsed=false override_class=[]>
    <CollapsibleListItem name="Context" override_class=[]>
      <GvRepresentationView represent=ctx />
    </CollapsibleListItem>
    <CollapsibleListItem name="Path" override_class=[]>
      <GvRepresentationView represent=path />
    </CollapsibleListItem>
  </CollapsibleList>;
};

[@react.component]
let make = (~goblint, ~inspect, ~cil, ~dispatch) =>
  switch (inspect) {
  | None => React.null
  | Some(inspect) =>{
    <CollapsibleList collapsed=false style=`Flush>
      {goblint#local_analyses(inspect)
       |> List.group(((id, _), (id', _)) => String.compare(id, id'))
       |> List.map(
            fun
            | [(id, (ctx, path))] =>
            {
              
              <CollapsibleListItem name={"Node: " ++ id}>
              {switch(List.find_map_opt((x)=> switch (x){
               | GFun(fundec,_) when String.length(id) > 3 && string_of_int(fundec.svar.vid) == string_after(id,3) => Some(fundec)
                | _ => None},cil)){
              | Some(fundec)=><FindUsageButton dispatch fundec />
              | None => <div/>
                }}
                {make_single(ctx, path)
                }
                
              </CollapsibleListItem>
            }
            | [(id, _), ..._] as group =>
              <CollapsibleListItem name={"Node: " ++ id}>
                <CollapsibleList style=`Flush>
                  {group
                   |> List.mapi((i, (_, (ctx, path))) =>
                        <CollapsibleListItem
                          name={"Tuple: " ++ string_of_int(i)}>
                          {make_single(ctx, path)}
                        </CollapsibleListItem>
                      )
                   |> React.list}
                </CollapsibleList>
              </CollapsibleListItem>
            | _ => failwith("List.group returned an empty group"),
          )
       |> React.list}
    </CollapsibleList>}
  };
