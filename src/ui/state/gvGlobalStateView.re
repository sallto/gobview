open Batteries;

[@react.component]
let make = (~analyses, ~dispatch, ~cil) => {
  <CollapsibleList style=`Flush>
    {analyses
     |> List.map(((n, results)) => {
          <CollapsibleListItem name=n>
            <GvAnalysesView results dispatch cil />
          </CollapsibleListItem>
        })
     |> React.list}
  </CollapsibleList>;
};
