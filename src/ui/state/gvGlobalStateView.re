open Batteries;

[@react.component]
let make = (~analyses, ~dispatch) => {
  <CollapsibleList style=`Flush>
    {analyses
     |> List.map(((n, results)) => {
          <CollapsibleListItem name=n>
            <GvAnalysesView results dispatch />
          </CollapsibleListItem>
        })
     |> React.list}
  </CollapsibleList>;
};
