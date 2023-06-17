open Batteries;
module Message = GvMessages.Message;

[@react.component]
let make = (~warnings, ~dispatch) => {
  <div className="filebox">
    {if (List.length(warnings) == 0) {
      <h2>
        {"No warnings found!" |> React.string}
      </h2>
    } else {
      <ul>
        {warnings
        |> List.map(w => (Message.severity_to_string(w),Message.to_string(w), Message.location(w), Message.severity_to_bs_alert(w)))
        |> List.mapi((i, (title,text, loc, alert)) => {
              let onClick =
                loc
                |> Option.map((loc, _) =>
                    dispatch @@ `InspectLine(GvInspect.Line.of_location(loc))
                  );
              <li className={"link-like alert " ++ alert} key={string_of_int(i)} ?onClick>
                <h4 className={"alert-heading"}> {title |> React.string} </h4>
                <p>{text |> React.string}</p>
              </li>;
            })
        |> React.list}
      </ul>
    }}
  </div>;
};
