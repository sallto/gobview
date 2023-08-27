open Batteries;
module Message = GvMessages.Message;
module Severity = Goblint_lib.Messages.Severity;

[@react.component]
let make = (~display: option(State.display), ~warnings, ~dispatch) => {
  let categories =
    Stream.from(i =>
      if (i >= Severity.min && i < Severity.max) {
        Severity.of_enum(i);
      } else {
        None;
      }
    )
    |> Stream.to_list;
  let by_cat =
    React.useMemo(() =>
      Array.init(List.length(categories), severity =>
        warnings
        |> List.filter(m => {
             Message.severity_to_hash(m) == Severity.min + severity
           })
      )
    );
  let (categories_displayed, set_categories_displayed) =
    React.useState(() => categories);
  let (current_file, set_current_file) = React.useState(() => true);
  <div className="filebox">
    {if (List.length(warnings) == 0) {
       <h2> {"No warnings found!" |> React.string} </h2>;
     } else {
       <>
         <ul>
           <li>
             {categories
              |> List.map(e =>
                   <Button
                     on_click={_ => {
                       set_categories_displayed(cats =>
                         List.exists(n => e == n, cats)
                           ? List.remove(cats, e) : [e, ...cats]
                       )
                     }}
                     class_=[
                       !List.exists(n => e == n, categories_displayed)
                         ? Severity.to_bootstrap_class(e) : "",
                       "mr-3",
                       "btn",
                     ]>
                     {e |> Severity.show |> React.string}
                   </Button>
                 )
              |> React.list}
             <Button
               class_=["btn", current_file ? "alert-dark" : ""]
               on_click={_ => {set_current_file(Bool.neg)}}>
               {"Current File" |> React.string}
             </Button>
           </li>
         </ul>
         <ul>
           {categories_displayed
            |> List.map(Severity.hash)
            |> List.map(Array.get(by_cat))
            |> List.flatten
            |> List.filter(w =>
                 if (!current_file) {
                   true;
                 } else {
                   switch (display) {
                   | Some(d) =>
                     switch (d) {
                     | File(f) =>
                       let b =
                         Message.location(w)
                         |> Option.map_default(
                              GvInspect.Line.of_location %> fst,
                              "",
                            );
                       b == f.path;

                     | _ => true
                     }
                   | _ => true
                   };
                 }
               )
            |> List.map(w =>
                 (
                   Message.severity_to_string(w),
                   Message.to_string(w),
                   Message.location(w),
                   Message.severity_to_bs_alert(w),
                 )
               )
            |> List.mapi((i, (title, text, loc, alert)) => {
                 let onClick =
                   loc
                   |> Option.map((loc, _) =>
                        dispatch @@
                        `InspectLine(GvInspect.Line.of_location(loc))
                      );
                 <li
                   className={"link-like alert " ++ alert}
                   key={string_of_int(i)}
                   ?onClick>
                   <h4 className="alert-heading"> {title |> React.string} </h4>
                   <p> {text |> React.string} </p>
                 </li>;
               })
            |> React.list}
         </ul>
       </>;
     }}
  </div>;
};
