open Batteries;

[@react.component]
let make =
    (
      ~name=?,
      ~collapsed=?,
      ~override_class=?,
      ~url=?,
      ~class_=?,
      ~on_click=?,
      ~callback_data=?,
      ~on_toggle=?,
      ~children=?,
    ) => {
  let (name, collapsed, override_class, on_toggle, children) =
    Utils.fix_opt_args5(name, collapsed, override_class, on_toggle, children);
  let (url, class_, on_click, callback_data) =
    Utils.fix_opt_args4(url, class_, on_click, callback_data);
  let name = Option.default("", name);
  let collapsed = Option.default(true, collapsed);
  let children = Option.default(React.null, children);

  let className =
    switch (override_class) {
    | Some(l) => String.concat(" ", l)
    | _ => "list-group-item"
    };
  let class_ =
    switch (class_) {
    | Some(c) => c
    | _ => ["text-link", "text-dark"]
    };

  <li className>
    <div>
      {switch (url, on_click, callback_data) {
       | (None, None, None) => name |> React.string
       | _ =>
         <Link ?url class_ ?on_click ?callback_data>
           {name |> React.string}
         </Link>
       }}
      <Button
        class_=["btn", "btn-sm", "dropdown-toggle"]
        color=`None
        on_click=?on_toggle>
        React.null
      </Button>
    </div>
    {if (collapsed) {React.null} else {children}}
  </li>;
};
