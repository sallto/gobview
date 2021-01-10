// jsoo-react seems to have some trouble with
// non-mandatory component properties. This is a
// workaround until the issue is resolved.
let make_extras =
    (
      ~type_=`Text,
      ~class_=["form-control"],
      ~from_string=s => s,
      ~to_string=s => s,
      (),
    ) => {
  (type_, class_, from_string, to_string);
};

[@react.component]
let make = (~value, ~on_change, ~extras) => {
  let (type_, class_, from_string, to_string) = extras;

  let type_ =
    switch (type_) {
    | `Text => "text"
    };

  let className = String.concat(" ", class_);

  let value = to_string(value);

  let onChange = ev => {
    React.Event.Synthetic.preventDefault(ev);
    React.Event.Synthetic.target(ev)
    |> Ojs.get(_, "value")
    |> Ojs.string_of_js
    |> from_string
    |> on_change;
  };

  <input type_ className value onChange />;
};