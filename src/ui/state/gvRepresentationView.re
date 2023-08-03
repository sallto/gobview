open Batteries;

let rec is_simple = r =>
  switch (r) {
  | `List([]) => true
  | `List([r]) => is_simple(r)
  | `Tagged(_, r) => is_simple(r)
  | _ => Representation.is_simple(r)
  };

let rec make_rec = represent =>
  switch (represent) {
  | `Nothing => "No representation available" |> React.string
  | `Value(s) => s |> React.string
  | `Pair(a, b) => make_pair(a, b)
  | `List(l) => make_list(l)
  | `Assoc(l) => make_assoc(l)
  | `Tagged("bot", `Value(s))
  | `Tagged("top", `Value(s)) => make_bot_top(s)
  | `Tagged("set", `List(l)) => make_set(l)
  | `Tagged("intervals", `Pair(`Value(x), `Value(y))) =>
    "[" ++ x ++ ", " ++ y ++ "]" |> React.string
  | `Tagged(_, r) => make_rec(r)
  | _ => "Unexpected value" |> React.string
  }

and make_pair = (a, b) =>
  if (Representation.is_simple(a) && Representation.is_simple(b)) {
    <>
      {"(" |> React.string}
      {make_rec(a)}
      {", " |> React.string}
      {make_rec(b)}
      {")" |> React.string}
    </>;
  } else {
    <ol> <li> {make_rec(a)} </li> <li> {make_rec(b)} </li> </ol>;
  }

and make_assoc = l =>
  <ul>
    {l
     |> List.mapi((i, (n, r)) => {
          <li key={string_of_int(i)}>
            {switch (r) {
             | `Nothing => n |> React.string
             | _ as r =>
               if (is_simple(r)) {
                 <>
                   <span style={React.Dom.Style.make(~fontWeight="bold", ())}>
                     {n ++ ": " |> React.string}
                   </span>
                   {make_rec(r)}
                 </>;
               } else {
                 <>
                   <div style={React.Dom.Style.make(~fontWeight="bold", ())}>
                     {n ++ ":" |> React.string}
                   </div>
                   {make_rec(r)}
                 </>;
               }
             }}
          </li>
        })
     |> React.list}
  </ul>

and make_list = rs =>
  switch (rs) {
  | [] => "[]" |> React.string
  | [r] => make_rec(r)
  | _ =>
    <ol>
      {rs
       |> List.map(make_rec)
       |> List.mapi((i, elt) => {<li key={string_of_int(i)}> elt </li>})
       |> React.list}
    </ol>
  }

and make_bot_top = s => {
  let style =
    if (List.mem(s, ["⊥", "⊤"])) {
      React.Dom.Style.make();
    } else {
      React.Dom.Style.make(~fontStyle="italic", ());
    };
  <span style> {s |> React.string} </span>;
}

and make_set = l =>
  switch (l) {
  | [] => "∅" |> React.string
  | _ when List.for_all(is_simple, l) =>
    l
    |> List.map(make_rec)
    |> List.interleave(
         ~first=React.string("{"),
         ~last=React.string("}"),
         React.string(", "),
       )
    |> React.list
  | _ => make_list(l)
  };
let rec find_node = (represent: Representation.t, name) => {
  switch (represent) {
  | `Tagged(n, _) when n == name => Some(represent)
  | `Tagged(_, child) => find_node(child, name)
  | `Assoc(list) =>
    switch (List.find_opt(((n, _)) => n == name, list)) {
    | Some((n, v)) => Some(`Tagged((n, v)))
    | None => List.find_map_opt(((_, v)) => find_node(v, name), list)
    }
  | `Pair(a, b) =>
    let first = find_node(a, name);
    if (first == None) {
      find_node(b, name);
    } else {
      first;
    };
  | `List(list) => List.find_map_opt(find_node(_, name), list)
  | `Value(_) => None
  | `Nothing => None
  };
};
let cmp_value = (a, b) => {
  let conditions = [
    String.contains(_, '['),
    v => Str.string_match(Str.regexp(".*Not.*"), v, 0),
    v => Str.string_match(Str.regexp(".*Unknown.*"), v, 0),
  ];

  let val_to_int = v =>
    conditions |> List.map(f => v |> f) |> List.map(b => b ? 1 : 0) |> List.sum;

  val_to_int(a) - val_to_int(b);
};
let var_eq_to_values = (var_eq: Representation.t) => {
  let values = Hashtbl.create(10);
  let equalities_to_entries = l =>
    switch (l) {
    | `List(equalities) =>
      equalities
      |> List.find_map_opt(
           fun
           // find int values
           | `Value(v) => v |> int_of_string_opt |> Option.map(string_of_int)
           | _ => None,
         )
      |> Option.may(value =>
           equalities
           |> List.filter_map(
                fun
                | `Value(name) when name != value => Some(name)
                | _ => None,
              )
           |> List.iter(Hashtbl.add(values, _, value))
         )
    | _ => ()
    };

  switch (var_eq) {
  | `List(l) => l |> List.iter(equalities_to_entries)
  | _ => ()
  };
  values;
};

let interval_to_entries = (interval: Representation.t) => {
  let values = Hashtbl.create(10);
  let expressions_to_entries =
    fun
    | (name, vals) => {
        switch (vals) {
        | `List(val_list) =>
          val_list
          |> List.iter(
               fun
               | `Value(value) =>
                 switch (Hashtbl.find_option(values, name)) {
                 | Some(old_val) =>
                   if (cmp_value(old_val, value) > 0) {
                     Hashtbl.replace(values, name, value);
                   }
                 | None => Hashtbl.add(values, name, value)
                 }
               | _ => (),
             )
        | _ => ()
        };
      };
  switch (interval) {
  | `Assoc(l) => List.iter(expressions_to_entries, l)
  | _ => ()
  };
  values;
};
[@react.component]
let make = (~represent: Representation.t) => {
  let values = Hashtbl.create(10);
  let merge = a =>
    Hashtbl.merge((_, v1, v2) => {v1 == None ? v2 : v1}, values, a);
  let var_eq = find_node(represent, "var_eq");
  let values =
    switch (var_eq) {
    | Some(`Tagged("var_eq", v)) => merge(var_eq_to_values(v))
    | _ => values
    };
  // TODO: potential different matches
  let interval = find_node(represent, "value domain");
  let values =
    switch (interval) {
    | Some(`Tagged("value domain", v)) => merge(interval_to_entries(v))
    | _ => values
    };
  <>
    {<table className="table">
       <thead>
         <tr>
           <th scope="col"> {"Name" |> React.string} </th>
           <th scope="col"> {"Expression" |> React.string} </th>
         </tr>
       </thead>
       <tbody>
         {values
          |> Hashtbl.to_list
          |> List.map(((k, v)) => {
               <tr>
                 <td> {k |> React.string} </td>
                 <td> {v |> React.string} </td>
               </tr>
             })
          |> React.list}
       </tbody>
     </table>}
    {make_rec(represent)}
  </>;
};
