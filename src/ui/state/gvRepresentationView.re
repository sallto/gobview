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
type value =
  | Exact(string, int)
  | Interval(string, int, int)
  | Exclusion(string, int, int, list(int));

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
  let equalities_to_entries = l =>
    switch (l) {
    | `List(equalities) =>
      equalities
      |> List.find_map_opt(
           fun
           // find int values
           | `Value(v) => v |> int_of_string_opt
           | _ => None,
         )
      |> Option.map_default(
           value =>
             equalities
             |> List.filter_map(
                  fun
                  | `Value(name) when name != string_of_int(value) =>
                    Some(name)
                  | _ => None,
                )
             |> List.map(name => Exact(name, value)),
           [],
         )
    | _ => []
    };

  switch (var_eq) {
  | `List(l) => l |> List.map(equalities_to_entries) |> List.concat
  | _ => []
  };
};
let interval_to_entries = (interval: Representation.t) => {
  let expressions_to_entries =
    fun
    | (name, vals) => {
        switch (vals) {
        | `List(val_list) =>
          val_list
          |> List.filter_map(
               fun
               | `Value(v) => {
                   let interval_regex = Str.regexp("\[\\(.+\\),\\(.+\\)\]");
                   let exc_regex =
                     Str.regexp(
                       "Not {\\(.+\\)}(\[\\([-0-9]+\\),\\([-0-9]+\\)\])",
                     );
                   print_string(v);
                   print_newline();
                   if (Str.string_match(interval_regex, v, 0)) {
                     print_endline(Str.matched_group(1, v));
                     print_endline(Str.matched_group(2, v));
                     print_newline();
                     Some(
                       Interval(
                         name,
                         int_of_string(Str.matched_group(1, v)),
                         int_of_string(Str.matched_group(2, v)),
                       ),
                     );
                   } else if (Str.string_match(exc_regex, v, 0)) {
                     String.split_on_char(',', Str.matched_group(1, v))
                     |> List.remove(_, "")
                     |> List.iter(print_endline);
                     print_endline(Str.matched_group(2, v));
                     print_endline(Str.matched_group(3, v));
                     print_newline();
                     let exp = l =>
                       if (l >= 31) {
                         Int.max_num;
                       } else if (l <= (-31)) {
                         Int.min_num;
                       } else {
                         l |> abs |> Int.pow(2) |> Int.copysign(l);
                       };

                     Some(
                       Exclusion(
                         name,
                         int_of_string(Str.matched_group(2, v)) |> exp,
                         int_of_string(Str.matched_group(3, v)) |> exp,
                         String.split_on_char(',', Str.matched_group(1, v))
                         |> List.remove(_, "")
                         |> List.map(int_of_string),
                       ),
                     );
                   } else {
                     int_of_string_opt(v) |> Option.map(v => Exact(name, v));
                   };
                 }
               | _ => None,
             )
        | _ => []
        };
      };
  let ll =
    switch (interval) {
    | `Assoc(l) => List.map(expressions_to_entries, l)
    | _ => []
    };
  ll |> List.concat;
};
let value_to_pair =
  fun
  | Exact(n, v) => (n, string_of_int(v))
  | Interval(n, l, u) => (
      n,
      "[" ++ string_of_int(l) ++ "," ++ string_of_int(u) ++ "]",
    )
  // TODO:
  | Exclusion(n, l, u, exc) => (
      n,
      "["
      ++ string_of_int(l)
      ++ ","
      ++ string_of_int(u)
      ++ "] \\ "
      ++ "{"
      ++ (exc |> List.map(string_of_int) |> String.concat(","))
      ++ "}",
    );

[@react.component]
let make = (~represent: Representation.t) => {
  let var_eq = find_node(represent, "var_eq");
  let values =
    switch (var_eq) {
    | Some(`Tagged("var_eq", v)) => var_eq_to_values(v)
    | _ => []
    };

  // TODO: potential different matches
  let interval = find_node(represent, "value domain");
  let values =
    switch (interval) {
    | Some(`Tagged("value domain", v)) => values @ interval_to_entries(v)
    | _ => values
    };
  let cmp = curry(Tuple2.mapn(value_to_pair %> fst) %> uncurry(compare));

  let values =
    values
    |> List.group(cmp)
    |> List.map(l =>
         List.fold(
           ((_, lower, upper, exc), value) => {
             switch (value) {
             | Exact(n, v) => (n, v, v, [])
             | Interval(n, l, u) => (
                 n,
                 Int.max(l, lower),
                 Int.min(u, upper),
                 exc,
               )
             | Exclusion(n, l, u, e) => (
                 n,
                 Int.max(l, lower),
                 Int.min(u, upper),
                 exc @ e |> List.unique,
               )
             }
           },
           ("", min_int, max_int, []),
           l,
         )
       )
    |> List.map(((name, lower, upper, exc)) =>
         if (lower == upper) {
           Exact(name, lower);
         } else if (List.is_empty(exc)) {
           Interval(name, lower, upper);
         } else {
           Exclusion(name, lower, upper, exc);
         }
       );
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
          |> List.map(value_to_pair)
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
