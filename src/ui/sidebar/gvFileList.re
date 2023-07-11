open Batteries;
open GoblintCil;

let make_func_list = (file, funcs, dispatch) => {
  let on_click = (data, _) => Option.may(dispatch, data);
  <ul>
    {funcs
     |> List.mapi((i, func) => {
          <li
            key={"inner" ++ string_of_int(i)}
            className="list-group-item px-5">
            <Link
              on_click
              callback_data={`DisplayFunc((func, file))}
              class_=["text-link"]>
              {func |> React.string}
            </Link>
          </li>
        })
     |> React.list}
  </ul>;
};

type tree =
  | File(string)
  | Directory(string, list(tree));

let rec make_inner = (tree, on_click, files, prefix, dispatch) => {
  let (current_path, l) =
    switch (tree) {
    | Directory(p, l) => (p, l)
    | File(s) => (s, [])
    };

  let path =
    if (prefix == "") {
      current_path;
    } else {
      prefix ++ "/" ++ current_path;
    };

  <CollapsibleList >
    {if (!List.is_empty(l)) {
       <CollapsibleListItem name=current_path>
         {l
          |> List.map(_e => make_inner(_e, on_click, files, path, dispatch))
          |> React.list}
       </CollapsibleListItem>;
     } else {
       <CollapsibleListItem
         on_click
         callback_data={`DisplayFile(path)}
         class_=["text-link"]
         name=current_path>
         {make_func_list(path, Hashtbl.find_all(files, path), dispatch)}
       </CollapsibleListItem>;
     }}
  </CollapsibleList>;
};
let split_first_directory = path =>
  if (String.contains(path, '/')) {
    String.split(path, ~by="/");
  } else {
    (path, "");
  };
let rec make_tree = (prefix, paths) =>
  if (paths |> List.remove_all(_, "") |> List.is_empty) {
    File(prefix);
  } else {
    let list =
      paths
      |> List.map(split_first_directory)
      |> List.fold_left(
           (map, (new_prefix, entry)) => {
             Map.update_stdlib(
               new_prefix,
               fun
               | None => Some([entry])
               | Some(lst) => Some([entry, ...lst]),
               map,
             )
           },
           Map.empty,
         )
      |> Map.enum
      |> Enum.map(((k, l)) => make_tree(k, l))
      |> List.of_enum;

    Directory(prefix, list);
  };
let rec compact_tree = t => {
  let flattened_tree =
    switch (t) {
    | Directory(p, [e]) =>
      switch (e) {
      | Directory(i, l) =>
        Directory(p ++ "/" ++ i, l |> List.map(compact_tree))
      | File(i) => File(p ++ "/" ++ i)
      }
    | Directory(p, l) => Directory(p, l |> List.map(compact_tree))
    // File entries cannot be flattened
    | file => file
    };
  // The new directory nodes that resulted from the flatten step might need to be flattened again
  if (flattened_tree != t) {
    compact_tree(flattened_tree);
  } else {
    flattened_tree;
  };
};

[@react.component]
let make = (~cil: Cil.file, ~dispatch) => {
  let files = Hashtbl.create(64);
  Cil.iterGlobals(
    cil,
    fun
    | GFun(fdec, loc) => Hashtbl.add(files, loc.file, fdec.svar.vname)
    | _ => (),
  );
  let tree =
    make_tree("", files |> Hashtbl.keys |> List.of_enum) |> compact_tree;

  let on_click = (data, _) => Option.may(dispatch, data);

  let list_of_files =
    switch (tree) {
    | Directory(_, l) => l
    | f => [f]
    };

  <>
    {list_of_files
     |> List.map(file => make_inner(file, on_click, files, "", dispatch))
     |> React.list}
  </>;
};
