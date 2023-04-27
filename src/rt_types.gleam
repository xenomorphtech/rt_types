import gleam/result
import gleam/dynamic
import gleam/io
import gleam/erlang/atom
import gleam/list
import rt_types/value_type 
import gleam/string
import gleam/map
import gleam/option

pub type Field {
  Field(atom_name: option.Option(atom.Atom), type_: value_type.ValueType)
}

pub type ConstructorNative {
  Constructor(record_atom: atom.Atom, string_name: String, fields: List(Field))
}

pub type TypeInfo {
  TypeInfo(typename: String, constructors: List(ConstructorNative))
  ExternalType(typename: String)
}

pub type GleamInfo {
  GleamInfo(version: Int, types: map.Map(String, TypeInfo))
}

external fn fetch_type_info_native(m) -> GleamInfo =
  "rt_types_erl" "fetch_type_info"

external fn is_atom(m) -> Bool =
  "erlang" "is_atom"

pub fn fetch_type_info(
  module_name: atom.Atom,
  b: String,
) -> Result(TypeInfo, Nil) {
  let info: GleamInfo = fetch_type_info_native(module_name)

  use t <- result.then(map.get(info.types, b))

  Ok(t)
}

pub fn to_json(
  module_name: atom.Atom,
  tname: String,
  value: a,
) -> Result(String, Nil) {
  use tinfo <- result.then(fetch_type_info(module_name, tname))
}

pub fn from_json(
  module_name: atom.Atom,
  tname: String,
  value: String,
) -> Result(a, Nil) {
  use tinfo <- result.then(fetch_type_info(module_name, tname))
}

pub fn show(
  typeinfo: value_type.ValueType,
  value: a,
  labeled: Bool,
) -> Result(String, String) {
  case typeinfo {
    value_type.String -> {
      let s = format("~p", [value])

      let s = string.slice(from: s, at_index: 2, length: string.length(s) - 4)

      Ok(s)
    }
    value_type.BitString -> {
      let r = format("~p", [value])

      Ok(r)
    }
    value_type.Int -> {
      let r = format("~p", [value])

      Ok(r)
    }
    value_type.Float -> {
      let r = format("~p", [value])

      Ok(r)
    }
    value_type.List(typeinfo) -> {
      let items =
        list.map(
          dynamic.unsafe_coerce(dynamic.from(value)),
          fn(v) { show(typeinfo, v, labeled) },
        )
      use items <- result.then(result.all(items))
      Ok(string.join(["[", string.join(items, ", "), "]"], ""))
    }
    value_type.Custom(a, b, args) -> {
      //io.debug(args)
      show_custom(a, b, args, value, labeled)
      |> result.then(fn(r) { Ok(r) })
      |> result.map_error(fn(_x) { "Failed to show substruct" })
    }
    _ -> {
      io.debug(typeinfo)
      todo
    }
  }
}

pub fn show_custom(
  module_name: atom.Atom,
  tname: String,
  args: List(value_type.ValueType),
  value: a,
  labeled: Bool,
) -> Result(String, String) {
  use tinfo <- result.then(
    fetch_type_info(module_name, tname)
    |> result.map_error(fn(_x) { "failed to fetch type info from module" }),
  )

  case tinfo {
    TypeInfo(..) -> show_custom_tf(module_name, tinfo, args, value, labeled)
    ExternalType(name) -> show_custom_external(name, dynamic.from(value))
  }
}

pub fn show_custom_external(
  name: String,
  value: dynamic.Dynamic,
) -> Result(String, String) {
  case name {
    "Atom" ->
      Ok(string.join(
        [
          "atom.create_from_string(\"",
          atom.to_string(dynamic.unsafe_coerce(value)),
          "\")",
        ],
        "",
      ))
    _ -> Ok(string.join(["#(external_type: ", name, ")"], ""))
  }
}

pub fn show_custom_tf(
  module_name: atom.Atom,
  tinfo: TypeInfo,
  args: List(value_type.ValueType),
  value: a,
  labeled: Bool,
) -> Result(String, String) {
  let assert TypeInfo(constructors: tinfo_constructors, ..) = tinfo

  //io.debug(tinfo)

  let res = case is_atom(value) {
    True -> {
      Ok(dynamic.unsafe_coerce(dynamic.from(value)))
    }
    False -> {
      let dvalue = dynamic.from(value)
      let dyn = dynamic.element(of: dynamic.dynamic, at: 0)
      use res <- result.then(dyn(dvalue))
      Ok(dynamic.unsafe_coerce(dynamic.from(res)))
    }
  }
  let res =
    res
    |> result.map_error(fn(_x) { "failed to grab constructor name" })

  use constructor_of_a <- result.then(res)

  //locate the right constructor
  use constructor <- result.then(
    list.find(
      tinfo_constructors,
      fn(x) {
        //io.debug(#(x.record_atom, constructor_of_a))
        x.record_atom == constructor_of_a
      },
    )
    |> result.then(fn(c) {
      //io.debug(c)

      Ok(c)
    })
    |> result.map_error(fn(_x) { "failed to match constructor name" }),
  )

  //io.debug(constructor.fields)

  use #(_count, res) <- result.then(list.fold(
    constructor.fields,
    Ok(#(1, [])),
    fn(acc, f) {
      use #(index, val) <- result.then(acc)

      let dyn = dynamic.element(of: dynamic.dynamic, at: index)
      use field_val <- result.then(
        dyn(dynamic.from(value))
        |> result.map_error(fn(_x) { "failed to fetch element of value" }),
      )

      //io.debug(f)

      let Field(name, typ) = f

      let assert Ok(typ) = case typ {
        value_type.VarType(a) -> list.at(args, a)
        _ -> Ok(typ)
      }

      use r <- result.then(show(typ, field_val, labeled))
      let r =
        show_label(name)
        |> string.append(r)

      Ok(#(index + 1, [r, ..val]))
    },
  ))

  //io.debug(res)

  case constructor.fields {
    [] ->
      Ok(string.join(
        [atom.to_string(module_name), ".", constructor.string_name],
        "",
      ))

    _ ->
      Ok(string.join(
        [
          atom.to_string(module_name),
          ".",
          constructor.string_name,
          "(",
          string.join(list.reverse(res), ", "),
          ")",
        ],
        "",
      ))
  }
}

pub fn show_label(name: option.Option(atom.Atom)) {
  case name {
    option.Some(a) -> {
      atom.to_string(a)
      |> string.append(": ")
    }

    option.None -> ""
  }
}

external fn format(a, b) -> String =
  "io_lib" "format"

pub fn read(
  module_name: atom.Atom,
  tname: String,
  value: String,
) -> Result(a, Nil) {
  use tinfo <- result.then(fetch_type_info(module_name, tname))
  //parse a gleam value
}

pub fn to_elixir(
  typeinfo: value_type.ValueType,
  value: a,
) -> Result(dynamic.Dynamic, String) {
  case typeinfo {
    value_type.String -> {
      Ok(dynamic.from(value))
    }
    value_type.BitString -> {
      Ok(dynamic.from(value))
    }
    value_type.Int -> {
      Ok(dynamic.from(value))
    }
    value_type.Float -> {
      Ok(dynamic.from(value))
    }
    value_type.List(typeinfo) -> {
      let items =
        list.map(
          dynamic.unsafe_coerce(dynamic.from(value)),
          fn(v) { to_elixir(typeinfo, v) },
        )
      use items <- result.then(result.all(items))
      Ok(dynamic.from(items))
    }
    value_type.Custom(a, b, args) -> {
      to_elixir_custom(a, b, args, value)
      |> result.then(fn(r) { Ok(r) })
      |> result.map_error(fn(_x) { "Failed to show substruct" })
    }
    _ -> {
      io.debug(typeinfo)
      todo
    }
  }
}

pub fn to_elixir_custom(
  module_name: atom.Atom,
  tname: String,
  args: List(value_type.ValueType),
  value: a,
) -> Result(dynamic.Dynamic, String) {
  use tinfo <- result.then(
    fetch_type_info(module_name, tname)
    |> result.map_error(fn(_x) { "failed to fetch type info from module" }),
  )

  case tinfo {
    TypeInfo(..) -> to_elixir_tf(module_name, tinfo, args, value)
    ExternalType(name) -> to_elixir_external(name, dynamic.from(value))
  }
}

pub fn to_elixir_external(
  name: String,
  value: dynamic.Dynamic,
) -> Result(dynamic.Dynamic, String) {
  case name {
    "Atom" -> Ok(value)
    _ -> Error("unsupported external type")
  }
}

pub fn to_elixir_tf(
  module_name: atom.Atom,
  tinfo: TypeInfo,
  args: List(value_type.ValueType),
  value: a,
) -> Result(dynamic.Dynamic, String) {
  let assert TypeInfo(constructors: tinfo_constructors, ..) = tinfo

  //io.debug(tinfo)

  let res = case is_atom(value) {
    True -> {
      Ok(dynamic.unsafe_coerce(dynamic.from(value)))
    }
    False -> {
      let dvalue = dynamic.from(value)
      let dyn = dynamic.element(of: dynamic.dynamic, at: 0)
      use res <- result.then(dyn(dvalue))
      Ok(dynamic.unsafe_coerce(dynamic.from(res)))
    }
  }
  let res =
    res
    |> result.map_error(fn(_x) { "failed to grab constructor name" })

  use constructor_of_a <- result.then(res)

  //locate the right constructor
  use constructor <- result.then(
    list.find(
      tinfo_constructors,
      fn(x) {
        //io.debug(#(x.record_atom, constructor_of_a))
        x.record_atom == constructor_of_a
      },
    )
    |> result.then(fn(c) {
      //io.debug(c)

      Ok(c)
    })
    |> result.map_error(fn(_x) { "failed to match constructor name" }),
  )

  //io.debug(constructor.fields)

  let x: map.Map(atom.Atom, dynamic.Dynamic) = map.new()

  use #(_count, res) <- result.then(list.fold(
    constructor.fields,
    Ok(#(1, x)),
    fn(acc, f) {
      use #(index, val) <- result.then(acc)

      let dyn = dynamic.element(of: dynamic.dynamic, at: index)
      use field_val <- result.then(
        dyn(dynamic.from(value))
        |> result.map_error(fn(_x) { "failed to fetch element of value" }),
      )

      //io.debug(f)

      let Field(name, typ) = f

      let assert Ok(typ) = case typ {
        value_type.VarType(a) -> list.at(args, a)
        _ -> Ok(typ)
      }

      //anynonymous_field 
      let nn = option.unwrap(name, dynamic.unsafe_coerce(dynamic.from(index)))

      use r <- result.then(to_elixir(typ, field_val))
      let val = map.insert(val, nn, r)

      Ok(#(index + 1, val))
    },
  ))

  //full type
  // let res =
  //   map.insert(
  //     res,
  //     atom.create_from_string("__type"),
  //     dynamic.from(#(module_name, constructor.string_name, args)),
  //   )

  let res =
    map.insert(
      res,
      atom.create_from_string("__constructor"),
      dynamic.from(constructor.string_name),
    )

  Ok(dynamic.from(res))
}

pub fn from_elixir(
  typeinfo: value_type.ValueType,
  value: a,
) -> Result(dynamic.Dynamic, String) {
  case typeinfo {
    value_type.String -> {
      Ok(dynamic.from(value))
    }
    value_type.BitString -> {
      Ok(dynamic.from(value))
    }
    value_type.Int -> {
      Ok(dynamic.from(value))
    }
    value_type.Float -> {
      Ok(dynamic.from(value))
    }
    value_type.List(typeinfo) -> {
      let items =
        list.map(
          dynamic.unsafe_coerce(dynamic.from(value)),
          fn(v) { from_elixir(typeinfo, v) },
        )
      use items <- result.then(result.all(items))
      Ok(dynamic.from(items))
    }
    value_type.Custom(a, b, args) -> {
      from_elixir_custom(a, b, args, value)
      |> result.then(fn(r) { Ok(r) })
    }
    //|> result.map_error(fn(x) { "Failed to show substruct" })
    _ -> {
      io.debug(typeinfo)
      todo
    }
  }
}

pub fn from_elixir_custom(
  module_name: atom.Atom,
  tname: String,
  args: List(value_type.ValueType),
  value: a,
) -> Result(dynamic.Dynamic, String) {
  use tinfo <- result.then(
    fetch_type_info(module_name, tname)
    |> result.map_error(fn(_x) { "failed to fetch type info from module" }),
  )

  case tinfo {
    TypeInfo(..) -> from_elixir_tf(module_name, tinfo, args, value)
    ExternalType(name) -> from_elixir_external(name, dynamic.from(value))
  }
}

pub fn from_elixir_external(
  name: String,
  value: dynamic.Dynamic,
) -> Result(dynamic.Dynamic, String) {
  case name {
    "Atom" -> Ok(value)
    _ -> Error("unsupported external type")
  }
}

pub fn from_elixir_tf(
  module_name: atom.Atom,
  tinfo: TypeInfo,
  args: List(value_type.ValueType),
  value: a,
) -> Result(dynamic.Dynamic, String) {
  let assert TypeInfo(constructors: tinfo_constructors, ..) = tinfo

  //io.debug(tinfo)

  let value_map: map.Map(atom.Atom, dynamic.Dynamic) =
    dynamic.unsafe_coerce(dynamic.from(value))

  let res = map.get(value_map, atom.create_from_string("__constructor"))

  let res =
    res
    |> result.map_error(fn(_x) { "failed to grab constructor name" })

  use constructor_of_a <- result.then(res)
  let constructor_of_a: atom.Atom = dynamic.unsafe_coerce(constructor_of_a)

  //locate the right constructor
  use constructor <- result.then(
    list.find(
      tinfo_constructors,
      fn(x) {
        //io.debug(#(x.record_atom, constructor_of_a))
        x.string_name == dynamic.unsafe_coerce(dynamic.from(constructor_of_a))
      },
    )
    |> result.then(fn(c) {
      //io.debug(c)

      Ok(c)
    })
    |> result.map_error(fn(_x) { "failed to match constructor name" }),
  )

  //io.debug(constructor.fields)

  case constructor.fields {
    [] -> Ok(dynamic.from(constructor.record_atom))
    _ -> {
      use #(_count, res) <- result.then(list.fold(
        constructor.fields,
        Ok(#(1, [dynamic.from(constructor.record_atom)])),
        fn(acc, f) {
          use #(index, val) <- result.then(acc)

          let Field(name, typ) = f

          let nn =
            option.unwrap(name, dynamic.unsafe_coerce(dynamic.from(index)))

          let assert Ok(field_val) = map.get(value_map, nn)

          let assert Ok(typ) = case typ {
            value_type.VarType(a) -> list.at(args, a)
            _ -> Ok(typ)
          }

          use r <- result.then(from_elixir(typ, field_val))

          //anynonymous_field 
          Ok(#(index + 1, [r, ..val]))
        },
      ))

      Ok(dynamic.from(list_to_tuple(list.reverse(res))))
    }
  }
}

pub external fn list_to_tuple(List(a)) -> b =
  "erlang" "list_to_tuple"

