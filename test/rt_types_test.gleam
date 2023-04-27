import gleeunit
import gleeunit/should
import gleam/io
import gleam/erlang/atom
import gleam/result
import gleam/dynamic
import gleam/option
import rt_types 
import rt_types/value_type 

pub external type Ex

pub type Something {
  Something(
    a: Int,
    b: List(Int),
    c: String,
    q: Something1,
    n: atom.Atom,
    x: option.Option(Int),
  )
}

pub type Something1 {
  Something1
}

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  let someatom = atom.create_from_string("t")
  let value = Something(1, [2, 3], "a", Something1, someatom, option.Some(1))
  let type_ = value_type.Custom(atom.create_from_string("rt_types_test"), "Something", [])
  let t = rt_types.show(type_, value, False)
  io.debug(t)
  let assert Ok(t) = t
  io.println(t)
  let assert Ok(t) = rt_types.to_elixir(type_, value)
  io.debug(t)

  let assert Ok(v) =
    rt_types.from_elixir(type_, t)
    |> result.map(fn(x) -> Something { dynamic.unsafe_coerce(x) })

  v |> should.equal(value)
}
