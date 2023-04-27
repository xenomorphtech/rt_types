import gleam/erlang/atom

pub type ValueType {
  String
  Int
  BitString
  Custom(module: atom.Atom, name: String, args: List(ValueType))
  List(item_type: ValueType)
  Float
  VarType(index: Int)
}
