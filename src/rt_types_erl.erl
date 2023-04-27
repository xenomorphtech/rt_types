-module(rt_types_erl).
-export([fetch_type_info/1]).

fetch_type_info(M)->
  M:gleam_info().
