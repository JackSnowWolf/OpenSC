open Datatypes

val bind : 'a1 option -> ('a1 -> 'a2 option) -> 'a2 option

val bind2 : ('a1, 'a2) prod option -> ('a1 -> 'a2 -> 'a3 option) -> 'a3 option

val optional_filter : 'a1 option -> 'a1 list
