let rec show_pos = function
  | BinNums.Coq_xI v -> (show_pos v) ^ "1"
  | BinNums.Coq_xO v -> (show_pos v) ^ "0"
  | BinNums.Coq_xH   -> "1"

let rec show_int = function
  | BinNums.Z0 -> "Z0"
  | BinNums.Zpos x -> Printf.sprintf "ZPOS%s" (show_pos x)
  | BinNums.Zneg x -> Printf.sprintf "ZNEG-%s" (show_pos x)

let rec show_nat = function
  | BinNums.N0     -> "N0"
  | BinNums.Npos x -> "N" ^ show_pos x
