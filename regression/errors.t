  $ ../ppx/pp_gt.exe -pretty -impl - <<EOF
  > type t = < foo: int > [@@deriving gt]
  > EOF
  File "_none_", line 1:
  Error: extensions in types `< foo: int   > ` are not yet implemented
  [1]
  $ ../ppx/pp_gt.exe -pretty -impl - <<EOF
  > type t = .. [@@deriving gt]
  > EOF
  File "_none_", line 1:
  Error: open types are not yet implemented
  [1]
