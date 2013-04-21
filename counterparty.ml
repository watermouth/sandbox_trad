(* counter party *)
type t = {
  name_:	string;
  }

let make name = {name_=name}

let to_string = function {name_=name} -> name

