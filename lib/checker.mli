type check_result = Satisfied | Unsatisfied of string | Unknown of string

type report = {
  requirement : Requirement.t;
  result : check_result;
  version : string option;
}

val check : Requirement.t -> report
val format : report -> string
