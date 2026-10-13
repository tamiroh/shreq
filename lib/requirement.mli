type kind = Command of string | Bash | Gnu_grep | Unsupported

type t = private {
  name : string;
  kind : kind;
  minimum : Version.t option;
  minimum_text : string option;
  line : int;
}

val parse : line:int -> string -> (t, string) result
val key : t -> string
