open! Base
open Printf

module Parse = struct
  type t' =
    | BangInPartAppl
    | UnderscoreInConstrAppl
    | UnterminatedComment
    | OcamllexError of string
    | MenhirError

  type t = Loc.t * t'

  exception Err of t

  let err ltuple t = raise (Err (Loc.make ltuple, t))

  let show_t' = function
    | BangInPartAppl -> "`!` isn't allowed in partial combinator application"
    | UnderscoreInConstrAppl -> "`_` isn't allowed in application"
    | UnterminatedComment -> "Unterminated comment"
    | OcamllexError s -> "Ocamllex: " ^ s
    | MenhirError -> "Menhir Error"

  let show (loc, t' : t) =
    Printf.sprintf "%s: %s" (Loc.show loc) (show_t' t')
end

module Check = struct
  type t' =
    | UndefinedVar of string
    | UndefinedConstr of string
    | UndefinedType of string
    | CondMoreThan32
    | UnsupportedEmptyBit
    | Unsupported of string
    | DuplicateConstr of string
    | DuplicateFunc of string
    | DuplicateArg of string
    | InvalidOptArgType
    | AmbiguousOptArg
    | NatVarCantBeCond
    | NatIsNotTypeExpr of int
    | InvalidNatVar of string option
    | BareResultType
    | InvalidResultType
    | InvalidTypeParam
    | InvalidTypeParamType
    | WrongTypeParams
    | RepeatWithoutMultAsFirstArg
    | RepeatConditional
    | RepeatMultIsNotNatExpr
    | InvalidTypeConstr
    | WrongNumberOfOperands
    | WrongNumberOfParams of string * int * int
    | IncompatParamNat
    | IncompatParamType
    | IncompatRepeat
    | IncompatError of string * string
    | BareNoConstructors
    | BareTooManyConstructors
    | BareInvalid
    | BareBuiltin

  type t = Loc.t * t'

  let show_t' = function
    | UndefinedVar n -> sprintf "Variable `%s` is not defined" n
    | UndefinedConstr n -> sprintf "Constructor `%s` is not defined" n
    | UndefinedType n -> sprintf "Type `%s` is not defined" n
    | CondMoreThan32 -> "Cannot use more than 32 bits for conditional"
    | UnsupportedEmptyBit ->
      "Conditionals without bit are not supported. 0 is used instead"
    | Unsupported msg -> "Not supported: " ^ msg
    | DuplicateConstr n -> sprintf
      "Constructor `%s` is already defined. Skipping" n
    | DuplicateFunc n -> sprintf "Function `%s` is already defined. Skipping" n
    | DuplicateArg n -> sprintf "Argument `%s` is already defined. Skipping" n
    | InvalidOptArgType -> "Optional arguments must be of type `#` or `Type`"
    | AmbiguousOptArg ->
      "An optional argument must be used at least once"
      ^ " in the result type *not modified* by `!`"
      ^ " or in an argument *modified* by `!`"
    | NatVarCantBeCond -> "A nat variable cannot be conditional"
    | NatIsNotTypeExpr n -> sprintf "A nat literal (%d) is not a type expression" n
    | InvalidNatVar (Some v) -> sprintf "`%s` is not a valid nat variable" v
    | InvalidNatVar None -> "The previous argument is not a valid nat variable"
    | BareResultType -> "Bare types are not allowed in a result type"
    | InvalidResultType -> "Invalid result type"
    | InvalidTypeParam -> "Invalid type parameter"
    | InvalidTypeParamType ->
      "Type parameters must be of type `#` or `Type`. `Type` is used instead"
    | WrongTypeParams -> "Wrong type parameters"
    | RepeatWithoutMultAsFirstArg ->
      "A repetition without multiplicity cannot be the first argument"
    | RepeatConditional -> "Conditionals are not allowed inside repetitions"
    | RepeatMultIsNotNatExpr -> "The multiplicity is not a valid nat expression"
    | InvalidTypeConstr -> "Invalid type constructor"
    | WrongNumberOfOperands -> "Wrong number of operands"
    | WrongNumberOfParams (t, exp, got) -> sprintf
      "The type constructor `%s` expects %d type parameter(s), but is applied to %d"
      t exp got
    | IncompatParamNat ->
      "A type expression is incompatible with a parameter of type `nat`"
    | IncompatParamType ->
      "A nat expression is incompatible with a parameter of type `Type`"
    | IncompatRepeat ->
      "A repetition is not allowed as a type parameter"
    | IncompatError (s1, s2) ->
      sprintf "Type `%s` is incompatible with a type parameter of type `%s`"
        s2 s1
    | BareNoConstructors -> "Cannot apply the `%` modifier: no constructors"
    | BareTooManyConstructors ->
      "The `%` modifier must be applied to types with only one constructor"
    | BareInvalid -> "Invalid use of the `%` modifier"
    | BareBuiltin -> "Cannot apply the `%` modifier to a builtin type"

  let show (loc, t')
    = Printf.sprintf "%s: %s" (Loc.show loc) (show_t' t')
end

type t = [
  | `ParseError of Parse.t
  | `CheckError of Check.t
]

let show: t -> string = function
  | `ParseError e -> "ParseError: " ^ Parse.show e
  | `CheckError e -> "CheckError: " ^ Check.show e
