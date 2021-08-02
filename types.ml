type var = string
[@@deriving yojson]

type handle = int
[@@deriving yojson]

type exp =
| Unit                            [@name "A"]
| Bool of bool                    [@name "B"]
| Pair of exp * exp               [@name "C"]
| Int of int                      [@name "D"]
| Str of string                   [@name "E"]
| Var of var                      [@name "F"]
| Fun of pat * exp                [@name "G"]
| App of exp * exp                [@name "H"]
| Let of pat * exp * exp          [@name "I"]
| LetRec of var * exp * exp       [@name "J"]
| Nil                             [@name "K"]
| Cons of exp * exp               [@name "L"]
| Assign of exp * exp             [@name "M"]
| Ref of exp                      [@name "N"]
| Deref of exp                    [@name "O"]
| Bin of bin * exp * exp          [@name "P"]
| Una of una * exp                [@name "Q"]
| Seq of exp * exp                [@name "R"]
| IfThen of exp * exp * exp       [@name "S"]
| Match of exp * (pat * exp) list [@name "T"]
| Await of pat * exp * exp        [@name "U"]
| Spawn of exp * exp              [@name "V"]
| Send of exp * exp               [@name "W"]
| Recv of exp                     [@name "X"]
| Join of exp                     [@name "Y"]
| Pick of exp                     [@name "Z"]
| Return of exp                   [@name "0"]
[@@deriving yojson]

and pat =
| PUnit              [@name "1"]
| PWild              [@name "2"]
| PBool of bool      [@name "3"]
| PInt of int        [@name "4"]
| PStr of string     [@name "5"]
| PVar of var        [@name "6"]
| PPair of pat * pat [@name "7"]
| PNil               [@name "8"]
| PCons of pat * pat [@name "9"]
[@@deriving yojson]

and bin =
| Add [@name "a"]
| Sub [@name "b"]
| Mul [@name "c"]
| Div [@name "d"]
| Mod [@name "e"]
| And [@name "f"]
| Or  [@name "g"]
| Lt  [@name "h"]
| Le  [@name "i"]
| Gt  [@name "j"]
| Ge  [@name "k"]
| Eq  [@name "l"]
| Ne  [@name "m"]
| Cat [@name "n"]
[@@deriving yojson]

and una =
| Neg [@name "o"]
| Not [@name "p"]
[@@deriving yojson]

type value =
| VUnit
| VBool of bool
| VInt of int
| VStr of string
| VFun of pat * exp * env
| VFunRec of pat * exp * env ref
| VDwt of value Dwt.t
| VRef of value ref
| VPair of value * value
| VNil
| VCons of value * value
| VHandle of handle

and env = (var * value) list
