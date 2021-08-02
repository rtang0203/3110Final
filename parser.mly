(* Token Definitions *)

%token <int> INT
%token <string> STR
%token <Types.var> VAR
%token TRUE FALSE
%token FUN ARROW
%token AWAIT LET IN WILDCARD REC
%token ADD SUB MUL DIV MOD AND OR LT LE GT GE EQ NE ASSIGN CAT
%token NOT REF DEREF
%token IF THEN ELSE
%token MATCH CASE CONS
%token SPAWN WITH
%token SEND TO
%token RECV
%token JOIN
%token PICK
%token RETURN
%token LPAREN RPAREN LBRACK RBRACK EOF BEGIN END SEMICOLON COMMA

(* Precedence and Associativity *)

%right ARROW

%nonassoc IN ELSE

%left SEMICOLON

%nonassoc ASSIGN

%nonassoc TO WITH RECV JOIN PICK

%right RETURN
%right CONS
%left OR
%left AND
%left EQ NE
%left LT LE GE GT
%left ADD SUB
%left MUL DIV MOD
%left CAT

%right REF
%right NOT
%right DEREF

%start <Types.exp> program

(* Helper functions *)

%{
  open Types

  (** [fold_fun ps e] is the function [fun p1 -> ... -> fun pn -> e]. *)
  let fold_fun ps e =
    List.fold_right (fun p f -> Fun (p, f)) ps e

  (** [defold_list es] desugars List [e1; ... ; en] to [Cons (e1, ... Cons (en, Nil))] *)
  let rec defold_list es =
    match es with
    | [] -> Nil
    | h::t -> Cons (h, defold_list t)

  (** [defold_plist ps] desugars PList [p1; ... ; pn] to [PCons (p1, ... Cons (pn, Nil))] *)
  let rec defold_plist ps =
    match ps with
    | [] -> PNil
    | p::t -> PCons (p, defold_plist t)

%}

%%

program:
| exp EOF { $1 }

exp:
| IF exp THEN exp ELSE exp       { IfThen ($2, $4, $6) }
| MATCH exp WITH case+ END       { Match ($2, $4) }
| FUN pat ARROW exp              { Fun ($2, $4) }
| LET pat EQ exp IN exp          { Let ($2, $4, $6) }
| LET VAR pat+ EQ exp IN exp     { Let (PVar $2, fold_fun $3 $5, $7) }
| AWAIT pat EQ exp IN exp        { Await ($2, $4, $6) }
| SEND exp TO exp                { Send ($2, $4) }
| RECV exp                       { Recv ($2) }
| SPAWN exp WITH exp             { Spawn ($2, $4) }
| exp SEMICOLON exp              { Seq ($1, $3) }
| exp CONS exp                   { Cons ($1, $3) }
| exp ASSIGN exp                 { Assign ($1, $3) }
| REF exp                        { Ref $2 }
| DEREF exp                      { Deref $2 }
| exp bin exp                    { Bin ($2, $1, $3) }
| una exp                        { Una ($1, $2) }
| JOIN exp                       { Join ($2) }
| PICK exp                       { Pick ($2) }
| RETURN exp                     { Return ($2) }
| LET REC VAR EQ exp IN exp      { LetRec ($3, $5, $7) }
| LET REC VAR pat+ EQ exp IN exp { LetRec ($3, fold_fun $4 $6, $8) }
| app                            { $1 }

pat:
| WILDCARD                                                    { PWild }
| LPAREN RPAREN                                               { PUnit }
| TRUE                                                        { PBool true }
| FALSE                                                       { PBool false }
| INT                                                         { PInt $1 }
| STR                                                         { PStr $1 }
| VAR                                                         { PVar ($1) }
| LPAREN pat COMMA pat RPAREN                                 { PPair ($2, $4) }
| pat CONS pat                                                { PCons ($1, $3) }
| LBRACK separated_list(SEMICOLON, pat) RBRACK                { defold_plist $2 }

case:
| CASE pat ARROW exp { ($2, $4) }

app:
| app value { App ($1, $2) }
| value     { $1 }

value:
| INT                                          { Int $1 }
| STR                                          { Str $1 }
| VAR                                          { Var $1 }
| TRUE                                         { Bool true }
| FALSE                                        { Bool false }
| LPAREN RPAREN                                { Unit }
| LPAREN exp RPAREN                            { $2 }
| LPAREN exp COMMA exp RPAREN                  { Pair ($2, $4) }
| LBRACK separated_list(SEMICOLON, app) RBRACK { defold_list $2}
| BEGIN exp END                                { $2 }

%inline bin:
| ADD    { Add }
| SUB    { Sub }
| MUL    { Mul }
| DIV    { Div }
| MOD    { Mod }
| AND    { And }
| OR     { Or }
| LT     { Lt }
| LE     { Le }
| GT     { Gt }
| GE     { Ge }
| EQ     { Eq }
| NE     { Ne }
| CAT    { Cat }

%inline una:
| SUB   { Neg }
| NOT   { Not }
