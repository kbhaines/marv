#lang brag

marv-spec: module-import* outer-decl* marv-module*

module-import: /"import" [ STRING | MODULE-IDENTIFIER ] [ "as" IDENTIFIER ]

outer-decl: func-decl | type-decl | type-template | var-decl | module-export


marv-module: [ "private" ] /"module" IDENTIFIER "(" @arguments ")" /"{" statement+ [ module-return ] /"}"
module-parameter: IDENTIFIER [ "=" expression ]
module-return: /"return" /"{" (return-parameter opt-comma)+ /"}"
return-parameter: ( STRING | IDENTIFIER | "type" ) /"=" expression

module-export: /"export" [ IDENTIFIER+ [ "as" IDENTIFIER ] ]+

arguments: (IDENTIFIER opt-comma)* (@named-parameter opt-comma)*

; type is explicitly allowed as it's common, and we need 'type' as a lexical token
; also allow STRING to allow user to avoid marv keywords
; TODO45 - remove?
named-parameter: ( STRING | IDENTIFIER | "type" ) "=" expression

statement: decl | pprint | assertion
decl: var-decl | res-decl | func-decl

pprint: /"pprint" /"(" expression /")"
comment: COMMENT
var-decl: IDENTIFIER /"=" expression
@opt-comma: [ /"," ]

res-decl: IDENTIFIER /":=" type-id map-expression

; TODO - mandatory at least one parameter, or func calls of e.g. C2.xxx(C1.yyy(B.zzz))
; may fail to parse correctly.

; TODO lambda?

func-decl: IDENTIFIER /"(" @arguments /")" /"=" expression

type-id: IDENTIFIER

; Hmm, added @xterm here and type-tests starts working...
; Order of map/num expression matters...

expression: @xterm | map-expression | list-expression | num-expression | string-expression | boolean-expression |alternate-expression | built-in

num-expression: num-term ( "+" | "-" ) num-expression | num-term
num-term: num-primary ( "*" | "/" ) num-term | num-primary
@num-primary: INTEGER | @xterm | /"(" num-expression /")"

string-expression: string-term [ string-operator string-term ]
@string-operator: '++'
@string-term: STRING | @xterm

boolean-expression: boolean | ( expression comparison-operator expression )
@boolean: "true" | "false"
@comparison-operator: "==" | "!="

list-expression: list-spec
@list-spec: "[" (expression [ /"," ])* "]" | @xterm

map-expression: map-term [ map-operator map-term | "<<" attr-list ]
@map-operator: "<-" | "->"
; Order is important - xterm first
@map-term: @xterm | map-spec | /"(" map-expression /")"
map-spec: /"{" [( STRING | IDENTIFIER | "type" ) /"=" [ "imm:" ] expression [ /"," ]]* /"}"

xterm: (func-apply | dot-apply | list-apply | IDENTIFIER )
func-apply: (dot-apply | list-apply | IDENTIFIER) "(" @func-call-parameters ")"
dot-apply:  (IDENTIFIER | map-expression) "." @attribute-name ;[ func-apply ]
list-apply: (IDENTIFIER | list-expression) "[" num-expression "]" ;[ func-apply ]

func-call-parameters: (expression opt-comma)* (@named-parameter opt-comma)*

attribute-name: ( STRING | IDENTIFIER | "type" )

; TODO45 - should nested maps be handled here, or above? map-expressions should yield maps, shrug, but then IDENTIFIERS wouldn't necessarily do so either....

attr-list: /"[" ( attribute-name [ /"," ] )* /"]"

@alternate-expression: expression '|' expression | /'(' expression '|' expression /')'


built-in: env-read | strf | base64encode | base64decode | urivars | uritemplate |assertion
        | "lowercase" /"(" string-expression /")"
        | "uppercase" /"(" string-expression /")"
        | "replace" /"(" string-expression opt-comma string-expression opt-comma string-expression /")"

env-read: /"env" /"(" STRING /")"
strf: /"strf" /"(" string-expression [ /"," ]( expression [ /"," ] ) + /")"
base64encode: /"base64encode" /"(" string-expression /")"
base64decode: /"base64decode" /"(" string-expression /")"
urivars: /"strvars" /"(" string-expression /")"
uritemplate: /"expandvars" /"(" expression opt-comma map-expression /")"
assertion: /"assert" /"(" expression comparison-operator expression /")"

type-decl: /"type" type-id /"=" ( /"{" func-decl+ [ type-wild ]* /"}" | type-parameters )
type-wild: /"*" /"=" IDENTIFIER /"." /"*"

type-parameters: type-id /"<" ( IDENTIFIER [/","] )+ /">"
type-template: /"type" type-parameters /"=" /"{" func-decl+ [ type-wild ]* /"}"
