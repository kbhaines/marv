#lang brag

marv-spec: module-import* outer-decl* marv-module*
module-import: /"import" [ STRING | MODULE-IDENTIFIER ] [ "as" IDENTIFIER ]
outer-decl: func-decl | type-decl | type-template | var-decl | module-export

marv-module: [ "private" ] /"module" IDENTIFIER "(" @arguments ")" /"{" statement+ [ module-return ] /"}"
module-parameter: IDENTIFIER [ "=" expression ]
module-return: /"return" /"{" return-parameter-list /"}"

@return-parameter-list: return-parameter (/"," return-parameter)*
return-parameter: ( STRING | IDENTIFIER | "type" ) /"=" expression

module-export: /"export" [ IDENTIFIER+ [ "as" IDENTIFIER ] ]+

arguments: identifier-list? [/"," named-parameter-list?] | named-parameter-list
@identifier-list: IDENTIFIER (/"," IDENTIFIER)*
@named-parameter-list: named-parameter (/"," named-parameter)*
@named-parameter: ( STRING | IDENTIFIER | "type" ) "=" expression

; type is explicitly allowed as it's common, but we need 'type' as a lexical token
; also allow STRING to allow user to avoid marv keywords

statement: decl | pprint | assertion
decl: var-decl | res-decl | func-decl

pprint: /"pprint" /"(" expression /")"
comment: COMMENT
var-decl: IDENTIFIER /"=" expression

res-decl: IDENTIFIER /":=" type-id map-expression

; TODO lambda?
@lambda-func: "lambda" func-spec
func-decl: IDENTIFIER func-spec
func-spec: /"(" @arguments /")" /"=" expression

type-id: IDENTIFIER

; NOTES:
; added @xterm here and type-tests starts working...
; Order of map/num expression matters...
; lambda-func before map doesn't work

expression: @xterm | map-expression | lambda-func | list-expression | num-expression | string-expression | boolean-expression |alternate-expression | built-in

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
@list-spec: "[" expression-list? "]" | @xterm
@expression-list: expression ( /"," expression )*
; Wonder if this list/comma separation being option has been a root
; factor in the parsing problems?

map-expression: map-term [ map-operator map-term | "<<" attr-list ]
@map-operator: "<-" | "->"
; Order is important - xterm first
@map-term: @xterm | map-spec | /"(" map-expression /")"
map-spec: /"{" (map-element [ /"," map-element]*)? /"}"
@map-element:( STRING | IDENTIFIER | "type" ) /"=" [ "imm:" ] expression

xterm: (func-apply | dot-apply | list-apply | IDENTIFIER )
func-apply: (IDENTIFIER | dot-apply | list-apply | func-apply) @func-call-parameters
dot-apply:  (IDENTIFIER | map-expression) "." @attribute-name [ "|" expression ]
list-apply: (IDENTIFIER | list-expression) "[" num-expression "]"

func-call-parameters: "(" (expression-list? [ /"," named-parameter-list] | named-parameter-list) ")"

attribute-name: ( STRING | IDENTIFIER | "type" )
attr-list: /"[" ( attribute-name [ /"," ] )* /"]"

@alternate-expression: expression '|' expression | /'(' expression '|' expression /')'

built-in: env-read | strf | base64encode | base64decode | urivars | uritemplate |assertion
        | "lowercase" /"(" string-expression /")"
        | "uppercase" /"(" string-expression /")"
        | "replace" /"(" string-expression /"," string-expression /"," string-expression /")"

env-read: /"env" /"(" STRING /")"
strf: /"strf" /"(" string-expression [ /"," ]( expression [ /"," ] ) + /")"
base64encode: /"base64encode" /"(" string-expression /")"
base64decode: /"base64decode" /"(" string-expression /")"
urivars: /"strvars" /"(" string-expression /")"
uritemplate: /"expandvars" /"(" expression /"," map-expression /")"
assertion: /"assert" /"(" expression comparison-operator expression /")"

type-decl: /"type" type-id /"=" ( /"{" func-decl+ [ type-wild ]* /"}" | type-parameters )
type-wild: /"*" /"=" IDENTIFIER /"." /"*"

type-parameters: type-id /"<" identifier-list /">"
type-template: /"type" type-parameters /"=" /"{" func-decl+ [ type-wild ]* /"}"
