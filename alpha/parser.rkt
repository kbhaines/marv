#lang brag

marv-spec: module-import* outer-decl* marv-module*

module-import: /"import" [ STRING | MODULE-IDENTIFIER ] [ "as" IDENTIFIER ]

outer-decl: func-decl | type-decl | type-template | var-decl | module-export

module-export: /"export" [ IDENTIFIER+ [ "as" IDENTIFIER ] ]+

marv-module: [ "private" ] /"module" IDENTIFIER ["(" module-parameter+ ")"] /"{" statement+ [ module-return ] /"}"
module-parameter: IDENTIFIER [ "=" expression ]
module-return: /"return" /"{" (return-parameter opt-comma)+ /"}"
return-parameter: ( STRING | IDENTIFIER | "type" ) /"=" expression

statement: decl | pprint | assertion
decl: var-decl | res-decl | func-decl

pprint: /"pprint" expression
comment: COMMENT
var-decl: IDENTIFIER /"=" expression
@opt-comma: [ /"," ]

res-decl: IDENTIFIER /":=" type-id map-expression

; TODO - mandatory at least one parameter, or func calls of e.g. C2.xxx(C1.yyy(B.zzz))
; may fail to parse correctly.
func-decl: IDENTIFIER /"(" (IDENTIFIER [ /"," ])+ /")" /"=" expression
func-call: IDENTIFIER @func-call-parameters

func-call-parameters: /"(" (expression [ /"," ])+ /")"

type-id: IDENTIFIER

@complex-ident: IDENTIFIER | indexed-identifier | dot-expression
indexed-identifier: IDENTIFIER "[" @num-expression "]"

expression: boolean-expression | string-expression | num-expression | map-expression | alternate-expression | complex-ident | expr-list
@expr-list: "[" (expression [ /"," ])* "]"

@list-expression: expression "[" num-expression "]"

boolean-expression: boolean | ( expression comparison-operator expression )
@boolean: "true" | "false"
@comparison-operator: "==" | "!="

string-expression: string-term [ string-operator string-term ]
@string-operator: '++'
@string-term: (STRING | complex-ident | built-in | func-call)

num-expression: num-term ( "+" | "-" ) num-expression | num-term
num-term: num-primary ( "*" | "/" ) num-term | num-primary
@num-primary: /"(" num-expression /")" | INTEGER | complex-ident | built-in | func-call

map-expression: map-term ( [ map-operator map-term ] | "<<" attr-list )
@map-operator: "<-" | "->"
@map-term: map-spec | complex-ident | func-call | map-parens-expr | map-expression | dot-expression

; TODO45 - complex-ident not dot-expression?
map-spec: /"{" [( STRING | IDENTIFIER | "type" ) /"=" [ "imm:" ] expression [ /"," ]]* /"}"
@map-parens-expr: /"(" map-expression /")"

attr-list: /"[" ( attribute-name [ /"," ] )* /"]"
attribute-name: ( STRING | IDENTIFIER | "type" )

@alternate-expression: expression '|' expression | /'(' expression '|' expression /')'

dot-expression: map-term /"." [ IDENTIFIER | @indexed-identifier ] [ @func-call-parameters ]

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

; type is explicitly allowed as it's common, and we need 'type' as a lexical token
; also allow STRING to allow user to avoid marv keywords
; TODO45 - remove?
named-parameter: ( STRING | IDENTIFIER | "type" ) /"=" expression

type-decl: /"type" type-id /"=" ( /"{" func-decl+ [ type-wild ]* /"}" | type-parameters )
type-wild: /"*" /"=" IDENTIFIER /"." /"*"

type-parameters: type-id /"<" ( IDENTIFIER [/","] )+ /">"
type-template: /"type" type-parameters /"=" /"{" func-decl+ [ type-wild ]* /"}"
