grammar rml;

@header {
    package rml.parser;
}

spec: (decl NEWLINE)* ;
decl: TEXP_ID ('<' vars '>')? '=' texp ;
texp: texp '*' texp # catTExp
    | texp '/\\' texp # andTExp
    | texp '\\/' texp # orTExp
    | texp '|' texp # shufTExp
    | 'empty' # exptyTExp
    | '{' 'var' vars ';' texp '}' # blockTExp
    | TEXP_ID ('<' vars '>')? # varTExp
    | evtype # evtypeTExp
    | '(' texp ')' # parTExp
    ;
vars: VAR_ID (',' VAR_ID)*;
evtype: EVTYPE_ID ('(' expSeq ')')? ;
expSeq: exp (',' exp)* ;
exp: VAR_ID # varExp
   | INT # intExp
   | STRING # stringExp
   ;

NEWLINE: [\r\n]+ ;
TEXP_ID: [a-zA-Z][a-zA-Z0-9_]* ;
EVTYPE_ID: [a-zA-Z][a-zA-Z0-9_]* ;
VAR_ID: [a-zA-Z][a-zA-Z0-9_]* ;
INT: [0-9]+ ;
STRING: '"' [ a-zA-Z0-0_] '"' ;

WHITESPACE: [ \t\r\n]+ -> skip ;