grammar rml;

@header {
    package rml.parser;
}

spec: texpDecl+ ;
texpDecl: UPPERCASE_ID ('<' vars '>')? '=' texp ';' ;
texp: texp '*' texp # catTExp
    | texp '/\\' texp # andTExp
    | texp '\\/' texp # orTExp
    | texp '|' texp # shufTExp
    | 'empty' # emptyTExp
    | '{' 'var' vars ';' texp '}' # blockTExp
    | UPPERCASE_ID ('<' vars '>')? # varTExp
    | evtype # evtypeTExp
    | '(' texp ')' # parTExp
    ;
vars: LOWERCASE_ID (',' LOWERCASE_ID)*;
evtype: LOWERCASE_ID ('(' terms ')')? ;
terms: term (',' term)* ;
term: LOWERCASE_ID # varTerm
    | INT # intTerm
    | STRING # stringTerm
    ;

UPPERCASE_ID: [A-Z] ID_CHAR* ;
LOWERCASE_ID: [a-z] ID_CHAR* ;
fragment ID_CHAR: [a-zA-Z0-9_] ;
INT: [0-9]+ ;
STRING: '"' [ a-zA-Z0-0_] '"' ;

WHITESPACE: [ \t\r\n]+ -> skip ;
// don't use [^\r\n]*, it's not the same
COMMENT: '//' ~[\r\n]* -> skip ;