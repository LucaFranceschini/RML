grammar rml;

@header {
    package rml.parser;
}

spec: evtypeDecl* texpDecl+ ;
evtypeDecl: evtype 'matches' value ';' # directEvtypeDecl
          | evtype 'matches' evtype ('|' evtype)* ';' # derivedEvtypeDecl
          ;
object: '{' field (',' field)* '}' ;
field: LOWERCASE_ID ':' value ;
value: value '|' value # orPatternVal
     | object # objectVal
     | '[' (value (',' value)*)? ']' # listVal
     | simpleValue # simpleVal
     ;
simpleValue: STRING # stringValue
           | INT # intValue
           | LOWERCASE_ID # varValue
           | '[' simpleValues? ']' # listValue
           ;
texpDecl: UPPERCASE_ID ('<' vars '>')? '=' texp ';' ;
texp: <assoc=right> texp texp # catTExp
    | texp '/\\' texp # andTExp
    | texp '\\/' texp # orTExp
    | texp '|' texp # shufTExp
    | evtype '>>' texp # filterExp
    | 'empty' # emptyTExp
    | '{' 'var' vars ';' texp '}' # blockTExp
    | UPPERCASE_ID ('<' vars '>')? # varTExp
    | evtype # evtypeTExp
    | '(' texp ')' # parTExp
    ;
vars: LOWERCASE_ID (',' LOWERCASE_ID)*;
evtype: LOWERCASE_ID ('(' simpleValues? ')')? ;
simpleValues: simpleValue (',' simpleValue)* ;

UPPERCASE_ID: [A-Z] ID_CHAR* ;
LOWERCASE_ID: [a-z] ID_CHAR* ;
fragment ID_CHAR: [a-zA-Z0-9_-] ;
INT: [0-9]+ ;
STRING: '\'' [ a-zA-Z0-0_.]* '\'' ;

WHITESPACE: [ \t\r\n]+ -> skip ;
// don't use [^\r\n]*, it's not the same
COMMENT: '//' ~[\r\n]* -> skip ;