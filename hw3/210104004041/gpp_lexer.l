%{
#include "gpp_interpreter.tab.h"
int yywrap() { return 1; }
%}

 /* flex -o gpp_lexer.c gpp_lexer.l */

%%  
[ \t\n\r]+          { /* Ignore spaces, tabs, newlines, and carriage returns */ }

;;.*                { return COMMENT; }

"and"               { return KW_AND; }
"or"                { return KW_OR; }
"not"               { return KW_NOT; }
"equal"             { return KW_EQUAL; }
"less"              { return KW_LESS; }
"nil"               { return KW_NIL; }
"list"              { return KW_LIST; }
"append"            { return KW_APPEND; }
"concat"            { return KW_CONCAT; }
"set"               { return KW_SET; }
"deffun"            { return KW_DEFFUN; }
"for"               { return KW_FOR; }
"if"                { return KW_IF; }
"exit"              { return KW_EXIT; }
"load"              { return KW_LOAD; }
"print"             { return KW_PRINT; }
"true"              { return KW_TRUE; }
"false"             { return KW_FALSE; }
"while"             { return KW_WHILE; }
"defvar"            { return KW_DEFVAR; }

"+"                 { return OP_PLUS; }
"-"                 { return OP_MINUS; }
"*"                 { return OP_MULT; }
"/"                 { return OP_DIV; }
"("                 { return OP_OP; }
")"                 { return OP_CP; }
","                 { return OP_COMMA; }

[0-9]+:[0-9]+       { return VALUEF; }  // Fractional values
[0-9]+              { return VALUEI; }  // Integer values

[a-zA-Z_][a-zA-Z0-9_]* { return IDENTIFIER; } // Identifiers

.                   { printf("SYNTAX_ERROR: Unexpected character '%s'\n", yytext); }
%%
