%{
#include <stdio.h>
#include <string.h>

// Flex requires yywrap to be defined. Return 1 to signal end of input.
int yywrap() { return 1; }
%}

%%

[ \t\n\r]+          { }/* Ignore spaces, tabs, newlines, and carriage returns */

;;.*                { printf("COMMENT\n"); }

"and"               { printf("KW_AND\n"); }
"or"                { printf("KW_OR\n"); }
"not"               { printf("KW_NOT\n"); }
"equal"             { printf("KW_EQUAL\n"); }
"less"              { printf("KW_LESS\n"); }
"nil"               { printf("KW_NIL\n"); }
"list"              { printf("KW_LIST\n"); }
"append"            { printf("KW_APPEND\n"); }
"concat"            { printf("KW_CONCAT\n"); }
"set"               { printf("KW_SET\n"); }
"deffun"            { printf("KW_DEFFUN\n"); }
"for"               { printf("KW_FOR\n"); }
"if"                { printf("KW_IF\n"); }
"exit"              { printf("KW_EXIT\n"); }
"load"              { printf("KW_LOAD\n"); }
"print"             { printf("KW_PRINT\n"); }
"true"              { printf("KW_TRUE\n"); }
"false"             { printf("KW_FALSE\n"); }

[+]               { printf("OP_PLUS\n"); }
[-]               { printf("OP_MINUS\n"); }
[*]               { printf("OP_MULT\n"); }
[/]                 { printf("OP_DIV\n"); }
[(]               { printf("OP_OP\n"); }
[)]               { printf("OP_CP\n"); }
[,]                 { printf("OP_COMMA\n"); }

[0-9]+:[0-9]+       { printf("VALUEF\n"); }  // Fractional values
[0-9]+              { printf("VALUEI\n"); } // Integer values

[a-zA-Z_][a-zA-Z0-9_]* {printf("IDENTIFIER\n");}

[0-9][a-zA-Z0-9]*   { printf("SYNTAX ERROR: %s cannot be tokenized\n",yytext); }
.                   { printf("SYNTAX_ERROR: Unexpected character '%s'\n", yytext); }
%%

// Main function
int main() {
    printf("Enter G++ code (Ctrl+D to exit):\n");
    yylex();
    return 0;
}
