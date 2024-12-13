%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int identifier_value = 0;


typedef struct Node {
    char *name;            
    double value;           
    struct Node **children; 
    int child_count;    
} Node;


Node* createNode(const char *name,const double value ,int child_count) {
    Node *node = (Node *)malloc(sizeof(Node));
    node->name = strdup(name);
    node->value = value;
    node->children = (Node **)malloc(sizeof(Node *) * child_count);
    node->child_count = child_count;
    return node;
}


void freeNode(Node *node) {
    if (node) {
        for (int i = 0; i < node->child_count; i++) {
            freeNode(node->children[i]);
        }
        free(node->children);
        free(node->name);
        free(node);
    }
}


void printTree(Node *node, int depth) {
    if (!node) return;
    for (int i = 0; i < depth; i++) printf("| ");
    printf("%s : %.2lf\n", node->name, node->value);
    for (int i = 0; i < node->child_count; i++) {
        printTree(node->children[i], depth + 1);
    }
}

Node *root = NULL;
int yylex();
void yyerror(const char *s){fprintf(stderr, "Error: %s\n", s);}

%}

%union {
    char *str;      // for token values
    struct Node *node;     // for parse tree nodes
    double value;   // for fractional values
}

%token <str> KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS 
%token <str> KW_SET KW_DEFFUN KW_FOR KW_IF 
%token <str>  KW_WHILE KW_DEFVAR
%token <str> OP_PLUS OP_MINUS OP_DIV OP_MULT OP_OP OP_CP 
%token <str> IDENTIFIER 
%token <value> VALUEF
%token <str> VALUEI COMMENT KW_APPEND KW_CONCAT KW_EXIT OP_COMMA KW_NIL KW_LIST KW_LOAD KW_PRINT KW_TRUE KW_FALSE

%type <node> start input expression expression_list identifier_list expression_boolean // list values

// bison -d gpp_interpreter.y
// flex -o gpp_lexer.c gpp_lexer.l
// clang gpp_lexer.c gpp_interpreter.tab.c -o gpp_interpreter
// ./gpp_interpreter
// ./gpp_interpreter < input

%%
start:
    input{
        root = createNode("start", $1->value, 1);
        root->children[0] = $1;
    }
;

input:
    expression_list{ 
        $$ = createNode("input", $1->value, 1);
        $$->children[0] = $1;
    }
;

expression:
    OP_OP OP_PLUS expression expression OP_CP{
        $$ = createNode("expression(plus)", ($3->value) + ($4->value), 2);
        $$->children[0] = $3;
        $$->children[1] = $4;
    }           
    | OP_OP OP_MINUS expression expression OP_CP{
        $$ = createNode("expression(minus)", ($3->value) - ($4->value), 2);
        $$->children[0] = $3;
        $$->children[1] = $4;
    }
    | OP_OP OP_MULT expression expression OP_CP {

        $$ = createNode("expression(mult)", ($3->value) * ($4->value), 2);
        $$->children[0] = $3;
        $$->children[1] = $4;

    }
    | OP_OP OP_DIV expression expression OP_CP {
        $$ = createNode("expression(div)", ($3->value) / ($4->value), 2);
        $$->children[0] = $3;
        $$->children[1] = $4;
    }
    | OP_OP KW_SET IDENTIFIER expression OP_CP{
        $$ = createNode("expression(set)", $4->value, 2);
        $$->children[0] = createNode("identifier", identifier_value, 0);
        $$->children[1] = $4;
    }
    | IDENTIFIER{
        $$ = createNode("expression(id)", identifier_value, 1);
        $$->children[0] = createNode("identifier", identifier_value, 0);
        
    }
    | VALUEF {
        $$ = createNode("expression(vf)", yyval.value, 1);
        $$->children[0] = createNode("valuef", yyval.value, 0);
    }
    //expression_i part
    |
    OP_OP KW_DEFFUN IDENTIFIER OP_OP identifier_list OP_CP expression_list OP_CP {//function definition
        $$ = createNode("expression_i(deffun)", $7->value, 3);
        $$->children[0] = createNode("identifier", identifier_value, 0);
        $$->children[1] = $5;
        $$->children[2] = $7;
    } 
    // i think it is impossible to implement the function call returning the last expression value
    | OP_OP IDENTIFIER expression_list OP_CP {//function call. actually returns the value of the rightmost parameter
        $$ = createNode("expression_i(fcall)", $3->value, 2);
        $$->children[0] = createNode("identifier", identifier_value, 0);
        $$->children[1] = $3;
    }
    | OP_OP KW_IF expression_boolean expression_list OP_CP {//if statement. returns the value of the boolean expression
        $$ = createNode("expression_i(if)", $3->value, 2);
        $$->children[0] = $3;
        $$->children[1] = $4;
    }
    | OP_OP KW_IF expression_boolean expression_list expression_list OP_CP {//if-else statement
        $$ = createNode("expression_i(if_else)", $3->value, 3);
        $$->children[0] = $3;
        $$->children[1] = $4;
        $$->children[2] = $5;
    }
    | OP_OP KW_WHILE expression_boolean expression_list OP_CP {//while loop
        $$ = createNode("expression_i(while)", $3->value, 2);
        $$->children[0] = $3;
        $$->children[1] = $4;
    }
    | OP_OP KW_FOR OP_OP IDENTIFIER expression expression OP_CP expression_list OP_CP {//for loop, returns the value of the last expression
        $$ = createNode("expression_i(for)", $8->value, 4);
        $$->children[0] = createNode("identifier", identifier_value, 0);
        $$->children[1] = $5;
        $$->children[2] = $6;
        $$->children[3] = $8;
    }
    | OP_OP KW_DEFVAR IDENTIFIER expression OP_CP {//variable declaration. returns the value of the expression_i, which is the value of the variable
        $$ = createNode("expression_i(defvar)", $4->value, 2);
        $$->children[0] = createNode("identifier", identifier_value, 0);
        $$->children[1] = $4;
    }

;

expression_list:
     expression_list expression {
        $$ = createNode("expression_list", $2->value, 2);
        $$->children[0] = $1;
        $$->children[1] = $2;
    }
    | expression{
        $$ = createNode("expression_list", $1->value, 1);
        $$->children[0] = $1;
    }
;


expression_boolean:
    OP_OP KW_AND expression expression OP_CP{
        $$ = createNode("expression_boolean(and)", ($3->value) && ($4->value), 2);
        $$->children[0] = $3;
        $$->children[1] = $4;
    }
    | OP_OP KW_OR expression expression OP_CP{
        $$ = createNode("expression_boolean(or)", ($3->value) || ($4->value), 2);
        $$->children[0] = $3;
        $$->children[1] = $4;
    }
    | OP_OP KW_NOT expression OP_CP{
        $$ = createNode("expression_boolean(not)", !($3->value), 1);
        $$->children[0] = $3;
    }
    | OP_OP KW_EQUAL expression expression OP_CP{
        $$ = createNode("expression_boolean(equal)", ($3->value) == ($4->value), 2);
        $$->children[0] = $3;
        $$->children[1] = $4;
    }
    | OP_OP KW_LESS expression expression OP_CP{
        $$ = createNode("expression_boolean(less)", ($3->value) < ($4->value), 2);
        $$->children[0] = $3;
        $$->children[1] = $4;
    }
;


identifier_list:
    identifier_list IDENTIFIER {
        $$ = createNode("identifier_list", identifier_value, 2);
        $$->children[0] = $1;
        $$->children[1] = createNode("identifier", identifier_value, 0);
    }
    | IDENTIFIER{
        $$ = createNode("identifier_list", identifier_value, 1);
        $$->children[0] = createNode("identifier", identifier_value, 0);
    }
    
;

/* list:
    '\'' OP_OP values OP_CP{
        $$ = createNode("list", 1);
        $$->children[0] = $3;
    }
    | '\'' OP_OP OP_CP{
        $$ = createNode("list", 0);
    }
    | KW_NIL{
        $$ = createNode("nil", 0);
    }
    ;

values:
    values OP_COMMA VALUEF{
        $$ = createNode("values", 2);
        $$->children[0] = $1;
        $$->children[1] = createNode($3, 0);
    }
    | VALUEF{
        $$ = createNode("values", 1);
        $$->children[0] = createNode($1, 0);
    }
    ; */

%%

int main() {

    printf("Starting G++ Syntax Analyzer...\n");
    if (yyparse() == 0) { // Successful parsing

        printf("Parsing successful! Parse Tree:\n");

        printTree(root, 0); // Print the parse tree
        freeNode(root);     // Free the parse tree
    } else {
        printf("Parsing failed.\n");
    }
    return 0;
}

