/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     KW_AND = 258,
     KW_OR = 259,
     KW_NOT = 260,
     KW_EQUAL = 261,
     KW_LESS = 262,
     KW_SET = 263,
     KW_DEFFUN = 264,
     KW_FOR = 265,
     KW_IF = 266,
     KW_WHILE = 267,
     KW_DEFVAR = 268,
     OP_PLUS = 269,
     OP_MINUS = 270,
     OP_DIV = 271,
     OP_MULT = 272,
     OP_OP = 273,
     OP_CP = 274,
     IDENTIFIER = 275,
     VALUEF = 276,
     VALUEI = 277,
     COMMENT = 278,
     KW_APPEND = 279,
     KW_CONCAT = 280,
     KW_EXIT = 281,
     OP_COMMA = 282,
     KW_NIL = 283,
     KW_LIST = 284,
     KW_LOAD = 285,
     KW_PRINT = 286,
     KW_TRUE = 287,
     KW_FALSE = 288
   };
#endif
/* Tokens.  */
#define KW_AND 258
#define KW_OR 259
#define KW_NOT 260
#define KW_EQUAL 261
#define KW_LESS 262
#define KW_SET 263
#define KW_DEFFUN 264
#define KW_FOR 265
#define KW_IF 266
#define KW_WHILE 267
#define KW_DEFVAR 268
#define OP_PLUS 269
#define OP_MINUS 270
#define OP_DIV 271
#define OP_MULT 272
#define OP_OP 273
#define OP_CP 274
#define IDENTIFIER 275
#define VALUEF 276
#define VALUEI 277
#define COMMENT 278
#define KW_APPEND 279
#define KW_CONCAT 280
#define KW_EXIT 281
#define OP_COMMA 282
#define KW_NIL 283
#define KW_LIST 284
#define KW_LOAD 285
#define KW_PRINT 286
#define KW_TRUE 287
#define KW_FALSE 288




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 56 "gpp_interpreter.y"
{
    char *str;      // For token values
    struct Node *node;     // For parse tree nodes
    double value;   // For fractional values
}
/* Line 1529 of yacc.c.  */
#line 121 "gpp_interpreter.tab.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

