/*
      mfpl.y

 	Specifications for the MFPL language, bison input file.

      To create syntax analyzer:

        flex mfpl.l
        bison mfpl.y
        g++ mfpl.tab.c -o mfpl_parser
        mfpl_parser < inputFileName
 */

/*
 *	Declaration section.
 */
%{
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <string>
#include <stack>
#include "SymbolTable.h"
using namespace std;

#define ARITHMETIC_OP	1   // classification for operators
#define LOGICAL_OP   	2
#define RELATIONAL_OP	3

/* Arithmetic operators */
#define ADD 1
#define SUB 2
#define MUL 3
#define DIV 4

/* Binary logical operators */
#define AND 11
#define OR  12

/* Relational Operators */
#define GT  21
#define GE  22
#define LT  23
#define LE  24
#define EQ  25
#define NE  26

typedef struct {
  int type;
  int operator;
} operator;

int lineNum = 1;

stack<SYMBOL_TABLE> scopeStack;    // stack of scope hashtables

bool isIntCompatible(const int theType);
bool isStrCompatible(const int theType);
bool isIntOrStrCompatible(const int theType);

void beginScope();
void endScope();
void cleanUp();
TYPE_INFO findEntryInAnyScope(const string theName);

void printRule(const char*, const char*);
int yyerror(const char* s) {
  printf("Line %d: %s\n", lineNum, s);
  cleanUp();
  exit(1);
}

extern "C" {
    int yyparse(void);
    int yylex(void);
    int yywrap() {return 1;}
}

%}

%union {
  char* text;
  int num;
  TYPE_INFO typeInfo;
  value val;
  operator op;
};

/*
 *	Token declarations
*/
%token  T_LPAREN T_RPAREN 
%token  T_IF T_LETSTAR T_PRINT T_INPUT
%token  T_ADD  T_SUB  T_MULT  T_DIV
%token  T_LT T_GT T_LE T_GE T_EQ T_NE T_AND T_OR T_NOT	 
%token  T_INTCONST T_STRCONST T_T T_NIL T_IDENT T_UNKNOWN

%type	<text> T_IDENT
%type <typeInfo> N_EXPR N_PARENTHESIZED_EXPR N_ARITHLOGIC_EXPR  
%type <typeInfo> N_CONST N_IF_EXPR N_PRINT_EXPR N_INPUT_EXPR 
%type <typeInfo> N_LET_EXPR N_EXPR_LIST  
%type <op> N_BIN_OP N_ARITH_OP N_REL_OP N_LOG_OP
%type <val> T_INTCONST T_STRCONST T_T T_NIL

/*
 *	Starting point.
 */
%start  N_START

/*
 *	Translation rules.
 */
%%
N_START		: N_EXPR
			{
			printRule("START", "EXPR");
			printf("\n---- Completed parsing ----\n\n");
			
      /* Print value */
      printf("\nValue of the expression is: ");
      if ($1.type == INT) {
        printf("%d\n", $1.val.intVal);
      }
      if ($1.type == STR) {
        printf("%s\n", $1.val.strVal);
      }
      if ($1.type == BOOL) {
        printf( "%s\n",  $1.val.boolVal ? "t" : "nil" );
      }
      
      return 0;
			}
			;
N_EXPR		: N_CONST
			{
			printRule("EXPR", "CONST");
			$$.type = $1.type;
      $$.val = $1.val; 
			}
                | T_IDENT
                {
			printRule("EXPR", "IDENT");
                	string ident = string($1);
                	TYPE_INFO exprTypeInfo = 
						findEntryInAnyScope(ident);
                	if (exprTypeInfo.type == UNDEFINED) {
                	  yyerror("Undefined identifier");
                	  return(0);
               	}
                	$$.type = exprTypeInfo.type;
                  $$.val = exprTypeInfo.val; 
			}
                | T_LPAREN N_PARENTHESIZED_EXPR T_RPAREN
                {
			printRule("EXPR", "( PARENTHESIZED_EXPR )");
			$$.type = $2.type; 
      $$.val = $2.val;
			}
			;
N_CONST		: T_INTCONST
			{
			printRule("CONST", "INTCONST");
                	$$.type = INT; 
                  $$.val.intVal = $1.intVal;
			}
                | T_STRCONST
			{
			printRule("CONST", "STRCONST");
                	$$.type = STR; 
                  $$.val.strVal = $1.strVal;
			}
                | T_T
                {
			printRule("CONST", "t");
                	$$.type = BOOL; 
                  $$.val.boolVal = true;
			}
                | T_NIL
                {
			printRule("CONST", "nil");
			$$.type = BOOL; 
      $$.val.boolVal = false;
			}
			;
N_PARENTHESIZED_EXPR	: N_ARITHLOGIC_EXPR 
				{
				printRule("PARENTHESIZED_EXPR",
                                "ARITHLOGIC_EXPR");
				$$.type = $1.type; 
        $$.val = $1.val;
				}
                      | N_IF_EXPR 
				{
				printRule("PARENTHESIZED_EXPR", "IF_EXPR");
				$$.type = $1.type; 
				}
                      | N_LET_EXPR 
				{
				printRule("PARENTHESIZED_EXPR", 
                                "LET_EXPR");
				$$.type = $1.type; 
        $$.val = $1.val;
				}
                      | N_PRINT_EXPR 
				{
				printRule("PARENTHESIZED_EXPR", 
					    "PRINT_EXPR");
				$$.type = $1.type; 
        $$.val = $1.val;
				}
                      | N_INPUT_EXPR 
				{
				printRule("PARENTHESIZED_EXPR",
					    "INPUT_EXPR");
				$$.type = $1.type; 
        $$.val = $1.val;
				}
                     | N_EXPR_LIST 
				{
				printRule("PARENTHESIZED_EXPR",
				          "EXPR_LIST");
				$$.type = $1.type; 
        $$.val = $1.val;
				}
				;
N_ARITHLOGIC_EXPR	: N_UN_OP N_EXPR
				{
				printRule("ARITHLOGIC_EXPR", 
				          "UN_OP EXPR");
                      $$.type = BOOL; 
                      if( $2.type & BOOL ) {
                        $$.val.boolVal = !$2.val.boolVal;
                      } else {
                        $$.val.boolVal = false;
                      }
				}
				| N_BIN_OP N_EXPR N_EXPR
				{
				printRule("ARITHLOGIC_EXPR", 
				          "BIN_OP EXPR EXPR");
                      $$.type = BOOL;
                      switch ($1)
                      {
                      case (ARITHMETIC_OP) :
                        $$.type = INT;
                        if (!isIntCompatible($2.type)) {
                          yyerror("Arg 1 must be integer");
                          return(0);
                     	  }
                     	  if (!isIntCompatible($3.type)) {
                          yyerror("Arg 2 must be integer");
                          return(0);
                     	  }

          switch( $1.operator ) {
          
          case ADD:
 $$.val.intval = $2.val.intVal + $3.val.intVal;
break;

case SUB:
$$.val.intVal = $2.val.intVal - $3.val.intVal;
break;

case MUL:
$$.val.intVal = $2.val.intVal * $3.val.intVal;
break;

case DIV:
$$.val.intVal = $2.val.intVal / $3.val.intval;
break;

          }

                        break;

				case (LOGICAL_OP) :

switch( $1.operator ) {

case AND:

  if( !( $2.type & bool ) || !( $3.type & bool ) ) {
    $$.val.boolVal = false;
  } else {

    $$.val.boolVal = $2.val.boolVal && $3.val.boolVal;

  }

  break;
case OR:
  if( !( $2.type & bool ) || !( $3.type & bool ) ) {
    $$.val.boolVal = false;
  } else {

    $$.val.boolVal = $2.val.boolVal || $3.val.boolVal;

  }


  break;
}

                        break;

                      case (RELATIONAL_OP) :
                        if (!isIntOrStrCompatible($2.type)) {
                          yyerror("Arg 1 must be integer or string");
                          return(0);
                        }
                        if (!isIntOrStrCompatible($3.type)) {
                          yyerror("Arg 2 must be integer or string");
                          return(0);
                        }
                        if (isIntCompatible($2.type) &&
                            !isIntCompatible($3.type)) {
                          yyerror("Arg 2 must be integer");
                          return(0);
                     	  }
                        else if (isStrCompatible($2.type) &&
                                 !isStrCompatible($3.type)) {
                               yyerror("Arg 2 must be string");
                               return(0);
                             }

switch( $1.operator ) {

case GT:
  if( ( $2.type & STR ) && ( $3.type & STR ) ) {
    $$.val.boolVal = ( strcmp( $2.val.strVal, $3.val.strVal ) > 0 );
  } else {
    $$.val.boolVal = $2.val.intVal > $3.val.intVal;
  }
break;
case GE:
  if( ( $2.type & STR ) && ( $3.type & STR ) ) {
    $$.val.boolVal = ( strcmp( $2.val.strVal, $3.val.strVal ) >= 0 );
  } else {
    $$.val.boolVal = $2.val.intVal >= $3.val.intVal;
  }
break;
case LT:
  if( ( $2.type & STR ) && ( $3.type & STR ) ) {
    $$.val.boolVal = ( strcmp( $2.val.strVal, $3.val.strVal ) < 0 );
  } else {
    $$.val.boolVal = $2.val.intVal > $3.val.intVal;
  }
break;
case LE:
  if( ( $2.type & STR ) && ( $3.type & STR ) ) {
    $$.val.boolVal = ( strcmp( $2.val.strVal, $3.val.strVal ) <= 0 );
  } else {
    $$.val.boolVal = $2.val.intVal <= $3.val.intVal;
  }

break;
case EQ:
  if( ( $2.type & STR ) && ( $3.type & STR ) ) {
    $$.val.boolVal = ( strcmp( $2.val.strVal, $3.val.strVal ) == 0 );
  } else {
    $$.val.boolVal = $2.val.intVal == $3.val.intVal;
  }

break;
case NE:
  if( ( $2.type & STR ) && ( $3.type & STR ) ) {
    $$.val.boolVal = ( strcmp( $2.val.strVal, $3.val.strVal ) != 0 );
  } else {
    $$.val.boolVal = $2.val.intVal != $3.val.intVal;
  }

break;

}

                        break; 
                      }  // end switch
				}
                     	;
N_IF_EXPR    	: T_IF N_EXPR N_EXPR N_EXPR
			{
			printRule("IF_EXPR", "if EXPR EXPR EXPR");
      
if( $2.val.boolVal ) {
  $$.type = $3.type;
  $$.val = $3.val;
} else {
  $$.type = $4.type;
  $$.type = $4.val;
}
 
			}
			;
N_LET_EXPR      : T_LETSTAR T_LPAREN N_ID_EXPR_LIST T_RPAREN 
                  N_EXPR
			{
			printRule("LET_EXPR", 
				    "let* ( ID_EXPR_LIST ) EXPR");
			endScope();
                $$.type = $5.type; 
                $$.val = $5.val;
			}
			;
N_ID_EXPR_LIST  : /* epsilon */
			{
			printRule("ID_EXPR_LIST", "epsilon");
			}
                | N_ID_EXPR_LIST T_LPAREN T_IDENT N_EXPR T_RPAREN 
			{
			printRule("ID_EXPR_LIST", 
                          "ID_EXPR_LIST ( IDENT EXPR )");
			string lexeme = string($3);
                 TYPE_INFO exprTypeInfo = $4;
                 printf("___Adding %s to symbol table\n", $3);
                 bool success = scopeStack.top().addEntry
                                (SYMBOL_TABLE_ENTRY(lexeme,
									 exprTypeInfo));
                 if (! success) {
                   yyerror("Multiply defined identifier");
                   return(0);
                 }
			}
			;
N_PRINT_EXPR    : T_PRINT N_EXPR
			{
			printRule("PRINT_EXPR", "print EXPR");
                $$.type = $2.type;
                $$.val = $2.val;
      if( $2.type & INT ) {
        printf( "%d\n", $2.val.intVal );
			} else if ( $2.type & BOOL ) {
        printf( "%s\n", $2.val.boolVal ? "t" : "nil" );
      } else { // type == str
        printf( "%s\n", $2.val.strVal );
      }
			;
N_INPUT_EXPR    : T_INPUT
			{
			printRule("INPUT_EXPR", "input");

string input;
getline( cin, input );
if( input[0] == '-' || input[0] == '+' ) {
  $$.type = INT;
  $$.val.intVal = atoi( input.c_str() );
} else {
  $$.type = STR;
  $$.val.strVal = input.c_str();
}
			}
			;
N_EXPR_LIST     : N_EXPR N_EXPR_LIST  
			{
			printRule("EXPR_LIST", "EXPR EXPR_LIST");
                $$.type = $2.type;
                $$.val = $2.val;
			}
                | N_EXPR
			{
			printRule("EXPR_LIST", "EXPR");
                $$.type = $1.type;
                $$.val = $1.val;
			}
			;
N_BIN_OP	     : N_ARITH_OP
			{
			printRule("BIN_OP", "ARITH_OP");
			$$ = ARITHMETIC_OP;
			}
			|
			N_LOG_OP
			{
			printRule("BIN_OP", "LOG_OP");
			$$ = LOGICAL_OP;
			}
			|
			N_REL_OP
			{
			printRule("BIN_OP", "REL_OP");
			$$ = RELATIONAL_OP;
			}
			;
N_ARITH_OP	     : T_ADD
			{
			printRule("ARITH_OP", "+");
      $$.operator = ADD;
			}
                | T_SUB
			{
			printRule("ARITH_OP", "-");
      $$.operator = SUB;
			}
			| T_MULT
			{
			printRule("ARITH_OP", "*");
      $$.operator = MUL;
			}
			| T_DIV
			{
			printRule("ARITH_OP", "/");
      $$.operator = DIV;
			}
			;
N_REL_OP	     : T_LT
			{
			printRule("REL_OP", "<");  
      operator = LT;
			}	
			| T_GT
			{
			printRule("REL_OP", ">");    
      operator = GT; 
			}	
			| T_LE
			{
			printRule("REL_OP", "<=");  
      operator = LE; 
			}	
			| T_GE
			{
			printRule("REL_OP", ">=");  
      operator = GE; 
			}	
			| T_EQ
			{
			printRule("REL_OP", "=");  
      operator = EQ;
			}	
			| T_NE
			{
			printRule("REL_OP", "/=");  
      operator = NE; 
			}
			;	
N_LOG_OP	     : T_AND
			{
			printRule("LOG_OP", "and");  
      operator = AND;
			}	
			| T_OR
			{
			printRule("LOG_OP", "or");  
      operator = OR; 
			}
			;
N_UN_OP	     : T_NOT
			{
			printRule("UN_OP", "not");
			}
			;
%%

#include "lex.yy.c"
extern FILE *yyin;

bool isIntCompatible(const int theType) {
  return((theType == INT) || (theType == INT_OR_STR) ||
         (theType == INT_OR_BOOL) || 
         (theType == INT_OR_STR_OR_BOOL));
}

bool isStrCompatible(const int theType) {
  return((theType == STR) || (theType == INT_OR_STR) ||
         (theType == STR_OR_BOOL) || 
         (theType == INT_OR_STR_OR_BOOL));
}

bool isIntOrStrCompatible(const int theType) {
  return(isStrCompatible(theType) || isIntCompatible(theType));
}

void printRule(const char* lhs, const char* rhs) {
  printf("%s -> %s\n", lhs, rhs);
  return;
}

void beginScope() {
  scopeStack.push(SYMBOL_TABLE());
  printf("\n___Entering new scope...\n\n");
}

void endScope() {
  scopeStack.pop();
  printf("\n___Exiting scope...\n\n");
}

TYPE_INFO findEntryInAnyScope(const string theName) {
  TYPE_INFO info = {UNDEFINED};
  if (scopeStack.empty( )) return(info);
  info = scopeStack.top().findEntry(theName);
  if (info.type != UNDEFINED)
    return(info);
  else { // check in "next higher" scope
	   SYMBOL_TABLE symbolTable = scopeStack.top( );
	   scopeStack.pop( );
	   info = findEntryInAnyScope(theName);
	   scopeStack.push(symbolTable); // restore the stack
	   return(info);
  }
}

void cleanUp() {
  if (scopeStack.empty()) 
    return;
  else {
        scopeStack.pop();
        cleanUp();
  }
}

int main( int argc, char** argv ) {

  if( argc < 2 ) {
    printf( "Usage: mfpl <input file>" );
    return 1;
  }

  yyin = fopen( argv[ 1 ], "r" );
  do {
    yyparse();
  } while( !feof( yyin ) );

  return 0;
}
