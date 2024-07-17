    /* cs152-miniL phase2 */
%{
#include <stdio.h>
extern int currLine;
extern int currPos;
extern FILE * yyin;void yyerror(const char *msg);

%}

%union{
	char * ident;
	int num;
}


%error-verbose
%start program

%token FUNCTION BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY INTEGER ARRAY OF IF THEN ENDIF ELSE WHILE DO FOR BEGINLOOP ENDLOOP CONTINUE READ WRITE TRUE FALSE ASSIGN SEMICOLON COLON COMMA L_PAREN R_PAREN L_SQUARE_BRACKET R_SQUARE_BRACKET RETURN MULT DIV MOD ADD SUB LT LTE GT GTE EQ NEQ NOT AND OR ENUM
%token <num> NUMBER 
%token <ident> IDENT

%% 

program: functions {printf("program -> functions\n");}
        | error {yyerrok; yyclearin;}
       		;

function: FUNCTION identifier SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY
		{printf("function -> FUNCTION identifier SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY\n");};

functions:
	/* Epsilon */
		{printf("functions -> epsilon\n");}
	| function functions
		{printf("functions -> function functions\n");};


vars: var {printf("vars -> var\n");}
    | var COMMA vars {printf("vars -> var COMMA vars\n");}
    ;

var: identifier {printf("var -> identifier\n");}
    | identifier L_SQUARE_BRACKET expression R_SQUARE_BRACKET {printf("var -> identifier L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n");}
    ;

declarations: {printf("declarations -> epsilon\n");}
              | declaration SEMICOLON  declarations {printf("declarations -> declaration declarations\n");}
            ;

declaration: identifiers  COLON INTEGER {printf("declaration -> identifiers COMMA COLON INTEGER\n");}
              | identifiers COLON ENUM L_PAREN identifiers R_PAREN {printf("declaration -> identifiers COLON ENUM L_PAREN identifiers R_PAREN\n");}
              | identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER {printf("declaration -> identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER %d R_SQUARE_BRACKET OF INTEGER \n", $5);}
              ;

identifiers: identifier {printf("identifiers -> identifier\n");}
              | identifier COMMA identifiers {printf("identifiers -> identifier COMMA identifiers\n");}
              ;

identifier: IDENT {printf("identifier -> IDENT %s\n", $1);}

expression: MultiplicativeExpr {printf("expression -> MultiplicativeExpr\n");}
          | MultiplicativeExpr ADD expression {printf("expression -> MultiplicativeExpr ADD expression\n");}
          | MultiplicativeExpr SUB expression {printf("expression -> MultiplicativeExpr SUB expression\n");}
          ;

statements: statement SEMICOLON {printf("statement -> statement SEMICOLON\n");}
            | statement SEMICOLON statements {printf("statements -> statement SEMICOLON statements\n");}
            ;

statement:  var ASSIGN expression {printf("statement -> var ASSIGN expression\n");}
            | IF BoolExpr THEN statements ENDIF {printf("statement -> IF BoolExpr THEN statements ENDIF\n");}
            | IF BoolExpr THEN statements ELSE statements ENDIF  {printf("statement -> IF BoolExpr THEN statements ELSE statements endif\n");}
            | WHILE BoolExpr BEGINLOOP statements ENDLOOP {printf("statement -> WHILE BoolExpr BEGINLOOP statements ENDLOO\n");}
            | DO BEGINLOOP statements ENDLOOP WHILE BoolExpr {printf("statement -> DO BEGINLOOP statements ENDLOOP WHILE BoolExp\n");}
            | READ vars {printf("statement -> READ vars\n");}
            | CONTINUE {printf("statement -> continue\n");}
            | WRITE vars {printf("statement -> WRITE vars\n");}
            | RETURN expression {printf("statement -> RETURN expression\n");}
            ;

BoolExpr:    RelationAndExpr {printf("BoolExpr -> RelationAndExpr\n");}
            | RelationAndExpr OR BoolExpr {printf("BoolExpr ->  RelationAndExpr OR BoolExpr\n");}
            ;

RelationAndExpr: RelationExpr {printf("RelationAndExpr -> RelationExpr\n");}
                | RelationExpr AND RelationAndExpr {printf("RelationAndExpr -> RelationExpr AND RelationAndExpr\n");}
                ;

RelationExpr:  expression compare expression {printf("RelationExpr -> expression compare expression\n");}
              | TRUE {printf("RelationExpr -> TRUE\n");}
              | FALSE {printf("RelationExpr -> FALSE\n");}
              | L_PAREN BoolExpr R_PAREN {printf("RelationExpr -> L_PAREN BoolExpr R_PAREN\n");}
              | NOT expression compare expression {printf("NOT RelationExpr -> expression compare expression\n");}
              | NOT TRUE {printf("RelationExpr -> NOT TRUE\n");}
              | NOT FALSE {printf("RelationExpr -> NOT FALSE\n");}
              | NOT L_PAREN BoolExpr R_PAREN {printf("RelationExpr -> NOT L_PAREN BoolExpr R_PAREN\n");}
              ;

compare: EQ {printf("compare -> EQ\n");}
        | NEQ {printf("compare -> NEQ\n");}
        | LT {printf("compare -> LT\n");}
        | GT {printf("compare -> GT\n");}
        | LTE {printf("compare -> LTE\n");}
        | GTE {printf("compare -> GTE\n");}
        ;

MultiplicativeExpr: term {printf("MultiplicativeExpr -> term\n");}
                  | term MOD MultiplicativeExpr {printf("MultiplicativeExpr -> term MOD MultiplicativeExpr\n");}
                  | term DIV MultiplicativeExpr {printf("MultiplicativeExpr -> term DIV MultiplicativeExpr\n");}
                  | term MULT MultiplicativeExpr {printf("MultiplicativeExpr -> term MULT MultiplicativeExpr\n");}
                  ;

term: identifier L_PAREN expressions R_PAREN {printf("term -> identifier L_PAREN expressions R_PAREN\n");}
    | var {printf("term -> var\n");}
    | NUMBER {printf("term -> NUMBER % d\n", $1);}
    | L_PAREN expression R_PAREN {printf("term -> L_PAREN expression R_PAREN\n");}
    | SUB var {printf("term -> SUB var\n");}
    | SUB NUMBER {printf("term -> SUB NUMBER %d\n", $2);}
    | SUB L_PAREN expression R_PAREN {printf("term -> SUB L_PAREN expression R_PAREN\n");}
    ;
    


expressions: expression {printf("expressions -> expression\n");}
            | expression COMMA expressions {printf("expressions -> expression COMMA expressions\n");}
            ;


%% 
/*
int main(int argc, char **argv) {

   if(argc >= 2){
      yyin = fopen(argv[1], "r");
      if(yyin == NULL){
         printf("syntax: %s filename\n", argv[0]);
      }
   }
   yyparse();
   return 0;
}
*/

int main(int argc, char ** argv) {
	if (argc >= 2) {
		yyin = fopen(argv[1], "r");
		if (yyin == NULL) {
			yyin = stdin;
		}
	}
	else {
		yyin = stdin;
	}
	yyparse();
	return 1;
}

void yyerror(const char *msg) {
   printf("** Line %d, position %d: %s\n", currLine, currPos, msg);
}