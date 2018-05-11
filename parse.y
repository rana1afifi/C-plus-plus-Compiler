%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "symbolTable.h"

/* prototypes */
int yylex(void);
extern int yylineno; 
extern FILE *yyin;
extern FILE *yyout;
void yyerror(char *s);
void assign(char* argument, int type, int scope, int val_type);
void declare(char *argument, int initialized, int type, int scope, int val_type);
void newScope();
void closeScope();
int validate_scope(int var_scope);
void validate_type(int val_type, int type, char* case_name);
void validate_usage(char* var_name);

int current_data_type;
int val_type, var_type;
int firstop_type, secondop_type; 
int switch_type;
int current_scope = 0;
int max_scope = 0;
int parent_scope[100];
%}

%union {
    int iValue;                 /* integer value */
    char cValue;                /*char value*/
	float fValue;
    char* varName;                /* var name*/
};

%start program
%token WHILE IF INT BOOL CHAR FLOAT SWITCH CASE BREAK DEFAULT CONST DO FOR FL TR PLUS CURLOPBR COLON NOT
%token DIVEQ MULEQ MINEQ PLUSEQ DEC INC AND OR SEMI NEG OPENBRAC CLOSEBRAC LESS EQU MULT CURLCLBR DIV CONTINUE
%token <iValue> INTEGER
%token <varName> VARIABLE
%token <cValue> CHARACTER
%token <fValue> FLOT
%nonassoc IFX
%nonassoc ELSE

%left GE LE EQ NE GREAT LESS
%left PLUS  NEG 
%left MULT  DIV 
%nonassoc UMINUS



%%

program:
        	function                { exit(0); }
;
function:
          	function stmt         {}
        	| /* NULL */
;
stmt:
          	SEMI                            {}
        	|post_prefix SEMI                {}
        	| CURLOPBR  stmt_list CURLCLBR    {}
        	|decleration
			|assign_stmt
			|loop
			|condition
			|BREAK SEMI
			|CONTINUE SEMI	
			|error SEMI
			|error CURLCLBR 
			|error COLON
			|error CLOSEBRAC  
				
;	
loop:
		DO  {newScope();} stmt WHILE  OPENBRAC logi_expr CLOSEBRAC SEMI {closeScope();}
	    |WHILE {newScope();} OPENBRAC logi_expr CLOSEBRAC stmt {closeScope();}  {}
		|FOR{newScope();} OPENBRAC init_cond logi_expr {validate_type(3, current_data_type, "condition");} SEMI loop_exp CLOSEBRAC stmt  {closeScope();} 
;
condition:
		SWITCH  {newScope();} OPENBRAC VARIABLE {validate_usage($4); switch_type = get_type($4); } CLOSEBRAC CURLOPBR case_stmt CURLCLBR {closeScope();}
		| IF {newScope();} OPENBRAC logi_expr {validate_type(3, current_data_type, "condition");} CLOSEBRAC stmt endCondition {closeScope();} {}
;
endCondition: %prec IFX | ELSE {closeScope();} {newScope();} stmt {closeScope();};

stmt_list:
		 stmt_list stmt        { }
		|
        ;
case_stmt:
        	CASE case_exp {validate_type(current_data_type, switch_type, "switchcase");} COLON stmt_list BREAK SEMI case_stmt  
        	|DEFAULT  COLON stmt_list BREAK SEMI                        {}
			|
;
case_exp:
		VARIABLE	{validate_usage($1); current_data_type = get_type($1);}
		|CHARACTER	{current_data_type = 2;}
		|INTEGER	{current_data_type = 0;}
		|FLOT		{current_data_type = 1;}
		|TR		{current_data_type = 3;}
		|FL		{current_data_type = 3;}
;
decleration:
		type VARIABLE SEMI {declare($2,0 , current_data_type,current_scope, -1);}
		|CONST type VARIABLE {var_type = current_data_type + 4;} EQU  val {val_type = current_data_type;} SEMI{ declare($3,1, var_type,current_scope,val_type);}
;
assign_stmt:
		VARIABLE {var_type = get_type($1);} ass_op val { val_type = current_data_type;} SEMI{assign($1,var_type, current_scope,val_type);}
		|type VARIABLE  {var_type = current_data_type;} EQU  val {val_type = current_data_type;} SEMI {declare($2,1 , var_type, current_scope, val_type);}
;
val:
		post_prefix 
		|logi_expr
;
ass_op:
		 EQU 
		|DIVEQ
		|MULEQ 
		|MINEQ 
		|PLUSEQ
		;
post_prefix:
		 VARIABLE INC {validate_usage($1); current_data_type = get_type($1); validate_type(0, current_data_type, "postprefix");}
		|VARIABLE DEC {validate_usage($1); current_data_type = get_type($1); validate_type(0, current_data_type, "postprefix");}
		|INC VARIABLE {validate_usage($2); current_data_type = get_type($2); validate_type(0, current_data_type, "postprefix");}
		|DEC VARIABLE {validate_usage($2); current_data_type = get_type($2); validate_type(0, current_data_type, "postprefix");}
;
math_expr:
		math_expr PLUS {firstop_type = current_data_type;} math_expr {validate_type(firstop_type, current_data_type, "mathexpr");}
		|math_expr NEG {firstop_type = current_data_type;} math_expr {validate_type(firstop_type, current_data_type, "mathexpr");}
		|math_expr MULT {firstop_type = current_data_type;} math_expr {validate_type(firstop_type, current_data_type, "mathexpr");}
		|math_expr DIV {firstop_type = current_data_type;} math_expr {validate_type(firstop_type, current_data_type, "mathexpr");}
		|term
;

term:
		INTEGER					{current_data_type = 0;}
		|VARIABLE				{validate_usage($1); current_data_type = get_type($1);}
		|NEG  math_expr %prec UMINUS {}
		|FLOT					{current_data_type = 1;}
		|OPENBRAC logi_expr CLOSEBRAC
		|CHARACTER				{current_data_type = 2;}
;
logi_expr:
		logi_expr OR and_expr {current_data_type = 3;}|and_expr
;
and_expr:
		and_expr AND equ_expr { current_data_type = 3;}|equ_expr
;
equ_expr:
		equ_expr {firstop_type = current_data_type;} EQ rel_stmt {validate_type(firstop_type, current_data_type, "compare"); current_data_type = 3;}|equ_expr {firstop_type = current_data_type;} NE rel_stmt {validate_type(firstop_type, current_data_type, "compare");  current_data_type = 3;}|rel_stmt 
;
rel_stmt:
		rel_stmt {firstop_type = current_data_type;} GREAT not_expr {validate_type(firstop_type, current_data_type, "compare"); current_data_type = 3;}
		|rel_stmt {firstop_type = current_data_type;} LESS not_expr {validate_type(firstop_type, current_data_type, "compare"); current_data_type = 3;}
		|rel_stmt {firstop_type = current_data_type;} GE not_expr {validate_type(firstop_type, current_data_type, "compare"); current_data_type = 3;}
		|rel_stmt {firstop_type = current_data_type;} LE not_expr {validate_type(firstop_type, current_data_type, "compare"); current_data_type = 3;}
		|TR {current_data_type = 3;}
		|FL {current_data_type = 3;}
		|not_expr		
;
not_expr:
		NOT math_expr	
		|math_expr
;
init_cond:
		assign_stmt
		|SEMI
;
loop_exp:
		post_prefix 
		| VARIABLE {firstop_type = current_data_type;} ass_op math_expr {validate_usage($1); validate_type(firstop_type, current_data_type, "assign");}
		|
;

type:
    		INT {current_data_type=0;}
		|BOOL{current_data_type=3;}
		|CHAR {current_data_type=2;}
		|FLOAT{current_data_type=1;}
;

%%

void declare(char *varName, int initialized, int type, int scope, int val_type){
	//printf("inside declaration \n");
	int check= validate_existence(varName);
	validate_type(val_type, type, "assign");
	//printf("val_type is %d while var type is %d \n", val_type, type);
		if(check == -1)
			add_variable(varName, initialized, type, scope);
		else{
			if(validate_scope(check) == -1){
		
				add_variable(varName, initialized, type, scope);
			}
			else{
			yyerror("Repeated Variable");}
		}
			
}

void assign(char* var_name, int type, int scope, int val_type){
	//printf("inside assign_stm1 %s \n",var_name); 
	int var_scope=validate_existence(var_name); 

	validate_type(val_type, type, "assign");

	if(var_scope==-1) yyerror("Variable not declared"); 
	else {
		if(type >=4){
			int check=validate_Constant(var_name, scope); 
			if(check==-1) yyerror("CONST already initialized");
		}
		validate_scope(var_scope);
	}
	
}

int validate_scope(int var_scope){
if(var_scope == current_scope || var_scope == 0) return;

	int current_parent = current_scope;
	
	while(current_parent != 0){
	current_parent = parent_scope[current_parent];
//printf("validating variable scope %d and current_scope is %d, current_parent is %d\n",  var_scope, current_scope, current_parent);
		if(current_parent == var_scope){
		//printf("returning 1");
		return 1;
		}
	}
	//printf("Variable not declared in this scope %d and var_scope is %d\n", current_scope, var_scope);
	yyerror("Variable not declared in this scope");
	return -1;
}

void validate_type(int val_type, int type, char* case_name){
	if(val_type != -1 && type!= val_type){ 
		if(case_name=="assign") yyerror("Type mismatch error");
		if(case_name== "switchcase") yyerror("Type mismatch error: Case statement type mismatch");
		if(case_name== "mathexpr") yyerror("Type mismatch error: Arguments type mismatch");
		if(case_name== "postprefix") yyerror("Type mismatch error: ++ and -- can only be used with variables of type int");
		if(case_name== "compare") yyerror("Type mismatch error: cannot compare variables of different types");
		if(case_name== "condition") yyerror("Type mismatch error: condition must be of type boolean");
		}
}

void validate_usage(char* var_name){
	int var_scope=validate_existence(var_name); 

	//validate_type(val_type, type);

	if(var_scope==-1){
		yyerror("Variable not declared");
	}
	else {
		int initialized=validate_Initialization(var_name,current_scope);
		if(initialized == -1){
			yyerror("Variable used but not initialized");
		}
		else{
			validate_scope(var_scope);
		}
	}
}

void newScope(){
	if(max_scope == 0) 
	parent_scope[max_scope] = 0;

	parent_scope[max_scope+1] = current_scope;
	current_scope = ++max_scope;
	
	//printf("new scope %d and max scope is %d parent is %d\n", max_scope, max_scope, parent_scope[max_scope]);
	
}

void closeScope(){
	current_scope--;
	//printf("closing scope, current is %d\n", current_scope);
}

void yyerror(char *s) {
	//yyout = fopen("errors.txt","w");
    fprintf(stdout, "%s in line number:%d\n", s,yylineno);

	//fclose(yyout);
}

int main(void) {
	FILE *myfile = fopen("test.txt", "r");
	// make sure it is valid:
	if (!myfile) {
		return -1;
	}
	// set lex to read from it instead of defaulting to STDIN:
	yyin = myfile;
	
	// parse through the input until there is no more:
	do {
		yyparse();
	} while (!feof(yyin));
	print_symbol_table_two();
	
    return 0;
}
