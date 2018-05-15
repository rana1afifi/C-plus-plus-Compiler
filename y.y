%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "symbolTable.h"

#define YYERROR_VERBOSE

/* prototypes */
int yylex(void);
extern int yylineno; 
extern FILE *yyin;
extern FILE *yyout;
void yyerror(char *s);
int assign(char* argument, int type, int scope, int val_type);
int declare(char *argument, int initialized, int type, int scope, int val_type);
void newScope();
void closeScope();
int validate_scope(int var_scope, int declare);
int validate_type(int val_type, int type, char* case_name);
int validate_usage(char* var_name);

int current_data_type;
int val_type, var_type;
int firstop_type, secondop_type; 
int switch_type;
int reg_count = 0;
int labelS_count=0;
int switch_id=0;
int if_id=0;
int assign_reg_count;
int firsthp_reg, second_reg;
int firstlp_reg;
int firstlog_reg, firstand_reg,firstor_reg,firstrel_reg, firstequ_reg,firstnot_reg;
int current_scope = 0;
int max_scope = 0;
int parent_scope[100];
int elif = 0;

FILE *assembly;
int register_number=0;
char current_variable[50];
char assign_expression[256];
char* op;
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
        	function                {print_unused_variables();print_symbol_table_two();exit(0); }
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
		DO  {newScope(); reg_count = 0; fprintf(assembly,"\n\n Label_%d: \t",current_scope);} stmt WHILE  OPENBRAC logi_expr CLOSEBRAC SEMI {fprintf(assembly,"\n JT R%d, Label_%d \t",reg_count,current_scope);  closeScope();}
	    |WHILE {newScope(); fprintf(assembly,"\n\n Label_%d:", current_scope);}  OPENBRAC logi_expr { fprintf(assembly,"\n JF R%d, Label_%d_Exit \t", reg_count,current_scope); reg_count = 0;} CLOSEBRAC stmt {fprintf(assembly,"\n JMP Label_%d: \n Label_%d_Exit: ",current_scope ,current_scope);closeScope();}
		|FOR{newScope();reg_count = 0;} OPENBRAC init_cond { fprintf(assembly,"\n\n Label_%d:", current_scope);}logi_expr { firstlog_reg = reg_count; validate_type(3, current_data_type, "condition");  fprintf(assembly," \n JF R%d, Label_%d_Exit \t",firstlog_reg,current_scope);} SEMI loop_exp CLOSEBRAC stmt  {fprintf(assembly,"\n JMP Label_%d: \n Label_%d_Exit: ",current_scope ,current_scope); closeScope();} 
;
condition:
		SWITCH  {newScope();} OPENBRAC VARIABLE {validate_usage($4); switch_type = get_type($4);reg_count = 0;fprintf(assembly,"\n MOV RS , %s",$4); } CLOSEBRAC CURLOPBR case_stmt CURLCLBR {fprintf(assembly,"\n Label_Switch%d_Exit: \t",switch_id++); closeScope();}
		| IF {if(elif == 1) {elif = 0; closeScope(); max_scope--;}; newScope(); reg_count = 0;} OPENBRAC logi_expr { fprintf(assembly,"\n\n JF R%d, Label_%d_Exit \t",reg_count,current_scope); validate_type(3, current_data_type, "condition");} CLOSEBRAC stmt {fprintf(assembly,"\n JMP Label_If%d_Exit \n Label_%d_Exit: \t",if_id,current_scope); closeScope();} endCondition  {fprintf(assembly,"\n Label_If%d_Exit: ",if_id++);}
;
endCondition: %prec IFX | ELSE {newScope(); elif = 1;} stmt {closeScope(); elif = 0;};

stmt_list:
		 stmt_list stmt        { }
		|
        ;
case_stmt:
        	CASE case_exp { validate_type(current_data_type, switch_type, "switchcase");fprintf(assembly,"\n CMPE R%d, RS, R%d",++reg_count,reg_count );fprintf(assembly,"\n\n JF R%d, LabelS_%d_Exit \t",reg_count,labelS_count);} COLON stmt_list BREAK SEMI{ fprintf(assembly,"\n JMP Label_Switch%d_Exit \t",switch_id);fprintf(assembly,"\n\n LabelS_%d_Exit: \t",labelS_count++);} case_stmt  
        	|DEFAULT  COLON stmt_list BREAK SEMI                        {}
			|
;
case_exp:
		VARIABLE	{validate_usage($1); current_data_type = get_type($1);fprintf(assembly,"\n MOV R%d, %s",reg_count, $1);}
		|CHARACTER	{current_data_type = 2; fprintf(assembly,"\n MOV R%d, '%c'",reg_count,$1 );}
		|INTEGER	{current_data_type = 0; fprintf(assembly,"\n MOV R%d, %d",reg_count, $1);}
		|FLOT		{current_data_type = 1; fprintf(assembly,"\n MOV R%d, %f", reg_count,$1 ); }
		|TR		{current_data_type = 3; fprintf(assembly,"\n MOV R%d, true", reg_count ); }
		|FL		{current_data_type = 3; fprintf(assembly,"\n MOV R%d, false", reg_count ); }
;
decleration:
		type VARIABLE SEMI {declare($2,0 , current_data_type,current_scope, -1);}
		|CONST type VARIABLE {var_type = current_data_type + 4;} EQU  val {val_type = current_data_type;} SEMI{ int valid = declare($3,1, var_type,current_scope,val_type); if(valid==1)fprintf(assembly,"\n MOV %s , R%d", $3 ,reg_count);}
;
assign_stmt:
		VARIABLE { reg_count = 0; var_type = get_type($1); set_used($1); } ass_op val { val_type = current_data_type;} SEMI{int valid=assign($1,var_type, current_scope,val_type); if(valid==1){{fprintf(assembly,"\n %s %s , R%d",op, $1 ,reg_count);} assign_variable($1);} }
		|type VARIABLE  {reg_count = 0; assign_reg_count = reg_count; var_type = current_data_type;} EQU  val {val_type = current_data_type; } SEMI {int valid=declare($2,1 , var_type, current_scope, val_type);if(valid==1){fprintf(assembly,"\n MOV %s , R%d", $2 ,reg_count);} }
;
val:
		post_prefix 
		|logi_expr
;
ass_op:
		 EQU {op="MOV";}
		|DIVEQ {op="DIVEQ";}
		|MULEQ {op="MULEQ";}
		|MINEQ {op="MINEQ";}
		|PLUSEQ {op="PLUSEQ";}
;
post_prefix:
		 VARIABLE INC {validate_usage($1); current_data_type = get_type($1); validate_type(0, current_data_type, "postprefix"); fprintf(assembly,"\n MOV R0 , %s",$1);fprintf(assembly,"\n MOV R1 , %s",$1);fprintf(assembly,"\n INC R1 ");fprintf(assembly,"\n MOV %s , R1",$1);}
		|VARIABLE DEC {validate_usage($1); current_data_type = get_type($1); validate_type(0, current_data_type, "postprefix"); fprintf(assembly,"\n MOV R0 , %s",$1);fprintf(assembly,"\n MOV R1 , %s",$1);fprintf(assembly,"\n DEC R1 ");fprintf(assembly,"\n MOV %s , R1",$1);}
		|INC VARIABLE {validate_usage($2); current_data_type = get_type($2); validate_type(0, current_data_type, "postprefix"); fprintf(assembly,"\n MOV R0 , %s",$2);fprintf(assembly,"\n INC R0 ");fprintf(assembly,"\n MOV %s , R0",$2);}
		|DEC VARIABLE {validate_usage($2); current_data_type = get_type($2); validate_type(0, current_data_type, "postprefix"); fprintf(assembly,"\n MOV R0 , %s",$2);fprintf(assembly,"\n DEC R0 ");fprintf(assembly,"\n MOV %s , R0",$2);}
;
math_expr:
		math_expr PLUS {firstop_type = current_data_type; firstlp_reg = reg_count++;} math_expr {validate_type(firstop_type, current_data_type, "mathexpr");  second_reg = reg_count++;  fprintf(assembly,"\n ADD R%d, R%d, R%d", reg_count,firstlp_reg,second_reg );}
		|math_expr NEG {firstop_type = current_data_type; firstlp_reg = reg_count++;} math_expr {validate_type(firstop_type, current_data_type, "mathexpr");second_reg = reg_count++; fprintf(assembly,"\n SUB R%d, R%d, R%d", reg_count,firstlp_reg, second_reg );}
		|math_expr MULT {firstop_type = current_data_type; firsthp_reg = reg_count++; } math_expr {validate_type(firstop_type, current_data_type, "mathexpr"); second_reg = reg_count++; fprintf(assembly,"\n MUL R%d, R%d, R%d", reg_count,firsthp_reg, second_reg );}
		|math_expr DIV {firstop_type = current_data_type; firsthp_reg = reg_count++;} math_expr {validate_type(firstop_type, current_data_type, "mathexpr"); second_reg = reg_count++; fprintf(assembly,"\n DIV R%d, R%d, R%d", reg_count,firsthp_reg, second_reg );}
		|term 
;


term:
		INTEGER					{current_data_type = 0; fprintf(assembly,"\n MOV R%d, %d",reg_count, $1);}
		|VARIABLE				{validate_usage($1); current_data_type = get_type($1); fprintf(assembly,"\n MOV R%d, %s",reg_count, $1);}
		|NEG  math_expr %prec UMINUS {fprintf(assembly,"\n NEG R%d", reg_count); }
		|FLOT					{current_data_type = 1; fprintf(assembly,"\n MOV R%d, %f", reg_count,$1 ); }
		|OPENBRAC {reg_count = 0;} logi_expr CLOSEBRAC
		|CHARACTER				{current_data_type = 2; fprintf(assembly,"\n MOV R%d, '%c'",reg_count,$1 ); }
;
logi_expr:
		logi_expr {firstlog_reg = reg_count++;}OR and_expr {current_data_type = 3; second_reg = reg_count++; fprintf(assembly,"\n OR R%d, R%d, R%d", reg_count,firstlog_reg,second_reg );}
		|and_expr
;
and_expr:
		and_expr {firstand_reg = reg_count++;} AND equ_expr { current_data_type = 3; second_reg = reg_count++; fprintf(assembly,"\n AND R%d, R%d, R%d", reg_count,firstand_reg,second_reg );}
		|equ_expr
;
equ_expr:
		 equ_expr {firstop_type = current_data_type;  firstequ_reg = reg_count++;} EQ rel_stmt {validate_type(firstop_type, current_data_type, "compare"); current_data_type = 3; second_reg = reg_count++;  fprintf(assembly,"\n CMPE R%d, R%d, R%d", reg_count,firstequ_reg,second_reg );}
		|equ_expr {firstop_type = current_data_type;  firstequ_reg = reg_count++;} NE rel_stmt {validate_type(firstop_type, current_data_type, "compare");  current_data_type = 3; second_reg = reg_count++;  fprintf(assembly,"\n CMPNE R%d, R%d, R%d", reg_count,firstequ_reg,second_reg );}
		|rel_stmt 
;
rel_stmt:
		rel_stmt {firstop_type = current_data_type;  firstrel_reg = reg_count++;} GREAT not_expr {validate_type(firstop_type, current_data_type, "compare"); current_data_type = 3; second_reg = reg_count++;  fprintf(assembly,"\n CMPG R%d, R%d, R%d", reg_count,firstrel_reg,second_reg );}
		|rel_stmt {firstop_type = current_data_type;  firstrel_reg = reg_count++;}  LESS not_expr {validate_type(firstop_type, current_data_type, "compare"); current_data_type = 3; second_reg = reg_count++;  fprintf(assembly,"\n CMPL R%d, R%d, R%d", reg_count,firstrel_reg,second_reg );}
		|rel_stmt {firstop_type = current_data_type; firstrel_reg = reg_count++;} GE not_expr {validate_type(firstop_type, current_data_type, "compare"); current_data_type = 3; second_reg = reg_count++;  fprintf(assembly,"\n CMPGE R%d, R%d, R%d", reg_count,firstrel_reg,second_reg );}
		|rel_stmt {firstop_type = current_data_type; firstrel_reg = reg_count++;} LE not_expr {validate_type(firstop_type, current_data_type, "compare"); current_data_type = 3; second_reg = reg_count++;  fprintf(assembly,"\n CMPLE R%d, R%d, R%d", reg_count,firstrel_reg,second_reg );}
		|TR {current_data_type = 3; fprintf(assembly,"\n MOV R%d, true", reg_count); }
		|FL {current_data_type = 3; fprintf(assembly,"\n MOV R%d, false", reg_count);}
		|not_expr		
;
not_expr:
		NOT math_expr	{second_reg = reg_count; fprintf(assembly,"\n NOT R%d", second_reg);}
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

int declare(char *varName, int initialized, int type, int scope, int val_type){
	//printf("inside declaration \n");
	int check= validate_existence(varName);
	int v_type=validate_type(val_type, type, "assign");
	if(v_type==-1) return -1;
	//printf("val_type is %d while var type is %d \n", val_type, type);
		if(check == -1)
			add_variable(varName, initialized, type, scope);
		else{
			if(validate_scope(check, 1) == -1){		
				add_variable(varName, initialized, type, scope);}
			else{
				yyerror("Repeated Variable");
				return -1;
				}
		}
  return 1;
			
}

int assign(char* var_name, int type, int scope, int val_type){
	//printf("inside assign_stm1 %s \n",var_name); 
	int var_scope=validate_existence(var_name); 
	if(var_scope==-1) {yyerror("Variable not declared"); return -1;}

	int v_type=validate_type(val_type, type, "assign");
	if(v_type==-1) return -1; 

	if(type >=4){
			int check=validate_Constant(var_name, scope); 
				if(check==-1){
						yyerror("CONST already initialized");
						return -1;}
		           }
	if(op != "MOV"){
		int initialized=validate_Initialization(var_name,current_scope);
		if(initialized == -1){
			yyerror("Variable used but not initialized");
			return -1;
		}
		else{
			return validate_scope(var_scope, 0);
		}
	}else return validate_scope(var_scope, 0);
}

int validate_scope(int var_scope, int declare){
if(var_scope == current_scope || var_scope == 0) return 1;

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
	if(declare == 0) yyerror("Variable not declared in this scope");
	return -1;
}

int validate_type(int val_type, int type, char* case_name){

	if(val_type != -1 && type!= val_type && type!=val_type+4 && type+4!=val_type){ 
		if(case_name=="assign") yyerror("Type mismatch error");
		if(case_name== "switchcase") yyerror("Type mismatch error: Case statement type mismatch");
		if(case_name== "mathexpr") yyerror("Type mismatch error: Arguments type mismatch");
		if(case_name== "postprefix") yyerror("Type mismatch error: ++ and -- can only be used with variables of type int");
		if(case_name== "compare") yyerror("Type mismatch error: cannot compare variables of different types");
		if(case_name== "condition") yyerror("Type mismatch error: condition must be of type boolean");
		return -1;
		}
	 return 1;
}

int validate_usage(char* var_name){
	int var_scope=validate_existence(var_name); 

	//validate_type(val_type, type);

	if(var_scope==-1){
		yyerror("Variable not declared");
		return -1;
	}
	else {
		int initialized=validate_Initialization(var_name,current_scope);
		if(initialized == -1){
			yyerror("Variable used but not initialized");
			return -1;
		}
		else{
            set_used(var_name);
			return validate_scope(var_scope, 0);
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
     int i=0;
	 current_scope = parent_scope[current_scope];
	 return;
	//printf("closing scope, current is %d\n", current_scope);
}

void yyerror(char *s) {
	//yyout = fopen("errors.txt","w");
    fprintf(stdout, "%s in line number:%d\n", s,yylineno);

	//fclose(yyout);
}

int main(void) {
   char output[256];
    scanf("%s",output);
    assembly= fopen(output, "w");
	
	if (assembly == NULL)
    {
    printf("Error opening assembly file!\n");
    exit(1);
    }

	fprintf(assembly,"## \t Generated Code \t ##");
	FILE *myfile = fopen("test.cpp", "r");
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
	
	
    return 0;
}
