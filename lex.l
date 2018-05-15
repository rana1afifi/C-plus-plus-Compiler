%{
#include <stdlib.h>
#include <string.h>
#include "y.tab.h"


int yylineno;
void yyerror(char *);
%}


%%
"default"       return DEFAULT;
"break"		    return BREAK;
"continue"		return CONTINUE;
"while"         return WHILE;
"if"            return IF;
"else"          return ELSE;
"do"            return DO;
"const"         return CONST;
"int"           return INT;
"float"         return FLOAT;
"char"          return CHAR;
"bool"          return BOOL;
"true"			return TR;
"false"			return FL;
"for"           return FOR;
"case"			return CASE;
"switch"		return SWITCH;
">="            return GE;
"<="            return LE;
"=="            return EQ;
"!="            return NE;
"++"			return INC;
"--"			return DEC;
"+="			return PLUSEQ;
"-="			return MINEQ;
"*="			return MULEQ;
"/="			return DIVEQ;
"||"			return OR;
"&&"			return AND;
";"				return SEMI;
"-"				return NEG;
"("				return OPENBRAC;
")"				return CLOSEBRAC;
"<"				return LESS;
">"				return GREAT;
"="				return EQU;
"+"				return PLUS;
"*"				return MULT;
"{"				return CURLOPBR;
"}"				return CURLCLBR;
":"				return COLON;
"/"				return DIV;
"!"				return NOT; 

[\'][a-zA-Z][\'] {
                yylval.cValue=yytext[1]; 
                return CHARACTER;
            	}
[a-zA-Z][_a-zA-Z0-9]* { 
 		yylval.varName=strdup(yytext);     
		return VARIABLE;
		}


[0-9]+ {
                yylval.iValue = atoi(yytext);
                return INTEGER;
            	}


[0-9]+[\.][0-9]+ {
		yylval.fValue= atof(yytext);
		return FLOT;
		}



 [ \t]+        ;       /* ignorewhitespace */
 \n {yylineno++;}

.               yyerror("Unknown character");
%%
int yywrap(void) {
    return 1;
}
