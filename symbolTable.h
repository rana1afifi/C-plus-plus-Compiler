#pragma once
#include "uthash.h"
// check--> scope , initialized , constant
enum data_type
{TYPE_INTEGER, 
 TYPE_FLOAT, 
 TYPE_CHAR,
 TYPE_BOOL,
 TYPE_CONST_INTEGER,
 TYPE_CONST_FLOAT,
 TYPE_CONST_CHAR, 
};

struct variable_info
{
	enum data_type type;
	int scope; // 0 refers to Global Scope
	int initialized; 
	int used;
};
struct variable {
	char variable_name[50];  // key 
	struct variable_info info; 
	UT_hash_handle hh; 
};

struct variable* symbol_table = NULL; 



// semantic error: referring to an unexisting variable in the current scope
int validate_existence(char*variable_name) {

	struct variable * v;

	HASH_FIND_STR(symbol_table, variable_name, v);
	if (v == NULL)  return -1; // this variable doesn't exist 
     

		
	return  v->info.scope;


}

// semantic error: referring to an unexisting variable in the current scope
int validate_Initialization(char*variable_name, int scope) {

	struct variable * v;

	HASH_FIND_STR(symbol_table, variable_name, v);
	if (v != NULL) {
		if (v->info.initialized == 0 ) return -1; // check 
	}
	
		
		return v->info.scope;

}

int validate_Constant(char*variable_name, int scope) {

	struct variable * v;

	HASH_FIND_STR(symbol_table, variable_name, v);
	if (v != NULL) {
		if (v->info.type >= 4 && v->info.type <=6 && v->info.initialized == 1) return -1; // check if constant was already initialized
	}
	//if (v->info.scope == scope || scope == 0) // global variables 
		
	return v->info.scope;



}

int validate_type_match(char* variable_name , enum data_type t)
{   

	struct variable * v;
	HASH_FIND_STR(symbol_table, variable_name, v);
	
    
	if (v->info.type == t || v->info.type==t+4) return 1; 

	return -1;

}

int set_used(char*variable_name)
{
    struct variable * v;

	HASH_FIND_STR(symbol_table, variable_name, v);
	if (v == NULL)  return -1;
   
	HASH_FIND_STR(symbol_table, variable_name, v);
	
    v->info.used=1;	
	return 1; 

}
int get_scope(char* variable_name) {
	struct variable * v;
	HASH_FIND_STR(symbol_table, variable_name, v);
	if (v != NULL) {
		
		return v->info.scope;
	}
	else {
		return -1;
	}
}


int get_type(char* variable_name) {
	
	struct variable * v;
	HASH_FIND_STR(symbol_table, variable_name, v);
	if (v != NULL) {
		return v->info.type;
	}
	else {
		return -1;
	}
}

int assign_variable(char* variable_name)
{
	struct variable * v;
	// check for semantic error: repeated declaration
	HASH_FIND_STR(symbol_table, variable_name, v);
	if (v != NULL) {
		v->info.initialized = 1;
	}
}

const char* get_type_name(enum data_type type) {
	switch (type) {
	case TYPE_INTEGER: return "int";
	case TYPE_FLOAT: return "float";
	case TYPE_CHAR:	return "char";
	case TYPE_BOOL: return "bool";
	case TYPE_CONST_INTEGER: return "int";
	case TYPE_CONST_FLOAT: return "float";
	case TYPE_CONST_CHAR:	return "char";
	}
}
void print_symbol_table_two()
{
	printf("\n-------------------PRINTING SYMBOL TABLE--------------------\n");
	printf("Variable\t Type \t\t Scope\t\t Initialized\n");
	printf("------------------------------------------------------------\n");
	struct variable *v;

	for (v = symbol_table; v != NULL; v = (struct variable*)(v->hh.next)) {
		printf("%s\t\t %s\t\t %d\t\t %s\t\t\n", v->variable_name, get_type_name(v->info.type), v->info.scope, v->info.initialized? "True" : "False");
	}
	printf("\n");
}

void print_unused_variables()
{

	struct variable *v;	
	for (v = symbol_table; v != NULL; v = (struct variable*)(v->hh.next)) {
		 if(v->info.used==0)
           {printf("\n Warning: variable %s is declared but not used", v->variable_name);}
         } 
	printf("\n");
}



int add_variable(char* variable_name, int initialized, enum data_type t, int scope)
{
	struct variable * v;
	// check for semantic error: repeated declaration
	HASH_FIND_STR(symbol_table, variable_name, v);
	if (v != NULL) { 
		if (v->info.scope == scope) return -1;
	}

	struct variable_info* i;
	i = (struct variable_info*)malloc(sizeof(struct variable_info));
	i->initialized = initialized;
	i->scope = scope;
	i->type = t;
    i->used=0;

	v = (struct variable*)malloc(sizeof(struct variable));
	strcpy(v->variable_name, variable_name);
	v->info = *i;

	HASH_ADD_STR(symbol_table, variable_name, v);
	//print_symbol_table_two();
	//printf("Added Successfully \n");
}
