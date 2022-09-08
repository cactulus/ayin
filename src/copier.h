#ifndef COPIER_H_
#define COPIER_H_

#include "ast.h"
#include "common.h"

struct Compiler;

struct Copier {
    Compiler *compiler;
    Ast_Scope *current_scope;

    Copier(Compiler *compiler);

   	Ast_Function *copy_function(Ast_Function *old); 
    Ast_Expression *copy(Ast_Expression *ast);
    Ast_Type_Info *copy_type(Ast_Type_Info *old);

	Ast_Scope *push_scope(Ast_Scope *old);
	void pop_scope();
};

#endif
