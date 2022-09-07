#ifndef TYPER_H_
#define TYPER_H_

#include "ast.h"
#include "common.h"

struct Compiler;
struct Typer {
	Compiler *compiler;	

	Ast_Function *current_function = 0;

	Typer(Compiler *compiler);

	void type_check_scope(Ast_Scope *scope);

	void type_check_variable_declaration(Ast_Declaration *decl);

	void type_check_function(Ast_Function *function);

	void infer_type(Ast_Expression *expression);
	Ast_Type_Info *resolve_type_info(Ast_Type_Info *type_info);

	Ast_Expression *check_expression_type_and_cast(Ast_Expression *expression, Ast_Type_Info *other_type);

	Ast_Cast *make_cast(Ast_Expression *expression, Ast_Type_Info *target_type);

	Ast_Function *get_polymorph_function(Ast_Call *call, Ast_Function *template_function);

	Ast_Statement *find_declaration_by_id(Ast_Identifier *id);
	Ast_Statement *find_declaration_by_name(Atom *name, Ast_Scope *scope);

	bool types_match(Ast_Type_Info *t1, Ast_Type_Info *t2);

	s32 get_pointer_level(Ast_Type_Info *type_info);
	bool type_points_to_void(Ast_Type_Info *type_info);

	String mangle_name(Ast_Function *decl);
	void mangle_type(String_Builder *builder, Ast_Type_Info *type);
};

#endif
