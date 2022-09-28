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

	void type_check_statement(Ast_Expression *stmt);

	void type_check_variable_declaration(Ast_Declaration *decl);

	void type_check_function(Ast_Function *function);

	void infer_type(Ast_Expression *expression);
	Ast_Type_Info *resolve_type_info(Ast_Type_Info *type_info);

	Ast_Expression *check_expression_type_and_cast(Ast_Expression *expression, Ast_Type_Info *other_type);

	Ast_Cast *make_cast(Ast_Expression *expression, Ast_Type_Info *target_type);

	Ast_Function *get_polymorph_function(Ast_Call *call, Ast_Function *template_function);
	Ast_Function *make_polymorph_function(Ast_Function *template_function, Array<Ast_Expression *> *arguments);
	bool can_fill_polymorph(Ast_Type_Info *par_type, Ast_Type_Info *arg_type); 

	bool types_match(Ast_Type_Info *t1, Ast_Type_Info *t2);

	s32 get_pointer_level(Ast_Type_Info *type_info);
	bool type_points_to_void(Ast_Type_Info *type_info);

	String mangle_name(Ast_Function *decl);
	void mangle_type(String_Builder *builder, Ast_Type_Info *type);

	Ast_Expression *make_compare_zero(Ast_Expression *target);

	Ast_Literal *make_integer_literal(s64 value, Ast_Type_Info *type_info, Ast *source_loc=0);
	Ast_Identifier *make_identifier(Atom *name);
	Ast_Member *make_member(Ast_Expression *aggregate_expression, Atom *field);
	Ast_Index *make_index(Ast_Expression *array, Ast_Expression *index);
};

#endif
