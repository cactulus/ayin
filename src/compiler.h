#ifndef COMPILER_H_
#define COMPILER_H_

#include <cstdarg>

#include "ast.h"
#include "common.h"
#include "copier.h"
#include "lexer.h"
#include "llvm.h"
#include "typer.h"

struct Compiler {
	LLVM_Converter *llvm_converter;
	Copier *copier;
	Typer *typer;

	Ast_Scope *global_scope;

	Atom_Table source_table;
	Atom_Table atom_table;
	s32 errors_reported = 0;

	Ast_Type_Info *type_void;
	Ast_Type_Info *type_bool;
	Ast_Type_Info *type_s8;
	Ast_Type_Info *type_s16;
	Ast_Type_Info *type_s32;
	Ast_Type_Info *type_s64;
	Ast_Type_Info *type_u8;
	Ast_Type_Info *type_u16;
	Ast_Type_Info *type_u32;
	Ast_Type_Info *type_u64;
	Ast_Type_Info *type_f32;
	Ast_Type_Info *type_f64;

	Atom *atom_main;
	Atom *atom_data;
	Atom *atom_length;
	Atom *atom_capacity;
	Atom *atom_it;
	Atom *atom_it_index;

	Compiler();

	void run(String entry_file);

	void parse_file(String file_path);

	Atom *make_atom(String name);

	void report_error(Source_Location location, const char *fmt, va_list args);
	void report_error(Token *token, const char *fmt, ...);
	void report_error(Ast *ast, const char *fmt, ...);
};

Ast_Type_Info *make_int_type(bool is_signed, s32 bytes);
Ast_Type_Info *make_float_type(s32 bytes);
Ast_Type_Info *make_pointer_type(Ast_Type_Info *element_type);

Ast_Expression *find_declaration_by_name(Atom *name, Ast_Scope *scope);
Ast_Expression *find_declaration_by_id(Ast_Identifier *id);

#endif
