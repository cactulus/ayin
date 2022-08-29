#ifndef COMPILER_H_
#define COMPILER_H_

#include <cstdarg>

#include "ast.h"
#include "common.h"
#include "lexer.h"

struct Compiler {
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

	Compiler();

	Ast_Scope *parse_file(String file_path);

	Ast_Type_Info *make_int_type(bool is_signed, s32 bytes);
	Ast_Type_Info *make_float_type(s32 bytes);

	Atom *make_atom(String name);

	void report_error(Source_Location location, const char *fmt, va_list args);
	void report_error(Token *token, const char *fmt, ...);
	void report_error(Ast *ast, const char *fmt, ...);
};

#endif