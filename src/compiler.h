#ifndef COMPILER_H_
#define COMPILER_H_

#include <cstdarg>

#include "ast.h"
#include "common.h"
#include "copier.h"
#include "lexer.h"
#include "llvm.h"
#include "typer.h"

struct CompileOptions {
	String input_file;
	String output_file;
	bool optimize = false;
	bool debug = false;
	bool emit_llvm = false;
	bool compile_only = false;

	Array<String> libraries;
	Array<String> linker_paths;
};

struct Compiler {
	CompileOptions *options;
	LLVM_Converter *llvm_converter;
	Copier *copier;
	Typer *typer;

	/* default scope */
	Ast_Scope *global_scope;

	/* source file management for error reporting */
	Array<String> source_table_files;
	Array<String> source_table_contents;
	Atom_Table atom_table;
	s32 errors_reported = 0;

	/* primitive type definitions */
	Ast_Type_Info *type_string;
	Ast_Type_Info *type_string_data;
	Ast_Type_Info *type_void;
	Ast_Type_Info *type_void_ptr;
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

	Array<String> definitions;

	String stdlib_path;

	Compiler(CompileOptions *options);

	void run();

	void link_program();

	void parse_file(String file_path);

	Atom *make_atom(String name);

	u8 *get_command_line(Array<String> *strings);

	void report_error_base(Source_Location loc, const char *msg);
	void report_error2(Source_Location loc1, const char *msg1, Source_Location loc2, const char *msg2);
	void report_error(Source_Location location, const char *fmt, va_list args);
	void report_error(Token *token, const char *fmt, ...);
	void report_error(Ast *ast, const char *fmt, ...);

	void init_definitions();
	void handle_directive(Ast_Directive *directive);

	String get_line(Source_Location location);
};

/* helper functions to create types */
Ast_Type_Info *make_int_type(bool is_signed, s32 bytes);
Ast_Type_Info *make_float_type(s32 bytes);
Ast_Type_Info *make_pointer_type(Ast_Type_Info *element_type);

Ast_Expression *find_declaration_by_name(Atom *name, Ast_Scope *scope);
Ast_Expression *find_declaration_by_id(Ast_Identifier *id);

String type_to_string(Ast_Type_Info *type);
void type_to_string_builder(Ast_Type_Info *type, String_Builder *builder);

#endif
