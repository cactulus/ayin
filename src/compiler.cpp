#include "compiler.h"
#include "ast.h"
#include "common.h"
#include "lexer.h"
#include "parser.h"

Compiler::Compiler() {
	type_void = new Ast_Type_Info();
	type_void->type = Ast_Type_Info::VOID;

	type_bool = new Ast_Type_Info();
	type_bool->type = Ast_Type_Info::BOOL;
	type_bool->alignment = 1;
	type_bool->size = 1;

	type_s8  = make_int_type(true, 1);
    type_s16 = make_int_type(true, 2);
    type_s32 = make_int_type(true, 4);
    type_s64 = make_int_type(true, 8);
    
    type_u8  = make_int_type(false, 1);
    type_u16 = make_int_type(false, 2);
    type_u32 = make_int_type(false, 4);
    type_u64 = make_int_type(false, 8);
    
    type_f32 = make_float_type(4);
    type_f64 = make_float_type(8);
}

Ast_Scope *Compiler::parse_file(String file_path) {
	String content;
	read_entire_file(file_path, &content);

	Atom *file_atom = new Atom();

	file_atom->id = copy_string(content);
	file_atom->hash = source_table.hash_str(file_path);

	source_table.data.add(file_atom);

	Lexer lexer(this, file_path, content);
	lexer.tokenize();
	
	Parser parser(this, &lexer);
	return parser.parse();
}

Ast_Type_Info *Compiler::make_int_type(bool is_signed, s32 bytes) {
	Ast_Type_Info *info = new Ast_Type_Info();
    info->type = Ast_Type_Info::INT;
    info->is_signed = is_signed;
    info->size = bytes;
    info->alignment = info->size;
    return info;
}

Ast_Type_Info *Compiler::make_float_type(s32 bytes) {
	Ast_Type_Info *info = new Ast_Type_Info();
    info->type = Ast_Type_Info::FLOAT;
    info->size = bytes;
    info->alignment = info->size;
    return info;
}

Atom *Compiler::make_atom(String name) {
	Atom *atom = atom_table.find_atom(name);
	if (!atom) {
		atom = new Atom();

		atom->id = copy_string(name);
		atom->hash = atom_table.hash_str(name);

		atom_table.data.add(atom);
	}

	return atom;
}

void Compiler::report_error(Source_Location location, const char *fmt, va_list args) {
	printf("aleph: \"%s\"(%lld:%lld): ", to_c_string(location.file), location.line, location.col);
	vprintf(fmt, args);
    printf("\n");

    String source = source_table.find_atom(location.file)->id;

	s32 line = 0;
	s32 col = 0;
	char *cur = source.data;

	while (*cur++) {
		col++;
		if (*cur == '\n') {
			line++;
			col = 0;
		}

		if (line == location.line) {
			break;
		}
	}
	
	while (*cur != '\n' && *cur != 0) {
		putc(*cur, stdout);

		cur++;
	}

	putc('\n', stdout);

	for (s32 i = 0; i < location.col; ++i) {
		putc(' ', stdout);
	}

	for (s32 i = 0; i < location.length; ++i) {
		putc('*', stdout);
	}

	putc('\n', stdout);

	errors_reported++;
}

void Compiler::report_error(Token *token, const char *fmt, ...) {
	va_list args;
    va_start(args, fmt);
    
    report_error(token->location, fmt, args);
    va_end(args);
}

void Compiler::report_error(Ast *ast, const char *fmt, ...) {
	va_list args;
    va_start(args, fmt);
    
    report_error(ast->location, fmt, args);
    va_end(args);
}

int main(int argc, char *argv[]) {

	return 0;
}