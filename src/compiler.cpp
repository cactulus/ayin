#include "compiler.h"
#include "ast.h"
#include "common.h"
#include "lexer.h"
#include "parser.h"
#include "llvm.h"

Compiler::Compiler() {
    llvm_converter = new LLVM_Converter(this);
	copier = new Copier(this);
    typer = new Typer(this);

    global_scope = new Ast_Scope();

	type_void = new Ast_Type_Info();
	type_void->type = Ast_Type_Info::VOID;

	type_bool = new Ast_Type_Info();
	type_bool->type = Ast_Type_Info::BOOL;
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

    atom_main = make_atom(to_string("main"));
    atom_data = make_atom(to_string("data"));
    atom_length = make_atom(to_string("length"));
    atom_capacity = make_atom(to_string("capacity"));
}

void Compiler::run(String entry_file) {
	parse_file(entry_file);
    if (errors_reported) return;

	typer->type_check_scope(global_scope);
    if (errors_reported) return;

	llvm_converter->convert_scope(global_scope);
    if (errors_reported) return;

	llvm_converter->emit_llvm_ir();
	// llvm_converter->emit_object_file();
}

void Compiler::parse_file(String file_path) {
	String content;
	read_entire_file(file_path, &content);

	Atom *file_atom = new Atom();

	file_atom->id = copy_string(content);
	file_atom->hash = source_table.hash_str(file_path);

	source_table.data.add(file_atom);

	Lexer lexer(this, file_path, content);
	lexer.tokenize();
	
	Parser parser(this, &lexer);
	parser.current_scope = global_scope;
	parser.parse();
}

Ast_Type_Info *make_int_type(bool is_signed, s32 bytes) {
	Ast_Type_Info *info = new Ast_Type_Info();
    info->type = Ast_Type_Info::INT;
    info->is_signed = is_signed;
    info->size = bytes;
    return info;
}

Ast_Type_Info *make_float_type(s32 bytes) {
	Ast_Type_Info *info = new Ast_Type_Info();
    info->type = Ast_Type_Info::FLOAT;
    info->size = bytes;
    return info;
}

Ast_Type_Info *make_pointer_type(Ast_Type_Info *element_type) {
	Ast_Type_Info *info = new Ast_Type_Info();
    info->type = Ast_Type_Info::POINTER;
    info->element_type = element_type;
    return info;
}

Ast_Expression *find_declaration_by_name(Atom *name, Ast_Scope *scope) {
	Ast_Scope *temp = scope;

	while (true) {
		for (auto decl : temp->declarations) {
			switch (decl->type) {
				case Ast::STRUCT: {
					auto strct = static_cast<Ast_Struct *>(decl);
					if (strct->identifier->atom->hash == name->hash)
						return decl;
				} break;
				case Ast::TYPE_ALIAS: {
					auto ta = static_cast<Ast_Type_Alias *>(decl);
					if (ta->identifier->atom->hash == name->hash)
						return decl;
				} break;
				case Ast::DECLARATION: {
					auto var_decl = static_cast<Ast_Declaration *>(decl);
					if (var_decl->identifier->atom->hash == name->hash)
						return decl;
				} break;
				case Ast::FUNCTION: {
					auto fun = static_cast<Ast_Function *>(decl);
					if (fun->identifier->atom->hash == name->hash)
						return decl;
				} break;
				case Ast::ENUM: {
					auto e = static_cast<Ast_Enum *>(decl);
					if (e->identifier->atom->hash == name->hash)
						return decl;
				} break;
			}
		}

		if (!temp->parent) {
			break;
		}
		temp = temp->parent;
	}

	return 0;
}

Ast_Expression *find_declaration_by_id(Ast_Identifier *id) {
	return find_declaration_by_name(id->atom, id->scope);
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
	printf("aleph: \"%.*s\"(%lld:%lld): ", location.file.length, location.file.data, location.line + 1, location.col + 1);
	vprintf(fmt, args);
    printf("\n");

    String source = source_table.find_atom_hash(location.file)->id;

	s32 line = 0;
	s32 pos = 0;
	s32 line_start = 0;
	s32 line_length = 0;

	char cur = source[pos];
	while (cur) {
		cur = source[++pos];

		if (cur == '\n') {
			line++;

			if (line == location.line + 1) {
				break;
			}

			line_start = pos + 1;
			line_length = 0;
		}

		line_length++;
	}

	String source_line = source.substring(line_start, line_length - 1);
	
	printf("%.*s\n", source_line.length, source_line.data);

	for (s32 i = 0; i < location.col; ++i) {
		if (isspace(source_line[i])) {
			putc(source_line[i], stdout);
		} else {
			putc(' ', stdout);
		}
	}

	for (s32 i = 0; i < location.length; ++i) {
		putc('*', stdout);
	}

	puts("\n");

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
	Compiler compiler;

	compiler.run(to_string("/Users/niko/Desktop/dev/aleph/examples/example.alf"));

	return 0;
}
