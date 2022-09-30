#include "compiler.h"
#include "ast.h"
#include "common.h"
#include "lexer.h"
#include "parser.h"
#include "llvm.h"

#include <iostream>

#ifdef _WIN32

#include "microsoft_craziness.h"

#define EXPORT __declspec(dllexport)
#else
#define EXPORT 
#endif

static String get_executable_path();

const int AYIN_MAX_PATH = 512;

Compiler::Compiler(CompileOptions options) {
	this->options = options;
    llvm_converter = new LLVM_Converter(this);
	global_scope = new Ast_Scope();
	copier = new Copier(this);
    typer = new Typer(this);

	type_void = new Ast_Type_Info();
	type_void->type = Ast_Type_Info::VOID;

	type_void_ptr = make_pointer_type(type_void);

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

	type_string_data = make_pointer_type(type_u8);

	type_string = new Ast_Type_Info();
	type_string->type = Ast_Type_Info::STRING;

    atom_main = make_atom(to_string("main"));
    atom_data = make_atom(to_string("data"));
    atom_length = make_atom(to_string("length"));
    atom_capacity = make_atom(to_string("capacity"));
	atom_it = make_atom(to_string("it"));
	atom_it_index = make_atom(to_string("it_index"));

	String path = get_executable_path();
	String exe_dir_path = basepath(path);
	String ayin_path;

	while (exe_dir_path.length) {
		String name = basename(exe_dir_path);

		if (name == to_string("ayin")) {
			ayin_path = exe_dir_path;
			break;
		}

		exe_dir_path = basepath(exe_dir_path);
	}
	char stdlib_path_str[AYIN_MAX_PATH];
	snprintf(stdlib_path_str, AYIN_MAX_PATH, "%.*sstdlib", ayin_path.length, ayin_path.data);

	stdlib_path = copy_string(to_string(stdlib_path_str));
}

void Compiler::run() {
	parse_file(options.input_file);

    if (errors_reported) return;

	while (directives.length > 0) {
		Ast_Directive *directive = directives[0];

		switch (directive->directive_type) {
		case Ast_Directive::INCLUDE:
			parse_file(directive->file);
			directives.ordered_remove(0);
			break;
		case Ast_Directive::USE:
			char stdlib_path_str[AYIN_MAX_PATH];
			snprintf(stdlib_path_str, AYIN_MAX_PATH,
				"%.*s/%.*s.ay", stdlib_path.length,
				stdlib_path.data, directive->file.length,
				directive->file.data);

			parse_file(to_string(stdlib_path_str));
			directives.ordered_remove(0);
			break;
		case Ast_Directive::IF:
			directives.ordered_remove(0);
			break;
		}
	}

	typer->type_check_scope(global_scope);
    if (errors_reported) return;

	llvm_converter->convert(global_scope);
    if (errors_reported) return;

	if (options.optimize) {
		llvm_converter->optimize();
	}

	if (options.emit_llvm) {
		llvm_converter->emit_llvm_ir();
	}

	llvm_converter->emit_object_file();

	if (!options.compile_only) {
		link_program();
	}
}

#ifdef _WIN32
#include "Windows.h"
#endif

void Compiler::link_program() {
#ifdef _WIN32
	auto win32_sdk = find_visual_studio_and_windows_sdk();

	if (win32_sdk.vs_exe_path) {
		const int LINE_SIZE = 4096;
		char exe_path[LINE_SIZE];
		char libpath[LINE_SIZE];

		Array<String> args;

		snprintf(exe_path, LINE_SIZE, "%S\\link.exe", win32_sdk.vs_exe_path);
		args.add(to_string(exe_path));

		if (win32_sdk.vs_library_path) {

			snprintf(libpath, LINE_SIZE, "/libpath:%S", win32_sdk.vs_library_path);
			args.add(copy_string(to_string(libpath)));
		}

		if (win32_sdk.windows_sdk_um_library_path) {
			snprintf(libpath, LINE_SIZE, "/libpath:%S", win32_sdk.windows_sdk_um_library_path);
			args.add(copy_string(to_string(libpath)));
		}

		if (win32_sdk.windows_sdk_ucrt_library_path) {
			snprintf(libpath, LINE_SIZE, "/libpath:%S", win32_sdk.windows_sdk_ucrt_library_path);
			args.add(copy_string(to_string(libpath)));
		}

		args.add(to_string("/NODEFAULTLIB:libcmt"));
		args.add(to_string("msvcrt.lib"));
		args.add(to_string("/nologo"));
		args.add(to_string("/DEBUG"));
		args.add(to_string("output.o"));

		char executable_name[LINE_SIZE];
		snprintf(executable_name, LINE_SIZE, "/OUT:%.*s.exe", options.output_file.length, options.output_file.data);
		convert_to_back_slashes(executable_name + 1);

		args.add(to_string(executable_name));

		auto cmd_line = get_command_line(&args);
		printf("Linker command: %s\n", cmd_line);

		// system((char *)cmd_line);
		STARTUPINFOA startup;
		memset(&startup, 0, sizeof(STARTUPINFOA));
		startup.cb = sizeof(STARTUPINFOA);
		startup.dwFlags = 0x00000100;
		startup.hStdInput = GetStdHandle(((DWORD)-10));
		startup.hStdOutput = GetStdHandle(((DWORD)-11));
		startup.hStdError = GetStdHandle(((DWORD)-12));

		PROCESS_INFORMATION process_info;
		CreateProcessA(nullptr, (char *)cmd_line, nullptr, nullptr, 1, 0, nullptr, nullptr, &startup, &process_info);
		WaitForSingleObject(process_info.hProcess, 0xFFFFFFFF);

		remove("output.o");
	}
	#else
	// @Incomplete should use the execpve family
	Array<String> args;
	args.add(to_string("ld"));
	args.add(to_string("output.o"));

	args.add(to_string("-o"));
	args.add(compiler->executable_name);

	auto cmd_line = get_command_line(&args);
	printf("Linker command: %s\n", cmd_line);
	system((char *)cmd_line);
	#endif
}

void Compiler::parse_file(String file_path) {
	for (auto included_file : source_table_files) {
		if (included_file == file_path) {
			return;
		}
	}

	String content;
	if (!read_entire_file(file_path, &content)) {
		printf("Failed to read file '%.*s'", file_path.length, file_path.data);
		std::exit(1);
	}

	file_path = copy_string(file_path);
	source_table_files.add(file_path);
	source_table_contents.add(copy_string(content));

	Lexer lexer(this, file_path, content);
	lexer.tokenize();
	
	Parser parser(this, &lexer);
	parser.current_scope = global_scope;
	parser.parse();
}


u8 *Compiler::get_command_line(Array<String> *strings) {
	s64 total_length = 0;

	for (String s : *strings) total_length += s.length;

	total_length += strings->length * 3 + 1;

	s64 cursor = 0;
	u8 *final = reinterpret_cast<u8 *>(malloc(total_length));

	for (String s : *strings) {
		final[cursor++] = '\"';

		memcpy(final + cursor, s.data, s.length);
		cursor += s.length;

		final[cursor++] = '\"';
		final[cursor++] = ' ';
	}

	final[cursor++] = 0;
	return final;
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
					if (strct->identifier->atom == name)
						return decl;
				} break;
				case Ast::TYPE_ALIAS: {
					auto ta = static_cast<Ast_Type_Alias *>(decl);
					if (ta->identifier->atom == name)
						return decl;
				} break;
				case Ast::DECLARATION: {
					auto var_decl = static_cast<Ast_Declaration *>(decl);
					if (var_decl->identifier->atom == name)
						return decl;
				} break;
				case Ast::FUNCTION: {
					auto fun = static_cast<Ast_Function *>(decl);
					if (fun->identifier->atom == name)
						return decl;
				} break;
				case Ast::ENUM: {
					auto e = static_cast<Ast_Enum *>(decl);
					if (e->identifier->atom == name)
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

void Compiler::report_error_base(Source_Location loc, const char *msg) {
	printf("ayin: \"%.*s\"(%lld:%lld): ", loc.file.length, loc.file.data, loc.line + 1, loc.col + 1);
	printf(msg);
	printf("\n");

	String source_line = get_line(loc);

	printf("%.*s\n", source_line.length, source_line.data);

	for (s32 i = 0; i < loc.col; ++i) {
		if (isspace(source_line[i])) {
			putc(source_line[i], stdout);
		} else {
			putc(' ', stdout);
		}
	}

	for (s32 i = 0; i < loc.length; ++i) {
		putc('*', stdout);
	}

	puts("\n");
}

void Compiler::report_error2(Source_Location loc1, const char *msg1, Source_Location loc2, const char *msg2) {
	report_error_base(loc1, msg1);
	report_error_base(loc2, msg2);

	errors_reported++;
}

void Compiler::report_error(Source_Location location, const char *fmt, va_list args) {
	char error[512];
	vsprintf_s(error, 512, fmt, args);

	report_error_base(location, error);

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


String Compiler::get_line(Source_Location location) {
	String source;
	for (int i = 0; i < source_table_files.length; ++i) {
		if (source_table_files[i] == location.file) {
			source = source_table_contents[i];
			break;
		}
	}

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

	return source.substring(line_start, line_length - 1);
}

#ifdef _WIN32

#include <shlwapi.h>

#pragma comment(lib, "shlwapi.lib") 

String get_executable_path() {
	const DWORD BUFFER_SIZE = 512;
	char buf[BUFFER_SIZE];

	auto module = GetModuleHandleA(nullptr);
	GetModuleFileNameA(module, buf, BUFFER_SIZE);

	convert_to_forward_slashes(buf);
	return copy_string(to_string(buf));
}
#endif

#ifdef __APPLE__
#include <mach-o/dyld.h>

String get_executable_path() {
	const u32 BUFFER_SIZE = 512;
	char buf[BUFFER_SIZE];

	u32 bufsize = BUFFER_SIZE;
	auto result = _NSGetExecutablePath(buf, &bufsize);
	if (result != 0) return to_string("");

	return copy_string(to_string(buf));
}
#endif

#ifdef UNIX
#include <sys/stat.h>

#endif