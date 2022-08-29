#include "parser.h"
#include "ast.h"
#include "lexer.h"
#include "compiler.h"

Parser::Parser(Compiler *compiler, Lexer *lexer) {
	this->compiler = compiler;
	this->lexer = lexer;
	this->pos = 0;
}

Ast_Scope *Parser::parse() {
	Ast_Scope *scope = AST_NEW(Ast_Scope);

	Token *t;
	while ((t = peek())->type != Token::END_OF_FILE) {
		scope->declarations.add(parse_global());
	}

	return scope;
}

Ast_Declaration *Parser::parse_global() {
	Token *t = peek();
	
	if (t->type == Token::STRUCT) {
		return parse_struct_declaration();
	}
	
	if (t->type == Token::ENUM) {
		return parse_enum_declaration();
	}
		
	if (t->type == Token::ALIAS) {
		return parse_type_alias();
	}
	
	if (t->type == Token::FUNC) {
		return parse_function_declaration();
	}

	Ast_Declaration *var_decl = parse_variable_declaration();
	if (var_decl) {
		return var_decl;
	}

	compiler->report_error(t, "Unexpected token: Expected function, struct, enum, variable or type declaration");
	return 0;
}

Ast_Declaration *Parser::parse_struct_declaration() {
	auto decl = AST_NEW(Ast_Declaration);
	decl->declaration_type = Ast_Declaration::STRUCT;

	decl->identifier = parse_identifier();
	if (!decl->identifier) {
		compiler->report_error(decl->identifier, "Expected struct name");
	}

	Ast_Type_Info *struct_type = new Ast_Type_Info();
	struct_type->type = Ast_Type_Info::STRUCT;

	if (!expect_eat('{')) {
		compiler->report_error(peek(), "Expected '{' after struct name");
		return 0;
	}

	while (!expect_eat('}')) {
		struct_type->struct_members.add(parse_variable_declaration());

		expect_eat(',');
	}

	decl->type_info = struct_type;
	
	return decl;
}

Ast_Declaration *Parser::parse_enum_declaration() {
	auto decl = AST_NEW(Ast_Declaration);
	decl->declaration_type = Ast_Declaration::ENUM;

	decl->identifier = parse_identifier();

	if (!decl->identifier) {
		compiler->report_error(decl->identifier, "Expected enum name");
	}

	Ast_Type_Info *enum_type = new Ast_Type_Info();
	enum_type->type = Ast_Type_Info::ENUM;

	if (!expect_eat('{')) {
		compiler->report_error(peek(), "Expected '{' after enum name");
		return 0;
	}

	s32 index = 0;
	while (!expect_eat('}')) {
		Ast_Identifier *mem_id = parse_identifier();

		enum_type->enum_members.add({mem_id->atom, index++});

		expect_eat(',');
	}

	decl->type_info = enum_type;

	return decl;
}

Ast_Declaration *Parser::parse_type_alias() {
	auto decl = AST_NEW(Ast_Declaration);
	decl->declaration_type = Ast_Declaration::TYPE;
	
	decl->identifier = parse_identifier();
	if (!decl->identifier) {
		compiler->report_error(decl->identifier, "Expected identifier for type alias");
	}

	decl->type_info = parse_type_specifier();

	return decl;
}

Ast_Declaration *Parser::parse_function_declaration() {
	auto decl = AST_NEW(Ast_Declaration);
	decl->declaration_type = Ast_Declaration::FUNCTION;

	

	return decl;
}

Ast_Declaration *Parser::parse_variable_declaration() {
	Ast_Declaration *var_decl = AST_NEW(Ast_Declaration);

	var_decl->declaration_type = Ast_Declaration::VARIABLE;
	var_decl->identifier = parse_identifier();

	if (!var_decl->identifier) {
		compiler->report_error(peek(), "Expected variable name");
		return 0;
	}

	if (expect_eat(':')) {
		if (expect_eat('=')) {
			var_decl->initializer = parse_expression();
		} else {
			var_decl->type_info = parse_type_specifier();

			if (expect_eat('=')) {
				var_decl->initializer = parse_expression();
			} else {
				var_decl->initializer = 0;
			}
		}
	} else {
		compiler->report_error(peek(), "Expected ':' after variable name");
		return 0;
	}

	return 0;
}

Ast_Expression *Parser::parse_expression() {
	return 0;
}

Ast_Identifier *Parser::parse_identifier() {
	if(!expect(Token::ATOM)) {
		return 0;
	}

	auto id = AST_NEW(Ast_Identifier);
	Token *t = next();

	id->atom = compiler->make_atom(t->lexeme);

	return id;
}

Ast_Type_Info *Parser::parse_type_specifier() {
	Ast_Type_Info *type_info = 0;
	Token *t = peek();

	switch (t->type) {
		case Token::VOID: type_info = compiler->type_void; break;
		case Token::BOOL: type_info = compiler->type_bool; break;
		case Token::S8: type_info = compiler->type_s8; break;
		case Token::S16: type_info = compiler->type_s16; break;
		case Token::S32: type_info = compiler->type_s32; break;
		case Token::S64: type_info = compiler->type_s64; break;
		case Token::U8: type_info = compiler->type_u8; break;
		case Token::U16: type_info = compiler->type_u16; break;
		case Token::U32: type_info = compiler->type_u32; break;
		case Token::U64: type_info = compiler->type_u64; break;
		case Token::F32: type_info = compiler->type_f32; break;
		case Token::F64: type_info = compiler->type_f64; break;
		default: break;
	}

	if (type_info) {
		return type_info;
	}

	if (t->type == '*') {
		next();

		Ast_Type_Info *element_type = parse_type_specifier();
		if (!element_type) {
			compiler->report_error(peek(), "Failed to parse pointer element type");
			return 0;
		}

		type_info = new Ast_Type_Info();
		type_info->type = Ast_Type_Info::POINTER;
		type_info->element_type = element_type;
		return type_info;
	}

	if (t->type == Token::ATOM) {
		/* TODO: Lookup type */
	}

	return 0;
}

Ast *Parser::ast_init(Ast *ast) {
	ast->location = peek()->location;
	return ast;
}

bool Parser::expect_eat(Token::Type type) {
	if (expect(type)) {
		pos++;
		return true;
	}
	return false;
}

bool Parser::expect_eat(char type) {
	return expect_eat((Token::Type) type);
}

bool Parser::expect(Token::Type type) {
	Token *tok = peek();
	if (tok->type == type) {
		return true;
	}

	if (tok->type == Token::END_OF_FILE) {
		compiler->report_error(tok, "Encountered unexpected 'end of file' during parsing");
		return true;
	}

	return false;
}

bool Parser::expect(char type) {
	return expect((Token::Type) type);
}

Token *Parser::peek() {
	return &lexer->tokens[pos];
}

Token *Parser::next() {
	return &lexer->tokens[pos++];
}
