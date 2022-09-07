#include "parser.h"
#include "ast.h"
#include "lexer.h"
#include "compiler.h"

Parser::Parser(Compiler *compiler, Lexer *lexer) {
	this->compiler = compiler;
	this->lexer = lexer;
	this->pos = 0;
}

void Parser::parse() {
	Token *t;
	while ((t = peek())->type != Token::END_OF_FILE) {
		current_scope->declarations.add(parse_global());
	}
}

Ast_Statement *Parser::parse_global() {
	Token *t = peek();
	
	if (expect_eat(Token::STRUCT)) {
		return parse_struct_declaration();
	}
	
	if (expect_eat(Token::ENUM)) {
		return parse_enum_declaration();
	}
		
	if (expect_eat(Token::ALIAS)) {
		return parse_type_alias();
	}
	
	if (expect_eat(Token::FUNC)) {
		return parse_function_declaration();
	}

	Ast_Declaration *var_decl = parse_variable_declaration(true);
	if (var_decl) {
		return var_decl;
	}

	compiler->report_error(t, "Unexpected token: Expected function, struct, enum, variable or type declaration");
	return 0;
}

Ast_Struct *Parser::parse_struct_declaration() {
	auto s = AST_NEW(Ast_Struct);

	s->identifier = parse_identifier();
	if (!s->identifier) {
		compiler->report_error(s->identifier, "Expected struct name");
	}

	Ast_Type_Info *struct_type = new Ast_Type_Info();
	struct_type->type = Ast_Type_Info::STRUCT;
	struct_type->struct_decl = s;

	if (!expect_eat('{')) {
		compiler->report_error(peek(), "Expected '{' after struct name");
		return 0;
	}

	while (!expect_eat('}')) {
		Ast_Declaration *decl = parse_variable_declaration();

		s->members.add(decl);
		struct_type->struct_members.add(decl->type_info);

		expect_eat(',');
	}

	s->type_info = struct_type;
	
	return s;
}

Ast_Enum *Parser::parse_enum_declaration() {
	auto e = AST_NEW(Ast_Enum);

	e->identifier = parse_identifier();

	if (!e->identifier) {
		compiler->report_error(e->identifier, "Expected enum name");
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

	e->type_info = enum_type;

	return e;
}

Ast_Type_Alias *Parser::parse_type_alias() {
	auto ta = AST_NEW(Ast_Type_Alias);
	
	ta->identifier = parse_identifier();
	if (!ta->identifier) {
		compiler->report_error(ta->identifier, "Expected identifier for type alias");
	}

	ta->type_info = parse_type_specifier();

	return ta;
}

Ast_Function *Parser::parse_function_declaration() {
	auto fn = AST_NEW(Ast_Function);

	fn->identifier = parse_identifier();
	if (!fn->identifier) {
		compiler->report_error(fn->identifier, "Expected identifier for function name");
	}

	Ast_Type_Info *type_info = new Ast_Type_Info();
	type_info->type = Ast_Type_Info::FUNCTION;

	if (expect_eat('<')) {

		push_scope();
		fn->template_scope = current_scope;
		fn->is_template_function = true;

		while (!expect_eat('>')) {
			Ast_Type_Alias *alias = AST_NEW(Ast_Type_Alias);
			alias->identifier = parse_identifier();
			alias->type_info = new Ast_Type_Info();
			alias->type_info->type = Ast_Type_Info::TYPE;

			if (!alias->identifier) {
				compiler->report_error(alias, "Expected template type name identifier");
			}

			fn->template_scope->declarations.add(alias);
		}
	}

	if (!expect_eat('(')) {
		compiler->report_error(peek(), "Expected '(' after function name");
	}

	push_scope();
	fn->parameter_scope = current_scope;
	while (!expect_eat(')')) {
		Ast_Declaration *par_decl = parse_variable_declaration();

		if (par_decl->initializer) {
			compiler->report_error(par_decl, "Can't initialize parameter");
		}

		fn->parameters.add(par_decl);
		type_info->parameters.add(par_decl->type_info);
		current_scope->declarations.add(par_decl);

		if (!expect(')')) {
			if (!expect_eat(',')) {
				compiler->report_error(peek(), "Expected ',' after parameter");
			}
		}
	}

	if (expect_eat('{')) {
		type_info->return_type = compiler->type_void;
	} else {
		type_info->return_type = parse_type_specifier();

		if (!expect_eat('{')) {
			compiler->report_error(peek(), "Expected '{' after return type specifier");
		}
	}

	fn->type_info = type_info;

	push_scope();
	fn->block_scope = current_scope;

	while (!expect_eat('}')) {
		Ast_Statement *statement_or_declaration = parse_declaration_or_statement();
		if (!statement_or_declaration) {
			return fn;
		}

		switch (statement_or_declaration->type) {
			case Ast::DECLARATION:
			case Ast::STRUCT:
			case Ast::ENUM:
			case Ast::TYPE_ALIAS:
			case Ast::FUNCTION:
				current_scope->declarations.add(statement_or_declaration);
				break;
			default:
				current_scope->statements.add(statement_or_declaration);
				break;

		}
	}

	if (fn->template_scope)
		pop_scope();

	if (fn->parameter_scope)
		pop_scope();

	pop_scope();

	return fn;
}

Ast_Declaration *Parser::parse_variable_declaration(bool expect_semicolon) {
	Ast_Declaration *var_decl = AST_NEW(Ast_Declaration);

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
			}
		}
	} else {
		compiler->report_error(peek(), "Expected ':' after variable name");
		return 0;
	}

	if (expect_semicolon) {
		if (!expect_eat(';')) {
			compiler->report_error(peek(), "Expected ';' after variable declaration");
		}
	}

	return var_decl;
}

Ast_Statement *Parser::parse_declaration_or_statement() {
	if (expect(Token::ATOM) && expect(':', 1)) {
		return parse_variable_declaration(true);
	}

	if (expect(Token::RETURN)) {
		Ast_Return *ret = AST_NEW(Ast_Return);
		next();
		if (!expect_eat(';')) {
			ret->return_value = parse_expression();
			if (!expect_eat(';')) {
				compiler->report_error(peek(), "expected ';' after return value");
			}
		}

		return ret;
	}
	
	compiler->report_error(peek(), "Unexpected token: expected statement or variable declaration");
	return 0;
}

Ast_Expression *Parser::parse_expression() {
	Token *t = peek();

	if (expect_eat(Token::ATOM)) {
		Ast_Identifier *id = AST_NEW(Ast_Identifier);
		id->atom = compiler->make_atom(t->lexeme);
		id->scope = current_scope;

		if (expect_eat('(')) {
			Ast_Call *call = AST_NEW(Ast_Call);
			call->identifier = id;

			while(!expect_eat(')')) {
				call->arguments.add(parse_expression());

				if (!expect(')')) {
					if (!expect_eat(',')) {
						compiler->report_error(peek(), "Expected ',' after argument");
					}
				}
			}

			return call;
		}

		return id;
	}

	if (expect_eat(Token::INT_LIT)) {
		Ast_Literal *lit = AST_NEW(Ast_Literal);
		lit->literal_type = Ast_Literal::INT;
		lit->int_value = t->int_value;
		return lit;
	}

	if (expect_eat(Token::FLOAT_LIT)) {
		Ast_Literal *lit = AST_NEW(Ast_Literal);
		lit->literal_type = Ast_Literal::FLOAT;
		lit->float_value = t->float_value;
		return lit;
	}
	
	if (expect_eat(Token::TRUE)) {
		Ast_Literal *lit = AST_NEW(Ast_Literal);
		lit->literal_type = Ast_Literal::BOOL;
		lit->int_value = 1;
		return lit;
	}
	
	if (expect_eat(Token::FALSE)) {
		Ast_Literal *lit = AST_NEW(Ast_Literal);
		lit->literal_type = Ast_Literal::BOOL;
		lit->int_value = 0;
		return lit;
	}

	return 0;
}

Ast_Identifier *Parser::parse_identifier() {
	if(!expect(Token::ATOM)) {
		return 0;
	}

	auto id = AST_NEW(Ast_Identifier);
	Token *t = next();

	id->atom = compiler->make_atom(t->lexeme);
	id->scope = current_scope;

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
		next();
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
		type_info = new Ast_Type_Info();
		type_info->type = Ast_Type_Info::UNINITIALIZED;
		type_info->unresolved_name = parse_identifier();
		return type_info;
	}

	return 0;
}

Ast *Parser::ast_init(Ast *ast) {
	ast->location = peek()->location;
	return ast;
}

void Parser::push_scope() {
	Ast_Scope *new_scope = AST_NEW(Ast_Scope);
	new_scope->parent = current_scope;
	current_scope = new_scope;
}

void Parser::pop_scope() {
	assert(current_scope->parent);
	current_scope = current_scope->parent;
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

bool Parser::expect(Token::Type type, int off) {
	Token *tok = peek(off);
	if (tok->type == type) {
		return true;
	}

	if (tok->type == Token::END_OF_FILE) {
		compiler->report_error(tok, "Encountered unexpected 'end of file' during parsing");
		return true;
	}

	return false;
}

bool Parser::expect(char type, int off) {
	return expect((Token::Type) type, off);
}

Token *Parser::peek(int off) {
	if (pos + off >= lexer->tokens.length) {
		Token *t = &lexer->tokens[lexer->tokens.length - 1];
		compiler->report_error(t, "Encountered unexpected 'end of file' during parsing");
		return t;
	}
	return &lexer->tokens[pos + off];
}

Token *Parser::next() {
	return &lexer->tokens[pos++];
}
