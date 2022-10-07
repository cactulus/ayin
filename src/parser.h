#ifndef PARSER_H_
#define PARSER_H_

#include "common.h"
#include "ast.h"
#include "lexer.h"

#define AST_NEW(type) (type *) ast_init(new type())

struct Compiler;
struct Parser {
	Compiler *compiler;
	Lexer *lexer;
	s64 pos;

	Ast_Scope *current_scope = 0;

	Parser(Compiler *compiler, Lexer *lexer);

	void parse();

	Ast_Expression *parse_global();

	Ast_Struct *parse_struct_declaration();
	Ast_Enum *parse_enum_declaration();
	Ast_Type_Alias *parse_type_alias();
	Ast_Function *parse_function_declaration(bool is_extern);
	Ast_Declaration *parse_variable_declaration(bool expect_semicolon=false);
	void parse_variable_declaration_base(Ast_Declaration *var_decl);

	Ast_Expression *parse_directive();

	Ast_Expression *parse_declaration_or_statement(bool expect_semicolon=true);
	Ast_Expression *parse_expression(int precedence = 1);

	Ast_Expression *parse_binary(int precedence);
	Ast_Expression *parse_unary();
	Ast_Expression *parse_postfix();
	Ast_Expression *parse_primary();
	Ast_Literal *parse_literal();

	Ast_Identifier *parse_identifier();

	Ast_Type_Info *parse_type_specifier();

	Ast *ast_init(Ast *ast);

	void push_scope();
	void pop_scope();

	bool expect_eat(Token::Type type);
	bool expect_eat(char type);
	bool expect(Token::Type type, int off=0);
	bool expect(char type, int off=0);
	Token *peek(int off=0);
	Token *next();
};

#endif
