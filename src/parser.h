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

	Ast_Statement *parse_global();

	Ast_Struct *parse_struct_declaration();
	Ast_Enum *parse_enum_declaration();
	Ast_Type_Alias *parse_type_alias();
	Ast_Function *parse_function_declaration();
	Ast_Declaration *parse_variable_declaration(bool expect_semicolon=false);

	Ast_Statement *parse_declaration_or_statement();
	Ast_Expression *parse_expression();

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
