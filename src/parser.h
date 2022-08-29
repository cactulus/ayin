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

	Parser(Compiler *compiler, Lexer *lexer);

	Ast_Scope *parse();

	Ast_Declaration *parse_global();

	Ast_Declaration *parse_struct_declaration();
	Ast_Declaration *parse_enum_declaration();
	Ast_Declaration *parse_type_alias();
	Ast_Declaration *parse_function_declaration();
	Ast_Declaration *parse_variable_declaration();

	Ast_Expression *parse_expression();

	Ast_Identifier *parse_identifier();

	Ast_Type_Info *parse_type_specifier();

	Ast *ast_init(Ast *ast);

	bool expect_eat(Token::Type type);
	bool expect_eat(char type);
	bool expect(Token::Type type);
	bool expect(char type);
	Token *peek();
	Token *next();
};

#endif
