#ifndef LEXER_H_
#define LEXER_H_

#include "common.h"

struct Source_Location {
	String file;
	s64 col;
	s64 line;
	s64 length;
};

struct Token {
	enum Type {
		ERROR = 0,
		END_OF_FILE = 1,

		COLON_COLON,
		DOT_DOT,
		DOT_DOT_DOT,

		PLUS_PLUS,
		MINUS_MINUS,

		ADD_EQ,
		SUB_EQ,
		MUL_EQ,
		DIV_EQ,
		MOD_EQ,

		EQ_EQ,
		NOT_EQ,
		LT_EQ,
		GT_EQ,

		SHL,
		SHR,
		SHR_EQ,
		SHL_EQ,
		AND_EQ,
		OR_EQ,
		XOR_EQ,

		AND_AND,
		BAR_BAR,

		ATOM = 128,
		INT_LIT,
		FLOAT_LIT,
		STRING_LIT,
		CHAR_LIT,

		RETURN,
		EXTERN,
		AS,
		TRUE,
		FALSE,
		IF,
		ELSE,
		WHILE,
		FOR,
		ENUM,
		FUNC,
		STRUCT,
		ALIAS,
		BREAK,
		CONTINUE,
		NIL,

		S8,
		S16,
		S32,
		S64,
		U8,
		U16,
		U32,
		U64,
		F32,
		F64,
		BOOL,
		VOID,
	};

	Type type;
	Source_Location location;
	String lexeme;

	union {
		s64 int_value;
		f64 float_value;
	};
};

struct Compiler;
struct Lexer {
	Array<char *> preproc_definitions;
	Array<Token> tokens;
	Compiler *compiler;
	String input;
  	String file;
	s64 input_len;

	s64 line;
	s64 col;
	s64 pos;

	Lexer(Compiler *compiler, String path, String code);

	void tokenize();

	Token read_token();

	void set_token_location(Token *t);
	void set_token_end(Token *t);

	char peek_char(int ahead = 0);
	void eat_char();
    
	void init_preproc_definitions();
};

#endif
