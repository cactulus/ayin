#include <iostream>
#include <sstream>

#include "common.h"
#include "lexer.h"
#include "compiler.h"

static String read_xchars(Lexer *l, bool (*check) (char c));
static void read_atom_or_keyword(Lexer *l, Token *t);
static void read_int_or_float(Lexer *l, Token *t);
static void read_string_lit(Lexer *l, Token *t);
static void read_char_lit(Lexer *l, Token *t);
static void read_hex_number(Lexer *l, Token *t);

static bool is_ident_char(char c);
static bool is_digit(char c);
static bool is_hex_digit(char c);
static bool is_not_quote(char c);
static bool is_alpha(char c);

String escape_str_lit(String text);
char make_escape(char c);

const char *operator_chars = "+-=*/();,.{}&|:<>![]@%^#";

const char *keyword_tokens[] = {
	"return",
	"extern",
	"true",
	"false",
	"if",
	"else",
	"while",
	"for",
	"enum",
	"func",
	"struct",
	"alias",
	"break",
	"continue",
	"nil",
	"cast",
	"sizeof",
	"s8",
	"s16",
	"s32",
	"s64",
	"u8",
	"u16",
	"u32",
	"u64",
	"f32",
	"f64",
	"bool",
	"void",
	"str",
};

const Token::Type keyword_token_types[] = {
	Token::RETURN,
	Token::EXTERN,
	Token::TRUE,
	Token::FALSE,
	Token::IF,
	Token::ELSE,
	Token::WHILE,
	Token::FOR,
	Token::ENUM,
	Token::FUNC,
	Token::STRUCT,
	Token::ALIAS,
	Token::BREAK,
	Token::CONTINUE,
	Token::NIL,
	Token::CAST,
	Token::SIZEOF,
	Token::S8,
	Token::S16,
	Token::S32,
	Token::S64,
	Token::U8,
	Token::U16,
	Token::U32,
	Token::U64,
	Token::F32,
	Token::F64,
	Token::BOOL,
	Token::VOID,
	Token::STR,
};

const char *two_char_tokens[] = {
	"..", "+=", "-=", "*=", "/=", "%=", "==", "!=", "<=", ">=", "&&", "||", "++", "--", "<<", ">>", "&=", "|=", "^="
};

const Token::Type two_char_token_types[] = {
	Token::DOT_DOT,
	Token::ADD_EQ,
	Token::SUB_EQ,
	Token::MUL_EQ,
	Token::DIV_EQ,
	Token::MOD_EQ,
	Token::EQ_EQ,
	Token::NOT_EQ,
	Token::LT_EQ,
	Token::GT_EQ,
	Token::AND_AND,
	Token::BAR_BAR,
	Token::PLUS_PLUS,
	Token::MINUS_MINUS,
	Token::SHL,
	Token::SHR,
	Token::AND_EQ,
	Token::OR_EQ,
	Token::XOR_EQ
};

const char *three_char_tokens[] = {
	"...", "<<=", ">>="
};

const Token::Type three_char_token_types[] = {
	Token::DOT_DOT_DOT,
	Token::SHL_EQ,
	Token::SHR_EQ,
};

Lexer::Lexer(Compiler *compiler, String path, String code) {
	this->compiler = compiler;
    this->file = path;
	this->input = code;

	init_preproc_definitions();
    
	input_len = code.length;
	col = 0;
	line = 0;
	pos = 0;
}

void Lexer::tokenize() {
	while (pos < input_len) {
		tokens.add(read_token());
	}
        
    assert(tokens.length > 0);
	if (tokens[tokens.length - 1].type  != Token::END_OF_FILE) {
		Token eof;
		eof.lexeme = to_string("eof");
		eof.type = Token::END_OF_FILE;
		tokens.add(eof);
	}
}

Token Lexer::read_token() {
	auto c = peek_char();

	while (isspace(c)) {
		eat_char();
		c = peek_char();
	}

	if (c == '/' && peek_char(1) == '/') {
		while (c != '\n') {
			eat_char();
			c = peek_char();
		}
		eat_char();
		return read_token();
	}

	if (c == '/' && peek_char(1) == '*') {
		while (c != '*' || peek_char(1) != '/') {
			eat_char();
			c = peek_char();
		}
		eat_char();
		eat_char();
		return read_token();
	}

	Token t;
	set_token_location(&t);

	if (c == '\0') {
		t.type = Token::END_OF_FILE;
	} else if (c == '_' || isalpha(c)) {
		read_atom_or_keyword(this, &t);
	} else if (c == '0' && peek_char(1) == 'x') {
		read_hex_number(this, &t);
	} else if (is_digit(c)) {
		read_int_or_float(this, &t);
	} else if (c == '"') {
		read_string_lit(this, &t);
	} else if (c == '\'') {
		read_char_lit(this, &t);
	} else {
		char peek_1 = peek_char(1);
		char peek_2 = peek_char(2);
		bool found = false;

		int i = 0;
		for (auto tct : three_char_tokens) {
			if (tct[0] == c) {
				if (tct[1] == peek_1) {
					if (tct[2] == peek_2) {
						t.type = three_char_token_types[i];
						eat_char();
						eat_char();
						eat_char();
						found = true;
						break;
					}
				}
			}
			++i;
		}

		if (!found) {
			i = 0;
			for (auto tct : two_char_tokens) {
				if (tct[0] == c) {
					if (tct[1] == peek_1) {
						t.type = two_char_token_types[i];
						eat_char();
						eat_char();
						found = true;
						break;
					}
				}
				++i;
			}
		}

		if (!found) {
			for (int i = 0; i < strlen(operator_chars); ++i) {
				if (operator_chars[i] == c) {
					t.type = (Token::Type)c;
					eat_char();
					found = true;
					break;
				}
			}
		}	

		if (!found) {
			eat_char();
			set_token_end(&t);
			compiler->report_error(&t, "Unexpected token");
		}
	}

	set_token_end(&t);
	return t;
}

void Lexer::set_token_location(Token *t) {
	t->location.file = file;
	t->location.col = col;
	t->location.line = line;
}

void Lexer::set_token_end(Token *t) {
	t->location.length = col - t->location.col;
}

char Lexer::peek_char(int ahead) {
	auto p = pos + ahead;
	if (p < input_len)
		return input[p];
	return '\0';
}

void Lexer::eat_char() {
	auto c = input[pos];
	if (c == '\n') {
		line++;
		col = 0;
		pos++;
		return;
	}
	col++;
	pos++;
}

void Lexer::init_preproc_definitions() {
#if defined(_WIN64)
    preproc_definitions.add("windows");
#elif defined(__APPLE__) || defined(__MACH__)
    preproc_definitions.add("unix");
    preproc_definitions.add("macos");
#elif defined(__linux__)
    preproc_definitions.add("unix");
    preproc_definitions.add("linux");
#endif
}

String read_xchars(Lexer *l, bool (*check) (char c)) {
	auto start_pos = l->pos;
	auto c = l->peek_char();

	while (check(c)) {
		l->eat_char();
		c = l->peek_char();
	}

	auto end_pos = l->pos;

	auto len = end_pos - start_pos;
	String lexeme = l->input.substring(start_pos, len);

	return lexeme;
}

void read_atom_or_keyword(Lexer *l, Token *t) {
	String lexeme = read_xchars(l, is_ident_char);
	char *cstr_lexeme = to_c_string(lexeme);
	t->lexeme = lexeme;
	int i = 0;
	for (auto keyword : keyword_tokens) {
		if (strcmp(keyword, cstr_lexeme) == 0) {
			t->type = keyword_token_types[i];
			return;
		}
		++i;
	}
	
	t->type = Token::ATOM;
}

void read_int_or_float(Lexer *l, Token *t) {
	String lexeme = read_xchars(l, is_digit);

	if (l->peek_char() == '.' && l->peek_char(1) != '.') {
		l->eat_char();

		String frac = read_xchars(l, is_digit);
		String_Builder sb;
		sb.append(lexeme);
		sb.putchar('.');
		sb.append(frac);

		t->type = Token::FLOAT_LIT;
		t->lexeme = lexeme;
		t->float_value = std::atof(to_c_string(sb.to_string()));
	} else {
		t->type = Token::INT_LIT;
		t->lexeme = lexeme;
		t->int_value = std::atof(to_c_string(lexeme));
	}
}

void read_string_lit(Lexer *l, Token *t) {
	l->eat_char();
	String lexeme = read_xchars(l, is_not_quote);
	l->eat_char();


	lexeme = escape_str_lit(lexeme);

	t->type = Token::STRING_LIT;
	t->lexeme = lexeme;
}

void read_char_lit(Lexer *l, Token *t) {
	l->eat_char();
	char c = l->peek_char();

	bool escape = false;
	if (c == '\\') {
		escape = true;
		l->eat_char();
		c = l->peek_char();
	}

	l->eat_char();
	char end = l->peek_char();

	l->eat_char();

	if (end != '\'') {
        /* report error */
	}

	t->type = Token::CHAR_LIT;
	t->int_value = c;
}

void read_hex_number(Lexer *l, Token *t) {
	l->eat_char();
	l->eat_char();

	String num = read_xchars(l, is_hex_digit);

	t->type = Token::INT_LIT;
	t->lexeme = num;
	t->int_value = std::strtol(to_c_string(num), 0, 16);
}

bool is_ident_char(char c) {
	return c == '_' || isalnum(c);
}

bool is_digit(char c) {
	return '0' <= c && c <= '9';
}

bool is_hex_digit(char c) {
	return ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F');
}

bool is_not_quote(char c) {
	return c != '"';
}

bool is_alpha(char c) {
    return isalpha(c);
}

String escape_str_lit(String text) {
	int sl = text.length, nl = sl, i, j;
	char *etext;

	for (i = 0; i < sl; ++i) {
		if (text[i] != '\\') continue;

		switch (text[i + 1]) {
		case 'n':
		case 'r':
		case 't':
		case '0':
			nl--;
			break;
		default:
			break;
		}
	}

	etext = new char[nl];
	for (i = 0, j = 0; i < sl; ++i, ++j) {
		if (text[i] != '\\') {
			etext[i] = text[i];
			continue;
		}

		char esc = make_escape(text[i + 1]);
		etext[j] = esc;
		i++;
	}

	String _new;
	_new.data = etext;
	_new.length = nl;
	return _new;
}

char make_escape(char c) {
	switch (c) {
	case 'n':
		return '\n';
	case 'r':
		return '\t';
	case 't':
		return '\r';
	case '0':
		return '\0';
	default:
		return c;
	}
}