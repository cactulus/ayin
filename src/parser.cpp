#include "parser.h"
#include "ast.h"
#include "common.h"
#include "lexer.h"
#include "compiler.h"

const int binary_operators[] = {
	Token::ADD_EQ,
	Token::SUB_EQ,
	Token::MUL_EQ,
	Token::DIV_EQ,
	Token::MOD_EQ,
	Token::SHL_EQ,
	Token::SHR_EQ,
	Token::XOR_EQ,
	Token::OR_EQ,
	Token::AND_EQ,
	'=',
	Token::BAR_BAR,
	Token::AND_AND,
	'|', '^', '&',
	Token::EQ_EQ,
	Token::NOT_EQ,
	Token::LT_EQ,
	Token::GT_EQ,
	'<', '>',
	Token::SHL,
	Token::SHR,
	'+', '-', '*', '/', '%',
};

const int binary_operators_precedence[] = {
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	2, 3, 4, 5, 6, 7, 7, 8, 8, 8, 8,
	9, 9, 19, 19, 11, 11, 11
};

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

Ast_Expression *Parser::parse_global() {
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
	
	if (expect_eat(Token::EXTERN)) {
		if (expect_eat(Token::FUNC)) {
			return parse_function_declaration(true);
		} else {
			compiler->report_error(peek(), "Expected 'func' after 'extern' keyword");
			return 0;
		}
	}

	if (expect_eat(Token::FUNC)) {
		return parse_function_declaration(false);
	}

	if (expect_eat('#')) {
		return parse_directive();
	}

	Ast_Declaration *var_decl = parse_variable_declaration(true);
	if (var_decl) {
		var_decl->flags |= VAR_GLOBAL;
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
		if (!decl) {
			compiler->report_error(peek(), "Expected variable name");
			return 0;
		}

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

	if (!expect_eat(';')) {
		compiler->report_error(peek(), "Expected ';' after type alias");
	}

	return ta;
}

Ast_Function *Parser::parse_function_declaration(bool is_extern) {
	auto fn = AST_NEW(Ast_Function);

	fn->identifier = parse_identifier();
	if (is_extern) {
		fn->flags |= FUNCTION_EXTERNAL;
	}
	
	if (!fn->identifier) {
		compiler->report_error(fn->identifier, "Expected identifier for function name");
	}

	if (expect_eat('<')) {
		push_scope();

		fn->template_scope = current_scope;
		fn->flags |= FUNCTION_TEMPLATE;

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
		if (!par_decl) {
			compiler->report_error(peek(), "Expected variable name");
			return fn;
		}

		if (par_decl->initializer) {
			compiler->report_error(par_decl, "Can't initialize parameter");
		}

		current_scope->declarations.add(par_decl);

		if (!expect(')')) {
			if (!expect_eat(',')) {
				compiler->report_error(peek(), "Expected ',' after parameter");
			}
		}

		if (expect_eat(Token::DOT_DOT_DOT)) {
			fn->flags |= FUNCTION_VARARG;
		}
	}

	if (expect_eat('{') || expect_eat(';')) {
		fn->return_type = compiler->type_void;
	} else {
		fn->return_type = parse_type_specifier();

		if (!expect_eat('{') && !is_extern) {
			compiler->report_error(peek(), "Expected '{' after return type specifier");
		}

		if (!expect_eat(';') && is_extern) {
			compiler->report_error(peek(), "Expected ';' after return type specifier");
		}
	}

	if (!is_extern) {
		push_scope();
		fn->block_scope = current_scope;

		while (!expect_eat('}')) {
			Ast_Expression *statement_or_declaration = parse_declaration_or_statement();
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
	} else {
		fn->block_scope = 0;
	}

	if (fn->template_scope)
		pop_scope();

	if (fn->parameter_scope)
		pop_scope();

	if (!is_extern) {
		pop_scope();
	}

	return fn;
}

Ast_Declaration *Parser::parse_variable_declaration(bool expect_semicolon) {
	Ast_Declaration *var_decl = AST_NEW(Ast_Declaration);

	var_decl->identifier = parse_identifier();

	if (!var_decl->identifier) {
		return 0;
	}

	if (expect_eat(':')) {
		parse_variable_declaration_base(var_decl);
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

void Parser::parse_variable_declaration_base(Ast_Declaration *var_decl) {
	if (expect_eat('=')) {
		var_decl->initializer = parse_expression();
	} else if (expect_eat(':')) {
		var_decl->flags |= VAR_CONSTANT;
		var_decl->initializer = parse_expression();
	} else {
		var_decl->type_info = parse_type_specifier();

		if (expect_eat('=')) {
			var_decl->initializer = parse_expression();
		}
	}
}

Ast_Directive *Parser::parse_directive() {
	Ast_Directive *directive = AST_NEW(Ast_Directive);

	auto id = next();

	if (id->type == Token::ATOM) {
		if (id->lexeme == to_string("use")) {
			directive->directive_type = Ast_Directive::USE;
		} else if (id->lexeme == to_string("include")) {
			directive->directive_type = Ast_Directive::INCLUDE;
		}

		auto token = peek();
		if (!expect_eat(Token::STRING_LIT)) {
			compiler->report_error(token, "Expected string literal after #include or #use");
			return directive;
		}

		String name = token->lexeme;
		String base_path = basepath(lexer->file);

		if (!expect_eat(';')) {
			compiler->report_error(token, "Expected ';'");
			return directive;
		}

		if (directive->directive_type == Ast_Directive::USE) {
			directive->file = name;
		} else {
			const int MAX_PATH = 512;
			char fullname[MAX_PATH];
			snprintf(fullname, MAX_PATH, "%.*s%.*s", base_path.length, base_path.data, name.length, name.data);

			directive->file = copy_string(to_string(fullname));
		}
	} else if (id->type == Token::IF) {
		directive->directive_type = Ast_Directive::IF;

	}

	compiler->directives.add(directive);
	return directive;
}

Ast_Expression *Parser::parse_declaration_or_statement(bool expect_semicolon) {
	if (expect(Token::RETURN)) {
		Ast_Return *ret = AST_NEW(Ast_Return);
		next();
		if (!expect_eat(';')) {
			ret->return_value = parse_expression();
			if (!expect_eat(';') && expect_semicolon) {
				compiler->report_error(peek(), "expected ';' after return value");
			}
		}

		return ret;
	}

	if (expect(Token::IF)) {
		Ast_If *_if = AST_NEW(Ast_If);
		next();

		_if->condition = parse_expression();
        if (compiler->errors_reported) return _if;
        
        if (!_if->condition) {
            compiler->report_error(_if, "'if' must be followed by an expression.\n");
            return _if;
        }
        
        _if->then_statement = parse_declaration_or_statement();
        
        if (expect(Token::ELSE)) {
            next();
            
            _if->else_statement = parse_declaration_or_statement();
        }

		return _if;
	}

	if (expect(Token::WHILE)) {
		Ast_While *_while = AST_NEW(Ast_While);
		next();

		_while->condition = parse_expression();
        
        if (!_while->condition) {
            compiler->report_error(_while, "'while' must be followed by an expression.\n");
            return _while;
        }
        
        _while->statement = parse_declaration_or_statement();

		return _while;
	}

	if (expect(Token::FOR)) {
		Ast_For *_for = AST_NEW(Ast_For);
		next();

		_for->initial_iterator_expression = parse_expression();

		auto token = peek();
		if (expect_eat(Token::DOT_DOT)) {
			if (!_for->initial_iterator_expression) {
				compiler->report_error(token, ".. operator must be preceeded by an expression.\n");
				return _for;
			}

			_for->upper_range_expression = parse_expression();
		}

		push_scope();
		_for->iterator_declaration_scope = current_scope;

		_for->body = parse_declaration_or_statement();

		pop_scope();
		return _for;
	}

	if (expect_eat('{')) {
		push_scope();
		Ast_Scope *scope = current_scope;

		while (!expect_eat('}')) {
			Ast_Expression *statement_or_declaration = parse_declaration_or_statement();
			if (!statement_or_declaration) {
				return scope;
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

		pop_scope();
		return scope;
	}

	if (expect(Token::CONTINUE)) {
		auto _continue = AST_NEW(Ast_Continue);
		next();
		
		if (!expect_eat(';') && expect_semicolon) {
			compiler->report_error(peek(), "expected ';' after 'continue'");
		}
			
		return _continue;
	}

	if (expect(Token::BREAK)) {
		auto _continue = AST_NEW(Ast_Break);
		next();

		if (!expect_eat(';') && expect_semicolon) {
			compiler->report_error(peek(), "expected ';' after 'break'");
		}

		return _continue;
	}

	if (expect_eat('#')) {
		return parse_directive();
	}
	 
	Ast_Expression *expr = parse_expression();
    if (!expect_eat(';') && expect_semicolon) {
        compiler->report_error(peek(), "expected ';' after expression");
    }
    return expr;
}

Ast_Expression *Parser::parse_expression(int precedence) {
	return parse_binary(precedence);
}

Ast_Expression *Parser::parse_binary(int precedence) {
	auto lhs = parse_unary();

	while (true) {
		auto tok = peek();
		auto tok_type = tok->type;

		int op_prec = 0;
		int index = 0;
		for (auto bin_op : binary_operators) {
			if (bin_op == tok_type) {
				op_prec = binary_operators_precedence[index];
				break;
			}
			index++;
		}

		if (!op_prec || op_prec < precedence) {
			break;
		}

		next();

		Ast_Binary *binary = AST_NEW(Ast_Binary);
		binary->lhs = lhs;
		binary->op = tok_type;
        binary->rhs = parse_binary(op_prec + 1);

        lhs = binary;
	}

	return lhs;
}

Ast_Expression *Parser::parse_unary() {
	auto tok = peek();

	if (expect('&')) {
		auto expr = AST_NEW(Ast_Unary);
		next();

		expr->target = parse_postfix();
		expr->op = '&';

		return expr;
	} else if (expect('*')) {
		auto expr = AST_NEW(Ast_Unary);
		next();

		expr->target = parse_unary();
		expr->op = '*';

		return expr;
	} else if (expect_eat('+')) {
		return parse_postfix();
	} else if (expect('!')) {
		auto expr = AST_NEW(Ast_Unary);
		next();

		expr->target = parse_postfix();
		expr->op = '!';

		return expr;
	} else if (expect('-')) {
		auto expr = AST_NEW(Ast_Unary);
		next();

		expr->target = parse_postfix();
		expr->op = '-';

		return expr;
	} else if (expect(Token::PLUS_PLUS) || expect(Token::MINUS_MINUS)) {
		auto expr = AST_NEW(Ast_Unary);
		next();

		expr->target = parse_postfix();
		expr->is_pre = true;
		expr->op = tok->type;

		return expr;
	} else if (expect(Token::CAST)) {
		auto cast = AST_NEW(Ast_Cast);
		next();

        if (!expect_eat('(')) {
        	compiler->report_error(peek(), "Expected '('");
        	return cast;
		}

		cast->target_type = parse_type_specifier();
        
        if (!expect_eat(')')) {
        	compiler->report_error(peek(), "Expected ')'");
			return cast;
		}

		cast->expression = parse_expression();
		return cast;
	}

	return parse_postfix();
}

Ast_Expression *Parser::parse_postfix() {
	auto target = parse_primary();
	auto tok = peek();

    while (!expect(Token::END_OF_FILE)) {
        if (expect('.')) {
            Ast_Member *member = AST_NEW(Ast_Member);
            next();
            
            auto right = parse_identifier();
            if (!right) return nullptr;
            
            member->left = target;
            member->field = right;
            
            target = member;
        } else if (expect('[')) {
			Ast_Index *index = AST_NEW(Ast_Index);
            next();

            index->expression = target;
            index->index = parse_expression();
            
			if (!expect_eat(']')) {
				compiler->report_error(peek(), "Expected ']'");
			}
            
            target = index;
        } else {
            break;
        }
	}

	tok = peek();
	if (expect_eat(Token::PLUS_PLUS) || expect_eat(Token::MINUS_MINUS)) {
		auto expr = AST_NEW(Ast_Unary);

		expr->target = target;
		expr->is_pre = false;
		expr->op = tok->type;

		return expr;
	}

	return target;
}

Ast_Expression *Parser::parse_primary() {
	Token *t = peek();

	if (expect_eat('(')) {
		auto expr = parse_expression();
		if (!expect_eat(')')) {
			compiler->report_error(peek(), "Expected ')'");
			next();
		}
		return expr;
	}

	if (expect(Token::ATOM)) {
		Ast_Identifier *id = AST_NEW(Ast_Identifier);
		id->atom = compiler->make_atom(t->lexeme);
		id->scope = current_scope;

		next();

		if (expect_eat(':')) {
			Ast_Declaration *decl = AST_NEW(Ast_Declaration);
			decl->location = id->location;
			decl->identifier = id;

			parse_variable_declaration_base(decl);

			return decl;
		}

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

	if (expect(Token::NIL)) {
		Ast_Literal *lit = AST_NEW(Ast_Literal);
		next();

		lit->literal_type = Ast_Literal::NIL;
		return lit;
	}

	if (expect(Token::INT_LIT)) {
		Ast_Literal *lit = AST_NEW(Ast_Literal);
		next();

		lit->literal_type = Ast_Literal::INT;
		lit->int_value = t->int_value;
		return lit;
	}

	if (expect(Token::FLOAT_LIT)) {
		Ast_Literal *lit = AST_NEW(Ast_Literal);
		next();

		lit->literal_type = Ast_Literal::FLOAT;
		lit->float_value = t->float_value;
		return lit;
	}
	
	if (expect(Token::STRING_LIT)) {
		Ast_Literal *lit = AST_NEW(Ast_Literal);
		next();

		lit->literal_type = Ast_Literal::STRING;
		lit->string_value = t->lexeme;
		return lit;
	}

	if (expect(Token::TRUE)) {
		Ast_Literal *lit = AST_NEW(Ast_Literal);
		next();

		lit->literal_type = Ast_Literal::BOOL;
		lit->int_value = 1;
		return lit;
	}
	
	if (expect(Token::FALSE)) {
		Ast_Literal *lit = AST_NEW(Ast_Literal);
		next();

		lit->literal_type = Ast_Literal::BOOL;
		lit->int_value = 0;
		return lit;
	}

	if (expect(Token::SIZEOF)) {
        Ast_Sizeof *size = AST_NEW(Ast_Sizeof);
        next();
        
        if (!expect_eat('(')) {
        	compiler->report_error(peek(), "Expected '('");
        	return size;
		}
        
        size->target_type = parse_type_specifier();
        
        if (!expect_eat(')')) {
        	compiler->report_error(peek(), "Expected ')'");
			return size;
		}
        
        return size;
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
		case Token::STR: type_info = compiler->type_string; break;
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
		type_info->size = 8;
		return type_info;
	}

	if (t->type == '[') {
		next();
		
		Token *num_token = peek();
		s32 arr_size = -1;
		bool dynamic = false;

		if (num_token->type == Token::INT_LIT) {
			next();
			arr_size = num_token->int_value;
			dynamic = false;
		} else if (expect(Token::DOT_DOT)) {
			next();
			dynamic = true;
		}

		if (!expect_eat(']')) {
			compiler->report_error(peek(), "Expected ']'");
			return 0;
		}

		Ast_Type_Info *element_type = parse_type_specifier();
		if (!element_type) {
			compiler->report_error(peek(), "Failed to parse element type of array type");
			return 0;
		}

		type_info = new Ast_Type_Info();
		type_info->type = Ast_Type_Info::ARRAY;
		type_info->is_dynamic = dynamic;
		type_info->array_size = arr_size; 
		type_info->element_type = element_type;
		return type_info;
	}

	if (t->type == Token::ATOM) {
		type_info = new Ast_Type_Info();
		type_info->type = Ast_Type_Info::UNRESOLVED;
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
