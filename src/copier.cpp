 #include "compiler.h"
#include "copier.h"
#include "ast.h"

#define COPY_NEW(t) new t(); \
					_new->location = old->location; \
					_new->type_info = copy_type(old->type_info)

#define COPY_F(name) (_new->name = old->name)
#define COPY_C(name) (_new->name = (decltype(_new->name))copy(old->name))
#define COPY_ARRAY(name) for (auto it : old->name) { _new->name.add((decltype(it))copy(it)); }
#define COPY_TYPE(name) (_new->name = copy_type(old->name))

Copier::Copier(Compiler *compiler) {
	this->compiler = compiler;
	this->current_scope = compiler->global_scope;
}

Ast_Expression *Copier::copy(Ast_Expression *ast) {
	if (!ast) return 0;

	switch (ast->type) {
		case Ast::SCOPE: {
			auto old = static_cast<Ast_Scope *>(ast);
			auto _new = push_scope(old);

			pop_scope();

			return _new;
		}
		case Ast::DECLARATION: {
			auto old = static_cast<Ast_Declaration *>(ast);
			auto _new = COPY_NEW(Ast_Declaration);

			COPY_C(identifier);
			COPY_C(initializer);
			COPY_F(flags);

			return _new;
		}
		case Ast::FUNCTION: {
			auto old = static_cast<Ast_Function *>(ast);

			return copy_function(old);
		}
		case Ast::STRUCT: {
			auto old = static_cast<Ast_Struct *>(ast);
			auto _new = COPY_NEW(Ast_Struct);

			COPY_C(identifier);
			COPY_ARRAY(members);

			return _new;
		}
		case Ast::ENUM: {
			auto old = static_cast<Ast_Enum *>(ast);
			auto _new = COPY_NEW(Ast_Enum);

			COPY_C(identifier);

			return _new;
		}
		case Ast::TYPE_ALIAS: {
			auto old = static_cast<Ast_Type_Alias *>(ast);
			auto _new = COPY_NEW(Ast_Type_Alias);

			COPY_C(identifier);

			return _new;
		}
		case Ast::IDENTIFIER: {
			auto old = static_cast<Ast_Identifier *>(ast);
			auto _new = COPY_NEW(Ast_Identifier);

			COPY_F(atom);
			_new->scope = current_scope;

			return _new;
		}
		case Ast::LITERAL: {
			auto old = static_cast<Ast_Literal *>(ast);
			auto _new = COPY_NEW(Ast_Literal);

			COPY_F(literal_type);
			COPY_F(int_value);
			COPY_F(float_value);
			COPY_F(string_value);

			return _new;
		}
		case Ast::CAST: {
			auto old = static_cast<Ast_Cast *>(ast);
			auto _new = COPY_NEW(Ast_Cast);

			COPY_C(expression);
			COPY_TYPE(target_type);

			return _new;
		}
		case Ast::RETURN: {
			auto old = static_cast<Ast_Return *>(ast);
			auto _new = COPY_NEW(Ast_Return);

			COPY_C(return_value);

			return _new;
		}
		case Ast::CALL: {
			auto old = static_cast<Ast_Call *>(ast);
			auto _new = COPY_NEW(Ast_Call);

			COPY_C(identifier);
			COPY_ARRAY(arguments);

			Ast_Expression *decl = find_declaration_by_id(_new->identifier);

			if (!decl) {
				compiler->report_error(_new->identifier, "Symbol not defined");
				return _new;
			}

			if (decl->type != Ast::FUNCTION) {
				compiler->report_error(_new->identifier, "Symbol is not a function");
				return _new;
			}

			Ast_Function *function = static_cast<Ast_Function *>(decl);
			_new->resolved_function = function;

			return _new;
		}
		case Ast::BINARY: {
			auto old = static_cast<Ast_Binary *>(ast);
			auto _new = COPY_NEW(Ast_Binary);

			COPY_F(op);
			COPY_C(lhs);
			COPY_C(rhs);

			return _new;
		}
		case Ast::UNARY: {
			auto old = static_cast<Ast_Unary *>(ast);
			auto _new = COPY_NEW(Ast_Unary);

			COPY_F(op);
			COPY_C(target);
			COPY_F(is_pre);

			return _new;
		}
		case Ast::SIZEOF: {
			auto old = static_cast<Ast_Sizeof *>(ast);
			auto _new = COPY_NEW(Ast_Sizeof);

			COPY_TYPE(target_type);

			return _new;
		}
		case Ast::IF: {
			auto old = static_cast<Ast_If *>(ast);
			auto _new = COPY_NEW(Ast_If);

			COPY_C(condition);
			COPY_C(then_statement);
			COPY_C(else_statement);

			return _new;
		}
		case Ast::WHILE: {
			auto old = static_cast<Ast_While *>(ast);
			auto _new = COPY_NEW(Ast_While);

			COPY_C(condition);
			COPY_C(statement);

			return _new;
		}
		case Ast::INDEX: {
			auto old = static_cast<Ast_Index *>(ast);
			auto _new = COPY_NEW(Ast_Index);

			COPY_C(expression);
			COPY_C(index);

			return _new;
		}
		case Ast::MEMBER: {
			auto old = static_cast<Ast_Member *>(ast);
			auto _new = COPY_NEW(Ast_Member);

			COPY_C(left);
			COPY_C(field);
			COPY_F(field_index);

			return _new;
		}
		case Ast::CONTINUE: {
			auto old = static_cast<Ast_Continue *>(ast);
			auto _new = COPY_NEW(Ast_Continue);

			return _new;
		}
		case Ast::BREAK: {
			auto old = static_cast<Ast_Break *>(ast);
			auto _new = COPY_NEW(Ast_Break);

			return _new;
		}
		case Ast::DIRECTIVE: {
			auto old = static_cast<Ast_Directive *>(ast);
			auto _new = COPY_NEW(Ast_Directive);

			COPY_F(directive_type);
			COPY_C(condition);
			COPY_C(then_stmt);
			COPY_C(else_stmt);
			COPY_F(file);

			return _new;
		}
		case Ast::FOR: {
			auto old = static_cast<Ast_For *>(ast);
			auto _new = COPY_NEW(Ast_For);

			_new->iterator_declaration_scope = push_scope(old->iterator_declaration_scope);

			COPY_C(iterator_decl);
			COPY_C(iterator_index_decl);
			COPY_C(initial_iterator_expression);
			COPY_C(upper_range_expression);

			pop_scope();
			return _new;
		}
	}

	assert(0);
	return 0;
}

Ast_Function *Copier::copy_function(Ast_Function *old) {
	Ast_Scope *old_current_scope = current_scope;
	current_scope = compiler->global_scope;

	Ast_Function *_new = COPY_NEW(Ast_Function);

	COPY_C(identifier);

	if (old->template_scope)
		_new->template_scope = push_scope(old->template_scope);

	if (old->parameter_scope)
		_new->parameter_scope = push_scope(old->parameter_scope);

	COPY_TYPE(return_type);
	_new->block_scope = push_scope(old->block_scope);

	COPY_F(linkage_name);
	COPY_F(flags);

	/* TODO: check if i have to copy polymorphed_overloads */

	if (old->template_scope)
		pop_scope();

	if (old->parameter_scope)
		pop_scope();

	pop_scope();

	current_scope = old_current_scope;

	return _new;
}

Ast_Type_Info *Copier::copy_type(Ast_Type_Info *old) {
	return old;
}

Ast_Scope *Copier::push_scope(Ast_Scope *old) {
	Ast_Scope *_new = new Ast_Scope();
	_new->parent = current_scope;
	current_scope = _new;

	for (auto decl : old->declarations) {
		_new->declarations.add(copy(decl));
	}

	for (auto expr : old->statements) {
		_new->statements.add(copy(expr));
	}

	return _new;
}

void Copier::pop_scope() {
	current_scope = current_scope->parent;
}
