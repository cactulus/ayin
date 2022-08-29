#ifndef AST_H_
#define AST_H_

#include "common.h"
#include "lexer.h"

struct Ast;

struct Ast_Scope;

struct Ast_Declaration;
struct Ast_Statement;
struct Ast_Expression;

struct Ast_Type_Info;

struct Ast_Identifier;

struct Ast {
	enum Type : u32 {
		SCOPE = 0,
		DECLARATION = 1,
		TYPE = 2,
		IDENTIFIER = 3
	};

	Source_Location location;
	Type type;
};

struct Ast_Scope : Ast {
	Ast_Scope() { 
		type = Ast::SCOPE;
	}

	Ast_Scope *parent;

	Array<Ast_Declaration *> declarations;
	Array<Ast_Statement *> statements;
};

struct Ast_Declaration : Ast {
	enum Declaration_Type {
		VARIABLE,
		FUNCTION,
		STRUCT,
		ENUM,
		TYPE
	};

	Declaration_Type declaration_type;
	Ast_Type_Info *type_info;
	Ast_Identifier *identifier;
	Ast_Expression *initializer;

	Ast_Declaration() {
		type = Ast::DECLARATION;
	}
};

struct Ast_Statement : Ast {

};

struct Ast_Expression : Ast {
};

struct Ast_Type_Info {
	enum Base_Type : u32 {
		STRUCT,
		ENUM,

		FUNCTION,

		POINTER,
		VOID,
		BOOL,
		INT,
		FLOAT,
	};

	struct Enum_Member {
		Atom *name;
		s64 index;
	};

	Base_Type type;

	Ast_Type_Info *element_type;

	Array<Ast_Declaration *> struct_members;
	Array<Enum_Member> enum_members;

	Array<Ast_Type_Info *> parameter_types;
	Ast_Type_Info *return_type;

	bool is_signed;

	/* size in bytes */
	s32 size;
	s32 alignment;
};

struct Ast_Identifier : Ast {
	Atom *atom;

	Ast_Identifier() {
		type = Ast::IDENTIFIER;
	}
};

#endif
