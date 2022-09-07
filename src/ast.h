#ifndef AST_H_
#define AST_H_

#include "common.h"
#include "lexer.h"

namespace llvm {
	class Value;
};

struct Ast;

struct Ast_Scope;

struct Ast_Statement;
struct Ast_Expression;

struct Ast_Type_Info;

struct Ast_Declaration;
struct Ast_Function;
struct Ast_Struct;
struct Ast_Enum;
struct Ast_Type_Alias;

struct Ast_Identifier;
struct Ast_Literal;
struct Ast_Cast;

struct Ast_Return;

struct Ast {
	enum Type : u32 {
		SCOPE = 0,
		DECLARATION = 1,
		FUNCTION = 2,
		STRUCT = 3,
		ENUM = 4,
		TYPE_ALIAS = 5,
		IDENTIFIER = 6,
		LITERAL = 7,
		CAST = 8,
		RETURN = 9,
		CALL = 10,
	};

	Source_Location location;
	Type type;
};

struct Ast_Scope : Ast {
	Ast_Scope() { 
		type = Ast::SCOPE;
	}

	Ast_Scope *parent = 0;

	Array<Ast_Statement *> declarations;
	Array<Ast_Statement *> statements;
};

struct Ast_Statement : Ast {
	Ast_Type_Info *type_info = 0;
};

struct Ast_Expression : Ast {
	Ast_Type_Info *inferred_type = 0;
};

struct Ast_Type_Info {
	enum Base_Type : u32 {
		UNINITIALIZED = 0,

		STRUCT,
		ENUM,

		FUNCTION,

		POINTER,
		VOID,
		BOOL,
		INT,
		FLOAT,

		TYPE,
	};

	struct Enum_Member {
		Atom *name;
		s64 index;
	};

	Base_Type type;

	Ast_Type_Info *element_type = 0;
	Ast_Identifier *unresolved_name;

	Ast_Struct *struct_decl;
	Array<Ast_Type_Info *> struct_members;
	Array<Enum_Member> enum_members;

	Array<Ast_Type_Info *> parameters;
	Ast_Type_Info *return_type;

	bool is_signed;

	/* size in bytes */
	s32 size;
	s32 alignment;
};

struct Ast_Declaration : Ast_Statement {
	llvm::Value *llvm_reference = 0;
	Ast_Identifier *identifier = 0;
	Ast_Expression *initializer = 0;

	Ast_Declaration() {
		type = Ast::DECLARATION;
	}
};

struct Ast_Function : Ast_Statement {
	llvm::Value *llvm_reference = 0;
	Ast_Identifier *identifier = 0;

	Array<Ast_Declaration *> parameters;
	Ast_Type_Info *return_type = 0;

	Ast_Scope *parameter_scope = 0;
	Ast_Scope *block_scope = 0;
	String linkage_name;

	bool is_template_function = false;
	Ast_Scope *template_scope = 0;
	Array<Ast_Function *> polymorphed_overloads;

	Ast_Function() {
		type = Ast::FUNCTION;
	}
};

struct Ast_Struct : Ast_Statement {
	llvm::Value *llvm_reference = 0;
	Ast_Identifier *identifier = 0;
	Array<Ast_Declaration *> members;

	Ast_Struct() {
		type = Ast::STRUCT;
	}
};

struct Ast_Enum : Ast_Statement {
	Ast_Identifier *identifier = 0;

	Ast_Enum() {
		type = Ast::ENUM;
	}
};

struct Ast_Type_Alias : Ast_Statement {
	Ast_Identifier *identifier = 0;

	Ast_Type_Alias() {
		type = Ast::TYPE_ALIAS;
	}
};

struct Ast_Identifier : Ast_Expression {
	Atom *atom;
	Ast_Statement *resolved_declaration = 0;
	Ast_Scope *scope = 0;

	Ast_Identifier() {
		type = Ast::IDENTIFIER;
	}
};

struct Ast_Literal : Ast_Expression {
	enum Literal_Type {
		BOOL,
		FLOAT,
		INT
	};

	Literal_Type literal_type;
	s64 int_value;
	f64 float_value;

	Ast_Literal() {
		type = Ast::LITERAL;
	}
};

struct Ast_Cast : Ast_Expression {
	Ast_Expression *expression;	
	Ast_Type_Info *target_type;

	Ast_Cast() {
		type = Ast::CAST;
	}
};

struct Ast_Return : Ast_Statement {
	Ast_Expression *return_value = 0;

	Ast_Return() {
		type = Ast::RETURN;
	}
};

struct Ast_Call : Ast_Expression {
	Ast_Identifier *identifier;
	Array<Ast_Expression *> arguments;
	Ast_Function *resolved_function = 0;

	Ast_Call() {
		type = Ast::CALL;
	}
};

inline bool type_is_int(Ast_Type_Info *type_info) {
	return type_info->type == Ast_Type_Info::INT;
}

inline bool type_is_float(Ast_Type_Info *type_info) {
	return type_info->type == Ast_Type_Info::FLOAT;
}

inline bool type_is_pointer(Ast_Type_Info *type_info) {
	return type_info->type == Ast_Type_Info::POINTER;
}

inline bool type_is_primitive(Ast_Type_Info *type_info) {
	switch (type_info->type) {
		case Ast_Type_Info::FLOAT:
		case Ast_Type_Info::INT:
		case Ast_Type_Info::VOID:
		case Ast_Type_Info::BOOL:
			return true;
		default:
			return false;
	}
}

#endif
