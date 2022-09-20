#ifndef AST_H_
#define AST_H_

#include "common.h"
#include "lexer.h"

namespace llvm {
	class Value;
};

struct Ast;

struct Ast_Scope;

struct Ast_Expression;

struct Ast_Type_Info;

struct Ast_Declaration;
struct Ast_Function;
struct Ast_Struct;
struct Ast_Enum;
struct Ast_Type_Alias;

struct Ast_Sizeof;
struct Ast_Unary;
struct Ast_Binary;
struct Ast_Identifier;
struct Ast_Literal;
struct Ast_Cast;
struct Ast_Index;
struct Ast_Member;

struct Ast_If;
struct Ast_While;

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
		BINARY = 11,
		UNARY = 12,
		SIZEOF = 13,
		IF = 14,
		WHILE = 15,
		INDEX = 16,
		MEMBER = 17,
	};

	Source_Location location;
	Type type;
};

struct Ast_Expression : Ast {
	Ast_Expression *substitution = 0;

	Ast_Type_Info *type_info = 0;
};

struct Ast_Scope : Ast_Expression {
	Ast_Scope() { 
		type = Ast::SCOPE;
	}

	Ast_Scope *parent = 0;

	Array<Ast_Expression *> declarations;
	Array<Ast_Expression *> statements;
};


struct Ast_Type_Info {
	enum Base_Type : u32 {
		UNRESOLVED = 0,

		STRUCT,
		ENUM,
		ARRAY,

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

	s32 array_size = 0;

	Ast_Struct *struct_decl;
	Array<Ast_Type_Info *> struct_members;
	Array<Enum_Member> enum_members;

	Array<Ast_Type_Info *> parameters;
	Ast_Type_Info *return_type;

	bool is_signed = false;
	bool is_dynamic = false;

	/* size in bytes for int and float */
	s32 size = 0;
};

struct Ast_Declaration : Ast_Expression {
	llvm::Value *llvm_reference = 0;
	Ast_Identifier *identifier = 0;
	Ast_Expression *initializer = 0;
	bool is_constant = false;

	Ast_Declaration() {
		type = Ast::DECLARATION;
	}
};

struct Ast_Function : Ast_Expression {
	llvm::Value *llvm_reference = 0;
	Ast_Identifier *identifier = 0;

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

struct Ast_Struct : Ast_Expression {
	llvm::Value *llvm_reference = 0;
	Ast_Identifier *identifier = 0;
	Array<Ast_Declaration *> members;

	Ast_Struct() {
		type = Ast::STRUCT;
	}
};

struct Ast_Enum : Ast_Expression {
	Ast_Identifier *identifier = 0;

	Ast_Enum() {
		type = Ast::ENUM;
	}
};

struct Ast_Type_Alias : Ast_Expression {
	Ast_Identifier *identifier = 0;

	Ast_Type_Alias() {
		type = Ast::TYPE_ALIAS;
	}
};

struct Ast_Sizeof : Ast_Expression {
	Ast_Type_Info *target_type;

	Ast_Sizeof() {
		type = Ast::SIZEOF;
	}
};

struct Ast_Unary : Ast_Expression {
	Ast_Expression *target;
	int op;
	bool is_pre = false;

	Ast_Unary() {
		type = Ast::UNARY;
	}
};

struct Ast_Binary : Ast_Expression {
	Ast_Expression *lhs;
	Ast_Expression *rhs;
	int op;

	Ast_Binary() {
		type = Ast::BINARY;
	}
};

struct Ast_Identifier : Ast_Expression {
	Atom *atom;
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

struct Ast_Index : Ast_Expression {
	Ast_Expression *expression;	
	Ast_Expression *index;

	Ast_Index() {
		type = Ast::INDEX;
	}
};

struct Ast_Member : Ast_Expression {
	Ast_Expression *left = 0;
	Ast_Identifier *field;

	s64 field_index = -1;

	Ast_Member() {
		type = Ast::MEMBER;
	}
};

struct Ast_Return : Ast_Expression {
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

struct Ast_If : Ast_Expression {
	Ast_Expression *condition = 0;
    
    Ast_Expression *then_statement = 0;
    Ast_Expression *else_statement = 0;

	Ast_If() {
		type = Ast::IF;
	}
};

struct Ast_While : Ast_Expression {
    Ast_Expression *condition = 0;
    Ast_Expression *statement = 0;

    Ast_While() {
    	type = Ast::WHILE;
	}
};

inline bool type_is_struct(Ast_Type_Info *type_info) {
	return type_info->type == Ast_Type_Info::STRUCT;
}

inline bool type_is_array(Ast_Type_Info *type_info) {
	return type_info->type == Ast_Type_Info::ARRAY;
}

inline bool type_is_bool(Ast_Type_Info *type_info) {
	return type_info->type == Ast_Type_Info::BOOL;
}

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

inline bool binop_is_assign(int op) {
	switch (op) {
		case '=':
		case Token::ADD_EQ:
		case Token::SUB_EQ:
		case Token::MUL_EQ:
		case Token::DIV_EQ:
		case Token::MOD_EQ:
		case Token::SHL_EQ:
		case Token::SHR_EQ:
			return true;
		default:
			return false;
	}
}

inline bool binop_is_logical(int op) {
	return op == Token::AND_AND || op == Token::BAR_BAR;
}

inline bool binop_is_binary(int op) {
    switch (op) {
		case '+':
		case '-':
		case '*':
		case '/':
		case '%':
		case '&':
		case '|':
		case '^':
        case Token::SHL:
        case Token::SHR:
            return true;
		default:
            return false;
    }
}

inline bool binop_is_conditional(int op) {
	switch (op) {
		case Token::EQ_EQ:
		case Token::NOT_EQ:
		case '<':
		case '>':
		case Token::LT_EQ:
		case Token::GT_EQ:
			return true;
		default:
			return false;
	}
}

#endif
