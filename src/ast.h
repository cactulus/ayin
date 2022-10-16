#ifndef AST_H_
#define AST_H_

#include "common.h"
#include "lexer.h"

namespace llvm {
	class Value;
};

struct Ast;

struct Ast_Scope;
struct Ast_Directive;

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
struct Ast_For;
struct Ast_Continue;
struct Ast_Break;

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
		CONTINUE = 18,
		BREAK = 19,
		DIRECTIVE = 20,
		FOR = 21,
	};

	Source_Location location;
	Type type;
};

struct Ast_Expression : Ast {
	Ast_Expression *substitution = 0;

	Ast_Type_Info *type_info = 0;
};

/*
* Equivalent of a C++ preprocessor directive
* #include  -> includes local file
* #use      -> includes file from library path
* #if/#else -> static/compile-time if for conditional code compilation
*/
struct Ast_Directive : Ast_Expression {

	enum Directive_Type : u32 {
		INCLUDE,
		USE,
	};

	Directive_Type directive_type;
	String file;

	Ast_Directive() {
		type = Ast::DIRECTIVE;
	}
};

/*
* Represents a scope in the program
* Default scope is the global scope, which has a parent of null
*/
struct Ast_Scope : Ast_Expression {
	Ast_Scope() { 
		type = Ast::SCOPE;
	}

	Ast_Scope *parent = 0;

	Array<Ast_Expression *> declarations;
	Array<Ast_Expression *> statements;
};

struct Ast_Type_Info {

	/*
	* Unresolved -> a non-primitive type name that cannot be resolved during the parsing stage,
	*				but is being resolved in the semantic analysis
	* 
	* Type		 -> type is used in type aliases and marks a generic type
	*/
	enum Base_Type : u32 {
		UNRESOLVED = 0,

		STRUCT,
		ENUM,
		ARRAY,

		FUNCTION,

		POINTER,
		/* type so it doesnt conflict with stupid Windows.h macro VOID */
		VOID_TYPE,
		BOOL,
		INT,
		FLOAT,
		STRING,

		TYPE,
	};

	struct Enum_Member {
		Atom *name;
		Ast_Literal *value = 0;
		s32 index;
	};

	Base_Type type;

	/* element type is used by the pointer and array type */
	Ast_Type_Info *element_type = 0;
	Ast_Identifier *unresolved_name;

	/* element count for static arrays */
	s32 array_size = 0;

	Ast_Struct *struct_decl;
	Array<Ast_Type_Info *> struct_members;
	Array<Enum_Member> enum_members;

	/* function type */
	Array<Ast_Type_Info *> parameters;
	Ast_Type_Info *return_type;

	/* integer type */
	bool is_signed = false;
	/* is dynamic array */
	bool is_dynamic = false;

	bool auto_cast = false;

	/* size in bytes for int and float */
	s32 size = 0;
};

/* Variable declaration flags */
const u8 VAR_GLOBAL = 0x1;
const u8 VAR_CONSTANT = 0x2;

struct Ast_Declaration : Ast_Expression {
	/* llvm_reference is set in the llvm code generation stage */
	llvm::Value *llvm_reference = 0;
	Ast_Identifier *identifier = 0;
	/* Initializer not null for defintions, null for declarations */
	Ast_Expression *initializer = 0;
	u8 flags = 0;

	Ast_Declaration() {
		type = Ast::DECLARATION;
	}
};

/*
* Function declaration flags
* Only external functions can have a variable length parameter
* Extern functions can not be template functions
*/
const u8 FUNCTION_EXTERNAL = 0x1;
const u8 FUNCTION_TEMPLATE = 0x2;
const u8 FUNCTION_VARARG = 0x4;

struct Ast_Function : Ast_Expression {
	/* llvm_reference is set in the llvm code generation stage */
	llvm::Value *llvm_reference = 0;
	Ast_Identifier *identifier = 0;

	Ast_Type_Info *return_type = 0;

	Ast_Scope *parameter_scope = 0;
	Ast_Scope *block_scope = 0;
	String linkage_name;

	u8 flags = 0;

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
	Ast_Type_Info *target_type = 0;

	Ast_Sizeof() {
		type = Ast::SIZEOF;
	}
};

struct Ast_Unary : Ast_Expression {
	Ast_Expression *target = 0;
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
	Atom *atom = 0;
	Ast_Scope *scope = 0;

	Ast_Identifier() {
		type = Ast::IDENTIFIER;
	}
};

struct Ast_Literal : Ast_Expression {
	enum Literal_Type {
		BOOL,
		FLOAT,
		INT,
		STRING,
		NIL,
		COMPOUND
	};

	Literal_Type literal_type;
	s64 int_value;
	f64 float_value;
	String string_value;

	Array<Ast_Expression *> values;
	Ast_Type_Info *compound_type_info;

	Ast_Literal() {
		type = Ast::LITERAL;
	}
};

struct Ast_Cast : Ast_Expression {
	Ast_Expression *expression = 0;
	Ast_Type_Info *target_type = 0;

	Ast_Cast() {
		type = Ast::CAST;
	}
};

struct Ast_Index : Ast_Expression {
	Ast_Expression *expression = 0;	
	Ast_Expression *index = 0;

	Ast_Index() {
		type = Ast::INDEX;
	}
};

struct Ast_Member : Ast_Expression {
	Ast_Expression *left = 0;
	Ast_Identifier *field = 0;

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

	bool by_function_pointer = false;

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

struct Ast_For : Ast_Expression {
	Ast_Declaration *iterator_decl = 0;
	Ast_Declaration *iterator_index_decl = 0;

	Ast_Expression *initial_iterator_expression = 0;
	Ast_Expression *upper_range_expression = 0;

	Ast_Scope *iterator_declaration_scope = 0;
	Ast_Expression *body = 0;

	Ast_For() {
		type = Ast::FOR;
	}
};

struct Ast_Continue : Ast_Expression {
	Ast_Continue() {
		type = Ast::CONTINUE;
	}
};
struct Ast_Break : Ast_Expression {
	Ast_Break() {
		type = Ast::BREAK;
	}
};

inline bool type_is_enum(Ast_Type_Info *type_info) {
	return type_info->type == Ast_Type_Info::ENUM;
}

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

inline bool type_is_function(Ast_Type_Info *type_info) {
	return type_info->type == Ast_Type_Info::FUNCTION;
}

inline bool type_is_string(Ast_Type_Info *type_info) {
	return type_info->type == Ast_Type_Info::STRING;
}

inline bool type_is_primitive(Ast_Type_Info *type_info) {
	switch (type_info->type) {
		case Ast_Type_Info::FLOAT:
		case Ast_Type_Info::INT:
		case Ast_Type_Info::VOID_TYPE:
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
