#ifndef LLVM_H_
#define LLVM_H_

#include "llvm/IR/InstrTypes.h"
namespace llvm {
	class Module;
	class LLVMContext;

	class Value;
	class Type;
	class Function;

	class TargetMachine;
	
    class ConstantFolder;
    class IRBuilderDefaultInserter;

	template<typename T, typename Inserter> class IRBuilder;
};

#include "ast.h"

struct Compiler;
struct LLVM_Converter {
	Compiler *compiler;

	llvm::Module *llvm_module;
	llvm::LLVMContext *llvm_context;
	llvm::TargetMachine *target_machine;
	llvm::IRBuilder<llvm::ConstantFolder, llvm::IRBuilderDefaultInserter> *irb;

	llvm::Type *type_void;
	llvm::Type *type_i1;
	llvm::Type *type_i8;
	llvm::Type *type_i16;
	llvm::Type *type_i32;
	llvm::Type *type_i64;
	llvm::Type *type_f32;
	llvm::Type *type_f64;

	LLVM_Converter(Compiler *compiler);

	void convert_scope(Ast_Scope *scope);

	void convert_statement(Ast_Statement *statement);

	llvm::Value *convert_expression(Ast_Expression *expression);

	llvm::Type *convert_type(Ast_Type_Info *type_info);

	void convert_function(Ast_Function *fun);

	void emit_llvm_ir();
	void emit_object_file();

	llvm::Function *get_or_create_function(Ast_Function *function); 
	llvm::Value *load(llvm::Value *value);
};

#endif
