#ifndef LLVM_H_
#define LLVM_H_

namespace llvm {
	class Module;
	class LLVMContext;

	class Value;
	class Type;
	class Instruction;
	class StructType;
	class Function;
	class DataLayout;
	class Argument;

	class BasicBlock;

	class TargetMachine;
	
    class ConstantFolder;
    class IRBuilderDefaultInserter;

    template<typename T> class ArrayRef;

	template<typename T, typename Inserter> class IRBuilder;

	class DICompileUnit;
	class DIBuilder;
	class DIType;
	class DIFile;
	class DISubprogram;
};

#include "ast.h"

struct LLVM_Converter;
struct DebugInfo {
	llvm::DICompileUnit *cu;
	llvm::DIBuilder *db;
	llvm::DIFile *file;
	const llvm::DataLayout *layout;
	llvm::DISubprogram *current_sp;
	Array<llvm::Instruction *> debug_values;

	void init(LLVM_Converter *converter, String entry_file);

	void add_function(Ast_Function *ast_func, llvm::Function *f);
	void add_parameter(Ast_Identifier *id, llvm::Value *var, int arg_index, llvm::Argument &arg, llvm::BasicBlock *block);
	void add_variable(Ast_Identifier *id, llvm::Value *var, llvm::BasicBlock *block);
	void add_inst(Ast_Expression *expr, llvm::Instruction *inst);

	llvm::DIType *convert_type(llvm::Type *type);

	void finalize();
};

struct Compiler;
struct LLVM_Converter {
	Compiler *compiler;
	DebugInfo debug;

	llvm::Module *llvm_module;
	llvm::LLVMContext *llvm_context;
	llvm::TargetMachine *target_machine;
	llvm::IRBuilder<llvm::ConstantFolder, llvm::IRBuilderDefaultInserter> *irb;

	llvm::Function *current_function = 0;

	Array<llvm::BasicBlock *> continue_blocks;
	Array<llvm::BasicBlock *> break_blocks;

	llvm::Type *type_void;
	llvm::Type *type_i1;
	llvm::Type *type_i8;
	llvm::Type *type_i16;
	llvm::Type *type_i32;
	llvm::Type *type_i64;
	llvm::Type *type_f32;
	llvm::Type *type_f64;
	llvm::StructType *type_string;

	LLVM_Converter(Compiler *compiler);

	void convert(String entry_file, Ast_Scope *scope);
	void convert_scope(Ast_Scope *scope);

	void convert_statement(Ast_Expression *expression);

	llvm::Value *convert_expression(Ast_Expression *expression, bool is_lvalue=false);
	llvm::Value *convert_binary(Ast_Binary *binary);

	llvm::Type *convert_type(Ast_Type_Info *type_info);

	void convert_function(Ast_Function *fun);

	void emit_llvm_ir();
	void emit_object_file();

	llvm::Function *get_or_create_function(Ast_Function *function); 
	llvm::Value *lalloca(llvm::Type *ty);
	llvm::Value *load(llvm::Value *value);
	llvm::Value *store(llvm::Value *value, llvm::Value *ptr);
	llvm::Value *gep(llvm::Value *ptr, llvm::ArrayRef<llvm::Value *> idx_list);
};

#endif
