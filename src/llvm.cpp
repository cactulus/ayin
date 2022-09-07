#include <string>

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/ConstantFolder.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

/* TODO: remove later. uniform llvm version */
#ifdef _WIN64
#include "llvm/Support/TargetRegistry.h"
#else
#include "llvm/MC/TargetRegistry.h"
#endif

#include "llvm/Target/TargetMachine.h"

#include "llvm/Support/CodeGen.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"

#include "ast.h"
#include "llvm.h"

#define STR_REF(x) StringRef(x.data, x.length)

using namespace llvm;

LLVM_Converter::LLVM_Converter(Compiler *compiler) {
	this->compiler = compiler;

	InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

	std::string target_triple = llvm::sys::getDefaultTargetTriple();
    std::string error;
    auto target = TargetRegistry::lookupTarget(target_triple, error);
    
    if (!target) {
        errs() << error;
        return;
    }
    
    auto cpu = "generic";
    auto features = "";
    
    TargetOptions opt;
    auto rm = Optional<Reloc::Model>();
    target_machine = target->createTargetMachine(target_triple, cpu, features, opt, rm);
    
    llvm_context = new LLVMContext();
    llvm_module = new Module("Aleph Module", *llvm_context);
    irb = new IRBuilder<ConstantFolder, IRBuilderDefaultInserter>(*llvm_context);

    type_void = Type::getVoidTy(*llvm_context);
    type_i1 = Type::getInt1Ty(*llvm_context);
    type_i8 = Type::getInt8Ty(*llvm_context);
    type_i16 = Type::getInt16Ty(*llvm_context);
    type_i32 = Type::getInt32Ty(*llvm_context);
    type_i64 = Type::getInt64Ty(*llvm_context);
    type_f32 = Type::getFloatTy(*llvm_context);
    type_f64 = Type::getDoubleTy(*llvm_context);
}

void LLVM_Converter::convert_scope(Ast_Scope *scope) {
	for (auto decl : scope->declarations) {
		convert_statement(decl);
	}

	for (auto stmt : scope->statements) {
		convert_statement(stmt);
	}
}

void LLVM_Converter::convert_statement(Ast_Statement *statement) {
	switch (statement->type) {
		case Ast::STRUCT: {
			
		} break;
		case Ast::FUNCTION: {
			auto fun = static_cast<Ast_Function *>(statement);
			if (fun->is_template_function) {
				for (auto f : fun->polymorphed_overloads) {
					convert_function(f);
				}
			} else {
				convert_function(fun);
			}
		} break;
		case Ast::DECLARATION: {
			auto decl = static_cast<Ast_Declaration *>(statement);
			auto var = irb->CreateAlloca(convert_type(decl->type_info));
			if (decl->initializer) {
				auto value = convert_expression(decl->initializer);
				irb->CreateStore(value, var);
			}
			decl->llvm_reference = var;
		} break;
		case Ast_Statement::RETURN: {
			Ast_Return *ret = static_cast<Ast_Return *>(statement);

			if (ret->return_value) {
				Value *return_value = convert_expression(ret->return_value);
				irb->CreateRet(return_value);
			} else {
				irb->CreateRetVoid();
			}
		} break;
	}
}

Value *LLVM_Converter::convert_expression(Ast_Expression *expression) {
	switch (expression->type) {
		case Ast_Expression::LITERAL: {
			Ast_Literal *literal = static_cast<Ast_Literal *>(expression);
			Type *ty = convert_type(expression->inferred_type);

			switch (literal->literal_type) {
				case Ast_Literal::BOOL:
				case Ast_Literal::INT: {
					return ConstantInt::get(ty, literal->int_value);
				}
				case Ast_Literal::FLOAT: {
					return ConstantFP::get(ty, literal->float_value);
				}
				default:
					assert(0 && "LLVM emission for literal type not implemented");
					return 0;
			}

		};
		case Ast_Expression::IDENTIFIER: {
			Ast_Identifier *id = static_cast<Ast_Identifier *>(expression);
			assert(id->resolved_declaration);

			auto declaration = id->resolved_declaration;
			// for now 
			assert(declaration->type == Ast::DECLARATION);
			auto decl = static_cast<Ast_Declaration *>(declaration);

			Value *ref = decl->llvm_reference;

			return load(ref);
		};
		case Ast_Expression::CAST: {
			Ast_Cast *cast = static_cast<Ast_Cast *>(expression);
            Value *value = convert_expression(cast->expression);
            
            auto src = cast->expression->inferred_type;
            auto dst = cast->target_type;
            
            auto src_type = convert_type(src);
            auto dst_type = convert_type(dst);
            if (type_is_int(src) && type_is_int(dst)) {
                if (src->size > dst->size) {
                    return irb->CreateTrunc(value, dst_type);
                } else if (src->size < dst->size) {
                    if (src->is_signed && dst->is_signed) {
                        return irb->CreateSExt(value, dst_type);
                    } else {
                        return irb->CreateZExt(value, dst_type);
                    }
                }
                
                return value;
            } else if (type_is_float(src) && type_is_float(dst)) {
                if (src->size < dst->size) {
                    return irb->CreateFPExt(value, dst_type);
                } else if (src->size > dst->size) {
                    return irb->CreateFPTrunc(value, dst_type);
                }
                
                return value;
            } else if (type_is_float(src) && type_is_int(dst)) {
                if (dst->is_signed) {
                    return irb->CreateFPToSI(value, dst_type);
                } else {
                    return irb->CreateFPToUI(value, dst_type);
                }
            } else if (type_is_int(src) && type_is_float(dst)) {
                if (src->is_signed) {
                    return irb->CreateSIToFP(value, dst_type);
                } else {
                    return irb->CreateUIToFP(value, dst_type);
                }
            } else if (type_is_pointer(src) && type_is_pointer(dst)) {
                return irb->CreatePointerCast(value, dst_type);
			}

			return load(value);
		};
		case Ast::CALL: {
			Ast_Call *call = static_cast<Ast_Call *>(expression);
			Function *fun = get_or_create_function(call->resolved_function);
			Array<Value *> arguments;
			
			for (auto arg : call->arguments) {
				arguments.add(convert_expression(arg));
			}

			return irb->CreateCall(fun, ArrayRef<Value *>(arguments.data, arguments.length));
		};
		default:
			assert(0 && "LLVM emission for expression not implemented");
			return 0;
	}
}

Type *LLVM_Converter::convert_type(Ast_Type_Info *type_info) {
	if (type_info->type == Ast_Type_Info::POINTER) {
		return convert_type(type_info->element_type)->getPointerTo();
	}

	if (type_info->type == Ast_Type_Info::INT) {
		switch (type_info->size) {
			case 1: return type_i8;
			case 2: return type_i16;
			case 4: return type_i32;
			case 8: return type_i64;
		}
	}

	if (type_info->type == Ast_Type_Info::FLOAT) {
		switch (type_info->size) {
			case 4: return type_f32;
			case 8: return type_f64;
		}
	}

	if (type_info->type == Ast_Type_Info::VOID) {
		return type_void;
	}

	if (type_info->type == Ast_Type_Info::BOOL) {
		return type_i1;
	}

	if (type_info->type == Ast_Type_Info::STRUCT) {
		Array<Type *> member_types;
        
        for (auto member : type_info->struct_members) {
            member_types.add(convert_type(member));
        }
        
        return StructType::get(*llvm_context, ArrayRef<Type *>(member_types.data, member_types.length), false);
	}

	if (type_info->type == Ast_Type_Info::FUNCTION) {
		Array<Type *> arg_types;
        
        for (auto par : type_info->parameters) {
            Type *par_type = convert_type(par);
            
            arg_types.add(par_type);
        }
        
        Type *return_type = convert_type(type_info->return_type);
        
        return FunctionType::get(return_type, ArrayRef<Type *>(arg_types.data, arg_types.length), false);
	}

	if (type_info->type == Ast_Type_Info::TYPE) {
		return convert_type(type_info->element_type);
	}

	assert(0 && "Should be unreachable");
	return 0;
}

void LLVM_Converter::convert_function(Ast_Function *fun) {
	auto fn = get_or_create_function(fun);

	s64 i = 0;
	for (auto arg_it = fn->arg_begin(); arg_it != fn->arg_end(); arg_it++) {
		Ast_Declaration *par = fun->parameters[i];
		
		Type *par_type = convert_type(par->type_info);
		Value *var = irb->CreateAlloca(par_type);
		par->llvm_reference = var;
		irb->CreateStore(&*arg_it, var);
		++i;
	}
	
	BasicBlock *entry_block = BasicBlock::Create(*llvm_context, "", fn);
	irb->SetInsertPoint(entry_block);

	convert_scope(fun->block_scope);
}

void LLVM_Converter::emit_llvm_ir() {
    auto file_name = "output.ll";
    std::error_code ec;
    raw_fd_ostream dest(file_name, ec, sys::fs::OF_None);

	llvm_module->print(dest, 0);
}

void LLVM_Converter::emit_object_file() {
	std::string target_triple = llvm::sys::getDefaultTargetTriple();
    
    llvm_module->setDataLayout(target_machine->createDataLayout());
    llvm_module->setTargetTriple(target_triple);
    
    auto file_name = "output.o";
    std::error_code ec;
    raw_fd_ostream dest(file_name, ec, sys::fs::OF_None);
    
    if (ec) {
        errs() << "Could not open file: " << ec.message();
        return;
    }
    
    legacy::PassManager pass;


	auto file_type = llvm::CGFT_ObjectFile;
    
    pass.add(createVerifierPass(false));
    if (target_machine->addPassesToEmitFile(pass, dest, nullptr, file_type)) {
        errs() << "TargetMachine can't emit a file of this type";
        return;
    }
    
    pass.run(*llvm_module);
    dest.flush();
}

Function *LLVM_Converter::get_or_create_function(Ast_Function *function) {
	auto fn_name = STR_REF(function->linkage_name);

	auto fn = llvm_module->getFunction(fn_name);

	printf("%p\n", fn);

	if (!fn) {
		auto fn_type = convert_type(function->type_info);
		fn = Function::Create(
				static_cast<FunctionType *>(fn_type),
				GlobalValue::LinkageTypes::ExternalLinkage,
				fn_name,
				llvm_module
		);
	}

	function->llvm_reference = fn;

	return fn;
}

Value *LLVM_Converter::load(Value *value) {
	return irb->CreateLoad(value->getType()->getPointerElementType(), value);
}
