#include "compiler.h"
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
    llvm_module = new Module("Ayin", *llvm_context);
    irb = new IRBuilder<ConstantFolder, IRBuilderDefaultInserter>(*llvm_context);

    type_void = Type::getVoidTy(*llvm_context);
    type_i1 = Type::getInt1Ty(*llvm_context);
    type_i8 = Type::getInt8Ty(*llvm_context);
    type_i16 = Type::getInt16Ty(*llvm_context);
    type_i32 = Type::getInt32Ty(*llvm_context);
    type_i64 = Type::getInt64Ty(*llvm_context);
    type_f32 = Type::getFloatTy(*llvm_context);
	type_f64 = Type::getDoubleTy(*llvm_context);
	type_string = StructType::create(*llvm_context, {type_i8->getPointerTo(), type_i64}, "string", true);
}

void LLVM_Converter::convert_scope(Ast_Scope *scope) {
	for (auto decl : scope->declarations) {
		convert_statement(decl);
	}

	for (auto stmt : scope->statements) {
		convert_statement(stmt);
	}
}

void LLVM_Converter::convert_statement(Ast_Expression *expression) {
	switch (expression->type) {
		case Ast::SCOPE: {
			convert_scope(static_cast<Ast_Scope *>(expression));
		} break;
		case Ast::DIRECTIVE:
		case Ast::TYPE_ALIAS:
		case Ast::STRUCT: {
		} break;
		case Ast::FUNCTION: {
			auto fun = static_cast<Ast_Function *>(expression);
			if (fun->flags & FUNCTION_TEMPLATE) {
				for (auto f : fun->polymorphed_overloads) {
					convert_function(f);
				}
			} else {
				convert_function(fun);
			}
		} break;
		case Ast::RETURN: {
			Ast_Return *ret = static_cast<Ast_Return *>(expression);

			if (ret->return_value) {
				Value *return_value = convert_expression(ret->return_value);
				irb->CreateRet(return_value);
			} else {
				irb->CreateRetVoid();
			}
		} break;
		case Ast::IF: {
			Ast_If *_if = static_cast<Ast_If *>(expression);
            auto cond = convert_expression(_if->condition);
            
            auto current_block = irb->GetInsertBlock();
            
            BasicBlock *next_block = BasicBlock::Create(*llvm_context, "", current_block->getParent());
            BasicBlock *then_block = nullptr;
            BasicBlock *else_block = nullptr;
            
            BasicBlock *failure_target = next_block;
            
            then_block = BasicBlock::Create(*llvm_context, "", current_block->getParent());
            if (_if->then_statement) {
                irb->SetInsertPoint(then_block);
                convert_statement(_if->then_statement);
                
            }
            if (!irb->GetInsertBlock()->getTerminator()) irb->CreateBr(next_block);
            
            if (_if->else_statement) {
                else_block = BasicBlock::Create(*llvm_context, "", current_block->getParent());
                irb->SetInsertPoint(else_block);
                convert_statement(_if->else_statement);
                
                if (!irb->GetInsertBlock()->getTerminator()) irb->CreateBr(next_block);
                
                failure_target = else_block;
            }
            
            irb->SetInsertPoint(current_block);
            irb->CreateCondBr(cond, then_block, failure_target);
            irb->SetInsertPoint(next_block);
            irb->SetInsertPoint(next_block);
		} break;
		case Ast::WHILE: {
			Ast_While *_while = static_cast<Ast_While *>(expression);

     		auto current_block = irb->GetInsertBlock();
            
            BasicBlock *next_block = BasicBlock::Create(*llvm_context, "", current_block->getParent());
            BasicBlock *loop_header = BasicBlock::Create(*llvm_context, "", current_block->getParent());
            BasicBlock *loop_body = BasicBlock::Create(*llvm_context, "", current_block->getParent());
            
			continue_blocks.add(loop_header);
			break_blocks.add(next_block);

            irb->CreateBr(loop_header);
            
            irb->SetInsertPoint(loop_header);
            auto cond = convert_expression(_while->condition);
            irb->SetInsertPoint(loop_header);
            irb->CreateCondBr(cond, loop_body, next_block);
            
            irb->SetInsertPoint(loop_body);
            convert_statement(_while->statement);
            if (!irb->GetInsertBlock()->getTerminator()) irb->CreateBr(loop_header);
            
            irb->SetInsertPoint(next_block);

			continue_blocks.pop();
			break_blocks.pop();
		} break;
		case Ast::FOR: {
			Ast_For *_for = static_cast<Ast_For *>(expression);
			Value *it_index_alloca;
			Value *it_alloca;

			auto it_decl = _for->iterator_decl;
			auto decl_type = it_decl->type_info;

			auto it_index_decl = _for->iterator_index_decl;
			Ast_Type_Info *it_index_type = 0;

			if (it_index_decl) {
				convert_statement(it_index_decl);

				it_index_type = it_index_decl->type_info;
				it_index_alloca = it_index_decl->llvm_reference;

				convert_statement(it_decl);
				it_alloca = it_decl->llvm_reference;
			} else {
				convert_statement(it_decl);
				it_alloca = it_decl->llvm_reference;

				it_index_type = decl_type;
				it_index_alloca = it_alloca;
			}

			auto cond_block = BasicBlock::Create(*llvm_context, "", current_function);
			auto body_block = BasicBlock::Create(*llvm_context, "", current_function);
			auto inc_block = BasicBlock::Create(*llvm_context, "", current_function);
			auto after_block = BasicBlock::Create(*llvm_context, "", current_function);

			irb->CreateBr(cond_block);
			irb->SetInsertPoint(cond_block);

			auto loaded_index = load(it_index_alloca);

			auto upper = convert_expression(_for->upper_range_expression);
			Value *cond = nullptr;
			if (it_index_type->is_signed) {
				cond = irb->CreateICmpSLT(loaded_index, upper);
			} else {
				cond = irb->CreateICmpULT(loaded_index, upper);
			}
		
			irb->CreateCondBr(cond, body_block, after_block);
			irb->SetInsertPoint(body_block);

			if (it_index_decl) {
				convert_statement(it_decl);
			}

			convert_statement(_for->body);
			
			irb->CreateBr(inc_block);
			irb->SetInsertPoint(inc_block);

			loaded_index = load(it_index_alloca);
			auto added = irb->CreateNSWAdd(
				loaded_index,
				ConstantInt::get(convert_type(it_index_type), 1)
			);
			irb->CreateStore(added, it_index_alloca);
			irb->CreateBr(cond_block);

			irb->SetInsertPoint(after_block);

			/*

			auto it_decl = _for->iterator_decl;
			auto decl_type = it_decl->type_info;

			Value *it_index_alloca;
			Value *it_alloca;

			auto it_index_decl = _for->iterator_index_decl;
			Ast_Type_Info *it_index_type = 0;
			if (it_index_decl) {
				convert_statement(it_index_decl);

				it_index_type = it_index_decl->type_info;
				it_index_alloca = it_index_decl->llvm_reference;

				convert_statement(it_decl);
				it_alloca = it_decl->llvm_reference;
			} else {
				convert_statement(it_decl);
				it_alloca = it_decl->llvm_reference;

				it_index_type = decl_type;
				it_index_alloca = it_alloca;
			}

			auto current_block = irb->GetInsertBlock();

			BasicBlock *next_block = BasicBlock::Create(*llvm_context, "", current_block->getParent());
			BasicBlock *loop_header = BasicBlock::Create(*llvm_context, "", current_block->getParent());
			BasicBlock *loop_body = BasicBlock::Create(*llvm_context, "", current_block->getParent());

			irb->CreateBr(loop_header);

			irb->SetInsertPoint(loop_header);
			auto it_index = irb->CreateLoad(it_index_alloca);

			auto upper = convert_expression(_for->upper_range_expression);
			Value *cond = nullptr;
			if (it_index_decl) {
				if (it_index_type->is_signed) {
					cond = irb->CreateICmpSLT(it_index, upper);
				} else {
					cond = irb->CreateICmpULT(it_index, upper);
				}
			} else {
				if (it_index_type->is_signed) {
					cond = irb->CreateICmpSLE(it_index, upper);
				} else {
					cond = irb->CreateICmpULE(it_index, upper);
				}
			}

			irb->SetInsertPoint(loop_header);
			irb->CreateCondBr(cond, loop_body, next_block);

			irb->SetInsertPoint(loop_body);
			if (it_index_decl) {
				convert_statement(it_decl);
			}

			convert_statement(_for->body);

			it_index = irb->CreateLoad(it_index_alloca);
			irb->CreateStore(irb->CreateAdd(it_index, ConstantInt::get(convert_type(it_index_type), 1)), it_index_alloca);
			if (!irb->GetInsertBlock()->getTerminator()) irb->CreateBr(loop_header);

			irb->SetInsertPoint(next_block);*/

			break;
		}
		case Ast::CONTINUE:
			if (continue_blocks.length > 0) {
				irb->CreateBr(continue_blocks[continue_blocks.length - 1]);
			} else {
				compiler->report_error(expression, "'continue' is not in a loop");
			}
			break;
		case Ast::BREAK:
			if (break_blocks.length > 0) {
				irb->CreateBr(break_blocks[break_blocks.length - 1]);
			} else {
				compiler->report_error(expression, "'break' is not in a loop");
			}
			break;
		default: {
			convert_expression(expression);
		}
	}
}

Value *LLVM_Converter::convert_expression(Ast_Expression *expression, bool is_lvalue) {
	while(expression->substitution) expression = expression->substitution;

	switch (expression->type) {
		case Ast_Expression::DECLARATION: {
			auto decl = static_cast<Ast_Declaration *>(expression);
			auto decl_type = convert_type(decl->type_info);

			if (decl->flags & VAR_GLOBAL) {
				auto var_name = STR_REF(decl->identifier->atom->id);
				llvm_module->getOrInsertGlobal(var_name, decl_type);
				auto var = llvm_module->getGlobalVariable(var_name);

				if (decl->initializer) {
					var->setConstant(decl->flags & VAR_CONSTANT);

					auto init = dyn_cast<Constant>(convert_expression(decl->initializer));
					var->setInitializer(init);
				} else {
					var->setExternallyInitialized(false);
				}
			} else {
				auto var = irb->CreateAlloca(decl_type);
				if (decl->initializer) {
					auto value = convert_expression(decl->initializer);
					irb->CreateStore(value, var);
				} else {
					irb->CreateStore(Constant::getNullValue(decl_type), var);
				}
				decl->llvm_reference = var;
			}

			return 0;
		} break;
		case Ast_Expression::LITERAL: {
			Ast_Literal *literal = static_cast<Ast_Literal *>(expression);
			Type *ty = convert_type(expression->type_info);

			switch (literal->literal_type) {
				case Ast_Literal::BOOL:
				case Ast_Literal::INT: {
					return ConstantInt::get(ty, literal->int_value);
				}
				case Ast_Literal::FLOAT: {
					return ConstantFP::get(ty, literal->float_value);
				}
				case Ast_Literal::STRING: {
					if (literal->string_value.length == 0 || literal->string_value.data == 0) {
						return Constant::getNullValue(type_string);
					}

					Constant *data = irb->CreateGlobalStringPtr(STR_REF(literal->string_value));
					Constant *length = ConstantInt::get(type_i64, literal->string_value.length);

					return ConstantStruct::get(type_string, { data, length });
				}
				case Ast_Literal::NIL: {
					return ConstantPointerNull::get(static_cast<PointerType *>(convert_type(literal->type_info)));
				}
				default:
					assert(0 && "LLVM emission for literal type not implemented");
					return 0;
			}

		}
		case Ast_Expression::IDENTIFIER: {
			Ast_Identifier *id = static_cast<Ast_Identifier *>(expression);

			auto declaration = find_declaration_by_id(id);
			// no need to check if found because we already do that in the type checking phase 

			/* safe cast for now */
			auto decl = static_cast<Ast_Declaration *>(declaration);

			Value *ref = decl->llvm_reference;

			if (is_lvalue) {
				return ref;
			}
			return load(ref);
		}
		case Ast_Expression::CAST: {
			Ast_Cast *cast = static_cast<Ast_Cast *>(expression);
            Value *value = convert_expression(cast->expression, is_lvalue);
            
            auto src = cast->expression->type_info;
            auto dst = cast->type_info;
            
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
		}
		case Ast::CALL: {
			Ast_Call *call = static_cast<Ast_Call *>(expression);
			Function *fun = get_or_create_function(call->resolved_function);
			Array<Value *> arguments;
			
			for (auto arg : call->arguments) {
				arguments.add(convert_expression(arg));
			}

			return irb->CreateCall(fun, ArrayRef<Value *>(arguments.data, arguments.length));
		}
		case Ast::BINARY: {
			Ast_Binary *binary = static_cast<Ast_Binary *>(expression);

			return convert_binary(binary);
		}
		case Ast::UNARY: {
			Ast_Unary *unary = static_cast<Ast_Unary *>(expression);
			switch (unary->op) {
				case '&': {
					return convert_expression(unary->target, true);
				}
				case '*': {
					auto value = convert_expression(unary->target, is_lvalue);

					return load(value);
				}
				case '!': {
					auto target = convert_expression(unary->target);
					return irb->CreateNot(target);
				}
				case '-': {
					auto target = convert_expression(unary->target);
					if (type_is_float(unary->type_info)) {
						return irb->CreateFNeg(target);
					} else {
						return irb->CreateNeg(target);
					}
					
				}
				case Token::PLUS_PLUS:
				case Token::MINUS_MINUS: {
					auto target = convert_expression(unary->target, true);
					auto loaded = load(target);
					auto type = convert_type(unary->type_info);
					Value *one;

					if (unary->op == Token::PLUS_PLUS) {
						one = ConstantInt::get(type, 1);
					} else {
						one = ConstantInt::get(type, -1);
					}

					auto val = irb->CreateAdd(loaded, one);
					irb->CreateStore(val, target);

					if (unary->is_pre) {
						return val;
					} else {
						return loaded;
					}
				}
			}
		}
		case Ast::SIZEOF: {
			Ast_Sizeof *size = static_cast<Ast_Sizeof *>(expression);
			Type *type = convert_type(size->target_type);
			int byte_size = llvm_module->getDataLayout().getTypeAllocSize(type);

			return ConstantInt::get(type_i32, byte_size);
		}
		case Ast::INDEX: {
			Ast_Index *array_index = static_cast<Ast_Index *>(expression);
			auto array = convert_expression(array_index->expression, true);
            auto index = convert_expression(array_index->index);
            
            auto type = array_index->expression->type_info;
            if (type_is_array(type) && type->array_size == -1) {
                array = gep(array, {ConstantInt::get(type_i32, 0), ConstantInt::get(type_i32, 0)});
                array = load(array);
                auto element = gep(array, index);
                
                if (!is_lvalue) return load(element);
                return element;
            } else if (type_is_pointer(type)) {
                auto ptr = load(array);
                auto element = gep(ptr, {index});
                
                if (!is_lvalue) return load(element);
                return element;
			} else if (type_is_string(type)) {
				array = gep(array, { ConstantInt::get(type_i32, 0), ConstantInt::get(type_i32, 0) });
				array = load(array);
				auto element = gep(array, index);

				if (!is_lvalue) return load(element);
				return element;
			}
            
            auto element = gep(array, {ConstantInt::get(type_i32, 0), index});
            
            if (!is_lvalue) return load(element);
            return element;

			return 0;
		}
		case Ast::MEMBER: {
			Ast_Member *member = static_cast<Ast_Member *>(expression);
			auto lhs = convert_expression(member->left, true);
            
			if (auto constant = dyn_cast<Constant>(lhs)) {
				return irb->CreateExtractValue(constant, member->field_index);
			} else {
				auto valueptr = gep(lhs, { ConstantInt::get(type_i32, 0), ConstantInt::get(type_i32, member->field_index) });
				if (!is_lvalue) return load(valueptr);
				return valueptr;
			}
			return 0;
		}
	}

	assert(0 && "LLVM expression emission unreachable");
	return 0;
}

Value *LLVM_Converter::convert_binary(Ast_Binary *binary) {
	auto rhs = convert_expression(binary->rhs);
	auto token_op = binary->op;
	Instruction::BinaryOps op;
	CmpInst::Predicate cmpop;
	Value *new_value = 0;
	auto ty = binary->lhs->type_info;
	bool is_ptr = type_is_pointer(ty);
	Type *llvm_type = convert_type(binary->type_info);

    if ((type_is_int(ty) && !ty->is_signed) || is_ptr) {
        switch (token_op) {
            case '+':
			case Token::ADD_EQ: op = Instruction::BinaryOps::Add; break;
            case '-':
			case Token::SUB_EQ: op = Instruction::BinaryOps::Sub; break;
            case '*':
			case Token::MUL_EQ: op = Instruction::BinaryOps::Mul; break;
            case '/':
			case Token::DIV_EQ: op = Instruction::BinaryOps::UDiv; break;
            case '%':
			case Token::MOD_EQ: op = Instruction::BinaryOps::URem; break;
			case Token::EQ_EQ: cmpop = CmpInst::Predicate::ICMP_EQ; break;
			case Token::NOT_EQ: cmpop = CmpInst::Predicate::ICMP_NE; break;
			case Token::LT_EQ: cmpop = CmpInst::Predicate::ICMP_ULE; break;
			case Token::GT_EQ: cmpop = CmpInst::Predicate::ICMP_UGE; break;
            case '<': cmpop = CmpInst::Predicate::ICMP_ULT; break;
            case '>': cmpop = CmpInst::Predicate::ICMP_UGT; break;
	    }
    } else if (type_is_int(ty) || type_is_bool(ty)) {
        switch (token_op) {
            case '+':
			case Token::ADD_EQ:  op = Instruction::BinaryOps::Add; break;
            case '-':
			case Token::SUB_EQ: op = Instruction::BinaryOps::Sub; break;
            case '*':
			case Token::MUL_EQ: op = Instruction::BinaryOps::Mul; break;
            case '/':
			case Token::DIV_EQ: op = Instruction::BinaryOps::SDiv; break;
            case '%':
			case Token::MOD_EQ: op = Instruction::BinaryOps::SRem; break;
			case Token::EQ_EQ: cmpop = CmpInst::Predicate::ICMP_EQ; break;
			case Token::NOT_EQ: cmpop = CmpInst::Predicate::ICMP_NE; break;
			case Token::LT_EQ: cmpop = CmpInst::Predicate::ICMP_SLE; break;
			case Token::GT_EQ: cmpop = CmpInst::Predicate::ICMP_SGE; break;
            case '<': cmpop = CmpInst::Predicate::ICMP_SLT; break;
            case '>': cmpop = CmpInst::Predicate::ICMP_SGT; break;
	    }
	} else if (type_is_float(ty)) {
		switch (token_op) {
			case '+':
			case Token::ADD_EQ: op = Instruction::BinaryOps::FAdd; break;
			case '-':
			case Token::SUB_EQ: op = Instruction::BinaryOps::FSub; break;
			case '*':
			case Token::MUL_EQ: op = Instruction::BinaryOps::FMul; break;
			case '/':
			case Token::DIV_EQ: op = Instruction::BinaryOps::FDiv; break;
			case '%':
			case Token::MOD_EQ: op = Instruction::BinaryOps::FRem; break;
			case Token::EQ_EQ: cmpop = CmpInst::Predicate::FCMP_UEQ; break;
			case Token::NOT_EQ: cmpop = CmpInst::Predicate::FCMP_UNE; break;
			case Token::LT_EQ: cmpop = CmpInst::Predicate::FCMP_ULE; break;
			case Token::GT_EQ: cmpop = CmpInst::Predicate::FCMP_UGE; break;
			case '<': cmpop = CmpInst::Predicate::FCMP_ULT; break;
			case '>': cmpop = CmpInst::Predicate::FCMP_UGT; break;
		}
	}

	if (token_op == '=') {
		new_value = rhs;
	    auto target = convert_expression(binary->lhs, true);

	    irb->CreateStore(new_value, target);
	} else if (binop_is_binary(token_op) || (token_op >= Token::ADD_EQ && token_op <= Token::MOD_EQ)) {
		auto lhs = convert_expression(binary->lhs);
	    if (is_ptr) {
	        new_value = irb->CreateInBoundsGEP(llvm_type, lhs, rhs);
        } else {
            switch (token_op) {
                case '|':
                    new_value = irb->CreateOr(lhs, rhs);
                    break;
                case '&':
                    new_value = irb->CreateAnd(lhs, rhs);
                    break;
                case '^':
                    new_value = irb->CreateXor(lhs, rhs);
                    break;
				case Token::SHR:
                    new_value = irb->CreateLShr(lhs, rhs);
                    break;
				case Token::SHL:
                    new_value = irb->CreateShl(lhs, rhs);
                    break;
                default:
                    new_value = irb->CreateBinOp(op, lhs, rhs);
                    break;
            }
        }
        if (token_op >= Token::ADD_EQ && token_op <= Token::MOD_EQ) {
	        auto target = convert_expression(binary->lhs, true);
	        irb->CreateStore(new_value, target);
        }
	}  else if (binop_is_conditional(token_op)) {
		auto lhs = convert_expression(binary->lhs);
		if (type_is_float(ty)) {
			new_value = irb->CreateFCmp(cmpop, lhs, rhs);
		} else {
			new_value = irb->CreateICmp(cmpop, lhs, rhs);
		}
    } else if (binop_is_logical(token_op)) {
		auto lhs = convert_expression(binary->lhs);
        BasicBlock *rhs_block = BasicBlock::Create(*llvm_context, "", current_function);
        BasicBlock *merge_block = BasicBlock::Create(*llvm_context, "", current_function);

        lhs = irb->CreateIsNotNull(lhs);

        if (token_op == Token::AND_AND) {
            irb->CreateCondBr(lhs, rhs_block, merge_block);
        } else {
            irb->CreateCondBr(lhs, merge_block, rhs_block);
        }

        BasicBlock *lhs_block = irb->GetInsertBlock();

    	irb->SetInsertPoint(rhs_block);
        rhs = irb->CreateIsNotNull(rhs);

        irb->CreateBr(merge_block);
        irb->SetInsertPoint(merge_block);

        PHINode *cmp = irb->CreatePHI(llvm_type, 2);
        cmp->addIncoming(lhs, lhs_block);
        cmp->addIncoming(rhs, rhs_block);

        return cmp;
    }

    return new_value;
}

Type *LLVM_Converter::convert_type(Ast_Type_Info *type_info) {
	if (type_info->type == Ast_Type_Info::POINTER) {
		if (type_info->element_type->type == Ast_Type_Info::VOID) {
			return type_i8->getPointerTo();
		}

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

	if (type_info->type == Ast_Type_Info::STRING) {
		return type_string;
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

	if (type_info->type == Ast_Type_Info::ARRAY) {
		auto element = convert_type(type_info->element_type);

		if (type_info->array_size >= 0) {
			return ArrayType::get(element, type_info->array_size);
		} else {
			auto data = element->getPointerTo();
			auto count = type_i64;

			if (type_info->is_dynamic) {
				return StructType::get(*llvm_context, {data, count, count});	
			} else {
				return StructType::get(*llvm_context, {data, count});	
			}
		}
	}

	assert(0 && "Should be unreachable");
	return 0;
}

void LLVM_Converter::convert_function(Ast_Function *fun) {
	auto fn = get_or_create_function(fun);
	current_function = fn;

	if (fun->flags & FUNCTION_EXTERNAL) return;

	BasicBlock *entry_block = BasicBlock::Create(*llvm_context, "", fn);
	irb->SetInsertPoint(entry_block);

	s64 i = 0;
	for (auto arg_it = fn->arg_begin(); arg_it != fn->arg_end(); arg_it++) {
		auto par = static_cast<Ast_Declaration *>(fun->parameter_scope->declarations[i]);
		
		Type *par_type = convert_type(par->type_info);
		Value *var = irb->CreateAlloca(par_type);
		par->llvm_reference = var;
		irb->CreateStore(&*arg_it, var);
		++i;
	}
	
	convert_scope(fun->block_scope);

	if (!irb->GetInsertBlock()->getTerminator()) {
		if (fun->return_type->type == Ast_Type_Info::VOID) {
			irb->CreateRetVoid();
		} else {
			compiler->report_error(fun, "Non-void function needs a return");
			return;
		}
	}
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

#if LLVM_VERSION_MAJOR==8
	auto file_type = llvm::TargetMachine::CGFT_ObjectFile;
#else
	auto file_type = llvm::CGFT_ObjectFile;
#endif

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

	if (!fn) {
        Array<Type *> arg_types;
        
        for (auto par : function->parameter_scope->declarations) {
            Type *par_type = convert_type(par->type_info);
            
            arg_types.add(par_type);
        }
        
        Type *return_type = convert_type(function->return_type);
        
        auto fn_type = FunctionType::get(return_type, ArrayRef<Type *>(arg_types.data, arg_types.length), function->flags & FUNCTION_VARARG);
        
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

Value *LLVM_Converter::gep(llvm::Value *ptr, ArrayRef<Value *> idx_list) {
	return irb->CreateInBoundsGEP(ptr->getType()->getPointerElementType(), ptr, idx_list);
}
