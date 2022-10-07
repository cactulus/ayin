#include "compiler.h"
#include "typer.h"
#include "ast.h"
#include "common.h"

static bool expr_is_targatable(Ast_Expression *expression);
static void copy_location_info(Ast *left, Ast *right);

Typer::Typer(Compiler *compiler) {
	this->compiler = compiler;
}

void Typer::type_check_scope(Ast_Scope *scope) {
	for (auto decl : scope->declarations) {
		switch (decl->type) {
			case Ast::DECLARATION: {
				auto var_decl = static_cast<Ast_Declaration *>(decl);
				type_check_variable_declaration(var_decl);
			} break;
			case Ast::TYPE_ALIAS: {
				break;
			}
			case Ast::ENUM: {
				break;
			}
			case Ast::FUNCTION: {
				auto fun = static_cast<Ast_Function *>(decl);

				if ((fun->flags & FUNCTION_EXTERNAL) && (fun->flags & FUNCTION_TEMPLATE)) {
					compiler->report_error(fun, "Function cannot be external and be a template function at the same time");
					break;
				}

				if ((fun->flags & FUNCTION_VARARG) && !(fun->flags & FUNCTION_EXTERNAL)) {
					compiler->report_error(fun, "Only external functions can be marked vararg");
					break;
				}

				if (fun->flags & FUNCTION_TEMPLATE)
					break;

				type_check_function(fun);
			} break;
			case Ast::STRUCT: {
				auto strct = static_cast<Ast_Struct *>(decl);
				for (auto decl : strct->members) {
					type_check_variable_declaration(decl);
				}
			} break;
			default:
				break;
		}
	}

	for (auto stmt : scope->statements) {
		type_check_statement(stmt);
	}
}

void Typer::type_check_statement(Ast_Expression *stmt) {
	if (!stmt) return;

	switch (stmt->type) {
		case Ast::RETURN: {
			Ast_Return *ret = static_cast<Ast_Return *>(stmt);

			if (ret->return_value) {
				infer_type(ret->return_value);
				ret->return_value = check_expression_type_and_cast(ret->return_value, current_function->return_type);

				if (!types_match(ret->return_value->type_info, current_function->return_type)) {
                    String ret_val_ty_str = type_to_string(ret->return_value->type_info);
                    String ret_ty_str = type_to_string(current_function->return_type);
                    
					compiler->report_error(ret, "Type of return value (%.*s) and return type of function (%.*s) do not match", ret_val_ty_str.length, ret_val_ty_str.data, ret_ty_str.length, ret_ty_str.data);
				}

			} else if (current_function->return_type->type != Ast_Type_Info::VOID) {
				compiler->report_error(ret, "Tried to return no value from non-void function");
			}
			break;
		}
		case Ast::SCOPE: {
			Ast_Scope *scope = static_cast<Ast_Scope *>(stmt);

			type_check_scope(scope);

			break;
		}
		case Ast::IF: {
			Ast_If *_if = static_cast<Ast_If *>(stmt);

			infer_type(_if->condition);
			type_check_statement(_if->then_statement);
			type_check_statement(_if->else_statement);

			break;
		}
		case Ast::WHILE: {
			Ast_While *_while = static_cast<Ast_While *>(stmt);

			infer_type(_while->condition);
			type_check_statement(_while->statement);

			break;
		}
		case Ast::FOR: {
			Ast_For *_for = static_cast<Ast_For*>(stmt);

			if (!_for->initial_iterator_expression) {
				compiler->report_error(_for, "'for' must be followed by an variable declaration.\n");
				return;
			}

			type_check_statement(_for->initial_iterator_expression);

			if (!_for->upper_range_expression) {
				auto init_type = _for->initial_iterator_expression->type_info;
				if (type_is_int(init_type)) {
					compiler->report_error(_for, "'for' must specify an upper-range. Ex: for 0..1\n");
					return;
				}
			} else {
				auto init_type = _for->initial_iterator_expression->type_info;
				if (!type_is_int(init_type)) {
					compiler->report_error(_for, "'..' operator may only be preceeded by an integer expression.\n");
					return;
				}

				infer_type(_for->upper_range_expression);

				auto init_expr = _for->initial_iterator_expression;
				auto upper_expr = _for->upper_range_expression;
			
				auto upper_type = _for->upper_range_expression->type_info;

				check_expression_type_and_cast(init_expr, upper_expr->type_info);
				
				init_type = _for->initial_iterator_expression->type_info;
				if (!type_is_int(upper_type)) {
					compiler->report_error(_for->upper_range_expression, "'for' upper-range must be an integer expression.\n");
					return;
				}

				if (!types_match(init_type, upper_type)) {
					compiler->report_error(_for, "'for' lower-range and upper-range types do not match!\n");
				}
			}

			auto init_type = _for->initial_iterator_expression->type_info;
			if (!type_is_int(init_type)) {
				bool supports_iteration_interface = type_is_array(init_type);

				if (!supports_iteration_interface) {
					compiler->report_error(_for->initial_iterator_expression, "Type of expression in 'for' condition is not iterable. Must be an integer range, a string or an array");
					return;
				}
			}

			if (!type_is_int(init_type)) {
				auto count_expr = make_member(_for->initial_iterator_expression, compiler->atom_length);
				infer_type(count_expr);

				auto zero = make_integer_literal(0, count_expr->type_info);
				auto it_index_ident = make_identifier(compiler->atom_it_index);
				copy_location_info(it_index_ident, _for);
				it_index_ident->scope = _for->iterator_declaration_scope;
				{
					Ast_Declaration *decl = new Ast_Declaration();
					copy_location_info(decl, _for);
					decl->identifier = it_index_ident;
					copy_location_info(decl->identifier, _for);
					decl->initializer = zero;

					_for->iterator_index_decl = decl;

					type_check_statement(_for->iterator_index_decl);
					if (compiler->errors_reported) return;
				}

				{
					auto indexed = make_index(_for->initial_iterator_expression, it_index_ident);

					Ast_Declaration *decl = new Ast_Declaration();
					copy_location_info(decl, _for);
					decl->identifier = make_identifier(compiler->atom_it);
					decl->identifier->scope = _for->iterator_declaration_scope;
					copy_location_info(decl->identifier, decl);
					decl->initializer = indexed;
					decl->type_info = indexed->type_info;

					_for->iterator_decl = decl;
				}

				assert(_for->upper_range_expression == nullptr);
				_for->upper_range_expression = count_expr;
			}

			if (_for->initial_iterator_expression->type == Ast_Expression::DECLARATION) {
				_for->iterator_decl = static_cast<Ast_Declaration *>(_for->initial_iterator_expression);
			} else if (!_for->iterator_decl) {
				// for integer ranges only
				Ast_Declaration *decl = new Ast_Declaration();
				copy_location_info(decl, _for);
				decl->identifier = make_identifier(compiler->atom_it);
				copy_location_info(decl->identifier, _for);
				decl->identifier->scope = _for->iterator_declaration_scope;
				decl->initializer = _for->initial_iterator_expression;

				_for->iterator_decl = decl;
			}

			_for->iterator_declaration_scope->declarations.add(_for->iterator_decl);
			if (_for->iterator_index_decl) _for->iterator_declaration_scope->declarations.add(_for->iterator_index_decl);

			type_check_statement(_for->iterator_decl);
			
			type_check_statement(_for->body);

			break;
		}
		case Ast::CONTINUE:
		case Ast::BREAK:
			break;
		default:
			infer_type(stmt);
			break;
	}
}

void Typer::type_check_variable_declaration(Ast_Declaration *decl) {
	Ast_Expression *existing = find_declaration_in_scope(decl->identifier->scope, decl->identifier);
	if (existing && existing != decl) {
		compiler->report_error2(decl->identifier->location, "Identifier is already defined", existing->location, "First definition here");
		return;
	}

	if (decl->type_info) {
        resolve_type_force(&decl->type_info);

		if (decl->initializer) {
			infer_type(decl->initializer);

			decl->initializer = check_expression_type_and_cast(decl->initializer, decl->type_info);

			if (!types_match(decl->type_info, decl->initializer->type_info)) {
				compiler->report_error(decl->initializer, "Type of initialization value and type of variable do not match");
			}
		}
	} else {
		assert(decl->initializer);
        
        infer_type(decl->initializer);
		decl->type_info = decl->initializer->type_info;
	}
}

void Typer::type_check_function(Ast_Function *function) {
	Ast_Function *old_current_function = current_function;
	current_function = function;

	for (auto par : function->parameter_scope->declarations) {
		type_check_variable_declaration(static_cast<Ast_Declaration *>(par));
	}
    
    function->return_type = resolve_type_info(function->return_type);
    if (!function->return_type) {
        compiler->report_error(function, "Can't resolve return type of function");
        return;
    }
    
    resolve_type_force(&function->type_info);

    if (compiler->errors_reported) return;

	function->linkage_name = mangle_name(function);

	if (!(function->flags & FUNCTION_EXTERNAL)) {
		type_check_scope(function->block_scope);
	}

	current_function = old_current_function;
}

void Typer::infer_type(Ast_Expression *expression) {
	while(expression->substitution) expression = expression->substitution;
	if (expression->type_info) return;

	switch (expression->type) {
		case Ast_Expression::DECLARATION: {
			auto var_decl = static_cast<Ast_Declaration *>(expression);
			type_check_variable_declaration(var_decl);
		} break;
		case Ast_Expression::LITERAL: {
			Ast_Literal *lit = static_cast<Ast_Literal *>(expression);
			switch (lit->literal_type) {
				case Ast_Literal::BOOL:
					lit->type_info = compiler->type_bool;
					break;
				case Ast_Literal::INT:
					lit->type_info = compiler->type_s32;
					break;
				case Ast_Literal::FLOAT:
					lit->type_info = compiler->type_f32;
					break;
				case Ast_Literal::STRING:
					lit->type_info = compiler->type_string;
					break;
				case Ast_Literal::NIL:
					lit->type_info = compiler->type_void_ptr;
					break;
			}
		} break;
		case Ast_Expression::IDENTIFIER: {
			Ast_Identifier *id = static_cast<Ast_Identifier *>(expression);
			Ast_Expression *decl = find_declaration_by_id(id);

			if (!decl) {
				compiler->report_error(id, "Variable is not defined");
				return;
			}

			if (decl->type == Ast_Expression::FUNCTION) {
				id->type_info = decl->type_info;
			} else {
				id->type_info = decl->type_info;
			}
		} break;
		case Ast_Expression::CAST: {
			Ast_Cast *cast = static_cast<Ast_Cast *>(expression);
			infer_type(cast->expression);
			cast->type_info = cast->target_type;

            resolve_type_force(&cast->type_info);
		} break;
		case Ast::CALL: {
			Ast_Call *call = static_cast<Ast_Call *>(expression);
			Ast_Expression *decl = find_declaration_by_id(call->identifier);

			if (!decl) {
				compiler->report_error(call->identifier, "Symbol not defined");
				return;
			}

			Ast_Function *function;

			if (decl->type == Ast::FUNCTION) {
				function = static_cast<Ast_Function *>(decl);
			} else {
				infer_type(call->identifier);

				auto cit = call->identifier->type_info;

				if (type_is_function(cit)) {
					call->by_function_pointer = true;
				} else {
					compiler->report_error(call->identifier, "Symbol is not a function");
					return;
				}
			}

			if (call->by_function_pointer) {
				auto cit = call->identifier->type_info;

				/* TODO: allow template functions and varargs for function pointers */
				call->type_info = cit->return_type;

				if (!compare_arguments(call->identifier, &call->arguments, cit->parameters, false)) {
					return;
				}
			} else {
				if (function->flags & FUNCTION_TEMPLATE) {
					function = get_polymorph_function(call, function);
					call->type_info = function->return_type;
					call->resolved_function = function;
				} else {
					call->type_info = function->return_type;
					call->resolved_function = function;

					if (!compare_arguments(call->identifier, &call->arguments, function->type_info->parameters, function->flags & FUNCTION_VARARG)) {
						return;
					}
				}
			}
		} break;
		case Ast_Expression::BINARY: {
			Ast_Binary *binary = static_cast<Ast_Binary *>(expression);
			infer_type(binary->lhs);
			infer_type(binary->rhs);

        	auto lhs_type = binary->lhs->type_info;
        	auto rhs_type = binary->rhs->type_info;
        	auto eval_type = lhs_type;

			if (binop_is_conditional(binary->op)) {
				eval_type = compiler->type_bool;
			}

			if (binop_is_logical(binary->op)) {
				eval_type = compiler->type_bool;

				if (!type_is_bool(lhs_type)) {
					binary->lhs = make_compare_zero(binary->lhs);
				}

				if (!type_is_bool(rhs_type)) {
					binary->rhs = make_compare_zero(binary->rhs);
				}
			}

			if (binop_is_assign(binary->op)) {
				if (!expr_is_targatable(binary->lhs)) {
					compiler->report_error(binary, "Can't assign value to this expression");
				}

				if (binary->lhs->type == Ast_Expression::IDENTIFIER) {
					Ast_Identifier *id = static_cast<Ast_Identifier *>(binary->lhs);
					Ast_Expression *var_expr = find_declaration_by_id(id);

					if (var_expr->type != Ast::DECLARATION) {
						compiler->report_error(id, "Expected name of variable - not of struct or function");
						return;
					}

					Ast_Declaration *decl = static_cast<Ast_Declaration *>(var_expr);
					if (decl->flags & VAR_CONSTANT) {
						compiler->report_error(binary->lhs, "Can't assign value to constant variable");
					}
				}
			}

			if (binop_is_binary(binary->op)) {
				if (!(type_is_pointer(lhs_type) && type_is_int(rhs_type)) &&
					(!type_is_int(lhs_type) && !type_is_int(rhs_type)) &&
					(!type_is_float(lhs_type) && !type_is_float(rhs_type))) {
					/* TODO: check whether this if even makes sense */
					if (type_is_pointer(lhs_type) && (binary->op != '+' && binary->op != '-')) {
						compiler->report_error(binary, "Illegal binary operator for pointer type");
					}
					compiler->report_error(binary, "Illegal type for binary expression");
				}
			}

			/* TODO: check bug where eval_type is not set to bool and types get casted
			 * is eval_type then outdated? */
			binary->rhs = check_expression_type_and_cast(binary->rhs, binary->lhs->type_info);
			if (!types_match(binary->rhs->type_info, binary->lhs->type_info)) {
				binary->lhs = check_expression_type_and_cast(binary->lhs, binary->rhs->type_info);
				if (!types_match(binary->rhs->type_info, binary->lhs->type_info)) {
					compiler->report_error(binary, "Lhs and Rhs of binary expression are of different types");
				}
			}

			binary->type_info = eval_type;
		} break;
		case Ast_Expression::UNARY: {
			Ast_Unary *unary = static_cast<Ast_Unary *>(expression);
			infer_type(unary->target);

			auto target_type = unary->target->type_info;

			switch (unary->op) {
				case Token::PLUS_PLUS:
				case Token::MINUS_MINUS:
					if (!type_is_int(target_type)) {
						compiler->report_error(unary, "Can't use ++ or -- on non-integer expression");
						break;
					}
					break;
				case '!':
					if (!type_is_bool(target_type)) {
						unary->target = make_compare_zero(unary->target);
					}
					target_type = compiler->type_bool;
					break;
				case '*': {
					if (!type_is_pointer(target_type)) {
						compiler->report_error(unary->target, "Can't dereference non variable");
						break;
					}

					target_type = target_type->element_type;
				} break;
				case '&':
					if (!expr_is_targatable(unary->target)) {
						compiler->report_error(unary->target, "Can't reference non variable");
						break;
					}

					Ast_Type_Info *new_ty = new Ast_Type_Info();

					new_ty->type = Ast_Type_Info::POINTER;
					new_ty->element_type = target_type;

					target_type = new_ty;
					break;
			}

			unary->type_info = target_type;
		} break;
		case Ast_Expression::SIZEOF: {
			Ast_Sizeof *size = static_cast<Ast_Sizeof *>(expression);
			size->type_info = compiler->type_s32;

            resolve_type_force(&size->target_type);
		} break;
		case Ast_Expression::INDEX: {
			Ast_Index *index = static_cast<Ast_Index *>(expression);

			infer_type(index->expression);
			infer_type(index->index);

			auto expr_type = index->expression->type_info;

			if (!type_is_array(expr_type) && !type_is_string(expr_type) && !type_is_pointer(expr_type)) {
				compiler->report_error(index->index, "Cannot index dereference type that is not a string, array or pointer");
				break;
			}

			if (!type_is_int(index->index->type_info)) {
				compiler->report_error(index->index, "Expected an integer type as index to array");
				break;
			}

			if (type_is_string(expr_type)) {
				index->type_info = compiler->type_u8;
			} else {
				index->type_info = expr_type->element_type;
			}
		} break;
		case Ast_Expression::MEMBER: {
			Ast_Member *member = static_cast<Ast_Member *>(expression);

			infer_type(member->left);

			if (type_is_pointer(member->left->type_info)) {
				auto un = new Ast_Unary();
				un->op = '*';
				un->target = member->left;
				copy_location_info(un, member->left);

				infer_type(un);

                member->left = un;
            }

            Atom *field_atom = member->field->atom;
			Ast_Type_Info *left_type = member->left->type_info;

			if (type_is_array(left_type)) {
 				if (left_type->array_size == -1) {
                    if (field_atom == compiler->atom_data) {
                        member->field_index = 0;
                        member->type_info = make_pointer_type(left_type->element_type);
                    } else if (field_atom == compiler->atom_length) {
                        member->field_index = 1;
                        member->type_info = compiler->type_s64;
                    } else if (left_type->is_dynamic && field_atom == compiler->atom_capacity) {
                        member->field_index = 2;
                        member->type_info = compiler->type_s64;
                    } else {
                        String field_name = field_atom->id;
                        compiler->report_error(member, "No member '%.*s' in type array.\n", field_name.length, field_name.data);
                    }
                } else {
                    if (field_atom == compiler->atom_data) {
                        auto index_lit = make_integer_literal(0, compiler->type_s64);
                        copy_location_info(index_lit, member);
                        
                        Ast_Index *index = new Ast_Index();
						index->expression = member->left;
						index->index = index_lit;
                        copy_location_info(index, member);
                        
                        Ast_Unary *addr = new Ast_Unary();
						addr->op = '*';
						addr->target = index;
                        copy_location_info(addr, member);
                        
                        infer_type(addr);
                        member->substitution = addr;
						member->type_info = compiler->type_s64;
                    } else if (field_atom == compiler->atom_length) {
                        auto lit = make_integer_literal(left_type->array_size, compiler->type_s64);
                        copy_location_info(lit, member);
                        member->substitution = lit;
						member->type_info = compiler->type_s64;
                    } else {
                        String field_name = field_atom->id;
                        compiler->report_error(member, "No member '%.*s' in known-size array.\n", field_name.length, field_name.data);
                    }
                }
			} else if (type_is_struct(left_type)) {
				bool found = false;
				auto strct = left_type->struct_decl;
				int index = 0;

                for (auto mem : strct->members) {
                    if (mem->identifier->atom == field_atom) {
                        found = true;
                        
                        member->field_index = index;
                        member->type_info = mem->type_info;
                        break;
                    }
                    index++;
                }
                
                if (!found) {
                    String field_name = field_atom->id;
                    String name = left_type->struct_decl->identifier->atom->id;
                    compiler->report_error(member, "No member '%.*s' in struct %.*s.\n", field_name.length, field_name.data, name.length, name.data);
                }
			} else if (type_is_string(left_type)) {
				if (field_atom == compiler->atom_data) {
					member->field_index= 0;
					member->type_info = compiler->type_string_data;
				} else if (field_atom == compiler->atom_length) {
					member->field_index = 1;
					member->type_info = compiler->type_s64;
				} else {
					String field_name = field_atom->id;
					compiler->report_error(member, "No member '%.*s' in type string.\n", field_name.length, field_name.data);
				}
			} else if (type_is_enum(left_type)) {
                bool found = false;
				for (auto enum_mem : left_type->enum_members) {
					if (field_atom == enum_mem.name) {
						if (enum_mem.value) {
							infer_type(enum_mem.value);
							member->substitution = enum_mem.value;
                            member->type_info = enum_mem.value->type_info;
						} else {
							Ast_Literal *enum_index = new Ast_Literal();
							copy_location_info(enum_index, member);

							enum_index->type_info = compiler->type_s32;
							enum_index->literal_type = Ast_Literal::INT;
							enum_index->int_value = enum_mem.index;

							member->substitution = enum_index;
                            member->type_info = enum_index->type_info;
						}
                        found = true;
					}
				}
                
                if (!found) {
                    compiler->report_error(member->field, "Enum member not found");
                }
			} else {
				compiler->report_error(member, "Tried to access type that is not a string, array or struct");
				return;
			}
		} break;
	}
}

/*
* resolves the types that could not be resolved during the parsing stage
* (mainly user defined types such as structs)
*/
Ast_Type_Info *Typer::resolve_type_info(Ast_Type_Info *type_info) {
	if (type_is_pointer(type_info) || type_is_array(type_info)) {
		Ast_Type_Info *current = type_info;

		while (type_is_pointer(current) || type_is_array(current)) {
			if (current->element_type->type != Ast_Type_Info::UNRESOLVED) {
				current = current->element_type;
				continue;
			}

            resolve_type_force(&current->element_type);
		}

		return type_info;
	}

	if (type_is_function(type_info)) {
		for (int i = 0; i < type_info->parameters.length; ++i) {
            resolve_type_force(&type_info->parameters[i]);
		}

        resolve_type_force(&type_info->return_type);
		return type_info;
	}

	if (type_info->type != Ast_Type_Info::UNRESOLVED) return type_info;

	Ast_Expression *decl = find_declaration_by_id(type_info->unresolved_name);
	if (!decl) {
		return 0;
	}

	switch (decl->type) {
		case Ast::STRUCT: {
			auto strct = static_cast<Ast_Struct *>(decl);

			Ast_Type_Info *struct_type = strct->type_info;
			struct_type->struct_decl = strct;

			return struct_type;
		} break;
		case Ast::TYPE_ALIAS: {
			auto ta = static_cast<Ast_Type_Alias *>(decl);
			if (ta->type_info) {
                resolve_type_force(&ta->type_info);
				return ta->type_info;
			}
			return 0;
		} break;
		case Ast::DECLARATION: {
			compiler->report_error(type_info->unresolved_name, "Illegal type specifier: variable");
		} break;
		case Ast::FUNCTION: {
			compiler->report_error(type_info->unresolved_name, "Illegal type specifier: function");
		} break;
		case Ast::ENUM: {
			compiler->report_error(type_info->unresolved_name, "Illegal type specifier: enum");
		} break;
	}

	return type_info;
}


void Typer::resolve_type_force(Ast_Type_Info **type_info) {
    Ast_Type_Info *new_type = resolve_type_info(*type_info);
    
    if (new_type) {
        *type_info = new_type;
    } else {
        auto name = (*type_info)->unresolved_name;
        compiler->report_error(name,
            "Can't resolve symbol '%.*s'",
            name->atom->id.length, name->atom->id.data);
    }
}

/*
* compares the types of the arguments of a function call to the types in the function declaration
* returns true if they are compatible (with implicit type conversion)
* returns false if they are incompatible
*/
bool Typer::compare_arguments(Ast_Identifier *call, Array<Ast_Expression *> *args, Array<Ast_Type_Info *> par_types, bool varags) {
	auto par_count = par_types.length;

	if (par_count > args->length) {
		compiler->report_error(call, "Arguments count does not match parameter count");
		return false;
	}

	for (int i = 0; i < args->length; ++i) {
		if (i >= par_count) {
			if (varags) {
				infer_type((*args)[i]);
				continue;
			} else {
				compiler->report_error(call, "Arguments count does not match parameter count");
				return false;
			}
		}

		infer_type((*args)[i]);

		Ast_Type_Info *par_type = par_types[i];

		(*args)[i] = check_expression_type_and_cast((*args)[i], par_type);
		Ast_Type_Info *arg_type = (*args)[i]->type_info;

		if (!types_match(arg_type, par_type)) {
            String par_ty_str = type_to_string(par_type);
            String arg_ty_str = type_to_string(arg_type);
            
			compiler->report_error(
				(*args)[i],
				"Type of %d. argument (%.*s) does not match type in function declaration (%.*s)",
				i + 1,
                arg_ty_str.length, arg_ty_str.data,
                par_ty_str.length, par_ty_str.data
			);
			return false;
		}
	}

	return true;
}

/*
* compares the type of an expression to a desired type
* If they don't match, it tries implicit type conversion
* It returns the original expression if the types match
* or the Ast_Cast expression if it converted the type implictly

* This function does not guarantee that types are compatible even after implicit type conversion
* Is is the callers responsibility to check if the types match in the end
*/
Ast_Expression *Typer::check_expression_type_and_cast(Ast_Expression *expression, Ast_Type_Info *other_type) {
	while(expression->substitution) expression = expression->substitution;

	auto rtype = expression->type_info;
	auto ltype = other_type;

    Ast_Expression *maybe_casted = expression;
    
    if (!types_match(ltype, rtype)) {
        if (type_is_int(ltype) && type_is_int(rtype)) {
            maybe_casted = make_cast(maybe_casted, ltype);
        } else if (type_is_float(ltype) && type_is_float(rtype)) {
            maybe_casted = make_cast(maybe_casted, ltype);
        } else if (type_is_float(ltype) && type_is_int(rtype)) {
            maybe_casted = make_cast(maybe_casted, ltype);
        } else if (type_is_pointer(ltype) && type_is_pointer(rtype)) {
            if (type_points_to_void(ltype)) {
                auto llevel = get_pointer_level(ltype);
                auto rlevel = get_pointer_level(rtype);
                
                if (llevel == rlevel) {
                    maybe_casted = make_cast(maybe_casted, ltype);
                }
            }
        }
    }
    
    return maybe_casted;
}

Ast_Cast *Typer::make_cast(Ast_Expression *expression, Ast_Type_Info *target_type) {
	Ast_Cast *cast = new Ast_Cast();

	cast->location = expression->location;
	cast->expression = expression;
	cast->target_type = target_type;
	cast->type_info = target_type;

	return cast;
}

/*
* Checks if a template functions has already been polymorphed with the argument types of the function call
* If there is already one, return it
* otherwise it creates a new polymorph function, type checks it and returns it
*/
Ast_Function *Typer::get_polymorph_function(Ast_Call *call, Ast_Function *template_function) {
	for (auto arg : call->arguments) {
		infer_type(arg);
	}

	for (auto overload : template_function->polymorphed_overloads) {
		bool does_match = true;

		if (call->arguments.length != template_function->parameter_scope->declarations.length) {
			compiler->report_error(call, "Argument count does not match parameter count");
			return template_function;
		}

		for (s64 i = 0; i < call->arguments.length; ++i) {
			auto par_type = overload->parameter_scope->declarations[i]->type_info;
			auto arg_type = call->arguments[i]->type_info;

			if (!types_match(par_type, arg_type)) {
				does_match = false;
			}
		}

		if (does_match) {
			return overload;
		}
	}

	auto polymorph = make_polymorph_function(template_function, &call->arguments);
	if (polymorph) {
		type_check_function(polymorph);
		template_function->polymorphed_overloads.add(polymorph);
	}

	return polymorph;
}

/*
* Creates the polymorphed function from a template function
* Resolves the generic type aliases
*/
Ast_Function *Typer::make_polymorph_function(Ast_Function *template_function, Array<Ast_Expression *> *arguments) {
	auto poly = compiler->copier->copy_function(template_function);
	poly->flags &= ~FUNCTION_TEMPLATE;

	for (s64 i = 0; i < arguments->length; ++i) {
		auto par = poly->parameter_scope->declarations[i];
		auto arg = (*arguments)[i];

		auto par_type = par->type_info;
		auto arg_type = arg->type_info;

		if (!can_fill_polymorph(par_type, arg_type)) {
			break;
		}
	}

	if (compiler->errors_reported)
		return poly;

	/* find bug where this triggers even though it shouldnt. For example calling array_reserve from array_add */
	for (auto decl : poly->template_scope->declarations) {
		auto alias = static_cast<Ast_Type_Alias *>(decl);
		if (alias->type_info->type == Ast_Type_Info::TYPE) {
			auto name = alias->identifier->atom->id;
			
		//	compiler->report_error(alias, "Failed to fill out type '%.*s'", name.length, name.data);
		}
	}

	return poly;
}

/*
* Checks if the type of an argument matches the type of the parameter
* If parameter is a generic type alias
* check if it is already filled in
*		if so     -> compare already filled in type with argument type
*		otherwise -> fill in type with argument type
*/
bool Typer::can_fill_polymorph(Ast_Type_Info *par_type, Ast_Type_Info *arg_type) {
	if (par_type->type == Ast_Type_Info::POINTER) {
        if (arg_type->type != Ast_Type_Info::POINTER) return false;
        
        return can_fill_polymorph(par_type->element_type, arg_type->element_type);
    }
    
    if (type_is_primitive(par_type)) {
        return types_match(par_type, arg_type);
    }
    
    if (par_type->type == Ast_Type_Info::UNRESOLVED) {
        auto decl = find_declaration_by_id(par_type->unresolved_name);
        if (!decl) {
            compiler->report_error(par_type->unresolved_name, "Undeclared identifier.\n");
            return false;
        }
        
        if (compiler->errors_reported) return false;
        
        if (decl->type == Ast::TYPE_ALIAS) {
            auto alias = static_cast<Ast_Type_Alias *>(decl);
            if (alias->type_info->type == Ast_Type_Info::TYPE) {
                alias->type_info = arg_type;
                return true;
            } else {
                return types_match(alias->type_info, arg_type);
            }
        } else if (decl->type == Ast::STRUCT) {
            auto _struct = static_cast<Ast_Struct *>(decl);
            return types_match(par_type, arg_type);
        } else {
            assert(false);
        }
    }

	if (par_type->type == Ast_Type_Info::ARRAY) {
		if (arg_type->type != Ast_Type_Info::ARRAY) return false;

		bool success = can_fill_polymorph(par_type->element_type, arg_type->element_type);
		if (compiler->errors_reported) return false;

		return success;	
	}

    return false;
}

/* checks if two types match */
bool Typer::types_match(Ast_Type_Info *t1, Ast_Type_Info *t2) {
	if (t1->type != t2->type) return false;
    
    if (t1->type == Ast_Type_Info::POINTER) {
        return types_match(t1->element_type, t2->element_type);
    }
    
    if (t1->type == Ast_Type_Info::STRUCT) {
    	return t1->struct_decl == t2->struct_decl;
    }

    if (t1->type == Ast_Type_Info::ARRAY) {
    	return types_match(t1->element_type, t2->element_type) &&
    		t1->array_size == t2->array_size &&
    		t1->is_dynamic == t2->is_dynamic;
    }

	if (t1->type == Ast_Type_Info::FUNCTION) {
		if (!types_match(t1->return_type, t2->return_type)) return false;
		if (t1->parameters.length != t2->parameters.length) return false;

		for (int i = 0; i < t1->parameters.length; ++i) {
			if (!types_match(t1->parameters[i], t2->parameters[i])) {
				return false;
			}
		}

		return true;
	}
    
    return t1->size == t2->size;
}

/*
* gets the depth a pointer type
* for example:
*		*u32   -> level 1
*		**u32  -> level 2
*		***u32 -> level 3
*/
s32 Typer::get_pointer_level(Ast_Type_Info *type_info) {
    s32 count = 0;
    
    while (type_info) {
        if (type_info->type == Ast_Type_Info::POINTER) {
            type_info = type_info->element_type;
            count++;
        } else {
            break;
        }
    }
    
    return count;
}

/*
* checks if a pointer type at some level points to a void
* used for void pointer coercion
*/
bool Typer::type_points_to_void(Ast_Type_Info *type_info) {
    while (type_info) {
        if (type_info->type == Ast_Type_Info::POINTER) {
            type_info = type_info->element_type;
        } else {
            return type_info->type == Ast_Type_Info::VOID;
        }
    }
    
    return false;
}

/*
* mangles function name
* if it is an external function or the main function -> return original name
* otherwise mangle name in the format:
*			NAME_PARAMETERTYPES
* 
* for example: func add(a: s32, b: s32) s32
* becomes:     add_s32s32
*/
String Typer::mangle_name(Ast_Function *decl) {
	String name = decl->identifier->atom->id;

	if ((decl->flags & FUNCTION_EXTERNAL) || decl->identifier->atom == compiler->atom_main)
		return name;

	String_Builder sb;
    sb.print("%.*s", name.length, name.data);

    sb.putchar('_');

	Ast_Type_Info *func_type = decl->type_info;
	for (auto par_type : decl->parameter_scope->declarations) {
		mangle_type(&sb, par_type->type_info);
	}

	return sb.to_string();
}

/*
* mangle type
* pointer        -> p$ELEMENT-TYPE
* void           -> v
* bool           -> b
* signed int     -> s$BYTE_SIZE0
* unsigned int   -> u$BYTE_SIZE
* float          -> f$BYTE_SIZE
* string         -> s
* struct	     -> S_$MEMBER-TYPES
* function	     -> F$RETURN-TYPE_$PARAMETER-TYPES
* dynamic array  -> Ad_$ELEMENT-TYPE_
* static array   -> As_$ELEMENT-TYPE_
* constant array -> Ak$SIZE_$ELEMENT-TYPE_
*/
void Typer::mangle_type(String_Builder *builder, Ast_Type_Info *type) {
	switch (type->type) {
		case Ast_Type_Info::POINTER: {
			builder->putchar('p');
			mangle_type(builder, type->element_type);
		} break;
		case Ast_Type_Info::VOID: {
			builder->putchar('v');
		} break;
		case Ast_Type_Info::BOOL: {
			builder->putchar('b');
		} break;
		case Ast_Type_Info::INT: {
			if (type->is_signed) builder->putchar('s');
			else builder->putchar('u');

			builder->print("%d", type->size * 8);
		} break;
		case Ast_Type_Info::FLOAT: {
			builder->putchar('f');
			builder->print("%d", type->size * 8);
		} break;
		case Ast_Type_Info::STRING: {
			builder->putchar('s');
		} break;
		case Ast_Type_Info::STRUCT: {
			builder->putchar('S');

			for (auto mem : type->struct_members) {
				mangle_type(builder, mem);
			}
		} break;
		case Ast_Type_Info::FUNCTION: {
			builder->putchar('F');
			mangle_type(builder, type->return_type);

			builder->putchar('_');
			for (auto par : type->parameters) {
				mangle_type(builder, par);
			}
		} break;
		case Ast_Type_Info::ARRAY: {
			builder->putchar('A');

			if (type->array_size >= 0) {
				builder->putchar('k');
				builder->print("%d", type->array_size);
			} else if (type->is_dynamic) {
				builder->putchar('d');
			} else {
				builder->putchar('s');
			}
			
			builder->putchar('_');
			mangle_type(builder, type->element_type);
			builder->putchar('_');
		} break;
		default: assert(0);
	}
}

/*
* creates an expression that compares the target expression to is not zero
* used when a non-conditional expression is used as a conditional expression
* for example:
* if 5 {} -> 5 becomes (5 != 0)
*/
Ast_Expression *Typer::make_compare_zero(Ast_Expression *target) {
	infer_type(target);

	Ast_Type_Info *target_type = target->type_info;

	Ast_Literal *lit = new Ast_Literal();
	lit->location = target->location;
	
	switch (target_type->type) {
		case Ast_Type_Info::INT:
		case Ast_Type_Info::FLOAT:
		case Ast_Type_Info::BOOL:
			break;
		default:
			compiler->report_error(target, "Expression has to be of type int, bool or float for auto compare zero");
	}

	Ast_Binary *be = new Ast_Binary();
	be->location = target->location;
	be->lhs = target;
	be->rhs = lit;
	be->op = Token::NOT_EQ;
	be->type_info = compiler->type_bool;
	return be;
}

Ast_Expression *Typer::find_declaration_in_scope(Ast_Scope *scope, Ast_Identifier *id) {
	Atom *name = id->atom;

	for (auto decl : scope->declarations) {
		switch (decl->type) {
		case Ast::STRUCT: {
			auto strct = static_cast<Ast_Struct *>(decl);
			if (strct->identifier->atom == name)
				return decl;
		} break;
		case Ast::TYPE_ALIAS: {
			auto ta = static_cast<Ast_Type_Alias *>(decl);
			if (ta->identifier->atom == name)
				return decl;
		} break;
		case Ast::DECLARATION: {
			auto var_decl = static_cast<Ast_Declaration *>(decl);
			if (var_decl->identifier->atom == name)
				return decl;
		} break;
		case Ast::FUNCTION: {
			auto fun = static_cast<Ast_Function *>(decl);
			if (fun->identifier->atom == name)
				return decl;
		} break;
		case Ast::ENUM: {
			auto e = static_cast<Ast_Enum *>(decl);
			if (e->identifier->atom == name)
				return decl;
		} break;
		}
	}

	return 0;
}

Ast_Literal *Typer::make_integer_literal(s64 value, Ast_Type_Info *type_info, Ast *source_loc) {
    Ast_Literal *lit = new Ast_Literal();
    lit->literal_type = Ast_Literal::INT;
    lit->int_value = value;
    lit->type_info = type_info;
    
    if (source_loc) copy_location_info(lit, source_loc);
    return lit;
}

Ast_Identifier *Typer::make_identifier(Atom *name) {
	Ast_Identifier *ident = new Ast_Identifier();
	ident->atom = name;
	return ident;
}

Ast_Member *Typer::make_member(Ast_Expression *aggregate_expression, Atom *field) {
	auto ident = make_identifier(field);
	copy_location_info(ident, aggregate_expression);

	Ast_Member *mem = new Ast_Member();
	copy_location_info(mem, aggregate_expression);
	mem->left = aggregate_expression;
	mem->field = ident;
	return mem;
}

Ast_Index *Typer::make_index(Ast_Expression *array, Ast_Expression *index) {
	Ast_Index *indx = new Ast_Index();
	indx->expression = array;
	indx->index = index;
	return indx;
}

bool expr_is_targatable(Ast_Expression *expression) {
	switch (expression->type) {
	case Ast_Expression::IDENTIFIER:
	case Ast_Expression::MEMBER:
	case Ast_Expression::INDEX:
		return true;
	default:
		return false;
	}
}

void copy_location_info(Ast *left, Ast *right) {
    left->location = right->location;
}
