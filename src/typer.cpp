#include "compiler.h"
#include "typer.h"
#include "ast.h"
#include "common.h"

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
			case Ast::FUNCTION: {
				auto fun = static_cast<Ast_Function *>(decl);
				if (fun->is_template_function)
					break;

				type_check_function(fun);
			} break;
			case Ast::STRUCT: {
				auto strct = static_cast<Ast_Struct *>(decl);
				for (auto decl : strct->members) {
					type_check_variable_declaration(decl);
				}
			} break;
			default: break;
		}
	}

	for (auto stmt : scope->statements) {
		switch (stmt->type) {
			case Ast::RETURN: {
				Ast_Return *ret = static_cast<Ast_Return *>(stmt);

				if (ret->return_value) {
					infer_type(ret->return_value);
					ret->return_value = check_expression_type_and_cast(ret->return_value, current_function->return_type);

					if (!types_match(ret->return_value->type_info, current_function->return_type)) {
						compiler->report_error(ret, "Type of return value and return type of function do not match");
					}

				} else if (current_function->return_type->type != Ast_Type_Info::VOID) {
					compiler->report_error(ret, "Tried to return no value from non-void function");
				}
				break;
			}
		}
	}
}

void Typer::type_check_variable_declaration(Ast_Declaration *decl) {
	if (decl->type_info) {
		decl->type_info = resolve_type_info(decl->type_info);
		if (!decl->type_info) {
			compiler->report_error(decl->type_info->unresolved_name,
					"Can't resolve symbol '%s'",
					decl->type_info->unresolved_name->atom->id.data);
			return;
		}

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
	current_function = function;

	for (auto par : function->parameter_scope->declarations) {
		type_check_variable_declaration(static_cast<Ast_Declaration *>(par));
	}
    
    function->return_type = resolve_type_info(function->return_type);
    if (!function->return_type) {
        compiler->report_error(function, "Can't resolve return type of function");
        return;
    }

	function->linkage_name = mangle_name(function);

	type_check_scope(function->block_scope);
}

void Typer::infer_type(Ast_Expression *expression) {
	if (expression->type_info) return;

	switch (expression->type) {
		case Ast_Expression::LITERAL: {
			Ast_Literal *lit = static_cast<Ast_Literal *>(expression);
			switch (lit->literal_type) {
				case Ast_Literal::BOOL:
					lit->type_info = compiler->type_bool;
					break;
				case Ast_Literal::INT:
					lit->type_info = compiler->type_s64;
					break;
				case Ast_Literal::FLOAT:
					lit->type_info = compiler->type_f64;
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

			id->type_info = decl->type_info;
		} break;
		case Ast_Expression::CAST: {
			Ast_Cast *cast = static_cast<Ast_Cast *>(expression);
			cast->type_info = cast->target_type;
		} break;
		case Ast::CALL: {
			Ast_Call *call = static_cast<Ast_Call *>(expression);
			Ast_Expression *decl = find_declaration_by_id(call->identifier);

			if (!decl) {
				compiler->report_error(call->identifier, "Symbol not defined");
				return;
			}

			if (decl->type != Ast::FUNCTION) {
				compiler->report_error(call->identifier, "Symbol is not a function");
				return;
			}

			Ast_Function *function = static_cast<Ast_Function *>(decl);
			Ast_Type_Info *function_type = function->type_info;

			if (function->is_template_function) {
				function = get_polymorph_function(call, function);
				call->type_info = function->return_type;
				call->resolved_function = function;
			} else {
				call->type_info = function_type->return_type;
				call->resolved_function = function;

				for (int i = 0; i < function_type->parameters.length; ++i) {
					if (i >= call->arguments.length) {
						compiler->report_error(call->identifier, "Arguments count does not match parameter count");
						return;
					}

					infer_type(call->arguments[i]);

					Ast_Type_Info *par_type = function->parameter_scope->declarations[i]->type_info;

					call->arguments[i] = check_expression_type_and_cast(call->arguments[i], par_type);
					Ast_Type_Info *arg_type = call->arguments[i]->type_info;

					if (!types_match(arg_type, par_type)) {
						compiler->report_error(
								call->arguments[i],
								"Type of %d. parameter does not match type in function declaration",
								i
								);
						return;
					}
				}
			}
		} break;
	}
}

Ast_Type_Info *Typer::resolve_type_info(Ast_Type_Info *type_info) {
	if (type_info->type != Ast_Type_Info::UNINITIALIZED) return type_info;

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

Ast_Expression *Typer::check_expression_type_and_cast(Ast_Expression *expression, Ast_Type_Info *other_type) {
	auto rtype = expression->type_info;
	auto ltype = other_type;

    Ast_Expression *maybe_casted = expression;
    
    if (!types_match(ltype, rtype)) {
        if (type_is_int(ltype) && type_is_int(rtype) && (ltype->is_signed == rtype->is_signed)) {
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

Ast_Function *Typer::make_polymorph_function(Ast_Function *template_function, Array<Ast_Expression *> *arguments) {
	auto poly = compiler->copier->copy_function(template_function);
	poly->is_template_function = false;

	for (s64 i = 0; i < arguments->length; ++i) {
		auto par = poly->parameter_scope->declarations[i];
		auto arg = (*arguments)[i];

		auto par_type = par->type_info;
		auto arg_type = arg->type_info;

		if (!can_fill_polymorph(par_type, arg_type)) {
			break;
		}
	}

	for (auto decl : poly->template_scope->declarations) {
		auto alias = static_cast<Ast_Type_Alias *>(decl);
		if (alias->type_info->type == Ast_Type_Info::TYPE) {
			auto name = alias->identifier->atom->id;
			compiler->report_error(alias, "Failed to fill out type '%.*s'", name.length, name.data);
		}
	}

	return poly;
}

bool Typer::can_fill_polymorph(Ast_Type_Info *par_type, Ast_Type_Info *arg_type) {
	if (par_type->type == Ast_Type_Info::POINTER) {
        if (arg_type->type != Ast_Type_Info::POINTER) return false;
        
        return can_fill_polymorph(par_type->element_type, arg_type->element_type);
    }
    
    if (type_is_primitive(par_type)) {
        return types_match(par_type, arg_type);
    }
    
    if (par_type->type == Ast_Type_Info::UNINITIALIZED) {
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

    return false;
}

bool Typer::types_match(Ast_Type_Info *t1, Ast_Type_Info *t2) {
	if (t1->type != t2->type) return false;
    if (t1->size != t2->size) return false;
    
    if (t1->type == Ast_Type_Info::POINTER) {
        return types_match(t1->element_type, t2->element_type);
    }
    
    if (t1->type == Ast_Type_Info::STRUCT) {
    	return t1->struct_decl == t2->struct_decl;
    }
    
    return true;
}

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

String Typer::mangle_name(Ast_Function *decl) {
	String name = decl->identifier->atom->id;
	if (decl->identifier->atom == compiler->atom_main) return name;

	String_Builder sb;
    sb.print("%.*s", name.length, name.data);

    sb.putchar('_');

	Ast_Type_Info *func_type = decl->type_info;
	for (auto par_type : decl->parameter_scope->declarations) {
		mangle_type(&sb, par_type->type_info);
	}

	return sb.to_string();
}

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
		case Ast_Type_Info::STRUCT: {
			builder->putchar('S');

			for (auto mem : type->struct_members) {
				mangle_type(builder, mem);
			}
		} break;
		default: assert(0);
	}
}
