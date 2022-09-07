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

				Ast_Type_Info *function_type = current_function->type_info;

				if (ret->return_value) {
					infer_type(ret->return_value);
					ret->return_value = check_expression_type_and_cast(ret->return_value, function_type->return_type);

					if (!types_match(ret->return_value->inferred_type, function_type->return_type)) {
						compiler->report_error(ret, "Type of return value and return type of function do not match");
					}

				} else if (function_type->return_type->type != Ast_Type_Info::VOID) {
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

			if (!types_match(decl->type_info, decl->initializer->inferred_type)) {
				compiler->report_error(decl->initializer, "Type of initialization value and type of variable do not match");
			}
		}
	} else {
		assert(decl->initializer);

		infer_type(decl->initializer);
		decl->type_info = decl->initializer->inferred_type;
	}
}

void Typer::type_check_function(Ast_Function *function) {
	current_function = function;
	function->linkage_name = mangle_name(function);

	for (auto par : function->parameters) {
		type_check_variable_declaration(par);
	}

	type_check_scope(function->block_scope);
}

void Typer::infer_type(Ast_Expression *expression) {
	if (expression->inferred_type) return;

	switch (expression->type) {
		case Ast_Expression::LITERAL: {
			Ast_Literal *lit = static_cast<Ast_Literal *>(expression);
			switch (lit->literal_type) {
				case Ast_Literal::BOOL:
					lit->inferred_type = compiler->type_bool;
					break;
				case Ast_Literal::INT:
					lit->inferred_type = compiler->type_s64;
					break;
				case Ast_Literal::FLOAT:
					lit->inferred_type = compiler->type_f64;
					break;
			}
		} break;
		case Ast_Expression::IDENTIFIER: {
			Ast_Identifier *id = static_cast<Ast_Identifier *>(expression);
			Ast_Statement *decl = find_declaration_by_id(id);

			if (!decl) {
				compiler->report_error(id, "Variable is not defined");
				return;
			}

			id->inferred_type = decl->type_info;
			id->resolved_declaration = decl;
		} break;
		case Ast_Expression::CAST: {
			Ast_Cast *cast = static_cast<Ast_Cast *>(expression);
			cast->inferred_type = cast->target_type;
		} break;
		case Ast::CALL: {
			Ast_Call *call = static_cast<Ast_Call *>(expression);
			Ast_Statement *decl = find_declaration_by_id(call->identifier);

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
				call->inferred_type = function->type_info->return_type;
				call->resolved_function = function;
			} else {
				call->inferred_type = function_type->return_type;
				call->resolved_function = function;

				for (int i = 0; i < function_type->parameters.length; ++i) {
					if (i >= call->arguments.length) {
						compiler->report_error(call->identifier, "Arguments count does not match parameter count");
						return;
					}

					infer_type(call->arguments[i]);

					Ast_Type_Info *par_type = function->parameters[i]->type_info;

					call->arguments[i] = check_expression_type_and_cast(call->arguments[i], par_type);
					Ast_Type_Info *arg_type = call->arguments[i]->inferred_type;

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

	Ast_Statement *decl = find_declaration_by_id(type_info->unresolved_name);
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
				if (ta->type_info->element_type) {
					return ta->type_info->element_type;
				}
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
	auto rtype = expression->inferred_type;
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
	cast->inferred_type = target_type;

	return cast;
}

Ast_Function *Typer::get_polymorph_function(Ast_Call *call, Ast_Function *template_function) {
	return 0;
}

Ast_Statement *Typer::find_declaration_by_id(Ast_Identifier *id) {
	return find_declaration_by_name(id->atom, id->scope);
}

Ast_Statement *Typer::find_declaration_by_name(Atom *name, Ast_Scope *scope) {
	Ast_Scope *temp = scope;

	while (true) {
		for (auto decl : temp->declarations) {
			switch (decl->type) {
				case Ast::STRUCT: {
					auto strct = static_cast<Ast_Struct *>(decl);
					if (strct->identifier->atom->hash == name->hash)
						return decl;
				} break;
				case Ast::TYPE_ALIAS: {
					auto ta = static_cast<Ast_Type_Alias *>(decl);
					if (ta->identifier->atom->hash == name->hash)
						return decl;
				} break;
				case Ast::DECLARATION: {
					auto var_decl = static_cast<Ast_Declaration *>(decl);
					if (var_decl->identifier->atom->hash == name->hash)
						return decl;
				} break;
				case Ast::FUNCTION: {
					auto fun = static_cast<Ast_Function *>(decl);
					if (fun->identifier->atom->hash == name->hash)
						return decl;
				} break;
				case Ast::ENUM: {
					auto e = static_cast<Ast_Enum *>(decl);
					if (e->identifier->atom->hash == name->hash)
						return decl;
				} break;
			}
		}

		if (!temp->parent) {
			break;
		}
		temp = temp->parent;
	}

	return 0;
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
	for (auto par_type : decl->parameters) {
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