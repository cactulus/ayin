#include "compiler.h"
#include "typer.h"
#include "ast.h"
#include "common.h"

Typer::Typer(Compiler *compiler) {
	this->compiler = compiler;
}

void Typer::type_check_scope(Ast_Scope *scope) {
	Ast_Scope *old_scope = current_scope;
	current_scope = scope;

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
				Ast_Type_Info *struct_type = strct->type_info;
				for (auto decl : struct_type->struct_members) {
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

	current_scope = old_scope;
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
	Ast_Scope *old_scope = current_scope;
	current_scope = function->scope;

	current_function = function;
	function->linkage_name = mangle_name(function);

	Ast_Type_Info *function_type = function->type_info;
	for (auto par : function_type->parameter_types) {
		type_check_variable_declaration(par);
	}

	type_check_scope(function->scope);

	current_scope = old_scope;
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
			Ast_Statement *decl = find_declaration_by_name(id->atom);

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
			Ast_Statement *decl = find_declaration_by_name(call->identifier->atom);

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

				for (int i = 0; i < function_type->parameter_types.length; ++i) {
					if (i >= call->arguments.length) {
						compiler->report_error(call->identifier, "Arguments count does not match parameter count");
						return;
					}

					infer_type(call->arguments[i]);

					Ast_Type_Info *par_type = function_type->parameter_types[i]->type_info;

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

	Ast_Statement *decl = find_declaration_by_name(type_info->unresolved_name->atom);
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
	Ast_Type_Info *tfn_type = template_function->type_info;

	if (call->arguments.length != tfn_type->parameter_types.length) {
		compiler->report_error(call->identifier, "Arguments count does not match parameter count");
    	return template_function;
    }
    
    for (auto arg : call->arguments) {
		infer_type(arg);
    }
    
    for (auto overload : template_function->polymorphed_overloads) {
        bool does_match = true;
        Ast_Type_Info *overload_type = overload->type_info;
        for (s64 i = 0; i < overload_type->parameter_types.length; ++i) {
            auto par_type = overload_type->parameter_types[i]->type_info;
            auto arg_type = call->arguments[i]->inferred_type;
            
            if (!types_match(par_type, arg_type)) {
                does_match = false;
                break;
            }
        }
        
        if (does_match) return overload;
    }
    
    auto polymorph = polymorph_function_with_arguments(template_function, &call->arguments);
    if (polymorph) {
        type_check_function(polymorph);
        template_function->polymorphed_overloads.add(polymorph);
    }
    
    return polymorph;
}

Ast_Function *Typer::polymorph_function_with_arguments(Ast_Function *poly, Array<Ast_Expression *> *arguments) {
    Ast_Function *poly_copy = new Ast_Function();
	poly_copy->identifier = poly->identifier;
    poly_copy->is_template_function = false; 

    poly_copy->template_scope = new Ast_Scope();
    for (auto decl : poly->template_scope->declarations) {
    	Ast_Type_Alias *type_alias = static_cast<Ast_Type_Alias *>(decl);

		Ast_Type_Alias *nta = new Ast_Type_Alias();
		nta->location = type_alias->location;
		nta->identifier = type_alias->identifier;
		nta->type_info = copy_type(type_alias->type_info);

    	poly_copy->template_scope->declarations.add(nta);
	}
	poly_copy->scope->extension = poly_copy->template_scope;

    Ast_Type_Info *oft = poly->type_info;
    Ast_Type_Info *nft = copy_type(oft);

    poly_copy->type_info = nft;
    
    for (s64 i = 0; i < arguments->length; ++i) {
        auto target_type_info = (*arguments)[i]->inferred_type;
        
        auto par_type = poly_copy->type_info->parameter_types[i]->type_info;
        bool success = try_to_fill_polymorphic_type_aliases(par_type, target_type_info);
        
        if (!success) break;
    }

    for (auto _alias : poly_copy->template_scope->declarations) {
        auto alias = static_cast<Ast_Type_Alias *>(_alias);
        if (!alias->type_info->element_type) {
            String name = alias->identifier->atom->id;
            compiler->report_error(alias, "Could not fill typealias '%.*s'.\n", name.length, name.data);
            return 0;
        }
    }
    
    return poly_copy;
}

bool Typer::try_to_fill_polymorphic_type_aliases(Ast_Type_Info *type_info, Ast_Type_Info *target_type_info) {
	if (type_info->type == Ast_Type_Info::POINTER) {
        if (target_type_info->type != Ast_Type_Info::POINTER) return false;
        
        return try_to_fill_polymorphic_type_aliases(type_info->element_type, target_type_info->element_type);
    }
    
    if (type_is_primitive(type_info)) {
        return types_match(type_info, target_type_info);
    }

	if (type_info->type == Ast_Type_Info::UNINITIALIZED) {
		auto decl = find_declaration_by_name(type_info->unresolved_name->atom, type_info->unresolved_name->scope);
        if (!decl) {
            compiler->report_error(
            		type_info->unresolved_name,
            		"Undeclared identifier '%s'\n",
            		to_c_string(type_info->unresolved_name->atom->id)
            		);
            return false;
        }
        
        if (compiler->errors_reported) return false;
        
        if (decl->type == Ast::TYPE_ALIAS) {
            auto alias = static_cast<Ast_Type_Alias *>(decl);
            printf("start %p\n", alias);

            if (!alias->type_info->element_type) {
                alias->type_info->element_type = target_type_info;
                return true;
            } else {
                return types_match(alias->type_info->element_type, target_type_info);
            }
        } else if (decl->type == Ast::STRUCT) {
        	return types_match(decl->type_info, target_type_info);
        } else {
            assert(false);
        }
	}
    
	if (type_info->type == Ast_Type_Info::STRUCT) {
        return types_match(type_info, target_type_info);
	}

    return false;
}

Ast_Statement *Typer::find_declaration_by_name(Atom *name) {
	return find_declaration_by_name(name, current_scope);
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

		if (temp->extension) {
			Ast_Statement *res = find_declaration_by_name(name, temp->extension);
			if (res) {
				return res;
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
	for (auto par_type : func_type->parameter_types) {
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
				mangle_type(builder, mem->type_info);
			}
		} break;
		default: assert(0);
	}
}

Ast_Declaration *Typer::copy_declaration(Ast_Declaration *decl) {
	Ast_Declaration *_new = new Ast_Declaration();
	_new->location = decl->location;
	_new->llvm_reference = decl->llvm_reference;
	_new->identifier = decl->identifier;
	_new->initializer = decl->initializer;
	_new->type_info = copy_type(decl->type_info);

	return _new;
}

Ast_Type_Info *Typer::copy_type(Ast_Type_Info *type_info) {
	if (!type_info)
		return 0;

	Ast_Type_Info *_new = new Ast_Type_Info();

	_new->type = type_info->type;
	_new->element_type = copy_type(type_info->element_type);
	_new->unresolved_name = type_info->unresolved_name;
	_new->struct_decl = type_info->struct_decl;
	_new->return_type = copy_type(type_info->return_type);
	_new->is_signed = type_info->is_signed;
	_new->size = type_info->size;
	_new->alignment = type_info->alignment;

	for (auto decl : type_info->struct_members) {
		_new->struct_members.add(copy_declaration(decl));
	}

	for (auto mem : type_info->enum_members) {
		_new->enum_members.add(mem);
	}

	for (auto par : type_info->parameter_types) {
		_new->parameter_types.add(copy_declaration(par));
	}

	return _new;
}
