#include "semal.hpp"
#include "diag.hpp"
#include <vector>
#include <queue>

namespace semal
{
	#define sem_assert_ice(expr, fmt, ...) diag::assert_that(expr, error_code::ice, "at {}: {}", node.loc.to_string(), std::format(fmt, ##__VA_ARGS__))
	#define sem_assert(expr, fmt, ...) diag::assert_that(expr, error_code::semal, "at {}: {}", node.loc.to_string(), std::format(fmt, ##__VA_ARGS__))

	syntax::node::variable_decl static_default_variables[] =
	{
		syntax::node::variable_decl
		{
			syntax::node::identifier{"os_unknown"},
			syntax::node::identifier{"i64 const static"},
			syntax::node::expression
			{
				syntax::node::expression::type::integer_literal,
				syntax::node::integer_literal{0}.unique_clone()
			}
		},
		syntax::node::variable_decl
		{
			syntax::node::identifier{"os_windows"},
			syntax::node::identifier{"i64 const static"},
			syntax::node::expression
			{
				syntax::node::expression::type::integer_literal,
				syntax::node::integer_literal{1}.unique_clone()
			}
		},
		syntax::node::variable_decl
		{
			syntax::node::identifier{"os_linux"},
			syntax::node::identifier{"i64 const static"},
			syntax::node::expression
			{
				syntax::node::expression::type::integer_literal,
				syntax::node::integer_literal{2}.unique_clone()
			}
		},
		syntax::node::variable_decl
		{
			syntax::node::identifier{"os_macos"},
			syntax::node::identifier{"i64 const static"},
			syntax::node::expression
			{
				syntax::node::expression::type::integer_literal,
				syntax::node::integer_literal{3}.unique_clone()
			}
		},
		syntax::node::variable_decl
		{
			syntax::node::identifier{"host_os"},
			syntax::node::identifier{"i64 const static"},
			syntax::node::expression
			{
				syntax::node::expression::type::identifier,
				#ifdef _WIN32
				syntax::node::identifier{"os_windows"}.unique_clone()
				#elif defined(__linux__)
				syntax::node::identifier{"os_linux"}.unique_clone()
				#elif defined(__APPLE__)
				syntax::node::identifier{"os_macos"}.unique_clone()
				#else
				syntax::node::identifier{"os_unknown"}.unique_clone()
				#endif
			}
		}
	};

	struct var_scope
	{
		std::unordered_map<std::string, const syntax::node::variable_decl*> decls = {};
		std::unordered_map<std::string, syntax::node::function_decl> fn_decls = {};
		const syntax::node::block* block = nullptr;
	};
	// stack starts with a single empty scope (this is global scope. should expect global variables to be defined here.)
	std::vector<var_scope> var_stack = {var_scope{}};
	// sometimes variable decls appear before the block scope they're in (e.g a function-decl will semal its parameters before its block (first child)).
	// when this happens, we put them here instead. next time a scope is pushed, move everything in the queue into the new scope.
	std::queue<const syntax::node::variable_decl*> premature_scope_variables = {};
	const syntax::node::variable_decl* find_variable(const std::string& name)
	{
		for(var_scope& scope : var_stack)
		{
			auto iter = scope.decls.find(name);
			if(iter != scope.decls.end())
			{
				return iter->second;
			}
		}
		return nullptr;
	}
	const syntax::node::function_decl* find_function(const std::string& name)
	{
		for(var_scope& scope : var_stack)
		{
			auto iter = scope.fn_decls.find(name);
			if(iter != scope.fn_decls.end())
			{
				return &iter->second;
			}
		}
		return nullptr;
	}
	var_scope& push_scope()
	{
		var_scope& ret = var_stack.emplace_back();
		while(premature_scope_variables.size())
		{
			const auto* var = premature_scope_variables.front();
			ret.decls[var->var_name.iden] = var;
			premature_scope_variables.pop();
		}
		return ret;
	}
	void pop_scope(){diag::assert_that(var_stack.size(), error_code::ice, "attempt to pop_scope when var stack was already empty."); var_stack.pop_back();}
	void add_var(const syntax::node::variable_decl& decl)
	{
		if(decl.impl_should_add_to_current_scope)
		{
			if(decl.impl_is_defined_before_parent_block)
			{
				premature_scope_variables.push(&decl);
			}
			else
			{
				var_stack.back().decls[decl.var_name.iden] = &decl;
			}
		}
	}
	void add_fn(const syntax::node::function_decl& fndecl)
	{
		auto maybe_already_fn = find_function(fndecl.func_name.iden);
		if(maybe_already_fn != nullptr)
		{
			if(maybe_already_fn->loc != fndecl.loc)
			{
				const auto& node = fndecl;
				sem_assert(false, "redefinition of function named \"{}\" (previously defined at {})", fndecl.func_name.iden, maybe_already_fn->loc.to_string());
			}
			else
			{
				return;
			}
		}
		var_stack.back().fn_decls[fndecl.func_name.iden] = fndecl;
	}

	std::unordered_map<std::size_t, static_value(*)(const syntax::inode* node)> semal_table;
	#define SEMAL_BEGIN(x) semal_table[syntax::node::x{}.hash()] = [](const syntax::inode* base_node)->static_value{ \
	const auto& node = *static_cast<const syntax::node::x*>(base_node);
	#define SEMAL_END };
	#define ILL_FORMED return static_value::type_only(incomplete_type("badtype").unique_clone())
	#define GETVAL(n) [&](){diag::assert_that(semal_table.contains((n).hash()), error_code::nyi, "type checking NYI for \"{}\" nodes (hash: {})", (n).name(), (n).hash()); static_value ret = semal_table.at((n).hash())(&(n)); if(node.semal.is_null()){node.semal = ret.clone();} if(ret.ty != nullptr) {sem_assert(ret.ty->is_static() ? ret.has_value() : true, "value of type {} must have static value available at compile time", ret.ty->get_qualified_name());} return ret;}()
	type_system tsys;

	void analyse(const syntax::inode* ast, type_system& tsys)
	{
		//bool should_create_scope = ast->is<syntax::node::block>();
		bool should_create_scope = NODE_IS(ast, block);
		if(should_create_scope)
		{
			push_scope();
		}
		auto iter = semal_table.find(ast->hash());
		diag::assert_that(iter != semal_table.end(), error_code::nyi, "type checking NYI for \"{}\" nodes (hash: {})", ast->name(), ast->hash());
		static_value val = iter->second(ast);
		if(val.ty != nullptr)
		{
			diag::assert_that(val.ty->is_well_formed(), error_code::type, "type checking of AST node {} (at {}) yielded a {} type: {}", ast->name(), ast->loc.to_string(), val.ty->hint_name(), val.ty->get_name());
		}
		// todo: verify the type?
		for(const auto& child : ast->children)
		{
			if(child != nullptr)
			{
				analyse(child.get(), tsys);
			}	
		}
		if(should_create_scope)
		{
			pop_scope();
		}
	}

	void populate_table()
	{
		SEMAL_BEGIN(identifier)
			type_ptr ret = tsys.get_type(node.iden);
			if(ret == nullptr)
			{
				// find a variable declared by this name.
				const syntax::node::variable_decl* decl = find_variable(node.iden);
				if(decl != nullptr)
				{
					return GETVAL(*decl);
				}
				const syntax::node::function_decl* fn = find_function(node.iden);
				if(fn != nullptr)
				{
					return GETVAL(*fn).with_type_qualifier(qual_static);
				}
				sem_assert(false, "could not decipher type of identifier \"{}\". wasn't a valid typename, nor was it a name of a variable available in this scope.", node.iden);
				ILL_FORMED;
			}
			return static_value::type_only(ret->unique_clone());
		SEMAL_END

		SEMAL_BEGIN(root)
			return static_value::null();
		SEMAL_END

		SEMAL_BEGIN(block)
			for(const auto& child : node.children)
			{
				GETVAL(*child);
			}
			return static_value::null();
		SEMAL_END

		SEMAL_BEGIN(variable_decl)
			add_var(node);
			static_value ret = static_value::null();
			if(node.type_name.iden != syntax::node::inferred_typename)
			{
				ret = static_value::type_only(tsys.get_type(node.type_name.iden));
			}
			else
			{
				sem_assert(!node.expr.is_null(), "variable declaration {} does not explicitly specify its type but also doesn't have an initialiser. if you want to infer the type, you must give it a valid initialiser at the point of declaration.", node.var_name.iden);
				ret = GETVAL(node.expr).discarded_type_qualifiers();
				// if variable type is inferred, the resultant type will drop all qualifiers (to avoid issues such as `myvar ::= 5` causing myvar to be a `i64 static` because a integer literal is static)
			}
			if(!node.expr.is_null())
			{
				static_value init = GETVAL(node.expr);
				typeconv conv = init.ty->can_implicitly_convert_to(*ret.ty);
				sem_assert(conv != typeconv::cant, "initialiser of variable {} is of type ({}) which is not implicitly convertible to {}'s type ({})", node.var_name.iden, init.ty->get_qualified_name(), node.var_name.iden, ret.ty->get_qualified_name());
				ret = init.do_convert(ret.ty->unique_clone(), node.loc);
				if(!ret.ty->is_static())
				{
					// dont pretend to have a constexpr value if the var isnt actually constxpr.
					ret.clear_value();
				}
			}
			return ret;
		SEMAL_END

		SEMAL_BEGIN(struct_decl)
			sem_assert_ice(node.children.size() == 1 && NODE_IS(node.children.front(), block), "Struct Declaration AST node must have a single child, and that child must be a block. Instead it has {} child{}{}", node.children.size(), node.children.size() == 1 ? "" : "ren", node.children.size() > 1 ? std::format("(first child is a \"{}\")", node.children.front()->name()) : "");
			type_system::struct_builder builder = tsys.make_struct(node.struct_name.iden);
			static_value ret;
			// get the children of the block. this is where methods and data member variable declarations will be.
			// however, possible TODO: you will almost certainly want to extract the method declarations here too, so it's easy to lookup by method name.
			for(const syntax::node_ptr& ptr : node.children.front()->children)
			{
				// the data members of a struct are encoded in the type. methods however are *not*
				if(NODE_IS(ptr, variable_decl))
				{
					const auto& data_member = static_cast<const syntax::node::variable_decl&>(*ptr);
					data_member.impl_should_add_to_current_scope = false;
					static_value memberval = GETVAL(data_member);
					builder.add_member(data_member.var_name.iden, memberval.ty->get_name());
					ret.children[data_member.var_name.iden] = memberval.clone();
				}
				else if(NODE_IS(ptr, function_decl))
				{
					auto& decl = *const_cast<syntax::node::function_decl*>(static_cast<const syntax::node::function_decl*>(ptr.get()));
					decl.struct_owner = node.struct_name.iden;
					// add a struct_ty& as the first parameter called `this`
					decl.params.decls.insert(decl.params.decls.begin(), syntax::node::variable_decl{
						syntax::node::identifier{"this"},
						syntax::node::identifier{std::format("{}&", node.struct_name.iden)}});
					GETVAL(decl);
				}
				else
				{
					sem_assert(false, "unexpected {} within block for struct named \"{}\", only expecting to see function (method) or variable declarations.", ptr->name(), node.struct_name.iden);
				}
			}
			ret.ty = builder.build();
			return ret;
		SEMAL_END

		SEMAL_BEGIN(function_decl)
			add_fn(node);
			if(node.children.empty()){}
			else if(node.children.size() != 1 || !NODE_IS(node.children.front(), block))
			{
				sem_assert(false, "function declaration AST node should have either 0 or 1 children (that child being a block). Instead, it has {} children, first one being a \"{}\"", node.children.size(), node.children.front()->name());
			}
			std::vector<std::string> param_type_names;
			for(const auto& param : node.params.decls)
			{
				if(node.children.empty())
				{
					param.impl_should_add_to_current_scope = false;
				}
				param.impl_is_defined_before_parent_block = true;
				type_ptr param_ty = GETVAL(param).ty;
				sem_assert(!param_ty->is_static(), "function parameters cannot be static. function \"{}\"'s param named \"{}\" is of type \"{}\"", node.func_name.iden, param.var_name.iden, param_ty->get_qualified_name());
				param_type_names.push_back(param_ty->get_qualified_name());
			}
			// static value representing a function is just a string representing the function name. (this is because functions as values are static i.e available at compile time.)
			return static_value::create(
				tsys.get_function_type(node.return_type_name.iden, param_type_names),
				node.func_name
			);
		SEMAL_END

		SEMAL_BEGIN(function_call)
			const syntax::node::function_decl* decl = find_function(node.func_name.iden);
			type_ptr fn_ty;
			if(decl != nullptr)
			{
				std::vector<type_ptr> params;
				for(const syntax::node::variable_decl& param : decl->params.decls)
				{
					params.push_back(GETVAL(param).ty);
				}
				
				fn_ty = function_type{tsys.get_type(decl->return_type_name.iden), std::move(params)}.unique_clone();
			}
			else
			{
				const syntax::node::variable_decl* fnptr = find_variable(node.func_name.iden);
				sem_assert(fnptr != nullptr, "unknown function \"{}\"", node.func_name.iden);
				fn_ty = GETVAL(*fnptr).ty;
			}
			sem_assert(fn_ty->is_function(), "attempt to invoke variable {} which is of non-function-type {}. you can only invoke functions or function pointers.", node.func_name.iden, fn_ty->get_qualified_name());
			auto* fn = static_cast<function_type*>(fn_ty.get());
			sem_assert(node.params.exprs.size() == fn->params.size(), "call to function \"{}\" with {} arguments, but it expects {} arguments", node.func_name.iden, node.params.exprs.size(), fn->params.size());
			for(std::size_t i = 0; i < fn->params.size(); i++)
			{
				static_value param_val = GETVAL(node.params.exprs[i]);
				const itype& expected_ty = *fn->params[i];
				typeconv conv = param_val.ty->can_implicitly_convert_to(expected_ty);
				typeconv explicit_conv = param_val.ty->can_explicitly_convert_to(expected_ty);
				sem_assert(conv != typeconv::cant, "in call to function \"{}\", cannot implicitly convert parameter at id {} from \"{}\" to expected type \"{}\"{}", node.func_name.iden, i, param_val.ty->get_qualified_name(), expected_ty.get_qualified_name(), explicit_conv != typeconv::cant ? "\nhint: you should try explicitly casting the parameter to the type above" : "");
			}
			type_ptr ret = fn->return_type->unique_clone();
			return static_value::type_only(fn->return_type->unique_clone());
		SEMAL_END

		SEMAL_BEGIN(namespace_access)
			std::string full_namespace_name;
			auto parts = node.lhs_parts;
			while(parts.size())
			{
				full_namespace_name += parts.front() + "::";
				parts.erase(parts.begin());
			}
			// todo: actually use the namespace name?
			return GETVAL(*node.rhs.expr);
		SEMAL_END

		SEMAL_BEGIN(if_statement)
			static_value cond = GETVAL(node.cond);
			typeconv conv = cond.ty->can_implicitly_convert_to(primitive_type{primitive::boolean});
			sem_assert(conv != typeconv::cant, "evaluated type of if-statement condition must be implicitly convertible to a bool, which {} is not", cond.ty->get_name());
			if(node.is_static)
			{
				sem_assert(cond.has_value(), "condition of static-if statement must be computable at compile time, which it isn't");
				sem_assert(cond.ty->is_static(), "condition static-if statement must be a static expression (known at compile time). you passed a: \"{}\"", cond.ty->get_qualified_name());
				// todo: get the condition value. if its false, cast-away constness and kill the block child so no compilation occurs.
				bool cond_value = cond.value_as<bool>();
				if(!cond_value)
				{
					auto& mutable_node = const_cast<syntax::node::if_statement&>(node);
					// first child of an if statement is always a block containing instructions. if a static-if condition is false, that code should not be compiled, so we simply clear that blocks children:
					auto& if_blk = *node.children.front();
					if_blk.children.clear();
				}
			}
			return static_value::null();
		SEMAL_END

		SEMAL_BEGIN(else_statement)
			if(!node.cond.is_null())
			{
				GETVAL(node.cond);
			}
			return static_value::null();
		SEMAL_END

		SEMAL_BEGIN(meta_region)
			return static_value::null();
		SEMAL_END

		SEMAL_BEGIN(alias)
			tsys.make_alias(node.alias_name.iden, GETVAL(node.type_value_expr).ty->get_name());
			return static_value::null();
		SEMAL_END

		SEMAL_BEGIN(designated_initialiser_list)
			for(const auto& init : node.inits)
			{
				GETVAL(init);
			}
			return static_value::null();
		SEMAL_END

		SEMAL_BEGIN(designated_initialiser)
			GETVAL(node.initialiser);
			return static_value::null();
		SEMAL_END
		
		SEMAL_BEGIN(expression)
			using type = syntax::node::expression::type;
			switch(node.t)
			{
				case type::integer_literal:
				{
					static_value ret = static_value::type_only(tsys.get_primitive_type(primitive::i64)->with_qualifier(qual_static));
					std::int64_t val = NODE_AS(node.expr.get(), integer_literal)->val;
					ret.val = to_int_value(*ret.ty, val);
					return ret;
				}
				break;
				case type::decimal_literal:
					return static_value::create(
						tsys.get_primitive_type(primitive::f64)->with_qualifier(qual_static),

						NODE_AS(node.expr.get(), decimal_literal)->val
					);
				break;
				case type::string_literal:
					return static_value::create(
						tsys.get_primitive_type(primitive::u8)->with_qualifier(qual_const)->ref()->with_qualifier(qual_static),

						NODE_AS(node.expr.get(), string_literal)->val
					);
				break;
				case type::char_literal:
					return static_value::create(
						tsys.get_primitive_type(primitive::u8)->with_qualifier(qual_static),

						NODE_AS(node.expr.get(), char_literal)->val
					);
				break;
				case type::bool_literal:
					return static_value::create(
						tsys.get_primitive_type(primitive::boolean)->with_qualifier(qual_static),

						NODE_AS(node.expr.get(), bool_literal)->val
					);
				break;
				case type::null_literal:
					return static_value::create(
						tsys.get_primitive_type(primitive::i8)->ref()->with_qualifier(qual_weak)->with_qualifier(qual_static),

						nullptr
					);
				break;
				case type::return_statement:
					if(node.expr == nullptr)
					{
						return static_value::null();
					}
					return GETVAL(*NODE_AS(node.expr.get(), expression));
				break;
			case type::cast:
			{
				// get the extra expression's type.
				// node.extra is either an identifier or yet another expression.
				sem_assert_ice(node.extra != nullptr, "in a cast expression, the `extra` (rhs) must be either an expression or an identifier. it's a nullptr. parse reduction has gone awry");
				sem_assert(NODE_IS(node.extra, identifier), "rhs of cast expression x@y should be an identifier. what you gave me was a \"{}\"", node.extra->name());
				static_value rhs = GETVAL(*node.extra);
				static_value lhs = GETVAL(*node.expr);
				sem_assert(lhs.ty->can_explicitly_convert_to(*rhs.ty) != typeconv::cant, "explicit cast from {} to {} is invalid", lhs.ty->get_qualified_name(), rhs.ty->get_qualified_name());
				if(lhs.has_value())
				{
					return lhs.do_explicit_convert(rhs.ty->unique_clone(), node.loc);
				}
				return static_value::type_only(std::move(rhs.ty));
			}
			break;
			case type::deref:
			{
				// note: deref cannot return a static value, only a type. how do you get the address of something at compile time?
				static_value val = GETVAL(*node.expr);
				val.clear_value();
				val.ty = val.ty->deref();
				return val;
			}
			break;
			case type::ref:
			{
				// note: ref cannot return a static value, only a type. how do you get the address of something at compile time?
				static_value val = GETVAL(*node.expr);
				val.clear_value();
				val.ty->remove_qualifier(qual_static);
				val.ty = val.ty->ref();
				return val;
			}
			break;
			case type::eqcompare:
			{
				static_value lhs = GETVAL(*node.expr);
				static_value rhs = GETVAL(*node.extra);
				sem_assert(lhs.ty->equality_comparable(*rhs.ty), "comparison is invalid because lhs type \"{}\" is not equality-comparaable to rhs type \"{}\"", lhs.ty->get_qualified_name(), rhs.ty->get_qualified_name());
				
				type_ptr retty = tsys.get_primitive_type(primitive::boolean);

				if(lhs.has_value() && rhs.has_value())
				{
					return static_value::create(retty->with_qualifier(qual_static), lhs.equals(rhs));
				}
				return static_value::type_only(std::move(retty));
			}
			break;
			case type::neqcompare:
			{
				static_value lhs = GETVAL(*node.expr);
				static_value rhs = GETVAL(*node.extra);
				sem_assert(lhs.ty->equality_comparable(*rhs.ty), "comparison is invalid because lhs type \"{}\" is not equality-comparaable to rhs type \"{}\"", lhs.ty->get_qualified_name(), rhs.ty->get_qualified_name());
				
				type_ptr retty = tsys.get_primitive_type(primitive::boolean);

				if(lhs.has_value() && rhs.has_value())
				{
					return static_value::create(retty->with_qualifier(qual_static), !lhs.equals(rhs));
				}
				return static_value::type_only(std::move(retty));
			}
			break;
			case type::struct_initialiser:
			{
				// lhs can either be an identifier or a namespace access.
				// i need the struct name, so we check both cases
				std::string struct_name;
				if(NODE_IS(node.expr, identifier))
				{
					struct_name = NODE_AS(node.expr.get(), identifier)->iden;
				}
				else if(NODE_IS(node.expr, namespace_access))
				{
					auto access = NODE_AS(node.expr.get(), namespace_access);
					// you should be able to get the left parts here easily once you get to that.
					sem_assert(NODE_IS(access->rhs.expr, identifier), "struct initialiser's type name is within a namespace. that namespace access' rhs should be an expression that resolves to an identifier, but instead resolves to a {}", access->rhs.expr->name());
					struct_name = NODE_AS(access->rhs.expr.get(), identifier)->iden;
				}
				else
				{
					sem_assert(false, "struct initialiser lhs should always be either an identifier or a namespace access, instead you've provided a {}", node.expr->name());
					ILL_FORMED;
				}
				auto struct_ty = tsys.get_type(struct_name);
				sem_assert(struct_ty != nullptr && struct_ty->is_well_formed(), "unknown struct type \"{}\" in struct initialiser", struct_name);
				sem_assert(struct_ty->is_struct(), "non-struct type \"{}\" detected in struct initialiser. type appears to be a {} type", struct_name, struct_ty->hint_name());

				static_value ret = static_value::type_only(struct_ty->unique_clone());

				sem_assert(NODE_IS(node.extra, designated_initialiser_list), "should be desiginitlist >:(");
				GETVAL(*node.extra.get());
				for(const auto& init : NODE_AS(node.extra.get(), designated_initialiser_list)->inits)
				{
					static_value dinit = GETVAL(init);
					ret.children[init.member.iden] = dinit.clone();
					// todo: get the type of the data member and type check it
				}
				return ret;
			}
			break;
			case type::typeinfo:
				return static_value::typeinfo(tsys, *GETVAL(*node.expr).ty);
				//return static_value::type_only(tsys.get_type("typeinfo")->with_qualifier(qual_static));
			break;
			case type::addition:
			{
				static_value lhs = GETVAL(*node.expr);
				static_value rhs = GETVAL(*node.extra);
				sem_assert((lhs.ty->is_integer() || lhs.ty->is_floating_point()) && (rhs.ty->is_integer() || rhs.ty->is_floating_point()), "addition lhs and rhs must both be numeric (integer or floating point) types. you provided \"{}\" and \"{}\"", lhs.ty->get_qualified_name(), rhs.ty->get_qualified_name());
				typeconv conv = rhs.ty->can_implicitly_convert_to(*lhs.ty);
				sem_assert(conv != typeconv::cant, "in addition, rhs type \"{}\" must be implicitly convertible to lhs type \"{}\", which is not the case", rhs.ty->get_qualified_name(), lhs.ty->get_qualified_name());
				if(lhs.has_value() && rhs.has_value())
				{
					std::int64_t rhsv;
					if(rhs.ty->is_integer())
					{
						rhsv = get_int_value(*rhs.ty, rhs.val);
					}
					else if(lhs.ty->is_floating_point())
					{
						rhsv = std::any_cast<double>(rhs.val);
					}
					else
					{
						diag::ice("dont know how to static-add \"{}\" and \"{}\"", lhs.ty->get_qualified_name(), rhs.ty->get_qualified_name());
					}

					if(lhs.ty->is_integer())
					{
						lhs.val = to_int_value(*lhs.ty, get_int_value(*lhs.ty, lhs.val) + rhsv);
					}
					else if(lhs.ty->is_floating_point())
					{
						lhs.val = std::any_cast<double>(lhs.val) + rhsv;
					}
					else
					{
						diag::ice("dont know how to static-add \"{}\" and \"{}\"", lhs.ty->get_qualified_name(), rhs.ty->get_qualified_name());
					}
				}
				return lhs;
			}
			break;
			case type::subtraction:
			{
				static_value lhs = GETVAL(*node.expr);
				static_value rhs = GETVAL(*node.extra);
				sem_assert((lhs.ty->is_integer() || lhs.ty->is_floating_point()) && (rhs.ty->is_integer() || rhs.ty->is_floating_point()), "subtraction lhs and rhs must both be numeric (integer or floating point) types. you provided \"{}\" and \"{}\"", lhs.ty->get_qualified_name(), rhs.ty->get_qualified_name());
				typeconv conv = rhs.ty->can_implicitly_convert_to(*lhs.ty);
				sem_assert(conv != typeconv::cant, "in subtraction, rhs type \"{}\" must be implicitly convertible to lhs type \"{}\", which is not the case", rhs.ty->get_qualified_name(), lhs.ty->get_qualified_name());
				if(lhs.has_value() && rhs.has_value())
				{
					std::int64_t rhsv;
					if(rhs.ty->is_integer())
					{
						rhsv = get_int_value(*rhs.ty, rhs.val);
					}
					else if(lhs.ty->is_floating_point())
					{
						rhsv = std::any_cast<double>(rhs.val);
					}
					else
					{
						diag::ice("dont know how to static-subtract \"{}\" and \"{}\"", lhs.ty->get_qualified_name(), rhs.ty->get_qualified_name());
					}

					if(lhs.ty->is_integer())
					{
						lhs.val = to_int_value(*lhs.ty, get_int_value(*lhs.ty, lhs.val) - rhsv);
					}
					else if(lhs.ty->is_floating_point())
					{
						lhs.val = std::any_cast<double>(lhs.val) - rhsv;
					}
					else
					{
						diag::ice("dont know how to static-subtract \"{}\" and \"{}\"", lhs.ty->get_qualified_name(), rhs.ty->get_qualified_name());
					}
				}
				return lhs;
			}
			break;
			case type::multiplication:
			{
				static_value lhs = GETVAL(*node.expr);
				static_value rhs = GETVAL(*node.extra);
				sem_assert((lhs.ty->is_integer() || lhs.ty->is_floating_point()) && (rhs.ty->is_integer() || rhs.ty->is_floating_point()), "multiplication lhs and rhs must both be numeric (integer or floating point) types. you provided \"{}\" and \"{}\"", lhs.ty->get_qualified_name(), rhs.ty->get_qualified_name());
				typeconv conv = rhs.ty->can_implicitly_convert_to(*lhs.ty);
				sem_assert(conv != typeconv::cant, "in multiplication, rhs type \"{}\" must be implicitly convertible to lhs type \"{}\", which is not the case", rhs.ty->get_qualified_name(), lhs.ty->get_qualified_name());
				if(lhs.has_value() && rhs.has_value())
				{
					std::int64_t rhsv;
					if(rhs.ty->is_integer())
					{
						rhsv = get_int_value(*rhs.ty, rhs.val);
					}
					else if(lhs.ty->is_floating_point())
					{
						rhsv = std::any_cast<double>(rhs.val);
					}
					else
					{
						diag::ice("dont know how to static-multiply \"{}\" and \"{}\"", lhs.ty->get_qualified_name(), rhs.ty->get_qualified_name());
					}

					if(lhs.ty->is_integer())
					{
						lhs.val = to_int_value(*lhs.ty, get_int_value(*lhs.ty, lhs.val) * rhsv);
					}
					else if(lhs.ty->is_floating_point())
					{
						lhs.val = std::any_cast<double>(lhs.val) * rhsv;
					}
					else
					{
						diag::ice("dont know how to static-multiply \"{}\" and \"{}\"", lhs.ty->get_qualified_name(), rhs.ty->get_qualified_name());
					}
				}
				return lhs;
			}
			break;
			case type::division:
			{
				static_value lhs = GETVAL(*node.expr);
				static_value rhs = GETVAL(*node.extra);
				sem_assert((lhs.ty->is_integer() || lhs.ty->is_floating_point()) && (rhs.ty->is_integer() || rhs.ty->is_floating_point()), "division lhs and rhs must both be numeric (integer or floating point) types. you provided \"{}\" and \"{}\"", lhs.ty->get_qualified_name(), rhs.ty->get_qualified_name());
				typeconv conv = rhs.ty->can_implicitly_convert_to(*lhs.ty);
				sem_assert(conv != typeconv::cant, "in division, rhs type \"{}\" must be implicitly convertible to lhs type \"{}\", which is not the case", rhs.ty->get_qualified_name(), lhs.ty->get_qualified_name());
				if(lhs.has_value() && rhs.has_value())
				{
					std::int64_t rhsv;
					if(rhs.ty->is_integer())
					{
						rhsv = get_int_value(*rhs.ty, rhs.val);
					}
					else if(lhs.ty->is_floating_point())
					{
						rhsv = std::any_cast<double>(rhs.val);
					}
					else
					{
						diag::ice("dont know how to static-divide \"{}\" and \"{}\"", lhs.ty->get_qualified_name(), rhs.ty->get_qualified_name());
					}
					sem_assert(rhsv != 0, "division by zero detected at compile-time");

					if(lhs.ty->is_integer())
					{
						lhs.val = to_int_value(*lhs.ty, get_int_value(*lhs.ty, lhs.val) / rhsv);
					}
					else if(lhs.ty->is_floating_point())
					{
						lhs.val = std::any_cast<double>(lhs.val) / rhsv;
					}
					else
					{
						diag::ice("dont know how to static-divide \"{}\" and \"{}\"", lhs.ty->get_qualified_name(), rhs.ty->get_qualified_name());
					}
				}
				return lhs;
			}
			break;
			case type::namespace_access:
			[[fallthrough]];
			case type::identifier:
			[[fallthrough]];
			case type::defer:
			[[fallthrough]];
			case type::function_call:
			[[fallthrough]];
			case type::parenthesised_expression:
			{
				return GETVAL(*node.expr);
			}
			break;
			case type::dot_access:
			{
				type_ptr lhs_ty = GETVAL(*node.expr).ty;
				std::string struct_name = lhs_ty->get_name();
				sem_assert(lhs_ty->is_struct(), "lhs of dot-access expression should be a struct type, instead you passed {} {}", lhs_ty->hint_name(), struct_name);
				const auto& struct_ty = static_cast<const struct_type&>(*lhs_ty);

				sem_assert(NODE_IS(node.extra, expression), "rhs of dot-access expression must be an expression, instead you have provided a {}", node.extra->name());
				auto rhs_expr = NODE_AS(node.extra.get(), expression);
				if(NODE_IS(rhs_expr->expr, identifier))
				{
					std::string_view member_name = NODE_AS(rhs_expr->expr.get(), identifier)->iden;
					// should be a data member
					for(const auto& member : struct_ty.members)
					{
						if(member.name == member_name)
						{
							return static_value::type_only(member.ty->unique_clone());
						}
					}
					sem_assert(false, "struct {} has no such member named {}", struct_ty.get_name(), member_name);
				}
				else if(NODE_IS(rhs_expr->expr, function_call))
				{
					const auto* call = NODE_AS(rhs_expr->expr.get(), function_call);
					const syntax::node::function_decl* fn = find_function(call->func_name.iden);
					sem_assert(fn != nullptr, "call to undefined function \"{}\" (as method)", call->func_name.iden);
					sem_assert(!fn->struct_owner.empty(), "attempt to call free-function \"{}\" as method of struct \"{}\"", call->func_name.iden, struct_name);
					sem_assert(fn->struct_owner == struct_name, "attempt to call method \"{}\" as method of struct \"{}\", but the method actually belongs to the struct \"{}\"", call->func_name.iden, struct_name, fn->struct_owner);
					return GETVAL(*call);
				}
				else
				{
					sem_assert(false, "rhs of dot-access is of expression-type \"{}\", which is unexpected.", syntax::node::expression::type_names[static_cast<int>(rhs_expr->t)]);
				}
				ILL_FORMED;
			}
			break;
			case type::assign:
			{
				// lhs = rhs
				// todo: make sure rhs is convertible to rhs and lhs is not const.
				static_value lhs = GETVAL(*node.expr);
				//type_ptr lhs = GETVAL(*node.expr);
				sem_assert(!lhs.ty->is_const(), "lhs of assignment is \"{}\". cannot assign to const values.", lhs.ty->get_qualified_name());
				sem_assert(!lhs.ty->is_static(), "lhs of assignment is \"{}\". cannot assign to static values.", lhs.ty->get_qualified_name());
				static_value rhs = GETVAL(*node.extra);
				typeconv conv = rhs.ty->can_implicitly_convert_to(*lhs.ty);
				typeconv explicit_conv = rhs.ty->can_explicitly_convert_to(*lhs.ty);
				sem_assert(conv != typeconv::cant, "assignment invalid because rhs type ({}) cannot be implicitly converted to lhs type ({}){}", rhs.ty->get_qualified_name(), lhs.ty->get_qualified_name(), explicit_conv != typeconv::cant ? "\nhint: an explicit cast will allow this conversion!" : "");
				return static_value::type_only(lhs.ty->unique_clone());
			}
			break;
				default:
					sem_assert(false, "semantic analysis for expression type {} is NYI", syntax::node::expression::type_names[static_cast<int>(node.t)]);
					ILL_FORMED;
				break;
			}
		SEMAL_END

		for(const syntax::node::variable_decl& static_default : static_default_variables)
		{
			analyse(&static_default, tsys);
		}
	}
}