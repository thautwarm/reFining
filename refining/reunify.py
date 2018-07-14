from .type_env import *
from .asdl import *
from functools import reduce
from .utils import log
import typing
from collections import OrderedDict


def get_type_of_symbol(name: str, env: Env, fresh_args: TypeEnvFreshPair):
    result = env.get_symbol(name)
    if result is None:
        raise NameError(name)
    _, result = result.fresh(fresh_args)
    env.set_symbol(name, result)
    return result


@log
def specify_type(type_term, ty_env: Env):
    @log
    def _type_def_helper(name, args, env: Env) -> typing.Tuple[Basic, typing.Dict[str, Undecided]]:
        """
        only typedef can define new basic type
        type S of 'a 'b = 'a * 'b, where
            S is a basic type(but there is no constructor for S, for S is actually a functor.
            S of 'a 'b can be expand to 'a * 'b

        there could be recursive types(guided ones are also allowed):
        type S of 'a * 'b =
            | S1 'a * 'b)
            | S2 'a * ('a * 'b)
            | S3 (S of bool * bool) * 'a * 'b

        type List of 'a =
            | Nil
            | Cons 'a * List of 'a

        let x  = fn x -> x + 1 with y -> y - 1 in
        match 1 with
        | x(a) -> a # a should be 0
        # type of x : int <=> int
        =>
            compiling time behaviours:
                1. make a invertible function `() <=> List of 'a`,
                2. make a invertible function `'(a * List of 'a) <=> List of 'a

        """

        new_basic = make_basic(name)
        env.set_named_type(name, new_basic)
        _ty_args = OrderedDict((arg, Undecided()) for arg in args)
        env.undecided_types.update(_ty_args)
        return new_basic, _ty_args

    if isinstance(type_term, TypeSym):
        var = ty_env.get_named_type(type_term.name)
        if var is None:
            raise NameError(type_term)
        return var

    elif isinstance(type_term, TypeSlot):
        var = ty_env.get_undecided_type(type_term.name)
        if var is None:
            var = Undecided()
            ty_env.set_undecided_type(type_term.name, var)
        return var
    elif isinstance(type_term, TypeInduct):
        named_ty = specify_type(type_term.sym, ty_env)
        ty_args = specify_type(type_term.ty, ty_env)
        return make_induct(named_ty, ty_args)
    elif isinstance(type_term, TypeDef):
        new_env = ty_env.create_sub()
        named_ty, ty_args = _type_def_helper(type_term.name, type_term.args, new_env)
        impl = specify_type(type_term.impl, new_env)
        Env.inductive_type_impl[named_ty] = Generic(list(ty_args.keys()), impl)
        ty_env.set_named_type(named_ty.name, named_ty)
        return make_induct(named_ty, *ty_args.values())

    # TODO: remove from specify_type
    elif isinstance(type_term, TypeAbbr):
        ty = specify_type(type_term.impl, ty_env)
        ty_env.set_named_type(type_term.name, ty)
        return ty

    elif isinstance(type_term, TypeJoin):
        components = map(lambda it: specify_type(it, ty_env), type_term.components)
        return make_join(components)

    elif isinstance(type_term, TypeFunction):
        left = specify_type(type_term.left, ty_env)
        right = specify_type(type_term.right, ty_env)
        return make_function(left, right)
    else:
        raise TypeError


def init_analyzer(primitive_type_mapper: typing.Callable[[Const], Basic]):
    @log
    def analyze(term: Term, env: Env):
        def apply_analyze(_t):
            return analyze(_t, env)

        if term is unit:
            return unit_type

        elif isinstance(term, Id):
            generic_vars = {}
            sym = get_type_of_symbol(term.repr_str, env, (set(env.undecided_types.values()), generic_vars))
            env.undecided_types.update({repr(v): v for v in generic_vars.values()})
            return sym

        elif isinstance(term, App):
            fun_ty, arg_ty = apply_analyze(term.fn), apply_analyze(term.arg)
            ret_ty = Undecided()
            is_unified = make_function(arg_ty, ret_ty).unify(fun_ty)
            if not is_unified:
                raise TypeError

            return ret_ty

        elif isinstance(term, Annotate):
            origin_ty = apply_analyze(term.term)
            manual_ty = specify_type(term.type, env)
            origin_ty.unified(manual_ty)

            return origin_ty
        elif isinstance(term, Lam):
            arg, annotate, body = term.arg, term.annotate, term.ret
            arg_ty = Undecided(specify_type(annotate, env)).pruned if annotate else Undecided()
            new_env = env.create_sub()
            new_env.set_symbol(arg.repr_str, arg_ty)
            new_env.set_undecided_type(repr(arg_ty), arg_ty)
            ret_ty = analyze(body, new_env)
            return make_function(arg_ty, ret_ty)

        elif isinstance(term, If):
            test, body, else_do = term.test, term.body, term.else_do
            test_ty = analyze(test, env)

            test_ty.unified(bool_type)

            true_env = env.create_sub()
            body_ty = analyze(body, true_env)

            false_env = env.create_sub()
            else_do_ty = analyze(else_do, false_env)

            body_ty.unified(else_do_ty)

            return body_ty

        elif isinstance(term, Let):
            tag, annotate, value, body = term.tag, term.annotate, term.value, term.do
            new_ty = Undecided(specify_type(annotate, env)).pruned if annotate else Undecided()
            new_env = env.create_sub()
            new_env.set_undecided_type(repr(new_ty), new_ty)
            new_env.set_symbol(tag, new_ty)

            value_ty = analyze(value, new_env)
            new_ty.unified(value_ty)

            return analyze(body, new_env)

        elif isinstance(term, Const):
            return primitive_type_mapper(term)

        elif isinstance(term, (TypeDef, TypeInduct, TypeAbbr)):
            return specify_type(term, env)

        elif isinstance(term, Tuple):
            return make_join(map(apply_analyze, term.items))

        elif isinstance(term, Stmts):
            return reduce(make_statement, (analyze(each, env) for each in term.terms))

        raise TypeError

    return analyze
