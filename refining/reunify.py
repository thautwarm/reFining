from .typevars import (make_join, make_function, make_induct, make_basic, TypeVar, Undecided, Basic, TypeImpl,
                       TypeEnvFreshPair, make_statement)
from .asdl import *
from functools import reduce
from Redy.Opt import constexpr, const, feature
from .utils import FrozenDict
import typing
from .setting import debug

if debug:
    def log(func):
        def apply(*args):
            result = func(*args)
            print('terms:', args[0])
            print('analyzed type:', result)
            print('env:', args[-1])
            print('===============================')
            return result

        return apply
else:
    def log(_):
        return _

staging = (const, constexpr)

int_type = make_basic('int')
float_type = make_basic('float')
str_type = make_basic('str')
bool_type = make_basic('bool')
any_type = make_basic('any')
unit_type = make_basic('unit')

type_map = {
    str: str_type, float: float_type, int: int_type, bool: bool_type
}

TypeEnv = FrozenDict[str, TypeVar]


def make_default_env():
    named_types = {
        'unit': unit_type, 'int': int_type, 'str': str_type, 'bool': bool_type, 'float': float_type, 'any': any_type
    }
    return Env(named_types, {}, {})


class Env:
    inductive_type_impl: typing.Dict[Basic, typing.Tuple[TypeImpl, TypeImpl]] = {}
    named_types: typing.Dict[str, TypeVar]
    undecided_types: typing.Dict[str, Undecided]
    symbols: typing.Dict[str, TypeVar]

    def __init__(self, named_types: typing.Dict[str, TypeVar], undecided_types: typing.Dict[str, Undecided],
                 symbols: typing.Dict[str, TypeVar]):
        self.named_types = named_types
        self.undecided_types = undecided_types
        self.symbols = symbols

        # TODO: copy on write
        def _get_named_type(name):
            return self.named_types.get(name)

        self.get_named_type = _get_named_type

        def _set_named_type(name, value: TypeVar):
            self.named_types[name] = value

        self.set_named_type = _set_named_type

        def _get_undecided_type(name):
            return self.undecided_types.get(name)

        self.get_undecided_type = _get_undecided_type

        def _set_undecided_type(name, value: TypeVar):
            self.undecided_types[name] = value

        self.set_undecided_type = _set_undecided_type

        def _get_symbol(name):
            return self.symbols.get(name)

        self.get_symbol = _get_symbol

        def _set_symbol(name, value: TypeVar):
            self.symbols[name] = value

        self.set_symbol = _set_symbol

    def get_named_type(self, name: str) -> typing.Optional[TypeVar]:
        ...

    def set_named_type(self, name: str, value: TypeVar):
        ...

    def get_symbol(self, name: str) -> typing.Optional[TypeVar]:
        ...

    def set_symbol(self, name: str, value: TypeVar):
        ...

    def get_undecided_type(self, name: str) -> typing.Optional[Undecided]:
        ...

    def set_undecided_type(self, name: str, value: TypeVar):
        ...

    def create_sub(self):
        # TODO: copy on write
        return Env(self.named_types.copy(), self.undecided_types.copy(), self.symbols.copy())

    def __repr__(self):
        return '{} | {}'.format(self.symbols, self.undecided_types)


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
    def _type_def_helper(inductive_ty: TypeInduct, env: Env) -> typing.Tuple[Basic, TypeImpl]:
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

        """

        name = inductive_ty.sym.name
        new_basic = make_basic(name)
        env.set_named_type(name, new_basic)
        _ty_args = specify_type(inductive_ty.ty, env)
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
        named_ty, ty_args = _type_def_helper(type_term.induct, new_env)
        impl = specify_type(type_term.impl, new_env)
        Env.inductive_type_impl[named_ty] = (ty_args, impl)
        ty_env.set_named_type(named_ty.name, named_ty)
        return make_induct(named_ty, ty_args)

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


@log
def analyze(term: Term, env: Env):
    def apply_analyze(_t):
        return analyze(_t, env)

    if term is unit:
        return unit_type
    elif isinstance(term, Id):
        generic_vars = {}
        sym = get_type_of_symbol(term.repr_str, env, (typing.cast(set, env.undecided_types.values()), generic_vars))
        env.undecided_types.update({repr(v): v for k, v in generic_vars.items()})
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
        is_unified = origin_ty.unify(manual_ty)

        if not is_unified:
            raise TypeError

        return origin_ty
    elif isinstance(term, Lam):
        arg, annotate, body = term.arg, term.annotate, term.ret
        arg_ty = Undecided(specify_type(annotate, env)).pruned if annotate else Undecided()
        new_env = env.create_sub()
        new_env.set_symbol(arg.repr_str, arg_ty)
        new_env.set_undecided_type(repr(arg_ty), arg_ty)
        ret_ty = analyze(body, new_env)
        return make_function(arg_ty, ret_ty)

    elif isinstance(term, Let):
        tag, annotate, value, body = term.tag, term.annotate, term.value, term.do
        new_ty = Undecided(specify_type(annotate, env)).pruned if annotate else Undecided()
        new_env = env.create_sub()
        new_env.set_undecided_type(repr(new_ty), new_ty)
        new_env.set_symbol(tag, new_ty)

        value_ty = analyze(value, new_env)
        is_unified = new_ty.unify(value_ty)
        if not is_unified:
            raise TypeError

        return analyze(body, new_env)

    elif isinstance(term, Const):
        return type_map.get(type(term.repr), any_type)

    elif isinstance(term, (TypeDef, TypeInduct, TypeAbbr)):
        return specify_type(term, env)

    elif isinstance(term, Tuple):
        return make_join(map(apply_analyze, term.items))

    elif isinstance(term, Stmts):
        return reduce(make_statement, (apply_analyze(each) for each in term.terms))

    raise TypeError
