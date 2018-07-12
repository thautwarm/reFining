from .typevars import make_join, make_function, make_induct, TypeVar, Undecided, Basic, TypeImpl
from .asdl import *
import typing


class Symbol:
    name: str
    ty: TypeVar
    value: object


SymEnv = typing.List[typing.Tuple[Symbol, TypeVar]]
TypeEnv = typing.List[typing.Tuple[str, TypeVar]]


def get_type_of_symbol(name: str, env: SymEnv, non_generic_types: typing.Dict[Undecided, Undecided]):
    result = next((v for k, v in env if name == k), None)
    if result is None:
        raise NameError(name)
    _, result = result.fresh(non_generic_types)
    return result
#
#
# def specify_type(type_terms, ty_env: TypeEnv):
#     if isinstance(ty, TVar):
#         specify_type(ty.ty, env)
#         name = ty.name
#         looked = next((v for _, v in env if v.name == name), None)
#
#         if looked is None:
#             if ty.ty is None:
#                 env.append((name, ty))
#                 return ty
#
#             raise NameError(name)
#         else:
#             ty.ty = looked
#
#     elif isinstance(ty, TFnSig):
#         specify_type(ty.left, env)
#         specify_type(ty.right, env)
#     elif isinstance(ty, Basic):
#         return ty
#
#
# def analyse(term_: Term, env_: SymEnv):
#     def analyserec(term: Term, env: SymEnv, non_generic_types: typing.Set[Type]):
#         def analyse_it(_t):
#             return analyserec(_t, env, non_generic_types)
#
#         if isinstance(term, Id):
#             return get_type_of_symbol(term.repr_str, env, non_generic_types)
#
#         elif isinstance(term, App):
#             fun_ty, arg_ty = map(analyse_it, term)
#             ret_ty = TVar.new()
#             unify(TFnSig.new(arg_ty, ret_ty), fun_ty)
#             return ret_ty
#         elif isinstance(term, Lam):
#             arg, body = term.arg, term.ret
#             arg_ty = TVar.new()
#             new_env = env.copy()
#             new_env.append((arg.repr_str, arg_ty))
#             new_free_types = non_generic_types.copy()
#             new_free_types.add(arg_ty)
#
#             ret_ty = analyserec(body, new_env, new_free_types)
#             return TFnSig.new(arg_ty, ret_ty)
#
#         elif isinstance(term, Let):
#             tag, value, body = term.tag, term.value, term.do
#
#             new_ty = TVar.new()
#             if isinstance(tag, Annotate):
#                 manual_ty = tag.ty
#                 specify_type(manual_ty, env)
#                 tag = tag.term
#                 unify(new_ty, manual_ty)
#
#             new_env = env.copy()
#             new_env.append((tag.repr_str, new_ty))
#             new_free_types = non_generic_types.copy()
#             non_generic_types.add(new_ty)
#             value_ty = analyserec(value, new_env, new_free_types)
#             unify(new_ty, value_ty)
#             return analyserec(body, new_env, non_generic_types)
#         elif isinstance(term, Const):
#             return Basic(type(term.repr).__name__)
#
#         elif isinstance(term, Annotate):
#             ty = term.ty
#             t = analyse_it(term.term)
#             specify_type(term.ty, env)
#
#             unify(t, ty)
#             return t
#
#
#         elif isinstance(term, Alias):
#             specify_type(term.typ, env)
#             name = term.name
#             if any(_ for _, v in env if name == v.name):
#                 raise NameError("duplicated type {}.".format(name))
#
#             # if support overwrite, we can implement a nested type env with
#             #  correct inheritances.
#             n_ty = TVar.new(None, term.typ, name)
#             env.append(('.{}'.format(name), n_ty))
#             return n_ty
#
#         elif isinstance(term, Stmts):
#             return Flow(tuple(analyse_it(each) for each in term.terms))
#
#         raise TypeError
#
#     return analyserec(term_, env_, set())

# class TypeEnv:  #     """
#
#
#     """
#
#     def __init__(self, _: typing.List[typing.Tuple[str, Type]], parent: typing.Optional['TypeEnv']):
#         self._ = _
#         self.parent = parent
#
#     def get(self, name: str, default: Type):
#         for k, v in self._:
#             if k == name:
#                 return v
#         if self.parent:
#             return self.parent.get(name, default)
#         if default:
#             return default
#
#         raise NameError(name)
