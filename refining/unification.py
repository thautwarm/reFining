from .types import *
import typing

globals()['Hint'] = object
globals()['isinstance'] = isa


def occurs_in_type(v_, t2_: Type):
    def _occurs_in_type(v, t2):
        pruned_t2 = t2.prune()
        if pruned_t2 == v:
            return True
        elif isinstance(pruned_t2, TFnSig):
            return occurs_in(v, (pruned_t2.left, pruned_t2.right))
        return False

    if isinstance(t2_, Basic):
        return False

    return _occurs_in_type(v_, t2_)


def occurs_in(t: Type, types: typing.Iterable[Type]):
    def _occurs_in(t: Type, types: typing.Iterable[Type]):
        def test(t2: Type):
            return occurs_in_type(t, t2)

        return any(map(test, types))

    if isinstance(t, Basic):
        return False
    return _occurs_in(t, types)


def is_generic(v: TVar, non_generic_types: typing.Set[TVar]):
    return not occurs_in(v, non_generic_types)


def fresh(t: Type, non_generic_types: typing.Set[TVar]):
    table: typing.Dict[TVar, TVar] = {}

    def freshrec(tp: Type):
        pruned_tp = tp.prune()
        if isinstance(pruned_tp, Basic):
            return pruned_tp

        if isinstance(pruned_tp, TVar):
            if is_generic(pruned_tp, non_generic_types):
                looked = table.get(pruned_tp)
                if looked is None:
                    new_var = TVar.new()
                    table[pruned_tp] = new_var
                    return new_var
                return looked
            return pruned_tp
        return TFnSig(*map(freshrec, pruned_tp))

    return freshrec(t)


Env = typing.List[typing.Tuple[str, Type]]


def get_type_of_symbol(name: str, env: Env, non_generic_types: typing.Set[TVar]):
    result = next((v for k, v in env if name == k), None)
    if result is None:
        raise NameError(name)
    return fresh(result, non_generic_types)


def unify(t1: Type, t2: Type):
    if isinstance(t2, Flow):
        t2 = t2.types[-1]

    pr1, pr2 = t1.prune(), t2.prune()
    if isinstance(pr1, Basic):
        if isinstance(pr2, Basic):
            if pr1 != pr2:
                raise TypeError("type mismatch.")
        elif isinstance(pr2, TVar):
            # pr2 = TVar(_, None, _)
            assert pr2.ty is None
            pr2.ty = pr1
        else:
            raise TypeError

    elif isinstance(pr1, TVar):
        # pr1 = TVar(_, None, _)
        assert pr1.ty is None

        if isinstance(pr2, Basic):
            pr1.ty = pr2
        elif pr1 != pr2:
            if occurs_in_type(pr1, pr2):
                raise TypeError("recursive unification")
            pr1.ty = pr2


    elif isinstance(pr1, TFnSig):
        if isinstance(pr2, TVar):
            unify(pr2, pr1)
        elif isinstance(pr2, TFnSig):
            unify(pr1.left, pr2.left)
            unify(pr1.right, pr2.right)
        elif isinstance(pr2, Basic):
            raise TypeError


def specify_type(ty: Type, env: Env):
    if isinstance(ty, TVar):
        specify_type(ty.ty, env)
        name = ty.name
        looked = next((v for _, v in env if v.name == name), None)

        if looked is None:
            if ty.ty is None:
                env.append((name, ty))
                return ty

            raise NameError(name)
        else:
            ty.ty = looked

    elif isinstance(ty, TFnSig):
        specify_type(ty.left, env)
        specify_type(ty.right, env)
    elif isinstance(ty, Basic):
        return ty


def analyse(term_: Term, env_: Env):
    def analyserec(term: Term, env: Env, non_generic_types: typing.Set[Type]):
        def analyse_it(_t):
            return analyserec(_t, env, non_generic_types)

        if isinstance(term, Id):
            return get_type_of_symbol(term.repr_str, env, non_generic_types)

        elif isinstance(term, App):
            fun_ty, arg_ty = map(analyse_it, term)
            ret_ty = TVar.new()
            unify(TFnSig.new(arg_ty, ret_ty), fun_ty)
            return ret_ty
        elif isinstance(term, Lam):
            arg, body = term.arg, term.ret
            arg_ty = TVar.new()
            new_env = env.copy()
            new_env.append((arg.repr_str, arg_ty))
            new_free_types = non_generic_types.copy()
            new_free_types.add(arg_ty)

            ret_ty = analyserec(body, new_env, new_free_types)
            return TFnSig.new(arg_ty, ret_ty)

        elif isinstance(term, Let):
            tag, value, body = term.tag, term.value, term.do

            new_ty = TVar.new()
            if isinstance(tag, Annotate):
                manual_ty = tag.ty
                specify_type(manual_ty, env)
                tag = tag.term
                unify(new_ty, manual_ty)

            new_env = env.copy()
            new_env.append((tag.repr_str, new_ty))
            new_free_types = non_generic_types.copy()
            non_generic_types.add(new_ty)
            value_ty = analyserec(value, new_env, new_free_types)
            unify(new_ty, value_ty)
            return analyserec(body, new_env, non_generic_types)
        elif isinstance(term, Const):
            return Basic(type(term.repr).__name__)

        elif isinstance(term, Annotate):
            ty = term.ty
            t = analyse_it(term.term)
            specify_type(term.ty, env)

            unify(t, ty)
            return t


        elif isinstance(term, Alias):
            specify_type(term.typ, env)
            name = term.name
            if any(_ for _, v in env if name == v.name):
                raise NameError("duplicated type {}.".format(name))

            # if support overwrite, we can implement a nested type env with
            #  correct inheritances.
            n_ty = TVar.new(None, term.typ, name)
            env.append(('.{}'.format(name), n_ty))
            return n_ty

        elif isinstance(term, Stmts):
            return Flow(tuple(analyse_it(each) for each in term.terms))

        raise TypeError

    return analyserec(term_, env_, set())

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
