from refining.typ import *
from refining.non_typ import *
from refining.py_overloader import def_pattern
from typing import overload
from Redy.Opt.ConstExpr import optimize, const

import sympy

_ST = typing.Set[typing.Union[sympy.Symbol, Undecided]]
_SKV = typing.Dict[typing.Union[sympy.Symbol, Undecided], typing.Union[sympy.Number, TypeClass]]

_undef = object()


class UndecidedTypeCollector:
    def __init__(self, _: _ST):
        self._ = _
        self.discrt = set()

    def add(self, var):
        return self._.add(var)

    def __iter__(self):
        return iter(self._)

    def update(self, *args, **kwargs):
        self._.update(*args)

    def add_discrete(self, d: DiscreteUnion):
        self.discrt.add(d)

    def find_discrete(self):
        return bool(self.discrt)


class TypeDecider:
    def __init__(self, _: _SKV, discrt: _SKV):
        self._ = _
        self.discrt = discrt

    def __iter__(self):
        return iter(self._)

    def items(self):
        return self._.items()

    def get(self, k, default):
        return self._.get(k, default)

    def trans_discrt(self, d: DiscreteUnion):
        return self.discrt[d]


UTC = UndecidedTypeCollector

TD = TypeDecider


@def_pattern
def _infer_undecided(a: TypeClass, collect: UTC):
    return type(a)


@overload
def _infer_undecided(a: BasicType, collect: UTC):
    pass


@overload
def _infer_undecided(a: TupleType, collect: UTC):
    for each in a.types:
        _infer_undecided(each, collect)


@overload
def _infer_undecided(a: UnionType, collect: UTC):
    for each in a.cases.values():
        _infer_undecided(each, collect)


@overload
def _infer_undecided(a: Representation, collect: UTC):
    for each in a.components:
        _infer_undecided(each, collect)


@overload
def _infer_undecided(a: Slot, collect: UTC):
    if a not in collect:
        collect.add(a)


@overload
def _infer_undecided(a: DiscreteUnion, collect: UTC):
    collect.add_discrete(
            a)  # raise TypeError("ConcreteUnion should be splited into concrete cases and then do type inferrence.\n")


@overload
def _infer_undecided(a: Any, collect: UTC):
    pass


@overload
def _infer_undecided(a: RecordType, collect: UTC):
    for each in a.fields.values():
        _infer_undecided(each, collect)


@overload
def _infer_undecided(a: Value, collect: UTC):
    collect.update(a.free_symbols)


@overload
def _infer_undecided(a: Function, collect: UTC):
    _infer_undecided(a.arg, collect)
    _infer_undecided(a.eqg, collect)
    _infer_undecided(a.ret, collect)


@overload
def _infer_undecided(a: Equation, collect: UTC):
    _infer_undecided(a.left, collect)
    _infer_undecided(a.right, collect)


@overload
def _infer_undecided(a: EquationGroup, collect: UTC):
    for each in a.eqs:
        _infer_undecided(each, collect)


@overload
def _infer_undecided(a: TypeClass, collect: UTC):
    raise SystemError


def infer_undecided(a, collect=None):
    s = UTC(set() if collect is None else collect)
    _infer_undecided(a, s)
    return s


@def_pattern
def _infer_out(a: TypeClass, collect: TD):
    """
    the method may not infer out the specific types, instead a partial
    inferred term would be given.
    """
    return type(a)


@overload
def _infer_out(a: TypeClass, collect: TD):
    raise SystemError


@overload
def _infer_out(a: BasicType, collect: TD):
    return a


@overload
def _infer_out(a: TupleType, collect: TD):
    return TupleType(tuple(_infer_out(each, collect) for each in a.types), a.extra_data)


@overload
def _infer_out(a: UnionType, collect: TD):
    return UnionType(FrozenDict(**{k: _infer_out(v, collect) for k, v in a.cases.items()}), a.extra_data)


@overload
def _infer_out(a: Representation, collect: TD):
    return Representation(a.name_str, tuple(_infer_out(e, collect) for e in a.components), a.extra_data)


@overload
def _infer_out(a: Slot, collect: TD):
    n = collect.get(a, _undef)
    if n is not _undef:
        return n
    return a


@overload
def _infer_out(a: DiscreteUnion, collect: TD):
    """
    discrete union will be inferred at last.
    for instance:

        flow{
            a: (A | B)
            ...

            d: (B | C | D)
        }

        we then should create "2 * 3"(Cartesian Product of each discrete union's branch number)
        branches to infer them respectively.
        Note:

        - if branches all fail, then inference fails and report type checking err.
        - if only one branches succeed, take the first branch result as the inference result.
        - if many succeed, union them all as a new discrete union.
    """
    return collect.trans_discrt(a)


@overload
def _infer_out(a: Any, collect: TD):
    return a


@overload
def _infer_out(a: RecordType, collect: TD):
    return RecordType(FrozenDict(**{k: _infer_out(v, collect) for k, v in a.fields.items()}), a.extra_data)


@overload
def _infer_out(a: Value, collect: TD):
    return a.subs(((k, v) for k, v in collect.items() if isinstance(k, sympy.Symbol)))


@overload
def _infer_out(a: Function, collect: TD):
    arg = _infer_out(a.arg, collect)
    eqg = _infer_out(a.eqg, collect)
    ret = _infer_out(a.ret, collect)
    return Function(arg, eqg, ret, a.extra_data)


@overload
def _infer_out(a: EquationGroup, collect: TD):
    return EquationGroup(tuple(_infer_out(each, collect) for each in a.eqs), a.extra_data)


@overload
def _infer_out(a: Equation, collect: TD):
    """
    The most important
    """
    from refining.solver import solve
    l, r = solve(a.left, a.right)
    return Equation(l, r, a.extra_data)
