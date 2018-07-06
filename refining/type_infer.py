from refining.typ import *
from refining.py_overloader import def_pattern
from typing import overload
from Redy.Opt.ConstExpr import optimize, const

import sympy

_ST = typing.Set[typing.Union[sympy.Symbol, Undecided]]


@def_pattern
def _infer_undecided(a: TypeClass, collect: _ST):
    return type(a)


@overload
def _infer_undecided(a: BasicType, collect: _ST):
    pass


@overload
def _infer_undecided(a: TupleType, collect: _ST):
    for each in a.types:
        _infer_undecided(each, collect)


@overload
def _infer_undecided(a: UnionType, collect: _ST):
    for each in a.cases.values():
        _infer_undecided(each, collect)


@overload
def _infer_undecided(a: Representation, collect: _ST):
    for each in a.components:
        _infer_undecided(each, collect)


@overload
def _infer_undecided(a: Slot, collect: _ST):
    if a not in collect:
        collect.add(a)


@overload
def _infer_undecided(a: ConcreteUnion, collect: _ST):
    raise TypeError("ConcreteUnion should be splited into concrete cases and then do type inferrence.\n")


@overload
def _infer_undecided(a: Any, collect: _ST):
    pass


@overload
def _infer_undecided(a: RecordType, collect: _ST):
    for each in a.fields.values():
        _infer_undecided(each, collect)


@overload
def _infer_undecided(a: Value, collect: _ST):
    collect.update(a.free_symbols)


@overload
def _infer_undecided(a: TypeClass, collect: _ST):
    raise SystemError


def infer_undecided(a, collect=None):
    s = set() if collect is None else collect
    _infer_undecided(a, s)
    return s
