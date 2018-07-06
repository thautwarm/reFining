from refining.typ import *
from Redy.Magic.Classic import record
from Redy.ADT.Core import data


class Equation(TypeClass):
    left: TypeClass
    right: TypeClass

    def __init__(self, left, right: TypeClass, extra_data=None):
        self.left = left
        self.right = right
        self.extra_data = extra_data


class EquationGroup(TypeClass):
    eqs: typing.Tuple[TypeClass, ...]

    def __init__(self, eqs: typing.Tuple[TypeClass, ...], extra_data=None):
        self.eqs = eqs
        self.extra_data = extra_data


class Function(TypeClass):
    arg: TypeClass
    eqg: EquationGroup
    ret: TypeClass

    def __init__(self, arg: TypeClass, eqg: EquationGroup, ret: TypeClass, extra_data=None):
        self.arg = arg
        self.eqg = eqg
        self.ret = ret
        self.extra_data = extra_data

    def __str__(self):
        return 'λ{}. {}[{}]'.format(self.arg, self.ret, self.eqg)
