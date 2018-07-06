import typing
from Redy.Tools.Hash import hash_from_stream
from refining.frozen_dict import FrozenDict
import sympy, itertools


class TypeClass:

    def __hash__(self):
        annotations = self.__annotations__
        return hash_from_stream(len(annotations) + 1, itertools.chain((hash(self.__class__),),
                                                                      (hash(getattr(self, attr)) for attr in
                                                                       annotations.keys())))

    def __eq__(self, other: 'TypeClass'):
        annotations = self.__annotations__
        if self.__class__ is not other.__class__:
            # assert self.__class__ is other.__class__
            # checking if id of annotations matches also means to check if type id matches.
            return False
        return all(map(lambda attr: getattr(self, attr) == getattr(other, attr), annotations))

    def __repr__(self):
        return str(self)


class BasicType(TypeClass):
    name_str: str

    def __init__(self, name_str: str, extra_data=None):
        self.name_str = name_str
        self.extra_data = extra_data

    def __str__(self):
        return '{}'.format(self.name_str)


class TupleType(TypeClass):
    types: typing.Tuple[TypeClass, ...]

    def __init__(self, types: typing.Tuple[TypeClass, ...], extra_data=None):
        self.types = types
        self.extra_data = extra_data

    def __str__(self):
        return '({})'.format(' '.join(map(str, self.types)))


class UnionType(TypeClass):
    """
    This union type refers to the tag union, like
        - rust union
            https://doc.rust-lang.org/reference/items/unions.html
        - ML discriminate unions(ADT).

    **A strongly typed language will never use unions with memory sharing.**
    """

    cases: FrozenDict[str, TypeClass]

    def __init__(self, cases: FrozenDict[str, TypeClass], extra_data=None):
        self.cases = cases
        self.extra_data = extra_data

    def __str__(self):
        return 'ADT({})'.format(self.cases)


class Representation(TypeClass):
    name_str: str
    components: typing.Tuple[TypeClass, ...]

    def __init__(self, name_str: str, components: typing.Tuple[TypeClass, ...], extra_data=None):
        self.name_str = name_str
        self.components = components
        self.extra_data = extra_data

    def __str__(self):
        return '{} of {}'.format(self.name_str, '*'.join(map(str, self.components)))


class Undecided(TypeClass):
    pass


class Slot(Undecided):
    name_str: str

    def __init__(self, name_str: str, extra_data=None):
        self.name_str = name_str
        self.extra_data = extra_data

    def __str__(self):
        return '\'{}'.format(self.name_str)


class DiscreteUnion(Undecided):
    """
    sometimes an occurrence cannot be completely decided, we use this one to represent the type instead of Any
    """
    types: typing.Tuple[TypeClass, ...]

    def __str__(self):
        return '({})'.format('|'.join(map(str, self.types)))


class Any(Undecided):

    def __init__(self, extra_data):
        self.extra_data = extra_data


class RecordType(TypeClass):
    fields: FrozenDict[str, TypeClass]

    def __init__(self, fields: FrozenDict[str, TypeClass], extra_data=None):
        self.fields = fields
        self.extra_data = extra_data

    def __str__(self):
        return 'Record({})'.format(self.fields)


class Value(sympy.Expr, TypeClass):

    def __init__(self, *args, extra_data=None):
        sympy.Expr.__init__(*args)
        self.extra_data = extra_data
