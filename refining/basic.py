from Redy.Tools.Hash import hash_from_stream
import typing

Hint = typing.NamedTuple

isa = lambda a, b: a.__class__ is b
globals()['Hint'] = object
globals()['isinstance'] = isa


class Eq:
    n: int

    def __init__(self, *args):
        annotations = self.__annotations__
        self.n = len(annotations)
        for k, v in zip(annotations, args):
            setattr(self, k, v)

    def __iter__(self):
        for _ in self.__annotations__:
            yield getattr(self, _)

    def __eq__(self, other):
        if self.__class__ is not other.__class__:
            return False
        return all(getattr(self, k) == getattr(other, k) for k in self.__annotations__)

    def __hash__(self):
        return hash_from_stream(self.n, (hash(getattr(self, e)) for e in self.__annotations__))

    def __str__(self):
        return repr(self)


class UniqueHash:
    def __hash__(self):
        return id(self)
