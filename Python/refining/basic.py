import typing
import operator
from functools import reduce

Hint = typing.NamedTuple

globals()['Hint'] = object


class Eq:
    n: int

    def __init__(self, *args):
        annotations = getattr(self, '__annotations__', None)
        if annotations is None:
            self.n = 0
            return
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
        return reduce(operator.xor, (hash(getattr(self, e)) for e in self.__annotations__), hash(self.__class__))

    def __str__(self):
        return repr(self)

    def update(self, update_pairs):
        if isinstance(update_pairs, dict):
            update_pairs = update_pairs.items()

        cls = self.__class__
        new_one = cls.__new__(cls)
        new_one.n = self.n
        annotations = getattr(self, 'annotations', None)
        if annotations is not None:
            for k, v in zip(annotations, self):
                setattr(self, k, next((v for _k, _v in update_pairs if _k == k), v))

    def collect(self, which: typing.Callable[['Eq'], bool], nested=False):
        for each in self:
            if isinstance(each, Eq) and which(each):
                yield each
                if nested:
                    yield from each.collect(which, True)


class UniqueHash:
    def __hash__(self):
        return id(self)
