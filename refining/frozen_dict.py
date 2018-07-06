import operator
import typing
import itertools

from Redy.Tools.Hash import hash_from_stream

K = typing.TypeVar('K')

V = typing.TypeVar('V')


class FrozenDict(typing.Mapping[K, V]):

    def __init__(self, **kwargs: typing.Mapping[K, V]):
        self._ = dict(sorted(kwargs.items()))
        self.len = len(kwargs)
        self._hash = None

    def __getitem__(self, item):
        return self._[item]

    def __len__(self):
        return self.len

    def __contains__(self, item: K):
        return item in self._

    def __hash__(self):
        if self._hash is None:
            self._hash = hash_from_stream(self.len + 1, itertools.chain((hash(FrozenDict),), map(hash, self._.items())))
        return self._hash

    def __eq__(self, other: 'FrozenDict'):
        if other.__class__ is not FrozenDict:
            return False
        if hash(self) != hash(other):
            return False

        return all(map(operator.eq, self.items(), other.items()))

    def __iter__(self):
        return iter(self._)

    def items(self):
        return self._.items()

    # noinspection PyMethodOverriding
    def get(self, k: K, default: V):
        return self._.get(k, V)

    def copy(self):
        return self._.copy()
