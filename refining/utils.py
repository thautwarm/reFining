import typing
import functools

K = typing.TypeVar('K')
V = typing.TypeVar('V')


def raise_exc(e):
    raise e


def binding(cls):
    def call(_):
        return cls

    return call


class FrozenDict(typing.Mapping[K, V]):

    def __init__(self, *args, **kwargs):
        self._ = dict(*args, **kwargs)
        self.len = len(self._)
        self._hash = None

    def update(self, *args, **kwargs):
        return {**self._, **dict(*args), **kwargs}

    def __getitem__(self, item):
        return self._[item]

    def __len__(self):
        return self.len

    def __contains__(self, item: K):
        return item in self._

    def __hash__(self):
        if self._hash is None:
            self._hash = functools.reduce(lambda a, b: a & hash(b), self._.items(), hash(FrozenDict))
        return self._hash

    def __eq__(self, other: 'FrozenDict'):
        if other.__class__ is not FrozenDict:
            return False
        if hash(self) != hash(other):
            return False

        return all(map(lambda a, b: a == b, self._.items(), other._.items()))

    def __iter__(self):
        return iter(self._)

    def items(self) -> typing.Iterable[typing.Tuple[K, V]]:
        return self._.items()

    # noinspection PyMethodOverriding
    def get(self, k: K, default: typing.Optional[V] = None):
        return self._.get(k, V)

    def copy(self):
        return self._.copy()

    def __str__(self):
        return str(self._)
