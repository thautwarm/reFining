import typing
import operator
import abc



class TypeImpl(abc.ABC):
    t = typing.Union['ADT', 'Record', 'Tuple', 'ADT', 'Induct', 'Basic', 'Undecided']

    def __init__(self, components: typing.Dict[object, 'TypeImpl.t']):
        self.components = components

    def prune(self) -> typing.Tuple[bool, 'TypeImpl']:
        components = self.components
        pruned_map = {key: impl for key, (is_pruned, impl) in ((key, impl.prune()) for key, impl in components.items())
                      if is_pruned}
        if pruned_map:
            components.update(pruned_map)
            return True, self
        return False, self

    def iter_fields(self):
        for each in self.components.values():
            yield from each.iter_fields()

    def occur_in(self, types: typing.Iterable['TypeImpl.t']):
        return any(map(lambda this: this.occur_in(types), self.iter_fields()))

    @property
    @abc.abstractmethod
    def components(self) -> typing.Dict[object, 'TypeImpl.t']:
        raise NotImplemented

    @components.setter
    @abc.abstractmethod
    def components(self, value: typing.Dict[object, 'TypeImpl.t']):
        raise NotImplemented

    def __eq__(self, other):
        return type(self) is type(other) and all(map(operator.eq, self.components, other.components))

    def __hash__(self):
        return id(self)

    def __repr__(self):
        cls_name = self.__class__.__name__
        return '{}({})'.format(cls_name, ', '.join(map(repr, self.components)))


class Tuple(TypeImpl):
    elements: typing.Dict[int, TypeImpl.t]

    @property
    def components(self):
        return self.elements

    @components.setter
    def components(self, value):
        self.elements = value


class ADT(TypeImpl):
    cases: typing.Dict[str, TypeImpl.t]

    @property
    def components(self):
        return self.cases

    @components.setter
    def components(self, value):
        self.cases = value


class Record(TypeImpl):
    fields: typing.Dict[str, TypeImpl.t]

    @property
    def components(self):
        return self.fields

    @components.setter
    def components(self, value):
        self.fields = value


class Induct(TypeImpl):
    factors: typing.Dict[str, TypeImpl.t]

    @property
    def components(self):
        return self.factors

    @components.setter
    def components(self, value):
        self.factors = value


type_name_unique_id = 0


class Basic(TypeImpl):
    name: str
    _unique_id = id

    def __init__(self, name):
        global type_name_unique_id
        self.name = name
        self._unique_id = type_name_unique_id
        type_name_unique_id += 1

    @property
    def components(self):
        raise TypeError('Basic type holds no components.')

    @components.setter
    def components(self, value):
        raise TypeError('Basic type holds no components.')

    def prune(self):
        return False, self

    def iter_fields(self):
        yield self

    def occur_in(self, types: typing.Iterable['TypeImpl.t']) -> bool:
        return False

    def __repr__(self):
        return '{}`{}'.format(self.name, self._unique_id)

    def __hash__(self):
        return id(self)

    def __eq__(self, other):
        return self is other


type_var_unique_id = 0


class Undecided(TypeImpl):
    ref: typing.Optional[TypeImpl.t]
    _unique_id: int

    def __init__(self, ref):
        global type_var_unique_id
        self.ref = ref
        self._unique_id = type_var_unique_id
        type_var_unique_id += 1

    @property
    def components(self):
        raise TypeError('Basic type holds no components.')

    @components.setter
    def components(self, value):
        raise TypeError('Basic type holds no components.')

    def prune(self):
        ref = self.ref
        if ref is None:
            return False, self

        self.ref = ref.prune()
        return True, ref

    def iter_fields(self):
        yield self

    def occur_in(self, types: typing.Iterable['TypeImpl.t']) -> bool:
        _, undecided = self.prune()
        for each in types:
            if undecided in each.iter_fields():
                return True
        return False

    def __repr__(self):
        return '`#{}'.format(self._unique_id)

    def __hash__(self):
        return id(self)

    def __eq__(self, other):
        return self is other


if __name__ == '__main__':
    a = Undecided(None)
    b = Undecided(None)
    i32 = Basic("i32")
    tp1 = Tuple({'a': a, 'i': i32})

    print(a.occur_in([tp1]))
