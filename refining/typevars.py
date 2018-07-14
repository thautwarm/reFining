import typing
import abc

_not_initialized = object()
NonGenericTypeVars = typing.Set['Undecided']

GenericTypeVarMap = typing.Dict['Undecided', 'Undecided']

TypeEnvFreshPair = typing.Tuple[NonGenericTypeVars, GenericTypeVarMap]


class TypeOperator:
    representation: str
    unify_method: typing.Optional[typing.Callable[['TypeImpl', 'TypeImpl'], bool]]

    def __init__(self, representation: str, unify_method=None):
        self.representation = representation
        self.unify_method = unify_method

    def __eq__(self, other):
        return self is other

    def __repr__(self):
        return self.representation

    @staticmethod
    def default_unify(left: 'TypeImpl', right: 'TypeImpl'):
        if not isinstance(right, TypeImpl):
            return False

        return right.op == left.op and len(left.components) == len(right.components) and all(
                left_child.unify(right_child) for left_child, right_child in zip(left.components, right.components))

    def operator_unify(self, left: 'TypeImpl', right: 'TypeImpl') -> bool:
        if self.unify_method:
            return self.unify_method(left, right)
        return self.default_unify(left, right)


class TypeVar(abc.ABC):
    t = typing.Union['TypeVar', 'TypeImpl', 'Basic', 'Undecided']

    @abc.abstractmethod
    def iter_fields(self) -> typing.Iterable['TypeVar']:
        raise NotImplemented

    @abc.abstractmethod
    def prune(self) -> typing.Tuple[bool, 'TypeVar']:
        raise NotImplemented

    @abc.abstractmethod
    def occur_in(self, types: typing.Iterable['TypeVar']) -> bool:
        raise NotImplemented

    @abc.abstractmethod
    def unify_impl(self, other: 'TypeVar') -> bool:
        raise NotImplemented

    @abc.abstractmethod
    def fresh_impl(self, fresh_args: TypeEnvFreshPair):
        raise NotImplemented

    def unify(self, other: 'TypeVar'):
        _, left = self.prune()
        _, right = other.prune()
        if isinstance(right, Undecided):
            left, right = right, left
        return left.unify_impl(right)

    def fresh(self, fresh_args: TypeEnvFreshPair) -> typing.Tuple[bool, 'TypeVar.t']:
        _, pruned_ty = self.prune()
        return pruned_ty.fresh_impl(fresh_args)

    @property
    def pruned(self):
        return self.prune()[1]

    def freshed(self, fresh_args: TypeEnvFreshPair):
        return self.fresh(fresh_args)[1]

    def unified(self, other):
        if not self.unify(other):
            raise TypeError


class TypeImpl(TypeVar):
    op: TypeOperator
    components: typing.List['TypeVar']

    def __init__(self, op: 'TypeOperator', components):
        self.op = op
        self.components = components

    def iter_fields(self):
        for each in self.components:
            yield from each.iter_fields()

    def prune(self) -> typing.Tuple[bool, 'TypeVar']:
        def stream():
            components = self.components
            for idx, elem in enumerate(components):
                is_pruned, elem = elem.prune()
                if is_pruned:
                    components[idx] = elem
                yield is_pruned

        return any(tuple(stream())), self

    def fresh_impl(self, fresh_args: TypeEnvFreshPair):
        is_freshed_booleans, components = zip(*map(lambda _: TypeVar.fresh(_, fresh_args), self.components))

        if any(is_freshed_booleans):
            return True, TypeImpl(self.op, list(components))
        return False, self

    def occur_in(self, types):
        return any(elem.occur_in(types) for elem in self.components)

    def unify_impl(self, other: 'TypeImpl'):
        return self.op.operator_unify(self, other)

    def __repr__(self):
        # right = self.right
        # right_str = ('({!r})'.format if isinstance(right, TypeImpl) else repr)(right)
        return "({})".format(' {!r} '.format(self.op).join(map(repr, self.components)))


type_var_unique_id = 0


class Basic(TypeVar):
    name: str
    _unique_id = id

    def __init__(self, name):
        global type_name_unique_id
        self.name = name
        self._unique_id = type_name_unique_id
        type_name_unique_id += 1

    def prune(self):
        return True, self

    def iter_fields(self):
        yield self

    def occur_in(self, types: typing.Iterable['TypeVar']) -> bool:
        return False

    def fresh_impl(self, fresh_args: TypeEnvFreshPair):
        return False, self

    def unify_impl(self, other: 'TypeVar'):
        return self is other

    def __repr__(self):
        return '{}`{}'.format(self.name, self._unique_id)


class RecordType(TypeVar):
    fields: typing.Dict[str, TypeVar]

    def __init__(self, fields):
        self.fields = dict(sorted(fields.items()))

    def prune(self) -> typing.Tuple[bool, 'TypeVar']:
        def stream():
            components = tuple(self.fields.items())
            fields = self.fields
            for k, v in components:
                is_pruned, v = v.prune()
                if is_pruned:
                    fields[k] = v
                yield is_pruned

        return any(tuple(stream())), self

    def iter_fields(self):
        for each in self.fields.values():
            yield from each.iter_fields()

    def occur_in(self, types: typing.Iterable['TypeVar']):
        _, undecided = self.prune()
        for each in types:
            if undecided in each.iter_fields():
                return True
        return False

    def unify_impl(self, other: 'TypeVar'):
        if not isinstance(other, RecordType):
            return False

        return all((k1 == k2 and v1.unify(v2)) for (k1, v1), (k2, v2) in zip(self.fields.items(), other.fields.items()))

    def fresh_impl(self, fresh_args: TypeEnvFreshPair):
        is_freshed_booleans, values = zip(*map(lambda _: TypeVar.fresh(_, fresh_args), self.fields.values()))

        if any(is_freshed_booleans):
            return True, RecordType(dict(zip(self.fields.keys(), values)))
        return False, self


type_name_unique_id = 0


class Undecided(TypeVar):
    ref: typing.Optional[TypeVar]
    _unique_id: int

    def __init__(self, ref=None):
        global type_var_unique_id
        self.ref = ref
        self._unique_id = type_var_unique_id
        type_var_unique_id += 1

    def prune(self):
        ref = self.ref
        if ref is None:
            return False, self
        _, self.ref = ref.prune()
        return True, ref

    def iter_fields(self):
        yield self

    def occur_in(self, types: typing.Iterable[TypeVar]) -> bool:
        _, undecided = self.prune()
        for each in types:
            if undecided in each.iter_fields():
                return True
        return False

    def unify_impl(self, other: 'TypeVar'):
        assert self.ref is None
        if isinstance(other, Basic):
            self.ref = other
        elif self is not other:
            if self.occur_in([other]):
                raise TypeError("recursive type.")
            self.ref = other

        # else self is other
        return True

    def fresh_impl(self, fresh_args: TypeEnvFreshPair):
        non_generic_set, generic_dict = fresh_args
        if not self.occur_in(non_generic_set):
            looked = generic_dict.get(self)
            if looked is None:
                freshed_var = generic_dict[self] = Undecided(None)
                return True, freshed_var
            return True, looked
        return False, self

    def __repr__(self):
        ref = self.ref
        return '#.{}'.format(self._unique_id) if not ref else repr(ref)


func_op = TypeOperator('=>')
invertible_func_op = TypeOperator('<=>')
join_op = TypeOperator('*')
induct_op = TypeOperator('of')
union_op = TypeOperator('|')
statement_op = TypeOperator(';')


def make_basic(name: str):
    return Basic(name)


def make_function(left, right):
    return TypeImpl(func_op, [left, right])


def make_invertible_function(left, right):
    return TypeImpl(invertible_func_op, [left, right])


def make_join(components):
    return TypeImpl(join_op, list(components))


def make_union(left, right):
    return TypeImpl(union_op, [left, right])


def make_induct(basic, *args):
    assert isinstance(basic, Basic)
    return TypeImpl(induct_op, [basic, *args])


def make_statement(left, right):
    return TypeImpl(statement_op, [left, right])


# the essential basic types
bool_type = make_basic('bool')
unit_type = make_basic('unit')

if __name__ == '__main__':
    a = Undecided(None)
    b = Undecided(None)
    i32 = Basic("i32")
    tp1 = make_join([i32, a])

    print(a.occur_in([tp1]))
