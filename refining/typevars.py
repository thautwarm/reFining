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
        return right.op == left.op and left.left.unify(right.left) and left.right.unify(right.right)

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


class TypeImpl(TypeVar):
    op: TypeOperator
    left: 'TypeImpl'
    right: 'TypeImpl'

    def __init__(self, op: 'TypeOperator', left: 'TypeImpl', right: 'TypeImpl'):
        self.op = op
        self.left = left
        self.right = right

    def iter_fields(self):
        yield from self.left.iter_fields()
        yield from self.right.iter_fields()

    def prune(self) -> typing.Tuple[bool, 'TypeVar']:
        is_left_pruned, left = self.left.prune()
        if is_left_pruned:
            self.left = left
        is_right_pruned, right = self.left.prune()

        if is_right_pruned:
            self.right = right
        return is_left_pruned or is_right_pruned, self

    def fresh_impl(self, fresh_args: TypeEnvFreshPair):
        is_left_freshed, left = self.left.fresh(fresh_args)
        is_right_freshed, right = self.right.fresh(fresh_args)
        if is_left_freshed or is_right_freshed:
            return True, TypeImpl(self.op, left, right)
        return False, self

    def occur_in(self, types):
        return self.left.occur_in(types) or self.right.occur_in(types)

    def unify_impl(self, other: 'TypeImpl'):
        return self.op.operator_unify(self, other)

    def __repr__(self):
        left = self.left
        left_repr = ('({!r})'.format if isinstance(left, TypeImpl) else repr)(left)
        return '{} {!r} {!r}'.format(left_repr, self.op, self.right)


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
        return False, self

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
                return False  # raise TypeError("recursive type.")
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
join_op = TypeOperator('*')
induct_op = TypeOperator('of')
union_op = TypeOperator('|')
statement_op = TypeOperator(';')


def make_basic(name: str):
    return Basic(name)


def make_function(left, right):
    return TypeImpl(func_op, left, right)


def make_join(left, right):
    return TypeImpl(join_op, left, right)


def make_union(left, right):
    return TypeImpl(union_op, left, right)


def make_induct(left, right):
    return TypeImpl(induct_op, left, right)


def make_statement(left, right):
    return TypeImpl(statement_op, left, right)


if __name__ == '__main__':
    a = Undecided(None)
    b = Undecided(None)
    i32 = Basic("i32")
    tp1 = make_join(i32, a)

    print(a.occur_in([tp1]))
