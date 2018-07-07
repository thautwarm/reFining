from .asdl import *

_next_unique_id = 0
_next_unique_name = '#.1'
globals()['Hint'] = object
globals()['isinstance'] = isa


class TVar(UniqueHash, Eq, Hint):
    id: int
    ty: typing.Optional['Type']
    name: str

    @classmethod
    def new(cls, id: typing.Optional[int] = None, ty=None, name: typing.Optional[str] = None):
        return TVar(cls._make_id(id), ty, cls._make_name(name))

    @classmethod
    def _make_id(cls, id):
        global _next_unique_id
        if id is not None:
            return id
        id = _next_unique_id
        _next_unique_id += 1
        return id

    @classmethod
    def _make_name(cls, name):
        global _next_unique_name
        if name is not None:
            return name
        name = _next_unique_name
        a, b = _next_unique_name.split('.')
        _next_unique_name = '{}.{}'.format(a, int(b) + 1)
        return name

    def prune(self):
        ty = self.ty
        if ty is not None and isinstance(ty, TVar):
            self.ty = ty = ty.prune()
            return ty  # no box
        return self

    def __repr__(self):
        ty = self.ty
        return '{}[{}]'.format(self.name, self.ty) if ty else self.name


class TFnSig(Eq, Hint):
    left: 'Type'
    right: 'Type'

    def __repr__(self):
        return ' {} -> {}'.format(self.left, self.right)

    @classmethod
    def new(cls, tyl, tyr):
        return TFnSig(tyl, tyr)

    def prune(self):
        return self


class Basic(Eq, Hint):
    name: str

    def __repr__(self):
        return str(self.name)

    def prune(self):
        return self


class Flow(Eq, Hint):
    types: typing.Tuple['Type', ...]

    def __repr__(self):
        return '{{\n{}\n}}'.format('\n'.join(map(repr, self.types)))


Type = typing.Union[TVar, TFnSig, Basic, Flow]


class Annotate(Eq, Hint):
    term: 'Term'
    ty: Type

    def __repr__(self):
        return '{} : {}'.format(self.term, self.ty)
