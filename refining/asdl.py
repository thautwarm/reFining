from .basic import *


class Unit(Eq):
    def __repr__(self):
        return '()'


unit = Unit()


class Const(Eq, Hint):
    repr: typing.Any

    def __repr__(self):
        return repr(self.repr)


class Id(Eq, Hint):
    repr_str: str

    def __repr__(self):
        return self.repr_str


class Lam(Eq, Hint):
    arg: 'Id'
    annotate: 'TypeTerm'
    ret: 'Term'

    def __repr__(self):
        if not self.annotate:
            return 'λ{!r}.{!r}'.format(self.arg, self.ret)
        return 'λ{!r}: {!r}.{!r}'.format(self.arg, self.annotate, self.ret)


class App(Eq, Hint):
    fn: 'Term'
    arg: 'Term'

    def __repr__(self):
        return '{} {}'.format(self.fn, self.arg)


class Let(Eq, Hint):
    """
    for python is lazy in terms of function definitions,
    `let rec` is forced to take place here.
    """
    tag: str
    annotate: 'TypeTerm'
    value: 'Term'
    do: 'Term'

    def __repr__(self):
        annotate = self.annotate
        annotate_str = ': {!r}'.format(annotate) if annotate else ''

        return 'let {}{} = {!r} in {!r}'.format(self.tag, annotate_str, self.value, self.do)


class Tuple(Eq, Hint):
    items: typing.Tuple['Term', ...]

    def __repr__(self):
        return '({})'.join(map(repr, self.items))


class TypeTerm:
    def __str__(self):
        return repr(self)

    pass


class TypeSym(Eq, TypeTerm, Hint):
    name: str

    def __repr__(self):
        return self.name


class TypeSlot(Eq, TypeTerm, Hint):
    name: str

    def __repr__(self):
        return '\'{}'.format(self.name)


class TypeInduct(Eq, TypeTerm, Hint):
    sym: TypeSym
    ty: TypeTerm

    def __repr__(self):
        return '{!r} of {!r}'.format(self.sym, self.ty)


class TypeDef(Eq, TypeTerm, Hint):
    induct: TypeInduct
    impl: TypeTerm

    def __repr__(self):
        return 'type {} = {!r}'.format(self.induct, self.impl)


class TypeAbbr(Eq, TypeTerm, Hint):
    name: str
    impl: TypeTerm

    def __repr__(self):
        return 'type {} = {!r}'.format(self.name, self.impl)


class TypeJoin(Eq, TypeTerm, Hint):
    left: TypeTerm
    right: TypeTerm

    def __repr__(self):
        right_str = ('({!r})'.format if not isinstance(self.right, (TypeSym, TypeSlot)) else repr)(self.right)
        return '{!r} * {}'.format(self.left, right_str)


class TypeFunction(Eq, TypeTerm, Hint):
    left: TypeTerm
    right: TypeTerm

    def __repr__(self):
        right_str = ('({!r})'.format if not isinstance(self.right, (TypeSym, TypeSlot)) else repr)(self.right)
        return '{!r} -> {}'.format(self.left, right_str)


class Annotate(Eq, Hint):
    term: "Term"
    type: TypeTerm

    def __repr__(self):
        return '{!r} : {!r}'.format(self.term, self.type)


class Stmts(Eq, Hint):
    terms: typing.Tuple['Term']

    def __repr__(self):
        return '{{\n{}\n}}'.format('\n'.join(map(repr, self.terms)))


Term = typing.Union[Id, Let, Lam, App, Const, Stmts, Annotate]

"""
type S = Int * Int * (Int * Int * Int)
"""
