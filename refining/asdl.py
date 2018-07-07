from .basic import *


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
    ret: 'Term'

    def __repr__(self):
        return 'λ{}.{}'.format(self.arg, self.ret)


class App(Eq, Hint):
    fn: 'Term'
    arg: 'Term'

    def __repr__(self):
        return '{} {}'.format(self.fn, self.arg)


class Let(Eq, Hint):
    tag: 'Id'
    value: 'Term'
    do: 'Term'

    def __repr__(self):
        return 'let {} = {} in {}'.format(self.tag, self.value, self.do)


class Alias(Eq, Hint):
    name: str
    typ: 'Type'

    def __repr__(self):
        return 'type {} = {}'.format(self.name, self.typ)


class Stmts(Eq, Hint):
    terms: typing.Tuple['Term']

    def __repr__(self):
        return '{{\n{}\n}}'.format('\n'.join(map(repr, self.terms)))


Term = typing.Union[Id, Let, Lam, App, Const, Alias, Stmts]
