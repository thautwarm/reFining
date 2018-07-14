from .typevars import *
from .typedef import *


class Env:
    inductive_type_impl: typing.Dict[Basic, ImplExpand] = {}
    named_types: typing.Dict[str, TypeVar]
    undecided_types: typing.Dict[str, Undecided]
    symbols: typing.Dict[str, TypeVar]

    parent: 'Env'
    subs: typing.List['Env']

    def __init__(self, named_types: typing.Dict[str, TypeVar], undecided_types: typing.Dict[str, Undecided],
                 symbols: typing.Dict[str, TypeVar], parent=None):
        self.named_types = named_types
        self.undecided_types = undecided_types
        self.symbols = symbols
        self.parent = parent
        self.subs = []

    def get_named_type(self, name: str) -> typing.Optional[Basic]:
        var = self.named_types.get(name, None)
        if var is None and self.parent:
            var = self.parent.get_named_type(name)
        return var

    def set_named_type(self, name: str, value: TypeVar):
        self.named_types[name] = value

    def get_symbol(self, name: str) -> typing.Optional[TypeVar]:
        var = self.symbols.get(name, None)
        if var is None and self.parent:
            var = self.parent.get_symbol(name)
        return var

    def set_symbol(self, name: str, value: TypeVar):
        self.symbols[name] = value

    def get_undecided_type(self, name: str) -> typing.Optional[Undecided]:
        var = self.undecided_types.get(name, None)
        if var is None and self.parent:
            var = self.parent.get_undecided_type(name)
        return var

    def set_undecided_type(self, name: str, value: TypeVar):
        self.undecided_types[name] = value

    def create_sub(self):
        # TODO: copy on write
        sub = Env({}, {}, {}, self)
        self.subs.append(sub)
        return sub

    def __repr__(self):
        return '{} | {}'.format(self.symbols, self.undecided_types)

    def get_implementation(self, branch: typing.Optional[str], basic: Basic, *args, **kwargs):
        assert isinstance(basic, Basic)
        impl = self.inductive_type_impl.get(basic, None)
        if impl is None:
            raise NameError("No type named `{}`.".format(basic))

        ty = impl.get_case(branch)
        non_generic = set(self.undecided_types.values())
        if impl.parameters:
            concrete_vars: typing.Dict[str, TypeVar] = dict(zip(impl.parameters, args), **kwargs)

            # TODO: use assignment expression to refactor
            partial_generic_vars = {it: concrete_vars.get(repr(it), None) for it in ty.iter_fields() if
                                    isinstance(it, Undecided) and not it.occur_in(non_generic)}
            generic_mapping = {template: var for template, var in partial_generic_vars.items() if var is not None}
        else:
            generic_mapping = {}

        ty = ty.freshed((non_generic, generic_mapping))
        if generic_mapping:
            self.undecided_types.update({repr(var): var for var in generic_mapping.values()})
        return ty
