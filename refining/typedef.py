from .typevars import *
from collections import OrderedDict


class ImplExpand(abc.ABC):
    parameters: typing.Dict[str, None]

    @abc.abstractmethod
    def get_case(self, case: typing.Optional[str]):
        raise NotImplemented


class Generic(ImplExpand):
    case: TypeVar

    def __init__(self, parameters, case: TypeVar):
        self.case = case
        self.parameters = parameters

    def get_case(self, _):
        return self.case


class ADTImpl(ImplExpand):
    cases: typing.Dict[str, TypeVar]

    def __init__(self, parameters, implementations):
        self.parameters = parameters
        self.cases = implementations

    def get_case(self, case: str):
        return self.cases[case]

    def match_cases(self, cases: typing.Set[str]) -> bool:
        return not cases.difference(self.cases)


"""
interface Iterable['a] =       
   next : Iterable 'a -> 'a 
   map  : ('a -> 'b) -> Iterable['a] -> Iterable['b]
"""
