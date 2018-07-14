from ..asdl import Const
from ..typevars import make_basic, bool_type, unit_type

int_type = make_basic('int')
float_type = make_basic('float')
str_type = make_basic('str')
any_type = make_basic('any')

type_map = {
    str: str_type, float: float_type, int: int_type, bool: bool_type, None.__class__: unit_type
}


def type_mapper(term: Const):
    return type_map.get(type(term.repr), any_type)
