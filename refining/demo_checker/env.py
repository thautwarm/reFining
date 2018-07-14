from .primitive_types import *
from ..type_env import Env


def make_default_env():
    named_types = {
        'unit': unit_type, 'int': int_type, 'str': str_type, 'bool': bool_type, 'float': float_type, 'any': any_type
    }
    return Env(named_types, {}, {})
