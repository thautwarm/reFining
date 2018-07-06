from Redy.Magic.Pattern import Pattern
import types


def def_pattern(func: types.FunctionType):
    glob = func.__globals__
    register_fn = Pattern(func)

    def wrap_pattern_overload(case: types.FunctionType):
        pat = next(v for k, v in case.__annotations__.items())
        return register_fn.match(pat)(case)

    glob['overload'] = wrap_pattern_overload

    return register_fn
