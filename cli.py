from refining import setting

setting.debug = False
from refining.reunify import *
import rbnf.zero as ze
import readline, io, sys

KeyWords = ['let', 'in', 'type', 'fn']
env = make_default_env()
ze_exp = ze.compile('import simple.[*]', use='Grammar')


def err_write(info):
    if isinstance(sys.stderr, io.BufferedWriter):
        if isinstance(info, str):
            info = info.encode()
        return sys.stderr.write(info)
    else:
        if isinstance(info, bytes):
            info = info.decode()
        return sys.stderr.write(info)


def completer(text, state):
    options = tuple(option for option in KeyWords if option.startswith(text))

    if state < len(options):
        return options[state]
    else:
        return None


def main():
    readline.parse_and_bind("tab: complete")
    readline.set_completer(completer)

    cache = []

    def active():
        it = ze_exp.match(' '.join(cache))
        if it is None:
            raise Exception
        if it.state.end_index != len(it.tokens):
            idx = min(it.state.max_fetched, len(it.tokens) - 1)
            tk = it.tokens[idx]
            raise SyntaxError("line {}, column {}".format(tk.lineno, tk.colno))
        res = it.result
        if res is not None:
            print('=> ', analyze(res, env))

        # err_write(e.__class__.__name__ + ':' + str(e) + '\n')

        cache.clear()

    while True:
        line: str = input('reF> ' if not cache else '      ')
        if line.startswith(':'):
            line = line[1:]
            cmd, var = map(str.strip, line.split(' '))

            if cmd == 'show':
                if var.startswith('\''):
                    print(env.get_undecided_type(var[1:]))
                else:
                    print(env.get_named_type(var))
            elif cmd == 'env':
                if var.startswith('symbol'):
                    print(env.symbols)
                elif var.startswith('slot'):
                    print(env.undecided_types)
                elif var.startswith('type'):
                    print(env.named_types)
                else:
                    raise ValueError("invalid command")
            continue

        if line.endswith(';;'):
            line = line[:-2]
            if not line and not cache:
                continue
            cache.append(line)
            active()
        else:
            cache.append(line)


if __name__ == '__main__':
    main()

# statements = ["""let s = 1 in 1""", """fn x -> x """, """type S = int; let x : S = 1 in x;"""]
# env = [('.i', Basic('int')), ('.s', Basic('str'))]
# for each in statements:
#     term = ze_exp.match(each).result
#     print(analyse(term, env))

#
# print(analyse(Lam(Id("x"), App(Id("x"), Const(1))), []))
