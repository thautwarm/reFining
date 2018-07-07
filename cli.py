from refining.unification import *
import rbnf.zero as ze
import readline, io, sys 

KeyWords = ['let', 'in', 'type', 'fn']
env = [('.i', Basic('int')), ('.s', Basic('str'))]
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
    ctx = dict(__name__=__name__, __file__=__file__)
    readline.parse_and_bind("tab: complete")
    readline.set_completer(completer)

    count_parentheses = 0
    cache = []

    def active():
        nonlocal count_parentheses
        try:
            it = ze_exp.match(' '.join(cache))
            if it is None:
                raise Exception
            if it.state.end_index != len(it.tokens):
                idx = min(it.state.max_fetched, len(it.tokens)-1)
                tk = it.tokens[idx]
                raise SyntaxError("line {}, column {}".format(tk.lineno, tk.colno))
            res = it.result
            if res is not None:
                print('=> ', analyse(res, env).types[-1])
        except Exception as e:
        	sys.exc_info()
            # err_write(e.__class__.__name__ + ':' + str(e) + '\n')

        cache.clear()
        count_parentheses = 0

    while True:
        line: str = input('reF> ' if not cache else '      ')
        
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
