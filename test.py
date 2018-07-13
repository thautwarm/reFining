from refining.reunify import *
import rbnf.zero as ze
import rbnf.ParserC
env = make_default_env()
ze_exp = ze.compile('import simple.[*]', use='Grammar')

# print(repr(analyze(ze_exp.match("""
# let s = fn x: 'a -> x in
#       let d = s "1" in
#       let k = fn x: 'a -> x in k;
# """).result, env)))

print(repr(analyze(ze_exp.match("""
let x : 'a * int = (1, 2) in x 
""").result, env)))
