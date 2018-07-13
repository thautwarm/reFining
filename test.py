from refining.reunify import *
import rbnf.zero as ze
import rbnf.ParserC
env = make_default_env()
ze_exp = ze.compile('import simple.[*]', use='Grammar')

print(repr(
    analyse(ze_exp.match("""
let s : 'a = fn x -> x in
      let d = s "1" in
      let k = fn x : 'a -> x in k;;
""").result, env)))

# env = [('.i', Basic('int')), ('.s', Basic('str'))]
#
# statements = ["""let s = 1 in 1""", """(fn x -> x) 1""", """type S = int; let x : S = 1 in x;""",
#               """let a = 1 in let b = 2 in a""",
#
#               """let g: 'a -> int = fn x -> x in
#                  let s = (g 1) in
#                     g """
#             ]
# for each in statements:
#     term = ze_exp.match(each).result
#     print(term)
#     print(analyse(term, env))
