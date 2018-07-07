from refining.unification import *
import rbnf.zero as ze

ze_exp = ze.compile('import simple.[*]', use='Grammar')

statements = ["""let s = 1 in 1""", """fn x -> x """, """type S = int; let x : S = 1 in x;"""]
env = [('.i', Basic('int')), ('.s', Basic('str'))]
for each in statements:
    term = ze_exp.match(each).result
    print(analyse(term, env))

#
# print(analyse(Lam(Id("x"), App(Id("x"), Const(1))), []))
