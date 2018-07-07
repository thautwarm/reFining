from refining.unification import *
import rbnf.zero as ze
import rbnf.ParserC

ze_exp = ze.compile('import simple.[*]', use='Grammar')
env = [('.i', Basic('int')), ('.s', Basic('str'))]

statements = ["""let s = 1 in 1""", """(fn x -> x) 1""", """type S = int; let x : S = 1 in x;""",
              """let a = 1 in let b = 2 in a""",

              """let g: 'a -> int = fn x -> x in 
                 let s = (g 1) in 
                    g """
            ]
for each in statements:
    term = ze_exp.match(each).result
    print(term)
    print(analyse(term, env))
