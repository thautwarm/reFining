import unittest
from refining.typ import *
from refining.type_infer import infer_undecided


class Test(unittest.TestCase):

    def test_record(self):
        msg = """
        cannot infer from simple record case:
        type S of 'a = {
            a: 'a
            b: int
        }
        """
        i = BasicType("int")
        a = Slot('a')
        fields = FrozenDict(a=a, b=i)
        t = RecordType(fields)
        res = infer_undecided(t)
        print(res)
        self.assertTrue(res == {a}, msg)

    def test_du(self):
        msg = """
        infer failed:
        type S of 'a = 
        | I 'a i
        | F 'a f 
        """
        i = BasicType('int')
        a = Slot('a')

        f = BasicType('float')

        du = UnionType(FrozenDict(I=TupleType((a, i)), F=TupleType((a, f))))
        res = infer_undecided(du)
        print(res)
        self.assertTrue(res == {a}, msg)

    def test_dependent_value(self):
        import sympy
        msg = """
        infer failed:
        type Vec of val[m] * 't  = 
        | Data of [|'t|]
        
        Vec of val[m] * 't
        """
        s = sympy.Symbol("s")
        m = Value(s)
        t = Slot('t')
        g = Slot('g')

        repr = Representation("V", (m, t, g))
        res = infer_undecided(repr)
        print(res)
        self.assertTrue(res == {s, t, g}, msg)


unittest.main()
