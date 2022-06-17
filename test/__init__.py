"""
test/__init__.py: Collection of unit test cases.
"""
from unittest import TestCase
from spec import Grammar
from crg import (
    subgrammar_table,
    reachable_table,
    normal_set,
    first_table,
    Regular,
    varstring,
    dyck,
    extr_dyck,
)
from lpdfa import State
from gen import split_guard


class TestSpec(TestCase):
    varstring = r"""
    S -> B V ;
    B -> /0/ [c := c * 2] B ;
    B -> /1/ [c := c * 2 + 1] B ;
    B -> /\ / ;
    V [c > 0] -> /./ [c := c - 1] V ;
    V [c == 0] -> ;
    """

    dyck = r"""
    S -> ;
    S -> I S ;
    I -> /\[/ S /]/ ;
    """

    extr_dyck = r"""
    X -> /\[/ param( S ) /]/ S;
    """

    def test_grammar(self):
        self.assertEqual(tuple(Grammar(TestSpec.varstring.lstrip())), varstring)
        self.assertEqual(tuple(Grammar(TestSpec.dyck.lstrip())), dyck)

    def test_extraction(self):
        self.assertEqual(tuple(Grammar(TestSpec.extr_dyck.lstrip())), extr_dyck)


class TestCRG(TestCase):
    def test_is_regular(self):
        self.assertFalse(varstring[0].is_triple())
        for rule in varstring[1:]:
            with self.subTest(rule=str(rule)):
                self.assertTrue(rule.is_triple())
        self.assertTrue(varstring[1].is_nonterminating())
        self.assertTrue(varstring[2].is_nonterminating())
        self.assertTrue(varstring[3].is_terminating())
        self.assertTrue(varstring[4].is_nonterminating())
        self.assertTrue(varstring[5].is_terminating())

        self.assertTrue(dyck[0].is_terminating())
        self.assertFalse(dyck[1].is_triple())
        self.assertFalse(dyck[2].is_triple())

    def test_reachable(self):
        self.assertEqual(reachable_table(()), {})
        self.assertEqual(
            reachable_table(varstring), {"S": {"B", "V"}, "B": set(), "V": set()}
        )
        self.assertEqual(reachable_table(dyck), {"S": {"I"}, "I": {"S"}})
        self.assertEqual(reachable_table(extr_dyck + dyck)["X"], {"S", "I"})

    def test_subgrammar(self):
        self.assertEqual(subgrammar_table(()), {})
        subgrammar = subgrammar_table(varstring)
        self.assertEqual(subgrammar["S"], varstring)
        self.assertEqual(subgrammar["B"], varstring[1:4])
        self.assertEqual(subgrammar["V"], varstring[4:])
        self.assertEqual(subgrammar_table(dyck)["S"], dyck)

    def test_normal(self):
        self.assertEqual(normal_set(()), set())
        self.assertEqual(normal_set(varstring), {"S", "B", "V"})
        self.assertEqual(normal_set(dyck), set())

    def test_first(self):
        self.assertEqual(first_table(()), {})
        self.assertEqual(
            first_table(varstring),
            {
                "S": {
                    Regular.new_literal(b"0"),
                    Regular.new_literal(b"1"),
                    Regular.new_literal(b" "),
                },
                "B": {
                    Regular.new_literal(b"0"),
                    Regular.new_literal(b"1"),
                    Regular.new_literal(b" "),
                },
                "V": {Regular.wildcard, Regular.epsilon},
            },
        )
        self.assertEqual(
            first_table(dyck),
            {
                "S": {Regular.new_literal(b"["), Regular.epsilon},
                "I": {Regular.new_literal(b"[")},
            },
        )


class TestLPDFA(TestCase):
    def test_powerset(self):
        qacc = State({}, decision=(1, None, "done"))
        q0 = State({0: {qacc}})
        s0 = q0.powerset()
        self.assertEqual(s0.ahead, (1, None, "done"))
        sacc = s0.as_deterministic()[0]
        self.assertTrue(sacc.is_accepted())
        self.assertFalse(sacc.byte_table)

        q1 = State({1: {q0, qacc}})
        s1 = q1.powerset()
        s0acc = s1.as_deterministic()[1]
        self.assertTrue(s0acc.is_accepted())
        sacc = s0acc.as_deterministic()[0]
        self.assertTrue(sacc.is_accepted())
        self.assertEqual(tuple(s1.reachable()), (s1, s0acc, sacc))

        q2 = State({State.epsilon: {q1}})
        q3 = State({3: {q2}})
        s3 = q3.powerset()
        s12 = s3.as_deterministic()[3]
        s0acc = s12.as_deterministic()[1]
        sacc = s0acc.as_deterministic()[0]
        self.assertEqual(tuple(s3.reachable()), (s3, s12, s0acc, sacc))

        # TODO write some real test cases


class TestGen(TestCase):
    def test_split_guard(self):
        self.assertEqual(tuple(split_guard(())), ())
        for guard in ({}, {"x": (0, 1)}, {"x": (0, 1), "y": (0, 1)}):
            with self.subTest(guard=guard):
                self.assertEqual(
                    tuple(split_guard((("S", guard),))),
                    (((), {frozenset(guard.items())}),),
                )

        x01, x02, x12 = {"x": (0, 1)}, {"x": (0, 2)}, {"x": (1, 2)}
        self.assertEqual(
            tuple(split_guard((("S", x01, "0..1"), ("S", x02, "0..2")))),
            (
                (("0..1",), {frozenset(x01.items())}),
                (("0..2",), {frozenset(x01.items()), frozenset(x12.items())}),
            ),
        )
