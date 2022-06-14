"""
spec.py: Frontend of Counting Context Free Grammar, the grammar of protocol and
extraction specifications.
"""
from string import ascii_letters, digits
from crg import ProductionRule, RuleItem, Regular


def name(s):
    assert s[0] in ascii_letters + "_"
    for i in range(1, len(s)):
        if s[i] not in ascii_letters + digits + "_":
            break
    return s[:i], s[i:].strip()


def skip(s, pattern):
    assert s.startswith(pattern)
    return None, s[len(pattern) :].strip()


def bracket(s):
    assert s[0] == "["
    # there should not be any possible nested bracket i guess...
    raw, s = s[1:].split("]", maxsplit=1)
    return (part.strip() for part in raw.split(";")), s.strip()


def regex(s):
    assert s[0] == "/"
    for i in range(1, len(s)):
        if s[i] == "/" and s[i - 1] != "\\":
            return s[1:i], s[i + 1 :].strip()
    assert False, "unclosed regex"


def unsigned(s):
    assert s[0] in digits
    for i in range(1, len(s)):
        if s[i] not in digits:
            break
    return int(s[:i]), s[i:].strip()


def guard(s):
    if ">=" in s:
        [variable, low] = s.split(">=")
        return variable.strip(), (int(low), None)
    if "<=" in s:
        [variable, inclusive_high] = s.split("<=")
        return variable.strip(), (None, int(inclusive_high) + 1)
    if "==" in s:  # why not just =
        [variable, low] = s.split("==")
        return variable.strip(), (int(low), int(low) + 1)
    if ">" in s:
        [variable, exclusive_low] = s.split(">")
        return variable.strip(), (int(exclusive_low) + 1, None)
    if "<" in s:
        [variable, high] = s.split("<")
        return variable.strip(), (None, high)
    assert False, f"unsupport guard {s}"


def rule(s):
    head, s = name(s)
    rule_guard = {}
    priority = ProductionRule.default_priority
    if s[0] == "[":
        guard_list, s = bracket(s)
        for part in guard_list:
            variable, bound = guard(part)
            assert variable not in rule_guard  # TODO support merging
            rule_guard[variable] = bound
    if s[0] in digits:
        priority, s = unsigned(s)
    _, s = skip(s, "->")
    item_list = []
    while s[0] != ";":
        if s[0] == "/":
            terminal, s = regex(s)
            # TODO
            if terminal == ".":
                terminal = Regular.wildcard
            else:
                terminal = Regular.new_literal(bytes(terminal, "utf-8"))
            item_list += [RuleItem(terminal=terminal)]
        elif s[0] in ascii_letters + "_":
            nonterminal, s = name(s)
            item_list += [RuleItem(nonterminal=nonterminal)]
        elif s[0] == "[":
            if not item_list:
                item_list = (RuleItem(terminal=Regular.epsilon),)
            assert not item_list[-1].action, "double action"
            action_list, s = bracket(s)
            item_list[-1] = RuleItem(
                terminal=item_list[-1].terminal,
                nonterminal=item_list[-1].nonterminal,
                action=tuple(action_list),
            )
        else:
            # consider presever the whole line for a better report
            assert False, f"unexpected {s.splitlines()[0]}"
    item_list = tuple(item_list)
    if not item_list:
        item_list = (RuleItem(terminal=Regular.epsilon),)
    _, s = skip(s, ";")
    return ProductionRule(head, rule_guard, priority, item_list), s


def grammar(s):
    s += "\n"
    while s:
        if s[0] == "#":
            [_, s] = s.split("\n", maxsplit=1)
        else:
            decl, s = rule(s)
            yield decl


# misc part
varstring = """
S -> B V;
B -> /0/ [c := c * 2] B;
B -> /1/ [c := c * 2 + 1] B;
B -> / /;
V [c > 0] -> /./ [c := c - 1] V;
V [c == 0] -> ;
"""

dyck = """
S -> ;
S -> I S;
I -> /[/ S /]/;
"""

from unittest import TestCase
from crg import varstring as varstring_grammar, dyck as dyck_grammar


class TestSpec(TestCase):
    def test_grammar(self):
        self.assertEqual(tuple(grammar(varstring.strip())), varstring_grammar)
        self.assertEqual(tuple(grammar(dyck.strip())), dyck_grammar)
