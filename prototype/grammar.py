from dataclasses import dataclass
from typing import Self


EOF = "eof"
MATH_NA = "∅"
EPS = "ϵ"


@dataclass
class Rule:
    lhs: str
    rhs: list[str]

    def is_empty(self) -> bool:
        return len(self.rhs) == 1 and self.rhs[0] == EPS


class Grammar:

    def __init__(self, goal: str, terminal: set[str], rules: list[Rule]):
        self.goal = goal
        self.terminal = terminal | {EOF, EPS}
        self.nonterminal = Grammar.select_symbols(rules).difference(self.terminal)
        self.rules = Grammar.clean_rules(rules)

    @staticmethod
    def select_symbols(rules: list[Rule]) -> set[str]:
        s = set()
        for r in rules:
            s.add(r.lhs)
            for v in r.rhs:
                s.add(v)
        return s

    @staticmethod
    def clean_rules(rules: list[Rule]) -> list[Rule]:
        new_rules = []
        for v in rules:
            rhs = v.rhs.copy() if v.rhs else [EPS]
            new_rules.append(Rule(lhs=v.lhs, rhs=rhs))
        return new_rules
