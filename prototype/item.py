from dataclasses import dataclass
from typing import Self

from grammar import Rule

DOT = "•"


@dataclass
class Item:
    lhs: str
    rhs: tuple[list[str], str]
    dot_pos: int

    @classmethod
    def from_rule(cls, rule: Rule, follow: str, dot_pos: int = 0) -> Self:
        return cls(
            lhs=rule.lhs,
            rhs=([v for v in rule.rhs], follow),
            dot_pos=dot_pos,
        )

    @classmethod
    def from_moving(cls, item: Self) -> Self:
        if item.dot_pos + 1 >= len(item.rhs[0]) + 1:
            raise IndexError(f"Dot position exceeds symbols amount")

        return cls(
            lhs=item.lhs,
            rhs=(item.rhs[0].copy(), item.rhs[1]),
            dot_pos=min(item.dot_pos + 1, len(item.rhs[0])),
        )

    def __repr__(self):
        return f"Item({self})"

    def __str__(self) -> str:
        rule, follow = self.rhs
        rule = rule.copy()
        rule.insert(self.dot_pos, DOT)
        return f"{self.lhs} → [{" ".join(rule)}, {follow}]"

    def __hash__(self):
        return hash((self.lhs, self.dot_pos, tuple(self.rhs[0]), self.rhs[1]))

    def __eq__(self, value: Self):
        lhs_equal = self.lhs == value.lhs
        rhs_equal = True

        for l, r in zip(self.rhs[0], value.rhs[0]):
            if l != r:
                rhs_equal = False
                break

        follow_equal = self.rhs[1] == value.rhs[1]
        dot_pos_equal = self.dot_pos == value.dot_pos

        return lhs_equal and rhs_equal and follow_equal and dot_pos_equal

    def next(
        self, skip: int = 0, take: int | None = None, include_follow: bool = True
    ) -> list[str]:
        """Returns the next symbols after dot, including follow symbol.

        Args:
            * `skip` - skip this amount of symbols after the dot
            * `take` - take this amount of symbols after the dot.
                       If `None` passed, takes all symbols to the end
            * `include_follow` - whether to include follow symbol or not.
                                 For `A -> [b DOT C d, a]` if `include_follow = True` then `a` is included

        Examples:
            For the item, `A -> [b DOT C d, a]`:
            - `item.next(0)` returns `[C, d, a]`
            - `item.next(1)` returns `[d, a]`
            - `item.next(0, 1)` returns `[C]`
        """
        left = self.dot_pos + skip

        after = self.rhs[0][left:]

        if include_follow:
            after.append(self.rhs[1])

        right = take if take else len(self.rhs) + 1
        return after[:right]

    def next_symbol(self, include_follow: bool = False) -> str | None:
        l = self.next(take=1, include_follow=include_follow)
        if not l:
            return None
        return l[0]
