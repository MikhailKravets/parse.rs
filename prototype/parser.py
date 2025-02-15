from collections import deque, defaultdict
from pprint import pformat, pprint
from typing import Iterator, Self

import pandas as pd
import networkx as nx

from matplotlib import pyplot as plt

from grammar import EOF, EPS, MATH_NA, Grammar, Rule
from item import Item


brackets = Grammar(
    goal="goal",
    terminal={"(", ")"},
    rules=[
        Rule(lhs="goal", rhs=["list"]),
        Rule(lhs="list", rhs=["list", "pair"]),
        Rule(lhs="list", rhs=["pair"]),
        Rule(lhs="pair", rhs=["(", "list", ")"]),
        Rule(lhs="pair", rhs=["(", ")"]),
    ],
)

math = Grammar(
    goal="goal",
    terminal={"+", "-", "*", "/", "(", ")", "n"},
    rules=[
        Rule(lhs="goal", rhs=["expr"]),
        Rule(lhs="expr", rhs=["expr", "+", "term"]),
        Rule(lhs="expr", rhs=["expr", "-", "term"]),
        Rule(lhs="expr", rhs=["term"]),
        Rule(lhs="term", rhs=["term", "*", "factor"]),
        Rule(lhs="term", rhs=["term", "/", "factor"]),
        Rule(lhs="term", rhs=["factor"]),
        Rule(lhs="factor", rhs=["(", "expr", ")"]),
        Rule(lhs="factor", rhs=["n"]),
    ],
)


empty = Grammar(
    goal="goal",
    terminal={"a"},
    rules=[
        Rule(lhs="goal", rhs=["A"]),
        Rule(lhs="A", rhs=["A", "a"]),
        Rule(lhs="A", rhs=["a"]),
        Rule(lhs="A", rhs=[]),
    ],
)


class ParseNode:

    def __init__(self, token: str, children: list[Self] = None):
        self.token = token
        self.children: list[Self] = children if children else []

    def append(self, node: Self):
        self.children.append(node)

    def __repr__(self):
        return f"Node({self})"

    def __str__(self):
        return f"{self.token}:{len(self.children)}"


class First:

    def __init__(self, grammar):
        self.first: dict[str, set[str]] = First.build(grammar)

    def __getitem__(self, symbols: list[str]) -> set:
        first = set()
        for s in symbols:
            first = first | self.first[s] - {EPS}
            if EPS not in self.first[s]:
                break
        return first

    @staticmethod
    def build(grammar: Grammar) -> dict[str, set[str]]:
        first = {}

        for t in grammar.terminal:
            first[t] = {t}

        for nt in grammar.nonterminal:
            first[nt] = set()

        is_changing = True
        while is_changing:
            is_changing = False
            for rule in grammar.rules:
                rhs: set = first[rule.rhs[0]] - {EPS}
                trailing = True

                for i in range(len(rule.rhs) - 1):
                    if EPS in first[rule.rhs[i]]:
                        rhs = rhs | first[rule.rhs[i + 1]] - {EPS}
                    else:
                        trailing = False
                        break

                if trailing and EPS in first[rule.rhs[-1]]:
                    rhs = rhs | {EPS}

                is_changing = is_changing or len(rhs.difference(first[rule.lhs])) > 0
                first[rule.lhs] = first[rule.lhs] | rhs
        return first

    def __str__(self) -> str:
        return pformat(self.first)


def get_next(it: Iterator[str]) -> str:
    try:
        return next(it)
    except StopIteration:
        return EOF


def closure(rules: list[Rule], FIRST: First, s: set[Item]) -> set[Item]:
    to_process = [item for item in s]

    while to_process:
        # A -> b * C d, a
        item = to_process.pop()
        s.add(item)

        lookahead = FIRST[item.next(1)]
        c = item.next_symbol()
        for p in filter(lambda e: e.lhs == c, rules):
            if p.is_empty():
                # This prevents addition of A -> [EPS, a] items. They are redundant
                continue
            for b in lookahead:
                new_item = Item.from_rule(p, b, dot_pos=0)
                if new_item not in s:
                    to_process.append(new_item)
    return s


def goto(rules: list[Rule], FIRST: First, s: set[Item], x: str) -> set:
    t = set()

    for item in s:
        if item.next_symbol() == x:
            t.add(Item.from_moving(item))

    return closure(rules, FIRST, t)


def build_canonical_collection(grammar: Grammar, FIRST: First) -> tuple[dict, pd.DataFrame]:
    cc0 = set()
    for rule in grammar.rules:
        if rule.lhs == grammar.goal:
            cc0.add(Item.from_rule(rule, follow=EOF))

    symbols = list(grammar.nonterminal | grammar.terminal)

    CC = {}

    cc0 = (frozenset(closure(grammar.rules, FIRST, cc0)), 0)
    to_process = [cc0]
    check_set = {cc0: 0}

    # List of records for DataFrame trace
    trace_records = []
    i = 1

    while to_process:
        cci, name = to_process.pop()
        CC[cci] = name
        record = {v: None for v in symbols}
        record["From"] = name

        x_set = {item.next_symbol() for item in cci}
        for x in filter(lambda e: e is not None, x_set):
            t = frozenset(goto(grammar.rules, FIRST, cci, x))
            if t not in check_set:
                goto_name = i
                to_process.append((t, goto_name))
                check_set[t] = goto_name
                record[x] = goto_name
                i += 1
            else:
                record[x] = check_set[t]
        trace_records.append(record)

    df = pd.DataFrame.from_records(trace_records)
    df = df.set_index("From")
    df = df.fillna(MATH_NA)
    df = df.sort_index()
    df = df.map(lambda v: int(v) if v != MATH_NA else v)
    # df.index = df.index.map(lambda v: f"cc{v}")

    return CC, df[sorted(df.columns)]


def fill_tables(
    grammar: Grammar,
    CC: dict[frozenset[Item], int],
    trace: pd.DataFrame,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    terminal = list(grammar.terminal)
    nonterminal = list(grammar.nonterminal)
    action = pd.DataFrame("∅", index=trace.index, columns=terminal)
    goto = pd.DataFrame("∅", index=trace.index, columns=nonterminal)

    for cc, i in CC.items():
        for item in cc:
            c = item.next_symbol()
            if c is not None and c in grammar.terminal:
                action.at[i, c] = ("shift", trace.at[i, c])
            elif c is None and item.lhs == grammar.goal and item.rhs[1] == EOF:
                action.at[i, EOF] = ("accept", item)
            elif c is None:
                if c and action.at[i, c] != "∅":
                    raise ValueError(f"Cannot write reduce. Cell is taken {action.at[i, c]}")
                action.at[i, item.rhs[1]] = ("reduce", item)

        for nt in nonterminal:
            goto.at[i, nt] = trace.at[i, nt]

    return action, goto


def parse(s: str, grammar: Grammar, action: pd.DataFrame, goto: pd.DataFrame) -> ParseNode:
    stack = [(ParseNode(grammar.goal), 0)]
    s = iter(s)
    word = get_next(s)
    i = 0

    while True:
        _, state = stack[-1]
        a, state = action.at[state, word]

        if a == "shift":
            stack.append((ParseNode(word), state))
            word = get_next(s)
        elif a == "reduce":
            item: Item = state
            print(f"{i}. {item}: {stack}")

            children = deque([])
            for _ in range(len(item.rhs[0])):
                children.appendleft(stack.pop()[0])

            node = ParseNode(item.lhs, children=list(children))
            _, state = stack[-1]
            stack.append((node, goto.at[state, item.lhs]))
        elif a == "accept":
            print(f"{i}. accept.")

            children = deque([])
            for _ in range(len(stack) - 1):
                children.appendleft(stack.pop()[0])

            goal = stack.pop()[0]
            goal.children = list(children)
            return goal

        i += 1


def print_set(cc: set[Item]):
    pprint(list(sorted(list(map(lambda e: str(e), cc)))))


def set2str(cc: set[Item] | frozenset[Item]) -> set:
    return list(sorted(list(map(lambda e: str(e), cc))))


def print_table(CC: dict[frozenset[Item], str]):
    reversed_cc = {v: set2str(k) for k, v in CC.items()}
    pprint(reversed_cc)


def visualize(root: ParseNode):
    graph = nx.DiGraph()
    queue = deque([(root, None)])

    node_index = 0
    layer = 0
    max_nodes_per_layer = 1
    layout = defaultdict(list)  # key - level, value - list of nodes

    while queue:
        i = len(queue)
        max_nodes_per_layer = max(max_nodes_per_layer, i)
        for _ in range(i):
            node, parent_id = queue.popleft()
            graph.add_node(node_index, token=node.token, layer=layer)

            layout[(layer, i)].append(node_index)

            if parent_id is not None:
                graph.add_edge(parent_id, node_index)

            for ch in node.children:
                queue.append((ch, node_index))

            node_index += 1
        layer += 1

    # nx.multipartite_layout doesn't preserve per-layer ordering of nodes
    # which is necessary
    pos = {}
    x_spacing = 1
    for (level, amount), nodes in layout.items():
        for i, node in enumerate(nodes):
            x = x_spacing * (i - amount / 2)
            pos[node] = (x, -level)

    nx.draw(graph, pos, arrows=True, node_shape="o", node_size=1500, alpha=0.4)
    nx.draw_networkx_labels(
        graph, pos, labels={k: v["token"] for k, v in graph.nodes.items()}
    )
    plt.show()


# TODO: handle grammar errors like shift / reduce
# TODO: make code more readable
if __name__ == "__main__":
    grammar = brackets

    first = First(grammar)
    print(first)
    print()

    # item = Item.from_rule(grammar.rules[0], follow=EOF, dot_pos=0)
    # print(item)
    # cc0 = closure(grammar.rules, first, {item})
    # cc3 = goto(grammar.rules, first, cc0, "(")
    # print_set(cc0)

    # cc3 = goto(brackets["rules"], first, cc0, "(")
    # print_cc(cc3)
    CC, df = build_canonical_collection(grammar, first)
    print_table(CC)
    print()
    print(df)
    print()

    action, goto_table = fill_tables(grammar, CC, df)
    print(action)
    print()
    print(goto_table)
    print()

    # FIXME: brackets grammar generate error for `(()())`.
    # FIXME: this is shift - shift collision. Review closure and goto functions
    tree = parse("(()())", grammar, action, goto_table)
    # parse("aaaa", grammar, action, goto_table)
    # tree = parse("n-(n+n)", grammar, action, goto_table)
    visualize(tree)
