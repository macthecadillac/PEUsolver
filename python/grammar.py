import copy

class Grammar:
    @classmethod
    def from_sygus(cls, sexp):
        sexp_cfg = sexp[1][4]
        sexp_start = sexp_cfg[0]
        _, start_sort, (start_symbol,) = sexp_start
        start = Program(NodeStart(start_symbol, start_sort))
        rules = {}
        for sexp_rule_group in sexp_cfg[1:]:
            nt_symbol, rule_sort, sexp_terms = sexp_rule_group
            rules[nt_symbol] = []
            for sexp_term in sexp_terms:
                if isinstance(sexp_term, list):
                    symbol, arg_symbols = sexp_term[0], sexp_term[1:]
                    term_node = NodeFun(symbol, rule_sort, arg_symbols)
                else:
                    symbol = sexp_term
                    term_node = NodeVar(symbol, rule_sort)
                rule = Rule(nt_symbol, rule_sort, term_node)
                rules[nt_symbol].append(rule)
        return cls(start, rules)

    def __init__(self, start, rules):
        self.rules = rules
        self.start = start

class Program:
    def __init__(self, start_node):
        self.root = start_node
        self.nonterminals = [self.root.children[0]]

    def __lt__(self, other):
        return False

    def __str__(self):
        return str(self.root)

    def is_ground(self):
        return len(self.nonterminals) == 0

    def unroll(self, i_nt, new_nt_node):
        nt_node = self.nonterminals[i_nt]
        nt_node.replace(new_nt_node)

        nt_pre = self.nonterminals[:i_nt]
        nt_post = self.nonterminals[i_nt + 1:]
        self.nonterminals = nt_pre + new_nt_node.children + nt_post

class Rule:
    def __init__(self, nt_symbol, sort, term_node):
        self.nt_symbol = nt_symbol
        self.sort = sort
        self.term_node = term_node

class Node:
    def __init__(self, symbol, sort):
        self.symbol = str(symbol)
        self.sort = sort
        self.parent = None
        self.parent_index = None
        self.children = []

    def __str__(self):
        return self.symbol

    def _append_child(self, child_node):
        child_node.parent = self
        child_node.parent_index = len(self.children)
        self.children.append(child_node)

    def generate(self):
        return copy.deepcopy(self)

    def replace(self, other_node):
        self.parent.children[self.parent_index] = other_node

class NodeStart(Node):
    def __init__(self, symbol, sort):
        super().__init__(None, None)
        self._append_child(NodeVar(symbol, sort))

    def __str__(self):
        return str(self.children[0])

class NodeVar(Node):
    def __str__(self):
        return f'"{self.symbol}"'

class NodeFun(Node):
    def __init__(self, symbol, sort, arg_symbols):
        super().__init__(symbol, sort)
        for arg_symbol in arg_symbols:
            self._append_child(NodeVar(arg_symbol, sort))

    def __str__(self):
        arg_str = ' '.join(list(map(str, self.children)))
        return f'({self.symbol} {arg_str})'
