import copy

from .ast import NodeFun, NodeStart, NodeVar

class Grammar:
    @classmethod
    def from_sygus(cls, sexp):
        sexp_cfg = sexp[1][4]
        sexp_start = sexp_cfg[0]
        _, _, (start_symbol,) = sexp_start
        start = Program(NodeStart(start_symbol))
        rules = {}
        for sexp_rule_group in sexp_cfg[1:]:
            nt_symbol, rule_sort, sexp_terms = sexp_rule_group
            rules[nt_symbol] = []
            for sexp_term in sexp_terms:
                if isinstance(sexp_term, list):
                    symbol, arg_symbols = sexp_term[0], sexp_term[1:]
                    term_node = NodeFun(symbol, arg_symbols)
                else:
                    symbol = sexp_term
                    term_node = NodeVar(symbol)
                rule = Rule(nt_symbol, rule_sort, term_node)
                rules[nt_symbol].append(rule)
        return cls(start, rules)

    def __init__(self, start, rules):
        self.rules = rules
        self.start = start

        self.nt_table = {}
        for nt_symbol, rules in rules.items():
            for rule in rules:
                self.nt_table[rule.term_node.symbol] = nt_symbol

    def look_up_nt(self, node_symbol):
        return self.nt_table[node_symbol]

    def merge(self, other_grammar):
        for nt_symbol, rules in other_grammar.rules.items():
            for rule in rules:
                term_node = rule.term_node
                if nt_symbol not in self.rules:
                    self.rules[nt_symbol] = []
                if term_node.symbol not in self.nt_table:
                    self.rules[nt_symbol].append(copy.deepcopy(rule))
                    self.nt_table[term_node.symbol] = nt_symbol

class Program:
    def __init__(self, start_node):
        self.root = start_node
        self.root_nt = self.root.children[0]
        self.nonterminals = [self.root_nt]

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
