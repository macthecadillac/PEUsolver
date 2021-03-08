import copy
import json
import math

class Heuristic:
    def estimate_cost(self, program):
        pass

    def estimate_production_cost(self, nonterminal, rhs):
        pass

    def estimate_nonterminal_cost(self, nonterminal):
        pass

class NoneHeuristic(Heuristic):
    def estimate_cost(self, program):
        return 0

    def estimate_rule_cost(self, rule):
        return 1

    def estimate_nonterminal_cost(self, nonterminal):
        return 0

class PCFGHeuristic(Heuristic):
    @classmethod
    def from_json(cls, json_path):
        with open(json_path, 'r') as f:
            pcfg = json.load(f)['pcfg']
        return cls(pcfg)

    def __init__(self, p_table):
        self.p_table = p_table

    def init_heuristics(self, grammar):
        h_table = {}
        for nt_symbol in grammar.rules.keys():
            h_table[nt_symbol] = 0
        while True:
            h_table_new = copy.deepcopy(h_table)
            for nt_symbol in h_table.keys():
                for rule in grammar.rules[nt_symbol]:
                    p = self.p_table[nt_symbol][rule.term_node.symbol]
                    for child_node in rule.term_node.children:
                        p *= h_table[child_node.symbol]
                    h_table_new[nt_symbol] = max(h_table_new[nt_symbol], p)
            if h_table_new == h_table:
                break
            h_table = h_table_new
        self.h_table = h_table

    def estimate_rule_cost(self, rule):
        p = self.p_table[rule.nt_symbol][rule.term_node.symbol]
        return -math.log2(p)

    def estimate_nonterminal_cost(self, nt_node):
        return self.h_table[nt_node.symbol]
