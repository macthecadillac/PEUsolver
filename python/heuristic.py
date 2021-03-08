import copy
import json
import math

class Heuristic:
    def get_rule_cost(self, rule):
        pass

    def estimate_nt_cost(self, nt_node):
        pass

class NoneHeuristic(Heuristic):
    def get_rule_cost(self, rule):
        return 1

    def estimate_nt_cost(self, nt_node):
        return 0

class PCFGHeuristic(Heuristic):
    @classmethod
    def from_json(cls, json_path, grammar):
        with open(json_path, 'r') as f:
            pcfg = json.load(f)['pcfg']
        return cls(pcfg, grammar)

    def __init__(self, p_table, grammar):
        self.p_table = p_table

        # Initial computation of nonterminal heuristics
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

    def get_rule_cost(self, rule):
        p = self.p_table[rule.nt_symbol][rule.term_node.symbol]
        return -math.log2(p)

    def estimate_nt_cost(self, nt_node):
        return self.h_table[nt_node.symbol]
