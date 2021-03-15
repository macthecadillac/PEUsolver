import copy
import json
import math
import pprint

from .stat import PCFG

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
            pcfg = json.load(f)['word-count']
        return cls(pcfg, grammar)

    def __init__(self, pcfg, grammar):
        print('Initializing PCFG')

        pcfg.smooth(grammar)
        self.p_table = pcfg.p_table
        print()
        print('probability table:')
        pprint.pp(self.p_table)
        print()

        # Initial computation of nonterminal heuristics
        epsilon = 0.01
        h_table = {}
        for nt_symbol in grammar.rules.keys():
            h_table[nt_symbol] = 0
        updated = True
        while updated:
            updated = False
            for nt_symbol, rules in grammar.rules.items():
                h_old = h_table[nt_symbol]
                p_max = h_old
                for rule in rules:
                    p = self.p_table[nt_symbol][rule.term_node.symbol]
                    for child_node in rule.term_node.children:
                        p *= h_table[child_node.symbol]
                    p_max = max(p_max, p)
                h_table[nt_symbol] = p_max
                updated = updated or (math.fabs(h_table[nt_symbol] - h_old) > epsilon)
        g_table = {}
        for nt_symbol, h in h_table.items():
            g_table[nt_symbol] = -math.log2(h)
        self.g_table = g_table
        print()
        print('heuristic table:')
        pprint.pp(self.g_table)
        print()

    def get_rule_cost(self, rule):
        p = self.p_table[rule.nt_symbol][rule.term_node.symbol]
        return -math.log2(p)

    def estimate_nt_cost(self, nt_node):
        return self.g_table[nt_node.symbol]
