import copy
import json
import math
import pprint

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

    def __init__(self, p_table, grammar):
        print('Initializing PCFG')

        # Trim p_table to be grammar specific
        new_p_table = {}
        for nt_symbol in grammar.rules.keys():
            counter = {}
            total_count = 0
            for rule in grammar.rules[nt_symbol]:
                symbol = rule.term_node.symbol
                counter[symbol] = p_table[nt_symbol].get(symbol, 1)
                total_count += counter[symbol]
            for rule in grammar.rules[nt_symbol]:
                counter[rule.term_node.symbol] /= total_count
            new_p_table[nt_symbol] = counter
        self.p_table = new_p_table
        print()
        print('probability table:')
        pprint.pp(self.p_table)
        print()

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
        print()
        print('heuristic table:')
        pprint.pp(self.h_table)
        print()

    def get_rule_cost(self, rule):
        p = self.p_table[rule.nt_symbol][rule.term_node.symbol]
        return -math.log2(p)

    def estimate_nt_cost(self, nt_node):
        return self.h_table[nt_node.symbol]
