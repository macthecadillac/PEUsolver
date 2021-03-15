import json

class PCFG:
    @classmethod
    def from_json(cls, json_path):
        pcfg = cls()
        with open(json_path, 'r') as f:
            pcfg.c_table = json.load(f)['word-count']
        return pcfg

    def __init__(self):
        self.c_table = {}
        self.p_table = {}

    def count(self, node, grammar):
        # count this node
        node_symbol = node.symbol
        nt_symbol = grammar.look_up_nt(node_symbol)
        if nt_symbol not in self.c_table:
            self.c_table[nt_symbol] = {}
        if node_symbol not in self.c_table[nt_symbol]:
            self.c_table[nt_symbol][node_symbol] = 0
        self.c_table[nt_symbol][node_symbol] += 1

        # count child nodes recursively
        for child_node in node.children:
            self.count(child_node, grammar)

    def smooth(self, grammar):
        # get a smooth probability table from raw count table
        for nt_symbol, rules in grammar.rules.items():
            self.p_table[nt_symbol] = {}
            total_count = sum(self.c_table[nt_symbol].values())
            n_rules = len(rules)
            for rule in rules:
                node_symbol = rule.term_node.symbol
                node_count = self.c_table[nt_symbol].get(node_symbol, 0)
                smooth_p = (node_count + 1) / (total_count + n_rules)
                self.p_table[nt_symbol][node_symbol] = smooth_p
