from itertools import islice

import src.sexp as sexp
from src.enumerator import BestFirstEnumerator
from src.grammar import Grammar
from src.heuristic import PCFGHeuristic
from src.stat import PCFG

if __name__ == '__main__':
    sygus_file = '../benchmark/string/train/dr-name.sl'
    with open(sygus_file, 'r') as f:
        sygus_sexp = sexp.load(f)
    grammar = Grammar.from_sygus(sygus_sexp)
    pcfg = PCFG.from_json('../benchmark/pcfg.json')
    heuristic = PCFGHeuristic(pcfg, grammar)
    enumerator = BestFirstEnumerator(grammar, heuristic)

    for cost, program in islice(enumerator.enumerate(), 0, 10):
        print(cost, program)
