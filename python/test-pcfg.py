from itertools import islice

import src.sexp as sexp
from src.enumerator import BestFirstEnumerator
from src.grammar import Grammar
from src.heuristic import PCFGHeuristic

if __name__ == '__main__':
    sygus_file = '../benchmark/string/train/dr-name.sl'
    with open(sygus_file, 'r') as f:
        sygus_sexp = sexp.load(f)
    grammar = Grammar.from_sygus(sygus_sexp)
    heuristic = PCFGHeuristic.from_json('../benchmark/pcfg.json', grammar)
    enumerator = BestFirstEnumerator(grammar, heuristic)

    for cost, program in islice(enumerator.enumerate(), 0, 10):
        print(cost, program)
