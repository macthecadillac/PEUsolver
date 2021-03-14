from itertools import islice

import sexp
from enumerator import BestFirstEnumerator
from grammar import Grammar
from heuristic import PCFGHeuristic

if __name__ == '__main__':
    sygus_file = '../euphony/benchmarks/string/train/dr-name.sl'
    with open(sygus_file, 'r') as f:
        sygus_sexp = sexp.load(f)
    grammar = Grammar.from_sygus(sygus_sexp)
    heuristic = PCFGHeuristic.from_json('../benchmark/pcfg.json', grammar)
    enumerator = BestFirstEnumerator(grammar, heuristic)

    for cost, program in islice(enumerator.enumerate(), 0, 10):
        print(cost, program)
