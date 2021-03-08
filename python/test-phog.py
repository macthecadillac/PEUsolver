from itertools import islice

import sexp
from enumerator import BestFirstEnumerator
from grammar import Grammar
from heuristic import NoneHeuristic

if __name__ == '__main__':
    sygus_file = '../euphony/benchmarks/string/train/dr-name.sl'
    with open(sygus_file, 'r') as f:
        sygus_sexp = sexp.load(f)
    grammar = Grammar.from_sygus(sygus_sexp)
    heuristic = NoneHeuristic()
    enumerator = BestFirstEnumerator(grammar, heuristic)

    for program in islice(enumerator.enumerate(), 0, 10):
        print(program)
