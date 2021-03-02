from itertools import islice

from enumerator import AStarEnumerator
from grammar import TestGrammar
from heuristic import NoneHeuristic

grammar = TestGrammar()
heuristic = NoneHeuristic()
enumerator = AStarEnumerator(grammar, heuristic)

for program in islice(enumerator.enumerate(), 0, 10):
    print(program)
