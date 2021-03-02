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

    def estimate_production_cost(self, nonterminal, rhs):
        return 1

    def estimate_nonterminal_cost(self, nonterminal):
        return 0
