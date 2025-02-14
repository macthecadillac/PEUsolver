import copy
import queue

class BestFirstEnumerator:
    def __init__(self, grammar, heuristic):
        self.grammar = grammar
        self.heuristic = heuristic
        self.working_list = queue.PriorityQueue()
        program = copy.deepcopy(self.grammar.start)
        cost = self.heuristic.estimate_nt_cost(program.root_nt)
        self.working_list.put((cost, program))

    def enumerate(self):
        while not self.working_list.empty():
            cost, program = self.working_list.get()
            if program.is_ground():
                yield cost, program
                continue
            for new_cost, new_program in self.unroll(program, cost):
                self.working_list.put((new_cost, new_program))

    def unroll(self, program, cost):
        i_nt = 0  # index of the leftmost nonterminal
        nt = program.nonterminals[i_nt]
        cost -= self.heuristic.estimate_nt_cost(program.nonterminals[i_nt])
        for rule in self.grammar.rules[nt.symbol]:
            new_cost = cost + self.heuristic.get_rule_cost(rule)
            new_program = copy.deepcopy(program)
            new_program.unroll(i_nt, rule.term_node.generate())
            di_nt = len(new_program.nonterminals) - len(program.nonterminals)
            for new_nt in new_program.nonterminals[i_nt:i_nt + di_nt + 1]:
                new_cost += self.heuristic.estimate_nt_cost(new_nt)
            yield new_cost, new_program
