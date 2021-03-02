import program

class TestGrammar:
    def __init__(self):
        self.terminals = None
        self.non_terrminals = None
        self.rules = {
            'L': [
                program.NodeSort,
                program.NodeSlice,
                program.NodeAdd,
                program.NodeSingleton,
                program.NodeInput
            ],
            'N': [
                program.NodeFind,
                program.NodeZero
            ]
        }
        self.start = program.Program('L')

    def unroll(self, nonterminal):
        nt_symbol = nonterminal.symbol
        return self.rules[nt_symbol]
