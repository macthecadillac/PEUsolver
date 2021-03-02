class Program:
    def __init__(self, start_symbol):
        self.root = NodeStart(start_symbol)
        self.nonterminals = [self.root.children[0]]

    def __lt__(self, other):
        return False

    def __str__(self):
        return str(self.root)

    def is_ground(self):
        return len(self.nonterminals) == 0

    def unroll(self, i_nt, rhs_class):
        nt_node = self.nonterminals[i_nt]
        rhs_node = rhs_class()
        nt_node.replace(rhs_node)

        nt_pre = self.nonterminals[:i_nt]
        nt_post = self.nonterminals[i_nt + 1:]
        self.nonterminals = nt_pre + rhs_node.children + nt_post

class ASTNode:
    def __init__(self):
        self.parent = None
        self.parent_index = None
        self.children = []

    def __str__(self):
        return ''

    def _append_child(self, child_node):
        child_node.parent = self
        child_node.parent_index = len(self.children)
        self.children.append(child_node)

    def replace(self, other_node):
        self.parent.children[self.parent_index] = other_node

class NodeStart(ASTNode):
    def __init__(self, start_symbol):
        super().__init__()
        self._append_child(NodeNonterminal(start_symbol))

    def __str__(self):
        return str(self.children[0])

class NodeTerminal(ASTNode):
    def __init__(self, value):
        super().__init__()
        self.value = value

    def __str__(self):
        return self.value

class NodeInput(NodeTerminal):
    def __init__(self):
        super().__init__('x')

class NodeZero(NodeTerminal):
    def __init__(self):
        super().__init__('0')

class NodeNonterminal(ASTNode):
    def __init__(self, symbol):
        super().__init__()
        self.symbol = symbol

    def __str__(self):
        return self.symbol

class NodeL(NodeNonterminal):
    def __init__(self):
        super().__init__('L')

class NodeN(NodeNonterminal):
    def __init__(self):
        super().__init__('N')

class NodeSort(ASTNode):
    def __init__(self):
        super().__init__()
        self._append_child(NodeL())

    def __str__(self):
        l, = self.children
        return f'sort({l})'

class NodeSlice(ASTNode):
    def __init__(self):
        super().__init__()
        self._append_child(NodeL())
        self._append_child(NodeN())
        self._append_child(NodeN())

    def __str__(self):
        l, n1, n2 = self.children
        return f'{l}[{n1}..{n2}]'

class NodeAdd(ASTNode):
    def __init__(self):
        super().__init__()
        self._append_child(NodeL())
        self._append_child(NodeL())

    def __str__(self):
        l1, l2 = self.children
        return f'{l1} + {l2}'

class NodeSingleton(ASTNode):
    def __init__(self):
        super().__init__()
        self._append_child(NodeN())

    def __str__(self):
        n, = self.children
        return f'[{n}]'

class NodeFind(ASTNode):
    def __init__(self):
        super().__init__()
        self._append_child(NodeL())
        self._append_child(NodeN())

    def __str__(self):
        l, n = self.children
        return f'find({l}, {n})'
