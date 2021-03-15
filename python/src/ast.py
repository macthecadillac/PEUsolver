import copy

def parse_sexp(sexp):
    node = None
    if isinstance(sexp, list):
        node = NodeFun(sexp[0])
        for sub_sexp in sexp[1:]:
            node.append_child(parse_sexp(sub_sexp))
    else:
        node = NodeVar(sexp)
    return node

class Node:
    def __init__(self, symbol):
        self.symbol = str(symbol)
        self.parent = None
        self.parent_index = None
        self.children = []

    def __str__(self):
        return self.symbol

    @property
    def left_sibling(self):
        if self.parent and self.parent_index > 0:
            return self.parent.children[self.parent_index - 1]
        return None

    @property
    def right_sibling(self):
        if self.parent and self.parent_index < len(self.parent.children) - 1:
            return self.parent.children[self.parent_index + 1]
        return None

    def append_child(self, child_node):
        child_node.parent = self
        child_node.parent_index = len(self.children)
        self.children.append(child_node)

    def generate(self):
        return copy.deepcopy(self)

    def replace(self, other_node):
        self.parent.children[self.parent_index] = other_node

class NodeStart(Node):
    def __init__(self, symbol):
        super().__init__(None)
        self.append_child(NodeVar(symbol))
        self.root_node = self.children[0]

    def __str__(self):
        return str(self.root_node)

class NodeVar(Node):
    pass

class NodeFun(Node):
    def __init__(self, symbol, arg_symbols=[]):
        super().__init__(symbol)
        for arg_symbol in arg_symbols:
            self.append_child(NodeVar(arg_symbol))

    def __str__(self):
        arg_str = ' '.join(list(map(str, self.children)))
        return f'({self.symbol} {arg_str})'
