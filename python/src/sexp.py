"""S-expression parser for SyGuS-IF.

Adapted from http://rosettacode.org/wiki/S-expressions#Python
"""
import re

term_regex = r'''(?mx)
    \s*(?:
        (?P<brackl>\()|
        (?P<brackr>\))|
        (?P<num>\-?\d+\.\d+|\-?\d+)|
        (?P<sq>"[^"]*")|
        (?P<s>[^(^)\s]+)
        )
'''

def load(fp, multiple=True):
    s = fp.read()
    return loads(s, multiple=multiple)

def loads(s, multiple=True):
    stack = []
    out = []
    for termtypes in re.finditer(term_regex, s):
        term, value = [(t,v) for t,v in termtypes.groupdict().items() if v][0]
        if term == 'brackl':
            stack.append(out)
            out = []
        elif term == 'brackr':
            assert stack, "Trouble with nesting of brackets"
            tmpout, out = out, stack.pop(-1)
            out.append(tmpout)
        elif term == 'num':
            out.append(value)
        elif term == 'sq':
            out.append(value)
        elif term == 's':
            out.append(value)
        else:
            raise NotImplementedError(f"Error: ({term}, {value})")
    assert not stack, "Trouble with nesting of brackets"
    if multiple:
        return out
    else:
        return out[0]
