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

def load(fp):
    s = fp.read()
    return loads(s)

def loads(s):
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
    return _sygus_prep(out)

def _sygus_prep(sexp):
    sexp_synth = sexp[1]

    # Create arg name replace table
    sexp_args = sexp_synth[2]
    arg_name_replace = {}
    for sexp_arg in sexp_args:
        arg_name, arg_sort = sexp_arg
        if arg_sort == 'String':
            arg_name_new = f'str.{arg_name}'
        elif arg_sort == 'Int':
            arg_name_new = f'int.{arg_name}'
        arg_name_replace[arg_name] = arg_name_new

    # Replace arg names
    _replace(sexp, arg_name_replace)
    return sexp

def _replace(sexp, replace_table):
    if not isinstance(sexp, list):
        return
    for i, sub_sexp in enumerate(sexp):
        if isinstance(sub_sexp, list):
            _replace(sub_sexp, replace_table)
        else:
            if sub_sexp in replace_table:
                sexp[i] = replace_table[sub_sexp]
