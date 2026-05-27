from lib2to3.pgen2 import token
from lib2to3.pygram import python_symbols as syms
from lib2to3 import fixer_base
from lib2to3.fixer_util import Name, Call

class FixJdoublevalue(fixer_base.BaseFix):
    BM_compatible = True
    PATTERN = """
    power< base=any+ trailer< '.' attr='doubleValue' > trailer< '(' ')' > >
    |
    power< head=any+ trailer< '.' attr='doubleValue' > not trailer< '(' ')' > >
    """
    order = "pre"

    def start_tree(self, tree, filename):
        super(FixJdoublevalue, self).start_tree(tree, filename)

    def transform(self, node, results):
        assert results
        base = results.get("base")
        if base:
            base = [n.clone() for n in base]
            base[0].prefix = u""
            node.replace(Call(Name(u"float", prefix=node.prefix), base))
