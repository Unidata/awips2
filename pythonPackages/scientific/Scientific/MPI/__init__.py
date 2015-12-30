from core import *
from core import _C_API

import sys
if sys.modules.has_key('epydoc'):
    import core, types
    core_name = core.__name__
    from core import __doc__
    for name in dir(core):
        object = getattr(core, name)
        if type(object) == types.ClassType:
            setattr(object, '__module__', 'Scientific.MPI')
        elif type(object) == types.FunctionType:
            object.func_globals['__name__'] = 'Scientific.MPI'
    core.__name__ = core_name
    del core
    del core_name
    del object
    del name
    del types
del sys
