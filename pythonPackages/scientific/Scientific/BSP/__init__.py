"""
High-level parallelization constructs based on the Bulk Synchronous
Parallel (BSP) model.

Parallelization requires a low-level communications library, which can
be either BSPlib or MPI. Programs must be run with the bsppython or
mpipython executables in order to use several processors. When run
with a standard Python interpreter, only one processor is available.

A warning about object identity: when a communication operation
transmits a Python object to the same processor, the object in the
return list can either be the sent object or a copy of it. Application
programs thus should not make any assumptions about received objects
being different from sent objects.

@undocumented: RemoteObjects
@undocumented: Tasks
@undocumented: core
"""

from core import numberOfProcessors, processorID, ParValue, ParConstant, \
     ParData, ParSequence, ParRootSequence, ParMessages, ParTuple, \
     ParAccumulator, ParFunction, ParRootFunction, ParIndex, ParIterator, \
     ParIndexIterator, ParClass, ParBase, ParInvalid, is_invalid

import sys
if sys.modules.has_key('epydoc'):
    import core, types
    imported_names = dir()
    core_name = core.__name__
    for name in dir(core):
        if name not in imported_names:
            continue
        object = getattr(core, name)
        if isinstance(object, type) or type(object) == types.ClassType:
            setattr(object, '__module__', 'Scientific.BSP')
        elif type(object) == types.FunctionType:
            object.func_globals['__name__'] = 'Scientific.BSP'
    core.__name__ = core_name
    del types
    del core_name
    del imported_names
del sys
