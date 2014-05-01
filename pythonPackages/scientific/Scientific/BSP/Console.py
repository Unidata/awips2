# Parallel console for interactive use of Python/BSP
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# last revision: 2006-6-12
#

"""
Interactive console for parallel computing
"""

from Scientific.BSP import ParValue, ParRootFunction, \
                           processorID, numberOfProcessors
import Scientific.BSP.core
from code import InteractiveInterpreter, InteractiveConsole
from cStringIO import StringIO
import exceptions, sys

#
# Special unique object used in synchronization.
#
class OKToken:

    def __init__(self):
        pass

ok = OKToken()
def isOK(object):
    try:
        return object.__class__ is OKToken
    except AttributeError:
        return 0

#
# Synchronise all processes when there is a possibility than some have crashed.
#
def synchronize():
    done = 0
    while not done:
        put(ok, range(processorID) + range(processorID+1, numberOfProcessors))
        status = sync()
        if len(status) == numberOfProcessors-1:
            done = 1
            for object in status:
                if not isOK(object):
                    done = 0

#
# Special exception object. It is raised by the modified sync()
# routine below when it notices that some other process has crashed.
#
class BSPSyncError(exceptions.Exception):
    pass

#
# Modified sync() routine used by interactively executed code
# When it captures one of the unique objects (that no other module
# can ever generate), it knows that some other process has crashed
# prematurely and it crashes its own processes as well to prevent
# lockups.
#
def syncCheck():
    messages = sync()
    for object in messages:
        if isOK(object):
            raise BSPSyncError
    return messages
put = Scientific.BSP.core.put
sync = Scientific.BSP.core.sync
Scientific.BSP.core.sync = syncCheck

#
# Wrappers around stdout and stderr for printing processor numbers
#
class OutputStream:

    def __init__(self, stream):
        self.stream = stream

    def write(self, string):
        global _pid_banner
        if _pid_banner is not None and string:
            self.stream.write(_pid_banner)
        self.stream.write(string)
        _pid_banner = None

    def __getattr__(self, attr):
        return getattr(self.stream, attr)

_pid_banner = None

#
# Type function that works "through" ParValue objects
#
def typePar(object):
    if hasattr(object, 'is_parvalue'):
        return ParValue(type(object.value))
    else:
        return type(object)

#
# Parallel console run on processor 0
#
class ParallelConsole(InteractiveConsole):

    def __init__(self):
        namespace = {"__name__": "__console__",
                     "__doc__": None,
                     "type": typePar}
        InteractiveConsole.__init__(self, namespace)

    def push(self, line):
        global _pid_banner
        self.buffer.append(line)
        source = "\n".join(self.buffer)
        put(source, range(1, numberOfProcessors))
        sync()
        _pid_banner = ("-- Processor %d " % processorID) + 40*'-' + '\n'
        more = self.runsource(source, self.filename)
        _pid_banner = None
        synchronize()
        output = sync()
        output.sort(lambda a, b: cmp(a[0], b[0]))
        for pid, stdout, stderr in output:
            if stdout or stderr:
                print ("-- Processor %d " % pid) + 40*'-'
                if stdout:
                    print stdout,
                if stderr:
                    print stderr,
        if not more:
            self.resetbuffer()
        return more

    def mainloop(self):
        cprt = 'Type "copyright", "credits" or "license" for more information.'
        banner = "Python %s on %s\n%s\n(Parallel console, %d processors)" % \
                 (sys.version, sys.platform, cprt, numberOfProcessors)
        while 1:
            try:
                self.interact(banner)
                break
            except KeyboardInterrupt:
                print "KeyboardInterrupt"
                banner = ""
        put(None, range(1, numberOfProcessors))
        sync()

#
# Interpreter run on all other processors
#
class ParallelInterpreter(InteractiveInterpreter):

    def __init__(self):
        locals = {"__name__": "__console__",
                  "__doc__": None,
                  "type": typePar}
        InteractiveInterpreter.__init__(self, locals)

    def mainloop(self):
        while 1:
            sys.stdout.close()
            sys.stdout = StringIO()
            sys.stderr.close()
            sys.stderr = StringIO()
            source = sync()[0]
            if source is None:
                break
            self.runsource(source)
            synchronize()
            put((processorID, sys.stdout.getvalue(), sys.stderr.getvalue()),
                [0])
            sync()

#
# Parallel console function
#
def console():
    sys.stdout = OutputStream(sys.stdout)
    sys.stderr = OutputStream(sys.stderr)
    my_console = ParallelConsole()
    my_console.raw_input = raw_input
    my_console.mainloop()

def interpreter():
    sys.stdout = StringIO()
    sys.stderr = StringIO()
    my_interpreter = ParallelInterpreter()
    my_interpreter.mainloop()

parallelConsole = ParRootFunction(console, interpreter)

#
# Run parallel console
#
if __name__ == '__main__':
    parallelConsole()
