# Example for distributed computing using a master-slave setup.
# You need Pyro (pyro.sourceforge.net) to run this example.
#
# 1) Type "ns" in a shell window to start the Pyro name server.
# 2) Type "python master_slave_demo.py" in a second shell
#    window to start the master process.
# 3) Type "task_manager slave demo" in a third shell
#    window to start one slave process.
#
# You can run as many slaves as you want (though for this trivial example,
# the first slave will do all the work before you have time to start a
# second one), and you can run them on any machine on the same local
# network as the one that runs the master process.
#
# See the Pyro manual for other setups, e.g. running slaves on remote
# machines connected to the Internet.
#
# Also see master.py and slave.py to see how master and slave process can
# be defined by separate programs. This is more convenient for multi-module
# programs.
#

from Scientific.DistributedComputing.MasterSlave \
     import MasterProcess, SlaveProcess, runJob, TaskRaisedException
from Scientific import N
import sys

class Master(MasterProcess):

    def run(self):
        for i in range(5):
            # For i==0 this raises an exception
            task_id = self.requestTask("sqrt", float(i-1))
        for i in range(5):
            try:
                task_id, tag, result = self.retrieveResult("sqrt")
                print result
            except TaskRaisedException, e:
                print "Task %s raised %s" % (e.task_id, str(e.exception))
                print e.traceback

class SquareRoot(SlaveProcess):
    
    def do_sqrt(self, x):
        return (x, N.sqrt(x))


runJob("demo", Master, SquareRoot)
