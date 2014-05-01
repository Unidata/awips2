# Example for distributed computing using a master-slave setup.
# You need Pyro (pyro.sourceforge.net) to run this example.
#
# 1) Type "ns" in a shell window to start the Pyro name server.
# 2) Type "python master.py" in a second shell window to start
#    the master process.
# 3) Type "task_manager slave demo" in a third shell window
#    to start one slave process.
#
# You can run as many slaves as you want (though for this trivial example,
# the first slave will do all the work before you have time to start a
# second one), and you can run them on any machine on the same local
# network as the one that runs the master process.
#
# See the Pyro manual for other setups, e.g. running slaves on remote
# machines connected to the Internet.
#
# Also see master_slave_demo.py to see how both master and slave can be
# combined within a single script, which is more convenient for short
# scripts.
#

from Scientific.DistributedComputing.MasterSlave import startSlaveProcess
from Scientific import N

# Define (or import) all the task handlers.
def do_sqrt(x):
    return (x, N.sqrt(x))


# Start the slave process after all task handlers have been defined.
startSlaveProcess()
