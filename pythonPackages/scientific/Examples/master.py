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

from Scientific.DistributedComputing.MasterSlave import \
     initializeMasterProcess, TaskRaisedException

tasks = initializeMasterProcess("demo", slave_script="slave.py")

# Do the master's work
for i in range(5):
    # For i==0 this raises an exception
    task_id = tasks.requestTask("sqrt", float(i-1))
for i in range(5):
    try:
        task_id, tag, result = tasks.retrieveResult("sqrt")
        print result
    except TaskRaisedException, e:
        print "Task %s raised %s" % (e.task_id, str(e.exception))
        print e.traceback
