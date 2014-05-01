#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

# Support library for tests that start multiple brokers, e.g. cluster
# or federation

import os, signal, string, tempfile, popen2, socket, threading, time, imp
import qpid, traceback
from qpid import connection, messaging, util
from qpid.compat import format_exc
from qpid.harness import Skipped
from unittest import TestCase
from copy import copy
from threading import Thread, Lock, Condition
from logging import getLogger

log = getLogger("qpid.brokertest")

# Values for expected outcome of process at end of test
EXPECT_EXIT_OK=1           # Expect to exit with 0 status before end of test.
EXPECT_EXIT_FAIL=2         # Expect to exit with non-0 status before end of test.
EXPECT_RUNNING=3           # Expect to still be running at end of test
    
def is_running(pid):
    try:
        os.kill(pid, 0)
        return True
    except:
        return False

class BadProcessStatus(Exception):
    pass

class ExceptionWrapper:
    """Proxy object that adds a message to exceptions raised"""
    def __init__(self, obj, msg):
        self.obj = obj
        self.msg = msg
        
    def __getattr__(self, name):
        func = getattr(self.obj, name)
        return lambda *args, **kwargs: self._wrap(func, args, kwargs)

    def _wrap(self, func, args, kwargs):
        try:
            return func(*args, **kwargs)
        except Exception, e:
            raise Exception("%s: %s" %(self.msg, str(e)))
        
def error_line(f):
    try:
        lines = file(f).readlines()
        if len(lines) > 0: return ": %s" % (lines[-1])
    except: pass
    return ""
    

class Popen(popen2.Popen3):
    """
    Similar to subprocess.Popen but using popen2 classes for portability.
    Can set and verify expectation of process status at end of test.
    Dumps command line, stdout, stderr to data dir for debugging.
    """

    def __init__(self, cmd, expect=EXPECT_EXIT_OK):
        if type(cmd) is type(""): cmd = [cmd] # Make it a list.
        self.cmd  = [ str(x) for x in cmd ]
        popen2.Popen3.__init__(self, self.cmd, True)
        self.expect = expect
        self.pname = "%s-%d" % (os.path.split(self.cmd[0])[-1], self.pid)
        msg = "Process %s" % self.pname
        self.stdin = ExceptionWrapper(self.tochild, msg)
        self.stdout = ExceptionWrapper(self.fromchild, msg)
        self.stderr = ExceptionWrapper(self.childerr, msg)
        self.dump(self.cmd_str(), "cmd")
        log.debug("Started process %s" % self.pname)

    def dump(self, str, ext):
        name = "%s.%s" % (self.pname, ext)
        f = file(name, "w")
        f.write(str)
        f.close()
        return name

    def unexpected(self,msg):
        self.dump(self.stdout.read(), "out")
        err = self.dump(self.stderr.read(), "err")
        raise BadProcessStatus("%s %s%s" % (self.pname, msg, error_line(err)))
    
    def stop(self):                  # Clean up at end of test.
        if self.expect == EXPECT_RUNNING:
            try:
                self.kill()
            except:
                self.unexpected("expected running, exit code %d" % self.wait())
        else:
            # Give the process some time to exit.
            delay = 0.1
            while (self.poll() is None and delay < 1):
                time.sleep(delay)
                delay *= 2
            if self.returncode is None: # Still haven't stopped
                self.kill()
                self.unexpected("still running")
            elif self.expect == EXPECT_EXIT_OK and self.returncode != 0:
                self.unexpected("exit code %d" % self.returncode)
            elif self.expect == EXPECT_EXIT_FAIL and self.returncode == 0:
                self.unexpected("expected error")
               
    def communicate(self, input=None):
        if input:
            self.stdin.write(input)
            self.stdin.close()
        outerr = (self.stdout.read(), self.stderr.read())
        self.wait()
        return outerr

    def is_running(self): return self.poll() is None

    def assert_running(self):
        if not self.is_running(): unexpected("Exit code %d" % self.returncode)

    def poll(self):
        self.returncode = popen2.Popen3.poll(self)
        if (self.returncode == -1): self.returncode = None
        return self.returncode

    def wait(self):
        self.returncode = popen2.Popen3.wait(self)
        return self.returncode

    def send_signal(self, sig):
        os.kill(self.pid,sig)
        self.wait()

    def terminate(self): self.send_signal(signal.SIGTERM)
    def kill(self): self.send_signal(signal.SIGKILL)

    def cmd_str(self): return " ".join([str(s) for s in self.cmd])

def checkenv(name):
    value = os.getenv(name)
    if not value: raise Exception("Environment variable %s is not set" % name)
    return value

class Broker(Popen):
    "A broker process. Takes care of start, stop and logging."
    _broker_count = 0

    def __init__(self, test, args=[], name=None, expect=EXPECT_RUNNING):
        """Start a broker daemon. name determines the data-dir and log
        file names."""

        self.test = test
        self._port = None
        cmd = [BrokerTest.qpidd_exec, "--port=0", "--no-module-dir", "--auth=no"] + args
        if name: self.name = name
        else:
            self.name = "broker%d" % Broker._broker_count
            Broker._broker_count += 1
        self.log = self.name+".log"
        cmd += ["--log-to-file", self.log, "--log-prefix", self.name]
        cmd += ["--log-to-stderr=no"] 
        self.datadir = self.name
        cmd += ["--data-dir", self.datadir]
        Popen.__init__(self, cmd, expect)
        test.cleanup_stop(self)
        self.host = "localhost"
        log.debug("Started broker %s (%s)" % (self.name, self.pname))

    def port(self):
        # Read port from broker process stdout if not already read.
        if (self._port is None):
            try: self._port = int(self.stdout.readline())
            except ValueError, e:
                raise Exception("Can't get port for broker %s (%s)%s" %
                                (self.name, self.pname, error_line(self.log)))
        return self._port

    def unexpected(self,msg):
        raise BadProcessStatus("%s: %s (%s)" % (msg, self.name, self.pname))

    def connect(self):
        """New API connection to the broker."""
        return messaging.Connection.open(self.host, self.port())

    def connect_old(self):
        """Old API connection to the broker."""
        socket = qpid.util.connect(self.host,self.port())
        connection = qpid.connection.Connection (sock=socket)
        connection.start()
        return connection;

    def declare_queue(self, queue):
        c = self.connect_old()
        s = c.session(str(qpid.datatypes.uuid4()))
        s.queue_declare(queue=queue)
        c.close()

    def send_message(self, queue, message):
        s = self.connect().session()
        s.sender(queue+"; {create:always}").send(message)
        s.connection.close()

    def send_messages(self, queue, messages):
        s = self.connect().session()
        sender = s.sender(queue+"; {create:always}")
        for m in messages: sender.send(m)
        s.connection.close()

    def get_message(self, queue):
        s = self.connect().session()
        m = s.receiver(queue+"; {create:always}", capacity=1).fetch(timeout=1)
        s.acknowledge()
        s.connection.close()
        return m

    def get_messages(self, queue, n):
        s = self.connect().session()
        receiver = s.receiver(queue+"; {create:always}", capacity=n)
        m = [receiver.fetch(timeout=1) for i in range(n)]
        s.acknowledge()
        s.connection.close()
        return m

    def host_port(self): return "%s:%s" % (self.host, self.port())
    
        
class Cluster:
    """A cluster of brokers in a test."""

    _cluster_count = 0

    def __init__(self, test, count=0, args=[], expect=EXPECT_RUNNING, wait=True):
        self.test = test
        self._brokers=[]
        self.name = "cluster%d" % Cluster._cluster_count
        Cluster._cluster_count += 1
        # Use unique cluster name
        self.args = copy(args)
        self.args += [ "--cluster-name", "%s-%s:%d" % (self.name, socket.gethostname(), os.getpid()) ]
        assert BrokerTest.cluster_lib
        self.args += [ "--load-module", BrokerTest.cluster_lib ]
        self.start_n(count, expect=expect, wait=wait)

    def start(self, name=None, expect=EXPECT_RUNNING, wait=True, args=[]):
        """Add a broker to the cluster. Returns the index of the new broker."""
        if not name: name="%s-%d" % (self.name, len(self._brokers))
        log.debug("Cluster %s starting member %s" % (self.name, name))
        self._brokers.append(self.test.broker(self.args+args, name, expect, wait))
        return self._brokers[-1]

    def start_n(self, count, expect=EXPECT_RUNNING, wait=True, args=[]):
        for i in range(count): self.start(expect=expect, wait=wait, args=args)

    # Behave like a list of brokers.
    def __len__(self): return len(self._brokers)
    def __getitem__(self,index): return self._brokers[index]
    def __iter__(self): return self._brokers.__iter__()

class BrokerTest(TestCase):
    """
    Tracks processes started by test and kills at end of test.
    Provides a well-known working directory for each test.
    """

    # Environment settings.
    qpidd_exec = checkenv("QPIDD_EXEC")
    cluster_lib = os.getenv("CLUSTER_LIB")
    xml_lib = os.getenv("XML_LIB")
    qpidConfig_exec = os.getenv("QPID_CONFIG_EXEC")
    qpidRoute_exec = os.getenv("QPID_ROUTE_EXEC")
    receiver_exec = os.getenv("RECEIVER_EXEC")
    sender_exec = os.getenv("SENDER_EXEC")
    store_lib = os.getenv("STORE_LIB")
    test_store_lib = os.getenv("TEST_STORE_LIB")
    rootdir = os.getcwd()

    def configure(self, config): self.config=config
    
    def setUp(self):
        outdir = self.config.defines.get("OUTDIR") or "brokertest.tmp"
        self.dir = os.path.join(self.rootdir, outdir, self.id())
        os.makedirs(self.dir)
        os.chdir(self.dir)
        self.stopem = []                # things to stop at end of test

    def tearDown(self):
        err = []
        for p in self.stopem:
            try: p.stop()
            except Exception, e: err.append(str(e))
        if err: raise Exception("Unexpected process status:\n    "+"\n    ".join(err))

    def cleanup_stop(self, stopable):
        """Call thing.stop at end of test"""
        self.stopem.append(stopable)

    def popen(self, cmd, expect=EXPECT_EXIT_OK):
        """Start a process that will be killed at end of test, in the test dir."""
        os.chdir(self.dir)
        p = Popen(cmd, expect)
        self.cleanup_stop(p)
        return p

    def broker(self, args=[], name=None, expect=EXPECT_RUNNING,wait=True):
        """Create and return a broker ready for use"""
        b = Broker(self, args=args, name=name, expect=expect)
        if (wait): b.connect().close()
        return b

    def cluster(self, count=0, args=[], expect=EXPECT_RUNNING, wait=True):
        """Create and return a cluster ready for use"""
        cluster = Cluster(self, count, args, expect=expect, wait=wait)
        return cluster

    def wait():
        """Wait for all brokers in the cluster to be ready"""
        for b in _brokers: b.connect().close()
        
class RethrownException(Exception):
    """Captures the original stack trace to be thrown later""" 
    def __init__(self, e, msg=""):
        Exception.__init__(self, msg+"\n"+format_exc())

class StoppableThread(Thread):
    """
    Base class for threads that do something in a loop and periodically check
    to see if they have been stopped.
    """
    def __init__(self):
        self.stopped = False
        self.error = None
        Thread.__init__(self)

    def stop(self):
        self.stopped = True
        self.join()
        if self.error: raise self.error
    
class NumberedSender(Thread):
    """
    Thread to run a sender client and send numbered messages until stopped.
    """

    def __init__(self, broker, max_depth=None):
        """
        max_depth: enable flow control, ensure sent - received <= max_depth.
        Requires self.received(n) to be called each time messages are received.
        """
        Thread.__init__(self)
        self.sender = broker.test.popen(
            [broker.test.sender_exec, "--port", broker.port()], expect=EXPECT_RUNNING)
        self.condition = Condition()
        self.max = max_depth
        self.received = 0
        self.stopped = False
        self.error = None

    def run(self):
        try:
            self.sent = 0
            while not self.stopped:
                if self.max:
                    self.condition.acquire()
                    while not self.stopped and self.sent - self.received > self.max:
                        self.condition.wait()
                    self.condition.release()
                self.sender.stdin.write(str(self.sent)+"\n")
                self.sender.stdin.flush()
                self.sent += 1
        except Exception, e: self.error = RethrownException(e, self.sender.pname)

    def notify_received(self, count):
        """Called by receiver to enable flow control. count = messages received so far."""
        self.condition.acquire()
        self.received = count
        self.condition.notify()
        self.condition.release()

    def stop(self):
        self.condition.acquire()
        self.stopped = True
        self.condition.notify()
        self.condition.release()
        self.join()
        if self.error: raise self.error
        
class NumberedReceiver(Thread):
    """
    Thread to run a receiver client and verify it receives
    sequentially numbered messages.
    """
    def __init__(self, broker, sender = None):
        """
        sender: enable flow control. Call sender.received(n) for each message received.
        """
        Thread.__init__(self)
        self.test = broker.test
        self.receiver = self.test.popen(
            [self.test.receiver_exec, "--port", broker.port()], expect=EXPECT_RUNNING)
        self.stopat = None
        self.lock = Lock()
        self.error = None
        self.sender = sender

    def continue_test(self):
        self.lock.acquire()
        ret = self.stopat is None or self.received < self.stopat
        self.lock.release()
        return ret
    
    def run(self):
        try:
            self.received = 0
            while self.continue_test():
                m = int(self.receiver.stdout.readline())
                assert(m <= self.received) # Allow for duplicates
                if (m == self.received):
                    self.received += 1
                    if self.sender:
                        self.sender.notify_received(self.received)
        except Exception, e:
            self.error = RethrownException(e, self.receiver.pname)

    def stop(self, count):
        """Returns when received >= count"""
        self.lock.acquire()
        self.stopat = count
        self.lock.release()
        self.join()
        if self.error: raise self.error

class ErrorGenerator(StoppableThread):
    """
    Thread that continuously generates errors by trying to consume from
    a non-existent queue. For cluster regression tests, error handling
    caused issues in the past.
    """

    def __init__(self, broker):
        StoppableThread.__init__(self)
        self.broker=broker
        broker.test.cleanup_stop(self)
        self.start()
        
    def run(self):
        c = self.broker.connect_old()
        try:
            while not self.stopped:
                try:
                    c.session(str(qpid.datatypes.uuid4())).message_subscribe(
                        queue="non-existent-queue")
                    assert(False)
                except qpid.session.SessionException: pass
        except: pass                    # Normal if broker is killed.

def import_script(path):
    """
    Import executable script at path as a module.
    Requires some trickery as scripts are not in standard module format
    """
    name=os.path.split(path)[1].replace("-","_")
    return imp.load_module(name, file(path), path, ("", "r", imp.PY_SOURCE))
