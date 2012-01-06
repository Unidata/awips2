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

#
# Support library for qpid python tests.
#

import os, re, signal, subprocess, time, unittest

class TestBase(unittest.TestCase):
    """
    Base class for qpid tests. Provides broker start/stop/kill methods
    """
    
    """
    The following environment vars control if and how the test is run, and determine where many of the helper
    executables/libs are to be found.
    """
    _storeLib = os.getenv("STORE_LIB")
    _storeEnable = _storeLib != None # Must be True for durability to be enabled during the test
    _qpiddExec = os.getenv("QPIDD_EXEC", "/usr/sbin/qpidd")
    _tempStoreDir = os.path.abspath(os.getenv("TMP_DATA_DIR", "/tmp/qpid"))
    
    """Global message counter ensures unique messages"""
    _msgCnt = 0
    
    # --- Helper functions for parameter handling ---
    
    def _paramBool(self, key, val, keyOnly = False):
        if val == None:
            return ""
        if keyOnly:
            if val:
                return " --%s" % key
            else:
                return ""
        else:
            if val:
                return " --%s yes" % key
            else:
                return " --%s no" % key
       
    # --- Helper functions for message creation ---
    
    def _makeMessage(self, msgSize):
        msg = "Message-%04d" % self._msgCnt
        self._msgCnt = self._msgCnt + 1
        msgLen = len(msg)
        if msgSize > msgLen:
            for i in range(msgLen, msgSize):
                if i == msgLen:
                    msg += "-"
                else:
                    msg += chr(ord('a') + (i % 26))
        return msg
    
    def _makeMessageList(self, numMsgs, msgSize):
        if msgSize == None:
            msgSize = 12
        msgs = ""
        for m in range(0, numMsgs):
            msgs += "%s\n" % self._makeMessage(msgSize)
        return msgs
    
    # --- Starting and stopping a broker ---
    
    def startBroker(self, qpiddArgs, logFile = None):
        """Start a single broker daemon, returns tuple (pid, port)"""
        if self._qpiddExec == None:
            raise Exception("Environment variable QPIDD is not set")
        cmd = "%s --daemon --port=0 %s" % (self._qpiddExec, qpiddArgs)
        portStr = os.popen(cmd).read()
        if len(portStr) == 0:
            err = "Broker daemon startup failed."
            if logFile != None:
                err += " See log file %s" % logFile
            raise Exception(err)
        port = int(portStr)
        pidStr = os.popen("%s -p %d -c" % (self._qpiddExec, port)).read()
        try:
            pid = int(pidStr)
        except:
            raise Exception("Unable to get pid: \"%s -p %d -c\" returned %s" % (self._qpiddExec, port, pidStr))
        #print "started broker: pid=%d, port=%d args: %s" % (pid, port, qpiddArgs)
        return (pid, port)
    
    def killBroker(self, nodeTuple, ignoreFailures = False):
        """Kill a broker using kill -9"""
        try:
            os.kill(nodeTuple[self.PID], signal.SIGKILL)
            try:
                os.waitpid(nodeTuple[self.PID], 0)
            except:
                pass
            #print "killed broker: port=%d pid=%d" % (nodeTuple[self.PORT], nodeTuple[self.PID])
        except:
            if ignoreFailures:
                print "WARNING: killBroker (port=%d pid=%d) failed - ignoring." % (nodeTuple[self.PORT], nodeTuple[self.PID])
            else:
                raise
    
    def stopBroker(self, nodeTuple, ignoreFailures = False):
        """Stop a broker using qpidd -q"""
        try:
            ret = os.spawnl(os.P_WAIT, self._qpiddExec, self._qpiddExec, "--port=%d" % nodeTuple[self.PORT], "--quit", "--no-module-dir")
            if ret != 0:
                raise Exception("stopBroker(): port=%d: qpidd -q returned %d" % (nodeTuple[self.PORT], ret))
            try:
                os.waitpid(nodeTuple[self.PID], 0)
            except:
                pass
            #print "stopped broker: port=%d pid=%d" % (nodeTuple[self.PORT], nodeTuple[self.PID])
        except:
            if ignoreFailures:
                print "WARNING: stopBroker (port=%d pid=%d) failed - ignoring." % (nodeTuple[self.PORT], nodeTuple[self.PID])
            else:
                raise



class TestBaseCluster(TestBase):
    """
    Base class for cluster tests. Provides methods for starting and stopping clusters and cluster nodes.
    """
    
    """
    The following environment vars control if and how the test is run, and determine where many of the helper
    executables/libs are to be found.
    """
    _clusterLib = os.getenv("CLUSTER_LIB")
    _clusterTestEnable = _clusterLib != None # Must be True for these cluster tests to run
    _xmlLib = os.getenv("XML_LIB")
    _xmlEnable = _xmlLib != None
    _qpidConfigExec = os.getenv("QPID_CONFIG_EXEC", "/usr/bin/qpid-config")
    _qpidRouteExec = os.getenv("QPID_ROUTE_EXEC", "/usr/bin/qpid-route")
    _receiverExec = os.getenv("RECEIVER_EXEC", "/usr/libexec/qpid/test/receiver")
    _senderExec = os.getenv("SENDER_EXEC", "/usr/libexec/qpid/test/sender")
    
    
    """
    _clusterDict is a dictionary of clusters:
        key = cluster name (string)
        val = dictionary of node numbers:
            key = integer node number
            val = tuple containing (pid, port)
    For example, two clusters "TestCluster0" and "TestCluster1" containing several nodes would look as follows:
    {"TestCluster0": {0: (pid0-0, port0-0), 1: (pid0-1, port0-1), ...}, "TestCluster1": {0: (pid1-0, port1-0), 1: (pid1-1, port1-1), ...}}
    where pidm-n and portm-n are the int pid and port for TestCluster m node n respectively.
    """
    _clusterDict = {}
    
    """Index for (pid, port) tuple"""
    PID = 0
    PORT = 1
    
    def run(self, res):
        """ Skip cluster testing if env var RUN_CLUSTER_TESTS is not defined."""
        if not self._clusterTestEnable:
            return
        unittest.TestCase.run(self, res)
    
    # --- Private helper / convenience functions ---
    
    def _checkPids(self, clusterName = None):
        for pid, port in self.getTupleList():
            try:
                os.kill(pid, 0)
            except:
                raise Exception("_checkPids(): Broker with pid %d expected but does not exist! (crashed?)" % pid)
        
    
    # --- Starting cluster node(s) ---
    
    def createClusterNode(self, nodeNumber, clusterName):
        """Create a node and add it to the named cluster"""
        if self._tempStoreDir == None:
            raise Exception("Environment variable TMP_DATA_DIR is not set")
        if self._clusterLib == None:
            raise Exception("Environment variable LIBCLUSTER is not set")
        name = "%s-%d" % (clusterName, nodeNumber)
        dataDir = os.path.join(self._tempStoreDir, "cluster", name)
        logFile = "%s.log" % dataDir
        args = "--no-module-dir --load-module=%s --data-dir=%s --cluster-name=%s --auth=no --log-enable=notice+ --log-to-file=%s" % \
            (self._clusterLib, dataDir, clusterName, logFile)
        if self._storeEnable:
            if self._storeLib == None:
                raise Exception("Environment variable LIBSTORE is not set")
            args += " --load-module %s" % self._storeLib
        self._clusterDict[clusterName][nodeNumber] = self.startBroker(args, logFile)
    
    def createCluster(self, clusterName, numberNodes = 0):
        """Create a cluster containing an initial number of nodes"""
        self._clusterDict[clusterName] = {}
        for n in range(0, numberNodes):
            self.createClusterNode(n, clusterName)
    
    def waitForNodes(self, clusterName):
        """Wait for all nodes to become active (ie finish cluster sync)"""
        # TODO - connect to each known node in cluster
        #        Until this is done, wait a bit (hack)
        time.sleep(1)
    
    # --- Cluster and node status ---
    
    def getTupleList(self, clusterName = None):
        """Get list of (pid, port) tuples of all known cluster brokers"""
        tList = []
        for c, l in self._clusterDict.iteritems():
            if clusterName == None or c == clusterName:
                for t in l.itervalues():
                    tList.append(t)
        return tList
    
    def getNumBrokers(self):
        """Get total number of brokers in all known clusters"""
        return len(self.getTupleList())
    
    def checkNumBrokers(self, expected = None, checkPids = True):
        """Check that the total number of brokers in all known clusters is the expected value"""
        if expected != None and self.getNumBrokers() != expected:
            raise Exception("Unexpected number of brokers: expected %d, found %d" % (expected, self.getNumBrokers()))
        if checkPids:
            self._checkPids()

    def getClusterTupleList(self, clusterName):
        """Get list of (pid, port) tuples of all nodes in named cluster"""
        if clusterName in self._clusterDict:
            return self._clusterDict[clusterName].values()
        return []
    
    def getNumClusterBrokers(self, clusterName):
        """Get total number of brokers in named cluster"""
        return len(self.getClusterTupleList(clusterName))
    
    def getNodeTuple(self, nodeNumber, clusterName):
        """Get the (pid, port) tuple for the given cluster node"""
        return self._clusterDict[clusterName][nodeNumber]
    
    def checkNumClusterBrokers(self, clusterName, expected = None, checkPids = True, waitForNodes = True):
        """Check that the total number of brokers in the named cluster is the expected value"""
        if expected != None and self.getNumClusterBrokers(clusterName) != expected:
            raise Exception("Unexpected number of brokers in cluster %s: expected %d, found %d" % \
                            (clusterName, expected, self.getNumClusterBrokers(clusterName)))
        if checkPids:
            self._checkPids(clusterName)
        if waitForNodes:
            self.waitForNodes(clusterName)

    def clusterExists(self, clusterName):
        """ Return True if clusterName exists, False otherwise"""
        return clusterName in self._clusterDict.keys()
    
    def clusterNodeExists(self, clusterName, nodeNumber):
        """ Return True if nodeNumber in clusterName exists, False otherwise"""
        if clusterName in self._clusterDict.keys():
            return nodeNumber in self._clusterDict[nodeName]
        return False
    
    def createCheckCluster(self, clusterName, size):
        """Create a cluster using the given name and size, then check the number of brokers"""
        self.createCluster(clusterName, size)
        self.checkNumClusterBrokers(clusterName, size)
    
    # --- Kill cluster nodes using signal 9 ---
    
    def killNode(self, nodeNumber, clusterName, updateDict = True, ignoreFailures = False):
        """Kill the given node in the named cluster using kill -9"""
        self.killBroker(self.getNodeTuple(nodeNumber, clusterName), ignoreFailures)
        if updateDict:
            del(self._clusterDict[clusterName][nodeNumber])
    
    def killCluster(self, clusterName, updateDict = True, ignoreFailures = False):
        """Kill all nodes in the named cluster"""
        for n in self._clusterDict[clusterName].iterkeys():
            self.killNode(n, clusterName, False, ignoreFailures)
        if updateDict:
            del(self._clusterDict[clusterName])
    
    def killClusterCheck(self, clusterName):
        """Kill the named cluster and check that the name is removed from the cluster dictionary"""
        self.killCluster(clusterName)
        if self.clusterExists(clusterName):
            raise Exception("Unable to kill cluster %s; %d nodes still exist" % \
                            (clusterName, self.getNumClusterBrokers(clusterName)))
    
    def killAllClusters(self, ignoreFailures = False):
        """Kill all known clusters"""
        for n in self._clusterDict.iterkeys():
            self.killCluster(n, False, ignoreFailures)
        self._clusterDict.clear() 
    
    def killAllClustersCheck(self, ignoreFailures = False):
        """Kill all known clusters and check that the cluster dictionary is empty"""
        self.killAllClusters(ignoreFailures)
        self.checkNumBrokers(0)
    
    # --- Stop cluster nodes using qpidd -q ---
    
    def stopNode(self, nodeNumber, clusterName, updateDict = True, ignoreFailures = False):
        """Stop the given node in the named cluster using qpidd -q"""
        self.stopBroker(self.getNodeTuple(nodeNumber, clusterName), ignoreFailures)
        if updateDict:
            del(self._clusterDict[clusterName][nodeNumber])
    
    def stopAllClusters(self, ignoreFailures = False):
        """Stop all known clusters"""
        for n in self._clusterDict.iterkeys():
            self.stopCluster(n, False, ignoreFailures)
        self._clusterDict.clear() 

    
    def stopCluster(self, clusterName, updateDict = True, ignoreFailures = False):
        """Stop all nodes in the named cluster"""
        for n in self._clusterDict[clusterName].iterkeys():
            self.stopNode(n, clusterName, False, ignoreFailures)
        if updateDict:
            del(self._clusterDict[clusterName])
    
    def stopCheckCluster(self, clusterName, ignoreFailures = False):
        """Stop the named cluster and check that the name is removed from the cluster dictionary"""
        self.stopCluster(clusterName, True, ignoreFailures)
        if self.clusterExists(clusterName):
            raise Exception("Unable to kill cluster %s; %d nodes still exist" % (clusterName, self.getNumClusterBrokers(clusterName)))
    
    def stopAllCheck(self, ignoreFailures = False):
        """Kill all known clusters and check that the cluster dictionary is empty"""
        self.stopAllClusters()
        self.checkNumBrokers(0)
    
    # --- qpid-config functions ---
    
    def _qpidConfig(self, nodeNumber, clusterName, action):
        """Configure some aspect of a qpid broker using the qpid_config executable"""
        port = self.getNodeTuple(nodeNumber, clusterName)[self.PORT]
        #print "%s -a localhost:%d %s" % (self._qpidConfigExec, port, action)
        ret = os.spawnl(os.P_WAIT, self._qpidConfigExec, self._qpidConfigExec, "-a", "localhost:%d" % port,  *action.split())
        if ret != 0:
            raise Exception("_qpidConfig(): cluster=\"%s\" nodeNumber=%d port=%d action=\"%s\" returned %d" % \
                            (clusterName, nodeNumber, port, action, ret))
    
    def addExchange(self, nodeNumber, clusterName, exchangeType, exchangeName, durable = False, sequence = False, \
                    ive = False):
        """Add a named exchange."""
        action = "add exchange %s %s" % (exchangeType, exchangeName)
        action += self._paramBool("durable", durable, True)
        action += self._paramBool("sequence", sequence, True)
        action += self._paramBool("ive", ive, True)
        self._qpidConfig(nodeNumber, clusterName, action)
    
    def deleteExchange(self, nodeNumber, clusterName, exchangeName):
        """Delete a named exchange"""
        self._qpidConfig(nodeNumber, clusterName, "del exchange %s" % exchangeName)

    def addQueue(self, nodeNumber, clusterName, queueName, configArgs = None):
        """Add a queue using qpid-config."""
        action = "add queue %s" % queueName
        if self._storeEnable:
            action += " --durable"
        if configArgs != None:
            action += " %s" % configArgs
        self._qpidConfig(nodeNumber, clusterName, action)
    
    def delQueue(self, nodeNumber, clusterName, queueName):
        """Delete a named queue using qpid-config."""
        self._qpidConfig(nodeNumber, clusterName, "del queue %s" % queueName)
    
    def bind(self, nodeNumber, clusterName, exchangeName, queueName, key):
        """Create an exchange-queue binding using qpid-config."""
        self._qpidConfig(nodeNumber, clusterName, "bind %s %s %s" % (exchangeName, queueName, key))
    
    def unbind(self, nodeNumber, clusterName, exchangeName, queueName, key):
        """Remove an exchange-queue binding using qpid-config."""
        self._qpidConfig(nodeNumber, clusterName, "unbind %s %s %s" % (exchangeName, queueName, key))
    
    # --- qpid-route functions (federation) ---
    
    def brokerDict(self, nodeNumber, clusterName, host = "localhost", user = None, password = None):
        """Returns a dictionary containing the broker info to be passed to route functions"""
        port = self.getNodeTuple(nodeNumber, clusterName)[self.PORT]
        return {"cluster": clusterName, "node":nodeNumber, "port":port, "host":host, "user":user, "password":password}
    
    def _brokerStr(self, brokerDict):
        """Set up a broker string in the format [user/password@]host:port"""
        str = ""
        if brokerDict["user"] !=None and brokerDict["password"] != None:
            str = "%s@%s" % (brokerDict["user"], brokerDict["password"])
        str += "%s:%d" % (brokerDict["host"], brokerDict["port"])
        return str
    
    def _qpidRoute(self, action):
        """Set up a route using qpid-route"""
        #print "%s %s" % (self._qpidRouteExec, action)
        ret = os.spawnl(os.P_WAIT, self._qpidRouteExec, self._qpidRouteExec, *action.split())
        if ret != 0:
            raise Exception("_qpidRoute(): action=\"%s\" returned %d" % (action, ret))
        
    def routeDynamicAdd(self, destBrokerDict, srcBrokerDict, exchangeName):
        self._qpidRoute("dynamic add %s %s %s" % (self._brokerStr(destBrokerDict), self._brokerStr(srcBrokerDict), exchangeName))
         
    def routeDynamicDelete(self, destBrokerDict, srcBrokerDict, exchangeName):
        self._qpidRoute("dynamic del %s %s %s" % (self._brokerStr(destBrokerDict), self._brokerStr(srcBrokerDict), exchangeName))
         
    def routeAdd(self, destBrokerDict, srcBrokerDict, exchangeName, routingKey):
        self._qpidRoute("route add %s %s %s %s" % (self._brokerStr(destBrokerDict), self._brokerStr(srcBrokerDict), exchangeName, routingKey))
         
    def routeDelete(self, destBrokerDict, srcBrokerDict, exchangeName, routingKey):
        self._qpidRoute("route del %s %s %s %s" % (self._brokerStr(destBrokerDict), self._brokerStr(srcBrokerDict), exchangeName, routingKey))
    
    def routeQueueAdd(self, destBrokerDict, srcBrokerDict, exchangeName, queueName):
        self._qpidRoute("queue add %s %s %s %s" % (self._brokerStr(destBrokerDict), self._brokerStr(srcBrokerDict), exchangeName, queueName))
    
    def routeQueueDelete(self, destBrokerDict, srcBrokerDict, exchangeName, queueName):
        self._qpidRoute("queue del %s %s %s %s" % (self._brokerStr(destBrokerDict), self._brokerStr(srcBrokerDict), exchangeName, queueName))
    
    def routeLinkAdd(self, destBrokerDict, srcBrokerDict):
        self._qpidRoute("link add %s %s" % (self._brokerStr(destBrokerDict), self._brokerStr(srcBrokerDict)))
     
    def routeLinkDelete(self, destBrokerDict, srcBrokerDict):
        self._qpidRoute("link del %s %s" % (self._brokerStr(destBrokerDict), self._brokerStr(srcBrokerDict)))
    
    # --- Message send and receive functions ---
    
    def _receiver(self, action):
        if self._receiverExec == None:
            raise Exception("Environment variable RECEIVER is not set")
        cmd = "%s %s" % (self._receiverExec, action)
        #print cmd
        return subprocess.Popen(cmd.split(), stdout = subprocess.PIPE)
    
    def _sender(self, action):
        if self._senderExec == None:
            raise Exception("Environment variable SENDER is not set")
        cmd = "%s %s" % (self._senderExec, action)
        #print cmd
        return subprocess.Popen(cmd.split(), stdin = subprocess.PIPE)
    
    def createReciever(self, nodeNumber, clusterName, queueName, numMsgs = None, receiverArgs = None):
        port = self.getNodeTuple(nodeNumber, clusterName)[self.PORT]
        action = "--port %d --queue %s" % (port, queueName)
        if numMsgs != None:
            action += " --messages %d" % numMsgs
        if receiverArgs != None:
            action += " %s" % receiverArgs
        return self._receiver(action)
    
    def createSender(self, nodeNumber, clusterName, exchangeName, routingKey, senderArgs = None):
        port = self.getNodeTuple(nodeNumber, clusterName)[self.PORT]
        action = "--port %d --exchange %s" % (port, exchangeName)
        if routingKey != None and len(routingKey) > 0:
            action += " --routing-key %s" % routingKey
        if self._storeEnable:
            action += " --durable yes"
        if senderArgs != None:
            action += " %s" % senderArgs
        return self._sender(action)
    
    def createBindDirectExchangeQueue(self, nodeNumber, clusterName, exchangeName, queueName):
        self.addExchange(nodeNumber, clusterName, "direct", exchangeName)
        self.addQueue(nodeNumber, clusterName, queueName)
        self.bind(nodeNumber, clusterName, exchangeName, queueName, queueName)
    
    def createBindTopicExchangeQueues(self, nodeNumber, clusterName, exchangeName, queueNameKeyList):
        self.addExchange(nodeNumber, clusterName, "topic", exchangeName)
        for queueName, key in queueNameKeyList.iteritems():
            self.addQueue(nodeNumber, clusterName, queueName)
            self.bind(nodeNumber, clusterName, exchangeName, queueName, key)
    
    def createBindFanoutExchangeQueues(self, nodeNumber, clusterName, exchangeName, queueNameList):
        self.addExchange(nodeNumber, clusterName, "fanout", exchangeName)
        for queueName in queueNameList:
            self.addQueue(nodeNumber, clusterName, queueName)
            self.bind(nodeNumber, clusterName, exchangeName, queueName, "")
    
    def sendMsgs(self, nodeNumber, clusterName, exchangeName, routingKey, numMsgs, msgSize = None, wait = True):
        msgs = self._makeMessageList(numMsgs, msgSize)
        sender = self.createSender(nodeNumber, clusterName, exchangeName, routingKey)
        sender.stdin.write(msgs)
        sender.stdin.close()
        if wait:
            sender.wait()
        return msgs
    
    def receiveMsgs(self, nodeNumber, clusterName, queueName, numMsgs, wait = True):
        receiver = self.createReciever(nodeNumber, clusterName, queueName, numMsgs)
        cnt = 0
        msgs = ""
        while cnt < numMsgs:
            rx = receiver.stdout.readline()
            if rx == "" and receiver.poll() != None: break
            msgs += rx
            cnt = cnt + 1
        if wait:
            receiver.wait()
        return msgs


    # --- Exchange-specific helper inner classes ---

    class TestHelper:
        """
        This is a "virtual" superclass for test helpers, and is not useful on its own, but the
        per-exchange subclasses are designed to keep track of the messages sent to and received
        from queues which have bindings to that exchange type.
        """
        
        def __init__(self, testBaseCluster, clusterName, numNodes, exchangeName, queueNameList):
        
            """Dictionary of queues and lists of messages sent to them."""
            self._txMsgs = {}
            """Dictionary of queues and lists of messages received from them."""
            self._rxMsgs = {}
            """List of node numbers currently in the cluster"""
            self._nodes = []
            """List of node numbers which have been killed and can therefore be recovered"""
            self._deadNodes = []
            """Last node to be used"""
            self._lastNode = None
            
            self._testBaseCluster = testBaseCluster
            self._clusterName = clusterName
            self._exchangeName = exchangeName
            self._queueNameList = queueNameList
            self._addQueues(queueNameList)
            self._testBaseCluster.createCheckCluster(clusterName, numNodes)
            self._nodes.extend(range(0, numNodes))
        
        def _addQueues(self, queueNameList):
             for qn in queueNameList:
                if not qn in self._txMsgs:
                    self._txMsgs[qn] = []
                if not qn in self._rxMsgs:
                    self._rxMsgs[qn] = []
       
        def _bindQueue(self, queueName, bindingKey, nodeNumber = None):
            """Bind a queue to an exchange using a binding key."""
            if nodeNumber == None:
                nodeNumber = self._nodes[0] # first available node
            self._testBaseCluster.addQueue(nodeNumber, self._clusterName, queueName)
            self._testBaseCluster.bind(nodeNumber, self._clusterName, self._exchangeName, queueName, bindingKey)
        
        def _highestNodeNumber(self):
            """Find the highest node number used so far between the current nodes and those stopped/killed."""
            highestNode = self._nodes[-1]
            if len(self._deadNodes) == 0:
                return highestNode
            highestDeadNode = self._deadNodes[-1]
            if highestNode > highestDeadNode:
                return highestNode
            return highestDeadNode
        
        def killCluster(self):
            """Kill all nodes in the cluster"""
            self._testBaseCluster.killCluster(self._clusterName)
            self._testBaseCluster.checkNumClusterBrokers(self._clusterName, 0)
            self._deadNodes.extend(self._nodes)
            self._deadNodes.sort()
            del self._nodes[:]
        
        def restoreCluster(self, lastNode = None, restoreNodes = True):
            """Restore a previously killed cluster"""
            self._testBaseCluster.createCluster(self._clusterName)
            if restoreNodes:
                numNodes = len(self._deadNodes)
                self.restoreNodes(lastNode)
                self._testBaseCluster.checkNumClusterBrokers(self._clusterName, numNodes)
        
        def addNodes(self, numberOfNodes = 1):
            """Add a fixed number of nodes to the cluster."""
            nodeStart = self._highestNodeNumber() + 1
            for i in range(0, numberOfNodes):
                nodeNumber = nodeStart + i
                self._testBaseCluster.createClusterNode(nodeNumber, self._clusterName)
                self._nodes.append(nodeNumber)
            self._testBaseCluster.checkNumClusterBrokers(self._clusterName, len(self._nodes))
            self._testBaseCluster.waitForNodes(self._clusterName)
        
        def restoreNode(self, nodeNumber):
            """Restore a cluster node that has been previously killed"""
            if nodeNumber not in self._deadNodes:
                raise Exception("restoreNode(): Node number %d not in dead node list %s" % (nodeNumber, self._deadNodes))
            self._testBaseCluster.createClusterNode(nodeNumber, self._clusterName)
            self._deadNodes.remove(nodeNumber)
            self._nodes.append(nodeNumber)
            self._nodes.sort()
        
        def restoreNodes(self, lastNode = None):
            """Restore all known cluster nodes that have been previously killed starting with a known last-used node"""
            if len(self._nodes) == 0: # restore last-used node first
                if lastNode == None:
                    lastNode = self._lastNode
                self.restoreNode(lastNode)
            while len(self._deadNodes) > 0:
                self.restoreNode(self._deadNodes[0])
            self._testBaseCluster.waitForNodes(self._clusterName)
        
        def killNode(self, nodeNumber):
            """Kill a cluster node (if it is in the _nodes list)."""
            if nodeNumber not in self._nodes:
                raise Exception("killNode(): Node number %d not in node list %s" % (nodeNumber, self._nodes))
            self._testBaseCluster.killNode(nodeNumber, self._clusterName)
            self._nodes.remove(nodeNumber)
            self._deadNodes.append(nodeNumber)
            self._deadNodes.sort()
        
        def sendMsgs(self, routingKey, numMsgs, nodeNumber = None, msgSize = None, wait = True):
            """Send a fixed number of messages using the given routing key."""
            if nodeNumber == None:
                nodeNumber = self._nodes[0] # Use first available node
            msgs = self._testBaseCluster._makeMessageList(numMsgs, msgSize)
            sender = self._testBaseCluster.createSender(nodeNumber, self._clusterName, self._exchangeName, routingKey)
            sender.stdin.write(msgs)
            sender.stdin.close()
            if wait:
                sender.wait()
            self._lastNode = nodeNumber
            return msgs.split()
        
        # TODO - this i/f is messy: one mumMsgs can be given, but a list of queues
        #        so assuming numMsgs for each queue
        #        A mechanism is needed to specify a different numMsgs per queue
        def receiveMsgs(self, numMsgs, nodeNumber = None, queueNameList = None, wait = True):
            """Receive a fixed number of messages from a named queue. If numMsgs == None, get all remaining messages."""
            if nodeNumber == None:
                nodeNumber = self._nodes[0] # Use first available node
            if queueNameList == None:
                queueNameList = self._txMsgs.iterkeys()
            for qn in queueNameList:
                nm = numMsgs
                if nm == None:
                    nm = len(self._txMsgs[qn]) - len(self._rxMsgs[qn]) # get all remaining messages
                if nm > 0:
                    while nm > 0:
                        receiver = self._testBaseCluster.createReciever(nodeNumber, self._clusterName, qn, nm)
                        cnt = 0
                        while cnt < nm:
                            rx = receiver.stdout.readline().strip()
                            if rx == "":
                                if receiver.poll() != None: break
                            elif rx not in self._rxMsgs[qn]:
                                self._rxMsgs[qn].append(rx)
                                cnt = cnt + 1
                        nm = nm - cnt
                    if wait:
                        receiver.wait()
                    self._rxMsgs[qn].sort()
                    self._lastNode = nodeNumber
        
        def receiveRemainingMsgs(self, nodeNumber = None, queueNameList = None, wait = True):
            """Receive all remaining messages on named queue."""
            self.receiveMsgs(None, nodeNumber, queueNameList, wait)
        
        def checkMsgs(self):
            """Return True if all expected messages have been received (ie the transmit and receive list are identical)."""
            txMsgTot = 0
            rxMsgTot = 0
            for qn, txMsgList in self._txMsgs.iteritems():
                rxMsgList = self._rxMsgs[qn]
                txMsgTot = txMsgTot + len(txMsgList)
                rxMsgTot = rxMsgTot + len(rxMsgList)
                if len(txMsgList) != len(rxMsgList):
                    return False
                for i, m in enumerate(txMsgList):
                    if m != rxMsgList[i]:
                        return False
            if txMsgTot == 0 and rxMsgTot == 0:
                print "WARNING: No messages were either sent or received"
            return True
        
        def finalizeTest(self):
            """Recover all the remaining messages on all queues, then check that all expected messages were received."""
            self.receiveRemainingMsgs()
            self._testBaseCluster.stopAllCheck()
            if not self.checkMsgs():
                self.printMsgs()                
                self._testBaseCluster.fail("Send - receive message mismatch")
        
        def printMsgs(self, txMsgs = True, rxMsgs = True):
            """Print all messages transmitted and received."""
            for qn, txMsgList in self._txMsgs.iteritems():
                print "Queue: %s" % qn
                if txMsgs:
                    print "  txMsgList = %s" % txMsgList
                if rxMsgs:
                    rxMsgList = self._rxMsgs[qn]
                    print "  rxMsgList = %s" % rxMsgList


    class DirectExchangeTestHelper(TestHelper):
        
        def __init__(self, testBaseCluster, clusterName, numNodes, exchangeName, queueNameList):
            TestBaseCluster.TestHelper.__init__(self, testBaseCluster, clusterName, numNodes, exchangeName, queueNameList)
            self._testBaseCluster.addExchange(0, clusterName, "direct", exchangeName)
            for qn in queueNameList:
                self._bindQueue(qn, qn)
        
        def addQueues(self, queueNameList):
            self._addQueues(queueNameList)
            for qn in queueNameList:
                self._bindQueue(qn, qn)
                
        def sendMsgs(self, numMsgs, nodeNumber = None, queueNameList = None, msgSize = None, wait = True):
            if queueNameList == None:
                queueNameList = self._txMsgs.iterkeys()
            for qn in queueNameList:
                self._txMsgs[qn].extend(TestBaseCluster.TestHelper.sendMsgs(self, qn, numMsgs, nodeNumber, msgSize, wait))


    class TopicExchangeTestHelper(TestHelper):
        
        def __init__(self, testBaseCluster, clusterName, numNodes, exchangeName, queueNameKeyList):
            self._queueNameKeyList = queueNameKeyList
            TestBaseCluster.TestHelper.__init__(self, testBaseCluster, clusterName, numNodes, exchangeName, queueNameKeyList.iterkeys())
            self._testBaseCluster.addExchange(0, clusterName, "topic", exchangeName)
            for qn, bk in queueNameKeyList.iteritems():
                self._bindQueue(qn, bk)                
        
        def addQueues(self, queueNameKeyList):
            self._addQueues(queueNameKeyList.iterkeys())
            for qn, bk in queueNameKeyList.iteritems():
                self._bindQueue(qn, bk)
        
        def _prepareRegex(self, bk):
            # This regex conversion is not very complete - there are other chars that should be escaped too
            return "^%s$" % bk.replace(".", r"\.").replace("*", r"[^.]*").replace("#", ".*")
        
        def sendMsgs(self, routingKey, numMsgs, nodeNumber = None, msgSize = None, wait = True):
            msgList = TestBaseCluster.TestHelper.sendMsgs(self, routingKey, numMsgs, nodeNumber, msgSize, wait)
            for qn, bk in self._queueNameKeyList.iteritems():
                if re.match(self._prepareRegex(bk), routingKey):
                    self._txMsgs[qn].extend(msgList)


    class FanoutExchangeTestHelper(TestHelper):
        
        def __init__(self, testBaseCluster, clusterName, numNodes, exchangeName, queueNameList):
            TestBaseCluster.TestHelper.__init__(self, testBaseCluster, clusterName, numNodes, exchangeName, queueNameList)
            self._testBaseCluster.addExchange(0, clusterName, "fanout", exchangeName)
            for qn in queueNameList:
                self._bindQueue(qn, "")
        
        def addQueues(self, queueNameList):
            self._addQueues(queueNameList)
            for qn in queueNameList:
                self._bindQueue(qn, "")
                
        def sendMsgs(self, numMsgs, nodeNumber = None, msgSize = None, wait = True):
            msgList = TestBaseCluster.TestHelper.sendMsgs(self, "", numMsgs, nodeNumber, msgSize, wait)
            for ml in self._txMsgs.itervalues():
                ml.extend(msgList) 
            
