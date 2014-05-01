#!/usr/bin/env python
#
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

import glob, os, subprocess
from subprocess import Popen
from optparse import OptionParser

interop_cases = ["InteropTestCase1DummyRun", "InteropTestCase2BasicP2P", "InteropTestCase3BasicPubSub", "InteropTestCase4P2PMessageSize", "InteropTestCase5PubSubMessageSize"]

interop_command = "java -cp %s org.apache.qpid.test.framework.distributedtesting.Coordinator --xml -e interop -o . -n interop org.apache.qpid.interop.testcases.%s"

# TODO: read this from the ant properties file
clientlibs = ["qpid-integrationtests-M4.jar",
              "qpid-junit-toolkit-M4.jar",
              "junit-3.8.1.jar",
              "qpid-systests-M4.jar",
              "qpid-junit-toolkit-M4.jar",
              "geronimo-jms_1.1_spec-1.0.jar",
              "log4j-1.2.12.jar"]

def main():
    parser = OptionParser()
    parser.add_option("-t", "--testlib", dest="testlib", action="store",
                      type="string",
                      help="The directory containing the test classes to run")
    parser.add_option("-b", "--brokers", dest="brokers", action="store",
                      type="string",
                      help="The directory containing the unpacked brokers to test")
    parser.add_option("-j", "--java", dest="java", action="store",
                      type="string",
                      help="The directory containing the java client to test")
    parser.add_option("-d", "--dotnet", dest="dotnet", action="store",
                      type="string",
                      help="The directory containing the .Net client to test")
    parser.add_option("-c", "--cpp", dest="cpp", action="store",
                      type="string",
                      help="The directory containing the C++ client to test")
    (options, args) = parser.parse_args()

    # check available brokers
    if (options.brokers == None or
        not os.path.exists(options.brokers) or
        not os.path.isdir(options.brokers)):
        parser.error("Broker directory must be specified and must exist")
    
    # check available clients
    if (options.java == None or
        not os.path.exists(options.java) or
        not os.path.isdir(options.java)):
        parser.error("Java client directory must be specified and must exist")

    # check available tests
    if (options.testlib == None or
        not os.path.exists(options.testlib) or
        not os.path.isdir(options.testlib)):
        parser.error("Test directory must be specified and must exist")

    # check dotnet test client
    if (options.dotnet == None or
        not os.path.exists(options.dotnet) or
        not os.path.exists(options.dotnet+"/TestClient.exe")):
        parser.error(".Net test directory must be specified and must contain TestClient.exe")

    # check cpp test client
    if (options.cpp == None or
        not os.path.exists(options.cpp) or
        not os.path.exists(options.cpp+"/src/tests/interop_runner")):
        parser.error("C++ test directory must be specified and must contain test client")
        
    # Get list of available broker and client versions
    brokers = glob.glob(options.brokers+"/qpid-[0-9].[0-9].[0-9].[0-9]")
    java_clients = glob.glob(options.java+"/qpid-[0-9].[0-9].[0-9].[0-9]*")

    if (not len(brokers) > 0):
        parser.error("Broker directory did not contain any brokers!")
        
    if (not len(java_clients) > 0):
        parser.error("Broker directory did not contain any brokers!")

    for broker in brokers:
        for client in java_clients:
            test(options.testlib, broker, client, options.dotnet,
                 options.cpp)

def start_dotnet(dotnetpath):
    return Popen(["%s/TestClient.exe" % os.path.abspath(dotnetpath),
                  "-bamqp://guest:guest@clientid/?brokerlist=\'tcp://localhost:5672\'"],
                 stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                 stderr=subprocess.STDOUT)

def start_java(javapath):
    # Setup classpath
    classpath = ""
    for lib in clientlibs:
        classpath = classpath + testlibdir+"/"+lib+";"
        
    classpath = classpath + javapath+"/lib/qpid-all.jar"        

    # Add qpid common since the tests need that, classpath hatefulness
    classpath = classpath + ";"+testlibdir+"/qpid-common-M4.jar"
    
    return Popen(["java", "-cp","\""+classpath+"\"",
                  "org.apache.qpid.test.framework.distributedtesting.TestClient"],
                 stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                 stderr=subprocess.STDOUT)

def start_cpp(cpppath):
    return Popen([cpppath+"tests/interop_runner"],
                 stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                 stderr=subprocess.STDOUT)

def run_tests():
    for testcase in interop_cases:
        cmd = " ".join(["java", "-cp","\""+classpath+"\"", "org.apache.qpid.test.framework.distributedtesting.Coordinator",
                        "--xml", "-e", "interop", "-o", ".", "-n", "interop", ("org.apache.qpid.interop.testcases.%s" % testcase)])
        fd = os.popen(cmd, "r")
        output = fd.read()
        fd.close()
        
def test(testlibdir, brokerpath, javapath, dotnetpath, cpppath):
    print ("Testing broker in %s\nclient in %s" % (brokerpath, javapath))
    print ("Logging too %s" % ("/tmp/qpid-interop-tests/%s-%s" %
                               (os.path.basename(brokerpath), os.path.basename(javapath))))
    os.environ["QPID_HOME"] = brokerpath
    os.environ["QPID_WORK"] = ("/tmp/qpid-interop-tests/%s-%s" %
                               (os.path.basename(brokerpath), os.path.basename(javapath)))

    brokerp, javacp, dotnetp, cppp, coordinatorp = (None, None, None, None, None)
    
    try:
        # Start broker
        brokerp = Popen(brokerpath+"/bin/qpid-server",
                        stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                        stderr=subprocess.STDOUT)
        print ("Started broker, pid %s" % brokerp.pid)
    
        cppp = start_cpp(cpppath)
        print ("Started C++ client, pid %s" % cppp.pid)
        
        dotnetp = start_dotnet(dotnetpath)
        print ("Started .Net client, pid %s" % dotnetp.pid)

        javacp = start_java(javapath)
        print ("Started client, pid %s" % javacp.pid)

        run_tests()
        
    finally:
        # Shutdown broker and clients 
        if javacp != None:
            os.kill(javacp.pid, 9)
        if dotnetp != None:
            os.kill(dotnetp.pid, 9)
        if coordinatorp != None:
            os.kill(coordinatorp.pid, 9)
        if brokerp != None:
            os.kill(brokerp.pid, 9)
        
    # Start coordinator
    

if __name__ == "__main__":
    main()
