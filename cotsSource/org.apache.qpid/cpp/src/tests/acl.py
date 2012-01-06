#!/usr/bin/env python
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

import sys
import qpid
from qpid.util import connect
from qpid.connection import Connection
from qpid.datatypes import uuid4
from qpid.testlib import TestBase010
from qmf.console import Session
from qpid.datatypes import Message

class ACLFile:
    def __init__(self):
        self.f = open('data_dir/policy.acl','w');
   
    def write(self,line):
        self.f.write(line)
    
    def close(self):
        self.f.close()
        
class ACLTests(TestBase010):

    def get_session(self, user, passwd):
        socket = connect(self.broker.host, self.broker.port)
        connection = Connection (sock=socket, username=user, password=passwd)
        connection.start()
        return connection.session(str(uuid4()))

    def reload_acl(self):
        acl = self.qmf.getObjects(_class="acl")[0]    
        return acl.reloadACLFile()

    def setUp(self):
        aclf = ACLFile()
        aclf.write('acl allow all all\n')
        aclf.close()
        TestBase010.setUp(self)
        self.startQmf()
        self.reload_acl()
        
   #=====================================
   # ACL general tests
   #=====================================     
        
    def test_deny_mode(self):
        """
        Test the deny all mode
        """
        aclf = ACLFile()
        aclf.write('acl allow anonymous all all\n')
        aclf.write('acl allow bob@QPID create queue\n')
        aclf.write('acl deny all all')
        aclf.close()        
        
        result = self.reload_acl()
        if (result.text.find("format error",0,len(result.text)) != -1):
            self.fail(result) 
        
        session = self.get_session('bob','bob')
        try:
            session.queue_declare(queue="deny_queue")
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow queue create request");
            self.fail("Error during queue create request");
        
        try:
            session.exchange_bind(exchange="amq.direct", queue="deny_queue", binding_key="routing_key")
            self.fail("ACL should deny queue bind request");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)           
    
    def test_allow_mode(self):
        """
        Test the allow all mode
        """
        aclf = ACLFile()
        aclf.write('acl deny bob@QPID bind exchange\n')
        aclf.write('acl allow all all')
        aclf.close()        
        
        result = self.reload_acl()
        if (result.text.find("format error",0,len(result.text)) != -1):
            self.fail(result)       
        
        session = self.get_session('bob','bob')
        try:
            session.queue_declare(queue="allow_queue")
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow queue create request");
            self.fail("Error during queue create request");
        
        try:
            session.exchange_bind(exchange="amq.direct", queue="allow_queue", binding_key="routing_key")
            self.fail("ACL should deny queue bind request");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)                
        

    def test_group_and_user_with_same_name(self):
        """
        Test a group and user with same name
        Ex. group admin admin 
        """
        aclf = ACLFile()
        aclf.write('group bob@QPID bob@QPID\n')
        aclf.write('acl deny bob@QPID bind exchange\n')
        aclf.write('acl allow all all')
        aclf.close()

        result = self.reload_acl()
        if (result.text.find("format error",0,len(result.text)) != -1):
            self.fail(result) 

        session = self.get_session('bob','bob')
        try:
            session.queue_declare(queue="allow_queue")
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow queue create request");
            self.fail("Error during queue create request");

        try:
            session.exchange_bind(exchange="amq.direct", queue="allow_queue", binding_key="routing_key")
            self.fail("ACL should deny queue bind request");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
       
 
   #=====================================
   # ACL file format tests
   #=====================================     
        
    def test_empty_groups(self):
        """
        Test empty groups
        """
        aclf = ACLFile()
        aclf.write('acl group\n')
        aclf.write('acl group admins bob@QPID joe@QPID\n')
        aclf.write('acl allow all all')
        aclf.close()        
        
        result = self.reload_acl()       
        if (result.text.find("Insufficient tokens for acl definition",0,len(result.text)) == -1):
            self.fail("ACL Reader should reject the acl file due to empty group name")    

    def test_illegal_acl_formats(self):
        """
        Test illegal acl formats
        """
        aclf = ACLFile()
        aclf.write('acl group admins bob@QPID joe@QPID\n')
        aclf.write('acl allow all all')
        aclf.close()
        
        result = self.reload_acl()       
        if (result.text.find("Unknown ACL permission",0,len(result.text)) == -1):
            self.fail(result)        
        
    def test_illegal_extension_lines(self):
        """
        Test illegal extension lines
        """
         
        aclf = ACLFile()
        aclf.write('group admins bob@QPID \ ')
        aclf.write('          \ \n')
        aclf.write('joe@QPID \n')
        aclf.write('acl allow all all')
        aclf.close()        
        
        result = self.reload_acl()       
        if (result.text.find("contains illegal characters",0,len(result.text)) == -1):
            self.fail(result)

    def test_user_without_realm(self):
        """
        Test a user defined without a realm
        Ex. group admin rajith
        """
        aclf = ACLFile()
        aclf.write('group admin bob\n')
        aclf.write('acl deny admin bind exchange\n')
        aclf.write('acl allow all all')
        aclf.close()
         
        result = self.reload_acl()
        if (result.text.find("Username 'bob' must contain a realm",0,len(result.text)) == -1):
            self.fail(result)

        
   #=====================================
   # ACL queue tests
   #=====================================
           
    def test_queue_allow_mode(self):
        """
        Test cases for queue acl in allow mode
        """
        aclf = ACLFile()
        aclf.write('acl deny bob@QPID create queue name=q1 durable=true passive=true\n')
        aclf.write('acl deny bob@QPID create queue name=q2 exclusive=true policytype=ring\n')
        aclf.write('acl deny bob@QPID access queue name=q3\n')
        aclf.write('acl deny bob@QPID purge queue name=q3\n')
        aclf.write('acl deny bob@QPID delete queue name=q4\n')  
        aclf.write('acl deny bob@QPID create queue name=q5 maxqueuesize=1000 maxqueuecount=100\n')              
        aclf.write('acl allow all all')
        aclf.close()        
        
        result = self.reload_acl()
        if (result.text.find("format error",0,len(result.text)) != -1):
            self.fail(result) 
        
        session = self.get_session('bob','bob')
        
        try:
            session.queue_declare(queue="q1", durable=True, passive=True)
            self.fail("ACL should deny queue create request with name=q1 durable=true passive=true");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')
        
        try:
            queue_options = {}
            queue_options["qpid.policy_type"] = "ring" 
            session.queue_declare(queue="q2", exclusive=True, arguments=queue_options)
            self.fail("ACL should deny queue create request with name=q2 exclusive=true qpid.policy_type=ring");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code) 
            session = self.get_session('bob','bob')
        
        try:
            queue_options = {}
            queue_options["qpid.policy_type"] = "ring_strict"   
            session.queue_declare(queue="q2", exclusive=True, arguments=queue_options)            
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow queue create request with name=q2 exclusive=true qpid.policy_type=ring_strict");

        try:
            queue_options = {}
            queue_options["qpid.max_count"] = 200
            queue_options["qpid.max_size"] = 500  
            session.queue_declare(queue="q5", exclusive=True, arguments=queue_options)
            self.fail("ACL should deny queue create request with name=q2, qpid.max_size=500 and qpid.max_count=200");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code) 
            session = self.get_session('bob','bob')

        try:
            queue_options = {}
            queue_options["qpid.max_count"] = 200
            queue_options["qpid.max_size"] = 100  
            session.queue_declare(queue="q2", exclusive=True, arguments=queue_options)
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow queue create request with name=q2, qpid.max_size=100 and qpid.max_count=200 ");
        try:
            session.queue_declare(queue="q3", exclusive=True)
            session.queue_declare(queue="q4", durable=True)
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow queue create request for q3 and q4 with any parameter");

        try:
            session.queue_query(queue="q3")
            self.fail("ACL should deny queue query request for q3");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')
        
        try:
            session.queue_purge(queue="q3")
            self.fail("ACL should deny queue purge request for q3");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')
            
        try:
            session.queue_purge(queue="q4")
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow queue purge request for q4");
                   
        try:
            session.queue_delete(queue="q4")
            self.fail("ACL should deny queue delete request for q4");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')
            
        try:
            session.queue_delete(queue="q3")
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow queue delete request for q3");
                

    def test_queue_deny_mode(self):
        """
        Test cases for queue acl in deny mode
        """
        aclf = ACLFile()
        aclf.write('acl allow bob@QPID create queue name=q1 durable=true passive=true\n')
        aclf.write('acl allow bob@QPID create queue name=q2 exclusive=true policytype=ring\n')
        aclf.write('acl allow bob@QPID access queue name=q3\n')
        aclf.write('acl allow bob@QPID purge queue name=q3\n')
        aclf.write('acl allow bob@QPID create queue name=q3\n')                
        aclf.write('acl allow bob@QPID create queue name=q4\n')                
        aclf.write('acl allow bob@QPID delete queue name=q4\n')   
        aclf.write('acl allow bob@QPID create queue name=q5 maxqueuesize=1000 maxqueuecount=100\n')                   
        aclf.write('acl allow anonymous all all\n')
        aclf.write('acl deny all all')
        aclf.close()        
        
        result = self.reload_acl()
        if (result.text.find("format error",0,len(result.text)) != -1):
            self.fail(result)     
        
        session = self.get_session('bob','bob')
                 
        try:
            session.queue_declare(queue="q1", durable=True, passive=True)
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow queue create request with name=q1 durable=true passive=true");
        
        try:
            session.queue_declare(queue="q1", durable=False, passive=False)
            self.fail("ACL should deny queue create request with name=q1 durable=true passive=false");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')

        try:
            session.queue_declare(queue="q2", exclusive=False)
            self.fail("ACL should deny queue create request with name=q2 exclusive=false");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code) 
            session = self.get_session('bob','bob')
        
        try:
            queue_options = {}
            queue_options["qpid.max_count"] = 200
            queue_options["qpid.max_size"] = 500  
            session.queue_declare(queue="q5", arguments=queue_options)
            self.fail("ACL should deny queue create request with name=q2 maxqueuesize=500 maxqueuecount=200");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code) 
            session = self.get_session('bob','bob')

        try:
            queue_options = {}
            queue_options["qpid.max_count"] = 100
            queue_options["qpid.max_size"] = 500 
            session.queue_declare(queue="q5", arguments=queue_options)  
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow queue create request with name=q2 maxqueuesize=500 maxqueuecount=200");

        try:
            queue_options = {}
            queue_options["qpid.policy_type"] = "ring"
            session.queue_declare(queue="q2", exclusive=True, arguments=queue_options)            
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow queue create request for q2 with exclusive=true policytype=ring");

        try:
            session.queue_declare(queue="q3")
            session.queue_declare(queue="q4")
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow queue create request for q3 and q4");

        try:
            session.queue_query(queue="q4")
            self.fail("ACL should deny queue query request for q4");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')
        
        try:
            session.queue_purge(queue="q4")
            self.fail("ACL should deny queue purge request for q4");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')
            
        try:
            session.queue_purge(queue="q3")
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow queue purge request for q3");

        try:
            session.queue_query(queue="q3")
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow queue query request for q3");
                   
        try:
            session.queue_delete(queue="q3")
            self.fail("ACL should deny queue delete request for q3");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')
            
        try:
            session.queue_delete(queue="q4")
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow queue delete request for q4");

   #=====================================
   # ACL exchange tests
   #=====================================
   
    def test_exchange_acl_allow_mode(self):
        session = self.get_session('bob','bob')        
        session.queue_declare(queue="baz")

        """
        Test cases for exchange acl in allow mode
        """
        aclf = ACLFile()
        aclf.write('acl deny bob@QPID create exchange name=testEx durable=true passive=true\n')
        aclf.write('acl deny bob@QPID create exchange name=ex1 type=direct\n')
        aclf.write('acl deny bob@QPID access exchange name=myEx queuename=q1 routingkey=rk1.*\n')
        aclf.write('acl deny bob@QPID bind exchange name=myEx queuename=q1 routingkey=rk1\n')
        aclf.write('acl deny bob@QPID unbind exchange name=myEx queuename=q1 routingkey=rk1\n')
        aclf.write('acl deny bob@QPID delete exchange name=myEx\n')
        aclf.write('acl allow all all')
        aclf.close()        
        
        result = self.reload_acl()
        if (result.text.find("format error",0,len(result.text)) != -1):
            self.fail(result)        
        
        session = self.get_session('bob','bob')
        session.queue_declare(queue='q1')
        session.queue_declare(queue='q2')
        session.exchange_declare(exchange='myEx', type='direct')

        try:
            session.exchange_declare(exchange='testEx', durable=True, passive=True)
            self.fail("ACL should deny exchange create request with name=testEx durable=true passive=true");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')
       
        try:
            session.exchange_declare(exchange='testEx', type='direct', durable=True, passive=False)
        except qpid.session.SessionException, e:
            print e
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow exchange create request for testEx with any parameter other than durable=true and passive=true");
                        
        try:
            session.exchange_declare(exchange='ex1', type='direct')
            self.fail("ACL should deny exchange create request with name=ex1 type=direct");
        except qpid.session.SessionException, e:    
            self.assertEqual(530,e.args[0].error_code) 
            session = self.get_session('bob','bob')
        
        try:
            session.exchange_declare(exchange='myXml', type='direct')
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow exchange create request for myXml with any parameter");

        try:
            session.exchange_query(name='myEx')
            self.fail("ACL should deny exchange query request for myEx");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')

        try:
            session.exchange_bound(exchange='myEx', queue='q1', binding_key='rk1.*')
            self.fail("ACL should deny exchange bound request for myEx with queuename=q1 and routing_key='rk1.*' ");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')

        try:
            session.exchange_query(name='amq.topic')            
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow exchange query request for exchange='amq.topic'");
                
        try:
            session.exchange_bound(exchange='myEx', queue='q1', binding_key='rk2.*')  
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow exchange bound request for myEx with queuename=q1 and binding_key='rk2.*'");

        try:
            session.exchange_bind(exchange='myEx', queue='q1', binding_key='rk1')
            self.fail("ACL should deny exchange bind request with exchange='myEx' queuename='q1' bindingkey='rk1'");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code) 
            session = self.get_session('bob','bob')

        try:
            session.exchange_bind(exchange='myEx', queue='q1', binding_key='x')
        except qpid.session.SessionException, e:            
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow exchange bind request for exchange='myEx', queue='q1', binding_key='x'");

        try:
            session.exchange_bind(exchange='myEx', queue='q2', binding_key='rk1')
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow exchange bind request for exchange='myEx', queue='q2', binding_key='rk1'");

        try:
            session.exchange_unbind(exchange='myEx', queue='q1', binding_key='rk1')
            self.fail("ACL should deny exchange unbind request with exchange='myEx' queuename='q1' bindingkey='rk1'");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code) 
            session = self.get_session('bob','bob')

        try:
            session.exchange_unbind(exchange='myEx', queue='q1', binding_key='x')
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow exchange unbind request for exchange='myEx', queue='q1', binding_key='x'");

        try:
            session.exchange_unbind(exchange='myEx', queue='q2', binding_key='rk1')
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow exchange unbind request for exchange='myEx', queue='q2', binding_key='rk1'");
                   
        try:
            session.exchange_delete(exchange='myEx')
            self.fail("ACL should deny exchange delete request for myEx");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')
            
        try:
            session.exchange_delete(exchange='myXml')
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow exchange delete request for myXml");
        

    def test_exchange_acl_deny_mode(self):
        session = self.get_session('bob','bob')
        session.queue_declare(queue='bar')

        """
        Test cases for exchange acl in deny mode
        """
        aclf = ACLFile()
        aclf.write('acl allow bob@QPID create exchange name=myEx durable=true passive=false\n')
        aclf.write('acl allow bob@QPID bind exchange name=amq.topic queuename=bar routingkey=foo.*\n') 
        aclf.write('acl allow bob@QPID unbind exchange name=amq.topic queuename=bar routingkey=foo.*\n')
        aclf.write('acl allow bob@QPID access exchange name=myEx queuename=q1 routingkey=rk1.*\n')
        aclf.write('acl allow bob@QPID delete exchange name=myEx\n')
        aclf.write('acl allow anonymous all all\n') 
        aclf.write('acl deny all all')
        aclf.close()        
        
        result = self.reload_acl()
        if (result.text.find("format error",0,len(result.text)) != -1):
            self.fail(result)        
        
        session = self.get_session('bob','bob')

        try:
            session.exchange_declare(exchange='myEx', type='direct', durable=True, passive=False)
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow exchange create request for myEx with durable=true and passive=false");
           
        try:
            session.exchange_declare(exchange='myEx', type='direct', durable=False)
            self.fail("ACL should deny exchange create request with name=myEx durable=false");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')
       
        try:
            session.exchange_bind(exchange='amq.topic', queue='bar', binding_key='foo.bar')
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow exchange bind request for exchange='amq.topic', queue='bar', binding_key='foor.bar'");

        try:
            session.exchange_bind(exchange='amq.topic', queue='baz', binding_key='foo.bar')
            self.fail("ACL should deny exchange bind request for exchange='amq.topic', queue='baz', binding_key='foo.bar'");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')

        try:
            session.exchange_bind(exchange='amq.topic', queue='bar', binding_key='fooz.bar')
            self.fail("ACL should deny exchange bind request for exchange='amq.topic', queue='bar', binding_key='fooz.bar'");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')

        try:
            session.exchange_unbind(exchange='amq.topic', queue='bar', binding_key='foo.bar')
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow exchange unbind request for exchange='amq.topic', queue='bar', binding_key='foor.bar'");
        try:
            session.exchange_unbind(exchange='amq.topic', queue='baz', binding_key='foo.bar')
            self.fail("ACL should deny exchange unbind request for exchange='amq.topic', queue='baz', binding_key='foo.bar'");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')

        try:
            session.exchange_unbind(exchange='amq.topic', queue='bar', binding_key='fooz.bar')
            self.fail("ACL should deny exchange unbind request for exchange='amq.topic', queue='bar', binding_key='fooz.bar'");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')

        try:
            session.exchange_query(name='amq.topic')
            self.fail("ACL should deny exchange query request for amq.topic");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')

        try:
            session.exchange_bound(exchange='myEx', queue='q1', binding_key='rk2.*')
            self.fail("ACL should deny exchange bound request for amq.topic with queuename=q1 and routing_key='rk2.*' ");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')

        try:
            session.exchange_query(name='myEx')            
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow exchange query request for exchange='myEx'");
                
        try:
            session.exchange_bound(exchange='myEx', queue='q1', binding_key='rk1.*')  
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow exchange bound request for myEx with queuename=q1 and binding_key='rk1.*'");

        try:
            session.exchange_delete(exchange='myXml')
            self.fail("ACL should deny exchange delete request for myXml");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')
            
        try:
            session.exchange_delete(exchange='myEx')
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow exchange delete request for myEx");

   #=====================================
   # ACL consume tests
   #=====================================
   
    def test_consume_allow_mode(self):
        """
        Test cases for consume in allow mode
        """
        aclf = ACLFile()
        aclf.write('acl deny bob@QPID consume queue name=q1\n')
        aclf.write('acl deny bob@QPID consume queue name=q2\n')                
        aclf.write('acl allow all all')
        aclf.close()        
        
        result = self.reload_acl()
        if (result.text.find("format error",0,len(result.text)) != -1):
            self.fail(result)        
        
        session = self.get_session('bob','bob')
        
        
        try:
            session.queue_declare(queue='q1')
            session.queue_declare(queue='q2')
            session.queue_declare(queue='q3')
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow create queue request");
        
        try:
            session.message_subscribe(queue='q1', destination='myq1')
            self.fail("ACL should deny subscription for queue='q1'");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')
            
        try:
            session.message_subscribe(queue='q2', destination='myq1')
            self.fail("ACL should deny subscription for queue='q2'");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')
              
        try:
            session.message_subscribe(queue='q3', destination='myq1')
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow subscription for q3");                                                  
                        

    def test_consume_deny_mode(self):
        """
        Test cases for consume in allow mode
        """
        aclf = ACLFile()
        aclf.write('acl allow bob@QPID consume queue name=q1\n')
        aclf.write('acl allow bob@QPID consume queue name=q2\n')
        aclf.write('acl allow bob@QPID create queue\n')                                
        aclf.write('acl allow anonymous all\n')                                
        aclf.write('acl deny all all')
        aclf.close()        
        
        result = self.reload_acl()
        if (result.text.find("format error",0,len(result.text)) != -1):
            self.fail(result)        
        
        session = self.get_session('bob','bob')
        
        
        try:
            session.queue_declare(queue='q1')
            session.queue_declare(queue='q2')
            session.queue_declare(queue='q3')
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow create queue request");

        try:
            session.message_subscribe(queue='q1', destination='myq1')
            session.message_subscribe(queue='q2', destination='myq2')
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow subscription for q1 and q2");                             
        
        try:
            session.message_subscribe(queue='q3', destination='myq3')
            self.fail("ACL should deny subscription for queue='q3'");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')
            

   #=====================================
   # ACL publish tests
   #=====================================
   
    def test_publish_acl_allow_mode(self):
        """
        Test various publish acl
        """
        aclf = ACLFile()
        aclf.write('acl deny bob@QPID publish exchange name=amq.direct routingkey=rk1\n')
        aclf.write('acl deny bob@QPID publish exchange name=amq.topic\n')
        aclf.write('acl deny bob@QPID publish exchange name=myEx routingkey=rk2\n')                
        aclf.write('acl allow all all')
        aclf.close()        
        
        result = self.reload_acl()
        if (result.text.find("format error",0,len(result.text)) != -1):
            self.fail(result)        
        
        session = self.get_session('bob','bob')
            
        props = session.delivery_properties(routing_key="rk1")
               
        try:            
            session.message_transfer(destination="amq.direct", message=Message(props,"Test"))
            self.fail("ACL should deny message transfer to name=amq.direct routingkey=rk1");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')                        
            
        try:
            session.message_transfer(destination="amq.topic", message=Message(props,"Test"))
            self.fail("ACL should deny message transfer to name=amq.topic");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')
                        
        try:
            session.exchange_declare(exchange='myEx', type='direct', durable=False)
            session.message_transfer(destination="myEx", message=Message(props,"Test"))
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow message transfer to exchange myEx with routing key rk1");               
                        
                        
        props = session.delivery_properties(routing_key="rk2")
        try:
            session.message_transfer(destination="amq.direct", message=Message(props,"Test"))
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow message transfer to exchange amq.direct with routing key rk2");


    def test_publish_acl_deny_mode(self):
        """
        Test various publish acl
        """
        aclf = ACLFile()
        aclf.write('acl allow bob@QPID publish exchange name=amq.direct routingkey=rk1\n')
        aclf.write('acl allow bob@QPID publish exchange name=amq.topic\n')
        aclf.write('acl allow bob@QPID publish exchange name=myEx routingkey=rk2\n')
        aclf.write('acl allow bob@QPID create exchange\n')    
        aclf.write('acl allow anonymous all all \n')    
        aclf.write('acl deny all all')
        aclf.close()        
        
        result = self.reload_acl()
        if (result.text.find("format error",0,len(result.text)) != -1):
            self.fail(result)        
        
        session = self.get_session('bob','bob')
            
        props = session.delivery_properties(routing_key="rk2")
               
        try:            
            session.message_transfer(destination="amq.direct", message=Message(props,"Test"))
            self.fail("ACL should deny message transfer to name=amq.direct routingkey=rk2");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')                        
            
        try:
            session.message_transfer(destination="amq.topic", message=Message(props,"Test"))
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow message transfer to exchange amq.topic with any routing key");               
                        
        try:
            session.exchange_declare(exchange='myEx', type='direct', durable=False)
            session.message_transfer(destination="myEx", message=Message(props,"Test"))
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow message transfer to exchange myEx with routing key=rk2");               
                        
        props = session.delivery_properties(routing_key="rk1")

        try:
            session.message_transfer(destination="myEx", message=Message(props,"Test"))
            self.fail("ACL should deny message transfer to name=myEx routingkey=rk1");
        except qpid.session.SessionException, e:
            self.assertEqual(530,e.args[0].error_code)
            session = self.get_session('bob','bob')                                                

        try:
            session.message_transfer(destination="amq.direct", message=Message(props,"Test"))
        except qpid.session.SessionException, e:
            if (530 == e.args[0].error_code):
                self.fail("ACL should allow message transfer to exchange amq.direct with routing key rk1");
