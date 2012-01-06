/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
package org.apache.qpid.test.unit.client.protocol;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.protocol.AMQProtocolHandler;
import org.apache.qpid.client.protocol.AMQProtocolSession;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.test.utils.QpidTestCase;
import org.apache.qpid.transport.TestNetworkDriver;
import org.apache.qpid.transport.NetworkDriver;

public class AMQProtocolSessionTest extends QpidTestCase
{
    private static class AMQProtSession extends AMQProtocolSession
    {

        public AMQProtSession(AMQProtocolHandler protocolHandler, AMQConnection connection)
        {
            super(protocolHandler,connection);
        }

        public TestNetworkDriver getNetworkDriver()
        {
            return (TestNetworkDriver) _protocolHandler.getNetworkDriver();
        }

        public AMQShortString genQueueName()
        {
            return generateQueueName();
        }
    }

    //private Strings for test values and expected results
    private String _brokenAddress = "tcp://myAddress;:";;
    private String _generatedAddress;
    private String _emptyAddress;
    private String _generatedAddress_2;
    private String _validAddress;
    private String _generatedAddress_3;
    private int _port;
    private AMQProtSession _testSession;

    protected void setUp() throws Exception
    {
        super.setUp();

        AMQConnection con = (AMQConnection) getConnection("guest", "guest");
        AMQProtocolHandler protocolHandler = new AMQProtocolHandler(con);
        protocolHandler.setNetworkDriver(new TestNetworkDriver());
        //don't care about the values set here apart from the dummy IoSession
        _testSession = new AMQProtSession(protocolHandler , con);

        //initialise addresses for test and expected results
        _port = 123;
        _brokenAddress = "tcp://myAddress;:";
        _generatedAddress = "tmp_tcpmyAddress123_1";
        _emptyAddress = "";
        _generatedAddress_2 = "tmp_localhost127.0.0.1123_2";
        _validAddress = "abc";
        _generatedAddress_3 = "tmp_abc123_3";
    }

    public void testGenerateQueueName()
    {
        AMQShortString testAddress;

        //test address with / and ; chars which generateQueueName should removeKey
        _testSession.getNetworkDriver().setLocalAddress(_brokenAddress);
        _testSession.getNetworkDriver().setPort(_port);

        testAddress = _testSession.genQueueName();
        assertEquals("Failure when generating a queue exchange from an address with special chars",_generatedAddress,testAddress.toString());

        //test empty address
        _testSession.getNetworkDriver().setLocalAddress(_emptyAddress);

        testAddress = _testSession.genQueueName();
        assertEquals("Failure when generating a queue exchange from an empty address",_generatedAddress_2,testAddress.toString());

        //test address with no special chars
        _testSession.getNetworkDriver().setLocalAddress(_validAddress);

        testAddress = _testSession.genQueueName();
        assertEquals("Failure when generating a queue exchange from an address with no special chars",_generatedAddress_3,testAddress.toString());

    }

    protected void tearDown() throws Exception
    {
        _testSession = null;
        _brokenAddress = null;
        _generatedAddress = null;
        _emptyAddress = null;
        _generatedAddress_2 = null;
        _validAddress = null;
        _generatedAddress_3 = null;
        super.tearDown();
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(AMQProtocolSessionTest.class);
    }
}
