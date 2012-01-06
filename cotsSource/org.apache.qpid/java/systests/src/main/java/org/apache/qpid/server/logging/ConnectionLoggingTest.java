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
package org.apache.qpid.server.logging;

import javax.jms.Connection;
import java.io.File;
import java.util.List;
import java.util.HashMap;
import java.util.TreeSet;

public class ConnectionLoggingTest extends AbstractTestLogging
{
    private static final String CONNECTION_PREFIX = "CON-";

    public void setUp() throws Exception
    {
        // set QPID_WORK to be [QPID_WORK|io.tmpdir]/<testName>
        setSystemProperty("QPID_WORK",
                          System.getProperty("QPID_WORK",
                                             System.getProperty("java.io.tmpdir"))
                          + File.separator + getName());

        //Start the broker
        super.setUp();
    }

    /**
     * Description:
     * When a new connection is made to the broker this must be logged.
     *
     * Input:
     * 1. Running Broker
     * 2. Connecting client
     * Output:
     * <date> CON-1001 : Open : Client ID {0}[ : Protocol Version : {1}] <version>
     *
     * Validation Steps:
     * 1. The CON ID is correct
     * 2. This is the first CON message for that Connection
     *
     * @throws Exception - if an error occurs
     */
    public void testConnectionOpen() throws Exception
    {
        assertLoggingNotYetOccured(CONNECTION_PREFIX);

        Connection connection = getConnection();

        List<String> results = _monitor.findMatches(CONNECTION_PREFIX);

        assertTrue("No CON messages logged", results.size() > 0);        

        // Validation
        // We should have at least three messages when running InVM but when running External
        // we will get 0-10 negotiation on con:0 whcih may close at some random point
        // MESSAGE [con:0(/127.0.0.1:46926)] CON-1001 : Open
        // MESSAGE [con:0(/127.0.0.1:46926)] CON-1001 : Open : Protocol Version : 0-10
        // MESSAGE [con:1(/127.0.0.1:46927)] CON-1001 : Open
        // MESSAGE [con:1(/127.0.0.1:46927)] CON-1001 : Open : Protocol Version : 0-9
        // MESSAGE [con:0(/127.0.0.1:46926)] CON-1002 : Close
        // MESSAGE [con:1(/127.0.0.1:46927)] CON-1001 : Open : Client ID : clientid : Protocol Version : 0-9

        //So check how many connections we have in the result set and extract the last one.
        // When running InVM we will have con:0 and externally con:1

        HashMap<Integer, List<String>> connectionData = splitResultsOnConnectionID(results);

        // Get the last Integer from keySet of the ConnectionData
        int connectionID = new TreeSet<Integer>(connectionData.keySet()).last();

        //Use just the data from the last connection for the test
        results = connectionData.get(connectionID);

        // If we are running inVM we will get three open messagse, if running externally weN will also have
        // open and close messages from the failed 0-10 negotiation 
        assertTrue("CON messages not logged:" + results.size(), results.size() >= 3);

        String log = getLog(results.get(0));
        //  MESSAGE [con:1(/127.0.0.1:52540)] CON-1001 : Open
        //1 & 2
        validateMessageID("CON-1001",log);

        //We get the size so that we can validate the last three CON- messages
        int resultsSize = results.size();
        // This is because when running externally we will also have logged the failed
        // 0-10 negotiation messages

        // 3 - Assert the options are correct
        log = getLog(results.get(resultsSize - 1));
        //  MESSAGE [con:1(/127.0.0.1:52540)] CON-1001 : Open : Client ID : clientid : Protocol Version : 0-9
        validateMessageID("CON-1001",log);
        assertTrue("Client ID option is not present", fromMessage(log).contains("Client ID :"));
        assertTrue("Client ID value is not present", fromMessage(log).contains(connection.getClientID()));

        assertTrue("Protocol Version option is not present", fromMessage(log).contains("Protocol Version :"));
        //fixme there is no way currently to find out the negotiated protocol version
        // The delegate is the versioned class ((AMQConnection)connection)._delegate

        log = getLog(results.get(resultsSize - 2));
        //  MESSAGE [con:1(/127.0.0.1:52540)] CON-1001 : Open : Protocol Version : 0-9
        validateMessageID("CON-1001",log);
        assertTrue("Protocol Version option is not present", fromMessage(log).contains("Protocol Version :"));
        //fixme agani we should check the version
        // Check that client ID is not present in log
        assertTrue("Client ID option is present", !fromMessage(log).contains("Client ID :"));

        log = getLog(results.get(resultsSize - 3));
        validateMessageID("CON-1001",log);
        // Check that PV is not present in log
        assertTrue("Protocol Version option is present", !fromMessage(log).contains("Protocol Version :"));
        // Check that client ID is not present in log
        assertTrue("Client ID option is present", !fromMessage(log).contains("Client ID :"));

        connection.close();
    }

    /**
     * Description:
     * When a connected client closes the connection this will be logged as a CON-1002 message.
     * Input:
     *
     * 1. Running Broker
     * 2. Connected Client
     * Output:
     *
     * <date> CON-1002 : Close
     *
     * Validation Steps:
     * 3. The CON ID is correct
     * 4. This must be the last CON message for the Connection
     * 5. It must be preceded by a CON-1001 for this Connection
     */
    public void testConnectionClose() throws Exception
    {
        assertLoggingNotYetOccured(CONNECTION_PREFIX);

        // Open and then close the conneciton
        getConnection().close();

        List<String> results = _monitor.findMatches(CONNECTION_PREFIX);

        // Validation

        // We should have at least four messages
        assertTrue("CON messages not logged:" + results.size(), results.size() >= 4);

        //We get the size so that we can validate the last set of CON- messages
        int resultsSize = results.size();

        // Validate Close message occurs
        String log = getLog(results.get(resultsSize - 1));
        validateMessageID("CON-1002",log);
        assertTrue("Message does not end with close:" + log, log.endsWith("Close"));

        // Extract connection ID to validate there is a CON-1001 messasge for it
        int connectionID = getConnectionID(log);

        //Previous log message should be the open
        log = getLog(results.get(resultsSize - 2));
        //  MESSAGE [con:1(/127.0.0.1:52540)] CON-1001 : Open : Client ID : clientid : Protocol Version : 0-9
        validateMessageID("CON-1001",log);
        assertEquals("Connection IDs do not match", connectionID, getConnectionID(fromActor(log)));
    }
}
