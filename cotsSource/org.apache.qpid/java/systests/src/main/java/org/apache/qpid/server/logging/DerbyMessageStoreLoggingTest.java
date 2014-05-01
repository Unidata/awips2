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

import org.apache.commons.configuration.Configuration;
import org.apache.qpid.server.configuration.ServerConfiguration;
import org.apache.qpid.server.logging.subjects.AbstractTestLogSubject;

import javax.jms.Connection;
import javax.jms.Queue;
import javax.jms.Session;
import java.util.List;
import java.io.File;

/**
 * The MessageStore test suite validates that the follow log messages as
 * specified in the Functional Specification.
 *
 * This suite of tests validate that the MessageStore messages occur correctly
 * and according to the following format:
 *
 * MST-1001 : Created : <name>
 * MST-1003 : Closed
 *
 * NOTE: Only for Persistent Stores
 * MST-1002 : Store location : <path>
 * MST-1004 : Recovery Start [: <queue.name>]
 * MST-1005 : Recovered <count> messages for queue <queue.name>
 * MST-1006 : Recovery Complete [: <queue.name>]
 */
public class DerbyMessageStoreLoggingTest extends MemoryMessageStoreLoggingTest
{

    @Override
    public void setUp() throws Exception
    {
        super.setUp();
        // MemoryMessageStoreLoggingTest setUp itself does not call super.setUp
        //We call super.setUp but this will not start the broker as that is
        //part of the test case.

        // Load the default configuration file to get the list of defined vhosts
        ServerConfiguration configuration = new ServerConfiguration(new File(_configFile.getParent() + "/config.xml"));
        List<String> vhosts = configuration.getConfig().getList("virtualhosts.virtualhost.name");

        // Make them all persistent i.e. Use DerbyMessageStore and
        // test that it logs correctly.
        for (String vhost : vhosts)
        {
            makeVirtualHostPersistent(vhost);
        }
    }

    /**
     * Description:
     * Persistent MessageStores will require space on disk to persist the data.
     * This value will be logged on startup after the MessageStore has been
     * created.
     * Input:
     * Default configuration
     * Output:
     *
     * <date> MST-1002 : Store location : <path>
     *
     * Validation Steps:
     *
     * 1. The MST ID is correct
     * 2. This must occur after MST-1001
     */
    public void testMessageStoreStoreLocation() throws Exception
    {
        assertLoggingNotYetOccured(MESSAGES_STORE_PREFIX);

        startBroker();

        List<String> results = _monitor.findMatches(MESSAGES_STORE_PREFIX);

        // Validation

        assertTrue("MST messages not logged", results.size() > 0);

        // Load VirtualHost list from file.
        ServerConfiguration configuration = new ServerConfiguration(_configFile);
        List<String> vhosts = configuration.getConfig().getList("virtualhosts.virtualhost.name");

        //Validate each vhost logs a creation
        results = _monitor.findMatches("MST-1002");

        assertEquals("Each vhost did not close its store.", vhosts.size(), results.size());

        for (int index = 0; index < results.size(); index++)
        {
            String result = getLog(results.get(index));

            // getSlize will return extract the vhost from vh(/test) -> '/test'
            // so remove the '/' to get the name
            String vhostName = AbstractTestLogSubject.getSlice("vh", result).substring(1);

            // To get the store class used in the configuration we need to know
            // the virtualhost name, found above. AND
            // the index that the virtualhost is within the configuration.
            // we can retrive that from the vhosts list previously extracted.
            String fullStoreName = configuration.getConfig().getString("virtualhosts.virtualhost(" + vhosts.indexOf(vhostName) + ")." + vhostName + ".store.class");

            // Get the Simple class name from the expected class name of o.a.q.s.s.MMS
            String storeName = fullStoreName.substring(fullStoreName.lastIndexOf(".") + 1);

            assertTrue("MST-1002 does not contain a store path" + getMessageString(result),
                       getMessageString(result).length() > 0);

            assertEquals("The store name does not match expected value",
                         storeName, AbstractTestLogSubject.getSlice("ms", fromSubject(result)));
        }
    }

    /**
     * Description:
     * Persistent message stores may have state on disk that they must recover
     * during startup. As the MessageStore starts up it will report that it is
     * about to start the recovery process by logging MST-1004. This message
     * will always be logged for persistent MessageStores. If there is no data
     * to recover then there will be no subsequent recovery messages.
     * Input:
     * Default persistent configuration
     * Output:
     * <date> MST-1004 : Recovery Start
     *
     * Validation Steps:
     *
     * 1. The MST ID is correct
     * 2. The MessageStore must have first logged a creation event.
     */
    public void testMessageStoreRecoveryStart() throws Exception
    {
        assertLoggingNotYetOccured(MESSAGES_STORE_PREFIX);

        startBroker();

        List<String> results = _monitor.findMatches(MESSAGES_STORE_PREFIX);

        // Validation

        assertTrue("MST messages not logged", results.size() > 0);

        // Load VirtualHost list from file.
        ServerConfiguration configuration = new ServerConfiguration(_configFile);
        List<String> vhosts = configuration.getConfig().getList("virtualhosts.virtualhost.name");

        //Validate each vhost logs a creation
        results = _monitor.findMatches("MST-1004");

        assertTrue("Each vhost did not close its store.", vhosts.size() <= results.size());

        for (int index = 0; index < results.size(); index++)
        {
            String result = getLog(results.get(index));

            if (getMessageString(result).contains("Recovery Start :"))
            {
                //Don't test queue start recoveries
                continue;
            }

            // getSlize will return extract the vhost from vh(/test) -> '/test'
            // so remove the '/' to get the name
            String vhostName = AbstractTestLogSubject.getSlice("vh", result).substring(1);

            // To get the store class used in the configuration we need to know
            // the virtualhost name, found above. AND
            // the index that the virtualhost is within the configuration.
            // we can retrive that from the vhosts list previously extracted.
            String fullStoreName = configuration.getConfig().getString("virtualhosts.virtualhost(" + vhosts.indexOf(vhostName) + ")." + vhostName + ".store.class");

            // Get the Simple class name from the expected class name of o.a.q.s.s.MMS
            String storeName = fullStoreName.substring(fullStoreName.lastIndexOf(".") + 1);

            assertEquals("MST-1004 does have expected message", "Recovery Start",
                         getMessageString(result));

            assertEquals("The store name does not match expected value",
                         storeName, AbstractTestLogSubject.getSlice("ms", fromSubject(result)));
        }
    }

    /**
     * Description:
     * Once all persistent queues have been recovered and the MessageStore has completed all recovery it must logged that the recovery process has completed.
     * Input:
     * Default persistent configuration
     * Output:
     *
     * <date> MST-1006 : Recovery Complete
     *
     * Validation Steps:
     *
     * 1. The MST ID is correct
     * 2. This is the last message from the MessageStore during startup.
     * 3. This must be proceeded by a MST-1006 Recovery Start.
     */
    public void testMessageStoreRecoveryComplete() throws Exception
    {
        assertLoggingNotYetOccured(MESSAGES_STORE_PREFIX);

        startBroker();

        List<String> results = _monitor.findMatches(MESSAGES_STORE_PREFIX);

        // Validation

        assertTrue("MST messages not logged", results.size() > 0);

        // Load VirtualHost list from file.
        ServerConfiguration configuration = new ServerConfiguration(_configFile);
        List<String> vhosts = configuration.getConfig().getList("virtualhosts.virtualhost.name");

        //Validate each vhost logs a creation
        results = _monitor.findMatches("MST-1006");

        assertTrue("Each vhost did not close its store.", vhosts.size() <= results.size());

        for (int index = 0; index < results.size(); index++)
        {
            String result = getLog(results.get(index));

            if (getMessageString(result).contains("Recovery Complete :"))
            {
                //Don't test queue start recoveries
                continue;
            }

            // getSlize will return extract the vhost from vh(/test) -> '/test'
            // so remove the '/' to get the name
            String vhostName = AbstractTestLogSubject.getSlice("vh", result).substring(1);

            // To get the store class used in the configuration we need to know
            // the virtualhost name, found above. AND
            // the index that the virtualhost is within the configuration.
            // we can retrive that from the vhosts list previously extracted.
            String fullStoreName = configuration.getConfig().getString("virtualhosts.virtualhost(" + vhosts.indexOf(vhostName) + ")." + vhostName + ".store.class");

            // Get the Simple class name from the expected class name of o.a.q.s.s.MMS
            String storeName = fullStoreName.substring(fullStoreName.lastIndexOf(".") + 1);

            assertEquals("MST-1006 does have expected message", "Recovery Complete",
                         getMessageString(result));

            assertEquals("The store name does not match expected value",
                         storeName, AbstractTestLogSubject.getSlice("ms", fromSubject(result)));
        }
    }

    /**
     * Description:
     * A persistent MessageStore may have data to recover from disk. The message store will use MST-1004 to report the start of recovery for a specific queue that it has previously persisted.
     * Input:
     * Default persistent configuration
     * Output:
     *
     * <date> MST-1004 : Recovery Start : <queue.name>
     *
     * Validation Steps:
     *
     * 1. The MST ID is correct
     * 2. This must occur after the recovery start MST-1004 has been logged.
     */
    public void testMessageStoreQueueRecoveryStart() throws Exception
    {
        assertLoggingNotYetOccured(MESSAGES_STORE_PREFIX);

        startBroker();

        List<String> results = _monitor.findMatches(MESSAGES_STORE_PREFIX);

        // Validation

        assertTrue("MST messages not logged", results.size() > 0);

        // Load VirtualHost list from file.
        ServerConfiguration configuration = new ServerConfiguration(_configFile);
        List<String> vhosts = configuration.getConfig().getList("virtualhosts.virtualhost.name");

        //Validate each vhost logs a creation
        results = _monitor.findMatches("MST-1004 : Recovery Start :");

        // We are only looking for the default queue defined in local host being
        // recovered. If other tests have made queues in test then we want to
        // exclude them here.
        results = filterResultsByVirtualHost(results, "/localhost");

        assertEquals("Recovered test queue not found.", 1, results.size());

        String result = getLog(results.get(0));

        // getSlize will return extract the vhost from vh(/test) -> '/test'
        // so remove the '/' to get the name
        String vhostName = AbstractTestLogSubject.getSlice("vh", result).substring(1);

        // To get the store class used in the configuration we need to know
        // the virtualhost name, found above. AND
        // the index that the virtualhost is within the configuration.
        // we can retrive that from the vhosts list previously extracted.
        String fullStoreName = configuration.getConfig().getString("virtualhosts.virtualhost(" + vhosts.indexOf(vhostName) + ")." + vhostName + ".store.class");

        // Get the Simple class name from the expected class name of o.a.q.s.s.MMS
        String storeName = fullStoreName.substring(fullStoreName.lastIndexOf(".") + 1);

        assertTrue("MST-1006 does end with queue 'test-queue':" + getMessageString(result),
                   getMessageString(result).endsWith("test-queue"));

        assertEquals("The store name does not match expected value",
                     storeName, AbstractTestLogSubject.getSlice("ms", fromSubject(result)));

    }

    /**
     * Description:
     * After the queue has been recovered the store will log that recovery has been completed. The MessageStore must not report further status about the recovery of this queue after this message. In addition every MST-1004 queue recovery start message must be matched with a MST-1006 recovery complete.
     * Input:
     * Default persistent configuration
     * Output:
     *
     * <date> MST-1006 : Recovery Complete : <queue.name>
     *
     * Validation Steps:
     *
     * 1. The MST ID is correct
     * 2. This must occur after the queue recovery start MST-1004 has been logged.
     * 3. The queue.name is non-empty
     * 4. The queue.name correlates with a previous recovery start
     */
    public void testMessageStoreQueueRecoveryComplete() throws Exception
    {
        assertLoggingNotYetOccured(MESSAGES_STORE_PREFIX);

        startBroker();

        List<String> results = _monitor.findMatches(MESSAGES_STORE_PREFIX);

        // Validation

        assertTrue("MST messages not logged", results.size() > 0);

        // Load VirtualHost list from file.
        ServerConfiguration configuration = new ServerConfiguration(_configFile);
        List<String> vhosts = configuration.getConfig().getList("virtualhosts.virtualhost.name");

        //Validate each vhost logs a creation
        results = _monitor.findMatches("MST-1006 : Recovery Complete :");

        // We are only looking for the default queue defined in local host being
        // recovered. If other tests have made queues in test then we want to
        // exclude them here.
        results = filterResultsByVirtualHost(results, "/localhost");

        assertEquals("Recovered test queue not found.", 1, results.size());

        String result = getLog(results.get(0));

        // getSlize will return extract the vhost from vh(/test) -> '/test'
        // so remove the '/' to get the name
        String vhostName = AbstractTestLogSubject.getSlice("vh", result).substring(1);

        // To get the store class used in the configuration we need to know
        // the virtualhost name, found above. AND
        // the index that the virtualhost is within the configuration.
        // we can retrive that from the vhosts list previously extracted.
        String fullStoreName = configuration.getConfig().getString("virtualhosts.virtualhost(" + vhosts.indexOf(vhostName) + ")." + vhostName + ".store.class");

        // Get the Simple class name from the expected class name of o.a.q.s.s.MMS
        String storeName = fullStoreName.substring(fullStoreName.lastIndexOf(".") + 1);

        assertTrue("MST-1006 does end with queue 'test-queue':" + getMessageString(result),
                   getMessageString(result).endsWith("test-queue"));

        assertEquals("The store name does not match expected value",
                     storeName, AbstractTestLogSubject.getSlice("ms", fromSubject(result)));

        results = _monitor.findMatches("MST-1004 : Recovery Start : test-queue");

        assertEquals("MST-1004 for test-queue not found", 1, results.size());
    }

    /**
     * Description:
     * A persistent queue must be persisted so that on recovery it can be restored independently of any messages that may be stored on it. This test verifies that the MessageStore will log that it has recovered 0 messages for persistent queues that do not have any messages.
     * Input:
     *
     * 1. Default persistent configuration
     * 2. Persistent queue with no messages enqueued
     * Output:
     *
     * <date> MST-1005 : Recovered 0 messages for queue <queue.name>
     *
     * Validation Steps:
     * 3. The MST ID is correct
     * 4. This must occur after the queue recovery start MST-1004 has been logged.
     * 5. The count is 0
     * 6. 'messages' is correctly printed
     * 7. The queue.name is non-empty
     */
    public void testMessageStoreQueueRecoveryCountEmpty() throws Exception
    {
        assertLoggingNotYetOccured(MESSAGES_STORE_PREFIX);

        String queueName = getTestQueueName();

        startBroker();
        Connection connetion = getConnection();
        Session session = connetion.createSession(false, Session.AUTO_ACKNOWLEDGE);
        Queue queue = session.createQueue("direct://amq.direct/" + queueName + "/" + queueName + "?durable='true'");

        session.createConsumer(queue).close();

        // Stop the broker so that we can test recovery
        stopBroker();

        int COUNT = 0;
        testDurableRecoveryCount(COUNT, queueName);
    }

    /**
     * Description:
     * On recovery all the persistent messages that are stored on disk must be returned to the queue. MST-1005 will report the number of messages that have been recovered from disk.
     * Input:
     *
     * 1. Default persistent configuration
     * 2. Persistent queue with multiple messages enqueued
     * Output:
     *
     * <date> MST-1005 : Recovered <count> messages for queue <queue.name>
     *
     * Validation Steps:
     * 3. The MST ID is correct
     * 4. This must occur after the queue recovery start MST-1004 has been logged.
     * 5. The count is > 1
     * 6. 'messages' is correctly printed
     * 7. The queue.name is non-empty
     */
    public void testMessageStoreQueueRecoveryCountPlural() throws Exception
    {
        assertLoggingNotYetOccured(MESSAGES_STORE_PREFIX);

        String queueName = getTestQueueName();

        int COUNT = 10;

        testDurableRecoveryCount(COUNT, queueName);
    }

    /**
     * Send a set number of messages to a new durable queue, as specified. Then
     * restart the broker and validate that they are restored.
     *
     * @param COUNT - the count to send
     * @param queueName - the new queue name
     * @throws Exception - if a problem occured.
     */
    private void testDurableRecoveryCount(int COUNT, String queueName) throws Exception
    {
        startBroker();
        Connection connetion = getConnection();
        Session session = connetion.createSession(false, Session.AUTO_ACKNOWLEDGE);
        Queue queue = session.createQueue("direct://amq.direct/" + queueName + "/" + queueName + "?durable='true'");

        session.createConsumer(queue).close();

        sendMessage(session, queue, COUNT);
        try
        {
            connetion.close();

            stopBroker();

            // Clear our monitor
            _monitor.reset();

            startBroker();

            List<String> results = _monitor.findMatches(MESSAGES_STORE_PREFIX);

            // Validation

            assertTrue("MST messages not logged", results.size() > 0);

            // Load VirtualHost list from file.
            ServerConfiguration configuration = new ServerConfiguration(_configFile);
            List<String> vhosts = configuration.getConfig().getList("virtualhosts.virtualhost.name");

            //Validate each vhost logs a creation
            results = _monitor.findMatches("MST-1004 : Recovery Start : " + queueName);

            assertEquals("Recovered test queue not found.", 1, results.size());

            String result = getLog(results.get(0));

            validateMessageID("MST-1004", result);

            assertTrue("MST-1004 does end with queue '" + queueName + "':" + getMessageString(result),
                       getMessageString(result).endsWith(queueName));

            results = _monitor.findMatches("MST-1005");

            assertTrue("Insufficient MST-1005 logged.", results.size()>0);

            result = null;

            // If the first message is not our queue the second one will be
            for(String resultEntry : results)
            {
                // Look for first match and set that to result
                if (resultEntry.contains(queueName))
                {
                    result = getLog(resultEntry);
                    break;
                }
            }

            assertNotNull("MST-1005 entry for queue:" + queueName + ". Not found", result);

            // getSlize will return extract the vhost from vh(/test) -> '/test'
            // so remove the '/' to get the name
            String vhostName = AbstractTestLogSubject.getSlice("vh", result).substring(1);

            // To get the store class used in the configuration we need to know
            // the virtualhost name, found above. AND
            // the index that the virtualhost is within the configuration.
            // we can retrive that from the vhosts list previously extracted.
            String fullStoreName = configuration.getConfig().getString("virtualhosts.virtualhost(" + vhosts.indexOf(vhostName) + ")." + vhostName + ".store.class");

            // Get the Simple class name from the expected class name of o.a.q.s.s.MMS
            String storeName = fullStoreName.substring(fullStoreName.lastIndexOf(".") + 1);

            assertTrue("MST-1005 does end with queue 'test-queue':" + getMessageString(result),
                       getMessageString(result).endsWith(queueName));

            assertTrue("MST-1005 does end show correct count:" + getMessageString(result),
                       getMessageString(result).contains("Recovered " + COUNT + " messages"));

            assertEquals("The store name does not match expected value",
                         storeName, AbstractTestLogSubject.getSlice("ms", fromSubject(result)));
        }
        finally
        {
            //Ensure we attempt to drain the queue.
            assertEquals("Unable to drain queue", COUNT, drainQueue(queue));
        }
    }

}
