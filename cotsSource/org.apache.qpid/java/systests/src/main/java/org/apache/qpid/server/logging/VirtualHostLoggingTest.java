/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 *
 *
 */

package org.apache.qpid.server.logging;

import junit.framework.AssertionFailedError;
import org.apache.commons.configuration.Configuration;
import org.apache.qpid.server.configuration.ServerConfiguration;

import java.util.List;

/**
 * Virtualhost Test Cases
 * The virtualhost test suite validates that the follow log messages as specified in the Functional Specification.
 * <p/>
 * This suite of tests validate that the management console messages occur correctly and according to the following format:
 * <p/>
 * VHT-1001 : Created : <name>
 * VHT-1002 : Work directory : <path>
 * VHT-1003 : Closed
 */
public class VirtualHostLoggingTest extends AbstractTestLogging
{
    private static final String VHT_PREFIX = "VHT-";

    /**
     * Description:
     * Testing can be performed using the default configuration. The goal is to validate that for each virtualhost defined in the configuration file a VHT-1001 Created message is provided.
     * Input:
     * The default configuration file
     * Output:
     * <p/>
     * <date> VHT-1001 : Created : <name>
     * Validation Steps:
     * <p/>
     * The VHT ID is correct
     * A VHT-1001 is printed for each virtualhost defined in the configuration file.
     * This must be the first message for the specified virtualhost.
     *
     * @throws Exception caused by broker startup
     */
    public void testVirtualhostCreation() throws Exception
    {

        List<String> results = _monitor.findMatches(VHT_PREFIX);
        try
        {
            // Validation
            ServerConfiguration configuration = new ServerConfiguration(_configFile);
            List<String> vhosts = configuration.getConfig().getList("virtualhosts.virtualhost.name");

            //Validate each vhost logs a creation
            results = _monitor.findMatches("VHT-1001");

            assertEquals("Each vhost did not create a store.", vhosts.size(), results.size());

            for (int index = 0; index < results.size(); index++)
            {
                String result = getLog(results.get(index));

                // Retrieve the vhostname from the log entry message 'Created : <vhostname>'
                String vhostName = getMessageString(fromMessage(result)).split(" ")[2];

                assertTrue("Virualhost named in log not found in config file:" + vhostName + ":" + vhosts, vhosts.contains(vhostName));
            }
        }
        catch (AssertionFailedError afe)
        {
            System.err.println("Log Dump:");
            for (String log : results)
            {
                System.err.println(log);
            }
            throw afe;
        }
    }

    /**
     * Description:
     * Testing can be performed using the default configuration. During broker shutdown a VHT-1002 Closed message will be printed for each of the configured virtualhosts. For every virtualhost that was started a close must be logged. After the close message has been printed no further logging will be performed by this virtualhost.
     * Input:
     * The default configuration file
     * Output:
     * <p/>
     * <date> VHT-1002 : Closed
     * Validation Steps:
     * <p/>
     * The VHT ID is correct
     * This is the last VHT message for the given virtualhost.
     *
     * @throws Exception caused by broker startup
     */
    public void testVirtualhostClosure() throws Exception
    {
        stopBroker();

        List<String> results = _monitor.findMatches(VHT_PREFIX);
        try
        {
            // Validation

            ServerConfiguration configuration = new ServerConfiguration(_configFile);
            List<String> vhosts = configuration.getConfig().getList("virtualhosts.virtualhost.name");

            //Validate each vhost logs a creation
            results = _monitor.findMatches("VHT-1002");

            assertEquals("Each vhost did not create a store.", vhosts.size(), results.size());
        }
        catch (AssertionFailedError afe)
        {
            System.err.println("Log Dump:");
            for (String log : results)
            {
                System.err.println(log);
            }
            throw afe;
        }
    }

}
