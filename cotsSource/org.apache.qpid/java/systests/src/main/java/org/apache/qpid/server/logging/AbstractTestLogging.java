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

import org.apache.qpid.server.logging.subjects.AbstractTestLogSubject;
import org.apache.qpid.test.utils.QpidTestCase;
import org.apache.qpid.util.LogMonitor;

import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

public class AbstractTestLogging extends QpidTestCase
{
    protected LogMonitor _monitor;

    @Override
    public void setUp() throws Exception
    {
        super.setUp();
        _monitor = new LogMonitor(_outputFile);
    }

    @Override
    public void tearDown() throws Exception
    {
        _monitor.close();
        super.tearDown();
    }

    /**
     * assert that the requested log message has not occured
     *
     * @param log
     *
     * @throws IOException
     */
    public void assertLoggingNotYetOccured(String log) throws IOException
    {
        // Ensure the alert has not occured yet
        assertEquals("Message has already occured:" + log, 0,
                     _monitor.findMatches(log).size());
    }

    protected void validateMessageID(String id, String log)
    {
        assertEquals("Incorrect message", id, getMessageID(log));
    }

    protected String getMessageID(String log)
    {
        String message = fromMessage(log);

        return message.substring(0, message.indexOf(" "));
    }

    /**
     * Return the first channel id from the log string
     * ' ch;X'  if there is no channel id return -1.
     *
     * @param log the log string to search.
     *
     * @return channel id or -1 if no channel id exists.
     */
    protected int getChannelID(String log)
    {
        int start = log.indexOf("ch:") + 3;

        // If we do a check for ] as the boundary we will get cases where log
        // is presented with the bounding. If we don't match a ] then we can use
        // the end of the string as the boundary.
        int end = log.indexOf("]", start);
        if (end == -1)
        {
            end = log.length();
        }

        try
        {
            return Integer.parseInt(log.substring(start, end));
        }
        catch (Exception e)
        {
            return -1;
        }
    }

    protected String fromMessage(String log)
    {
        int startSubject = log.indexOf("]") + 1;
        int start = log.indexOf("]", startSubject) + 1;

        // If we don't have a subject then the second indexOf will return 0
        // in which case we can use the end of the actor as the index.
        if (start == 0)
        {
            start = startSubject;
        }

        return log.substring(start).trim();
    }

    /**
     * Extract the Subject from the Log Message.
     *
     * The subject is the second block inclosed in brackets '[ ]'.
     *
     * If there is no Subject or the second block of brackets '[ ]' cannot be
     * identified then an empty String ("") is returned.
     *
     * The brackets '[ ]' are not included in the returned String.
     *
     * @param log The log message to process
     *
     * @return the Subject string or the empty string ("") if the subject can't be identified.
     */
    protected String fromSubject(String log)
    {
        int start = log.indexOf("[") + 1;
        // Take the second index
        start = log.indexOf("[", start) + 1;

        // There may not be a subject so in that case return nothing.
        if (start == 0)
        {
            return "";
        }

        int end = log.indexOf("]", start);
        try
        {
            return log.substring(start, end);
        }
        catch (IndexOutOfBoundsException iobe)
        {
            return "";
        }
    }

    /**
     * Extract the actor segment from the log message.
     * The Actor segment is the first section enclosed in '[ ]'.
     *
     * No analysis is performed to ensure that the first '[ ]' section of the
     * given log is really an Actor segment.
     *
     * The brackets '[ ]' are not included in the returned String.
     *
     * @param log the Log Message
     *
     * @return the Actor segment or "" if unable to locate '[ ]' section
     */
    protected String fromActor(String log)
    {
        int start = log.indexOf("[") + 1;
        int end = log.indexOf("]", start);
        try
        {
            return log.substring(start, end).trim();
        }
        catch (IndexOutOfBoundsException iobe)
        {
            return "";
        }
    }

    /**
     * Return the message String from the given message section
     *
     * @param log the Message Section
     *
     * @return the Message String.
     */
    protected String getMessageString(String log)
    {
        // Remove the Log ID from the returned String
        int start = log.indexOf(":") + 1;

        return log.substring(start).trim();
    }

    /**
     * Given our log message extract the connection ID:
     *
     * The log string will contain the connectionID identified by 'con:'
     *
     * So extract the value shown here by X:
     *
     * 'con:X('
     *
     * Extract the value between the ':' and '(' and process it as an Integer
     *
     * If we are unable to find the right index or process the substring as an
     * Integer then return -1.
     *
     * @param log the log String to process
     *
     * @return the connection ID or -1.
     */
    protected int getConnectionID(String log)
    {
        int conIDStart = log.indexOf("con:") + 4;
        int conIDEnd = log.indexOf("(", conIDStart);
        try
        {
            return Integer.parseInt(log.substring(conIDStart, conIDEnd));
        }
        catch (Exception e)
        {
            return -1;
        }
    }

    /**
     * Extract the log entry from the raw log line which will contain other
     * log4j formatting.
     *
     * This formatting may impead our testing process so extract the log message
     * as we know it to be formatted.
     *
     * This starts with the string MESSAGE
     *
     * @param rawLog the raw log
     *
     * @return the log we are expecting to be printed without the log4j prefixes
     */
    protected String getLog(String rawLog)
    {
        int start = rawLog.indexOf("MESSAGE");
        return rawLog.substring(start);
    }

    /**
     * Given a list of messages that have been pulled out of a log file
     * Process the results splitting the log statements in to lists based on the
     * actor's connection ID.
     *
     * So for each log entry extract the Connecition ID from the Actor of the log
     *
     * Then use that as a key to a HashMap storing the list of log messages for
     * that connection.
     *
     * @param logMessages The list of mixed connection log messages
     *
     * @return Map indexed by connection id to a list of log messages just for that connection.
     */
    protected HashMap<Integer, List<String>> splitResultsOnConnectionID(List<String> logMessages)
    {
        HashMap<Integer, List<String>> connectionSplitList = new HashMap<Integer, List<String>>();

        for (String log : logMessages)
        {
            // Get the connectionID from the Actor in the Message Log.
            int cID = getConnectionID(fromActor(getLog(log)));

            List<String> connectionData = connectionSplitList.get(cID);

            // Create the initial List if we don't have one already
            if (connectionData == null)
            {
                connectionData = new LinkedList<String>();
                connectionSplitList.put(cID, connectionData);
            }

            // Store the log
            connectionData.add(log);
        }

        return connectionSplitList;
    }

    /**
     * Filter the give result set by the specficifed virtualhost.
     * This is done using the getSlice to identify the virtualhost (vh) in the
     * log message
     *
     * @param results         full list of logs
     * @param virtualHostName the virtualhostName to filter on
     *
     * @return the list of messages only for that virtualhost
     */
    protected List<String> filterResultsByVirtualHost(List<String> results, String virtualHostName)
    {
        List<String> filteredResults = new LinkedList<String>();
        Iterator<String> iterator = results.iterator();

        while (iterator.hasNext())
        {
            String log = iterator.next();

            if (AbstractTestLogSubject.getSlice("vh", log).equals(virtualHostName))
            {
                filteredResults.add(log);
            }
        }

        return filteredResults;
    }

}
