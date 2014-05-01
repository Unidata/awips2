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
package org.apache.qpid.server.logging.rawloggers;

import junit.framework.TestCase;
import org.apache.log4j.AppenderSkeleton;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.spi.LoggingEvent;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

/** Test that the Log4jMessageLogger defaults behave as expected */
public class Log4jMessageLoggerTest extends TestCase
{
    private File _lodgfile;

    Level _rootLevel;
    Log4jTestAppender _appender;

    @Override
    public void setUp() throws IOException
    {
        // Setup a file for logging
        _appender = new Log4jTestAppender();

        Logger root = Logger.getRootLogger();
        root.addAppender(_appender);

        _rootLevel = Logger.getRootLogger().getLevel();
        if (_rootLevel != Level.INFO)
        {
            root.setLevel(Level.INFO);
            root.warn("Root Logger set to:" + _rootLevel + " Resetting to INFO for test.");
        }
        root.warn("Adding Test Appender:" + _appender);
    }

    @Override
    public void tearDown()
    {
        Logger root = Logger.getRootLogger();
        root.warn("Removing Test Appender:" + _appender);
        root.warn("Resetting Root Level to : " + _rootLevel);

        Logger.getRootLogger().setLevel(_rootLevel);

        Logger.getRootLogger().removeAppender(_appender);

        //Call close on our appender. This will clear the log messages
        // from Memory
        _appender.close();
    }

    /**
     * Verify that the default configuraion of Log4jMessageLogger will
     * log a message.
     *
     */
    public void testDefaultLogsMessage()
    {
        // Create a logger to test
        Log4jMessageLogger logger = new Log4jMessageLogger();

        //Create Message for test
        String message = "testDefaults";

        // Log the message
        logger.rawMessage(message);

        verifyLogPresent(message);
    }

    /**
     * This test verifies that the Log4jMessageLogger does not inherit a logging
     * level from the RootLogger. The Log4jMessageLogger default of INFO
     * will result in logging being presented.
     *
     */
    public void testLoggerDoesNotInheritRootLevel()
    {
        //Set default logger level to off
        Logger.getRootLogger().setLevel(Level.OFF);

        testDefaultLogsMessage();
    }

    /**
     * Test that changing the logger works.
     * <p/>
     * Test this by setting the default logger level to off which has been
     * verified to work by test 'testDefaultsLevelObeyed'
     *
     */
    public void testDefaultLoggerAdjustment()
    {
        String loggerName = "TestLogger";
        // Create a logger to test
        Log4jMessageLogger logger = new Log4jMessageLogger(Log4jMessageLogger.DEFAULT_LEVEL, loggerName);

        //Create Message for test
        String message = "testDefaults";

        //Disable the default Log4jMessageLogger logger
        Level originalLevel = Logger.getLogger(Log4jMessageLogger.DEFAULT_LOGGER).getLevel();
        Logger.getLogger(Log4jMessageLogger.DEFAULT_LOGGER).setLevel(Level.OFF);

        // Log the message
        logger.rawMessage(message);

        verifyLogPresent(message);

        // Restore the logging level
        Logger.getLogger(Log4jMessageLogger.DEFAULT_LOGGER).setLevel(originalLevel);
    }


    /**
     * Check that the Log Message reached log4j
     * @param message the message to search for
     */
    private void verifyLogPresent(String message)
    {
        List<String> results = findMessageInLog(message);

        //Validate we only got one message
        assertEquals("The result set was not as expected.", 1, results.size());

        // Validate message
        String line = results.get(0);

        assertNotNull("No Message retrieved from log file", line);
        assertTrue("Message not contained in log.:" + line,
                   line.contains(message));
    }

    /**
     * Check that the given Message is not present in the log4j records.
     * @param message the message to search for
     */
    private void verifyNoLog(String message)
    {
        List<String> results = findMessageInLog(message);

        //Validate we only got one message
        if (results.size() > 0)
        {
            System.err.println("Unexpected Log messages");

            for (String msg : results)
            {
                System.err.println(msg);
            }
        }

        assertEquals("No messages expected.", 0, results.size());
    }

    /**
     * Get the appenders list of events and return a list of all the messages
     * that contain the given message
     *
     * @param message the search string
     * @return The list of all logged messages that contain the search string.
     */
    private List<String> findMessageInLog(String message)
    {
        List<LoggingEvent> log = _appender.getLog();

        // Search Results for requested message
        List<String> result = new LinkedList<String>();

        for (LoggingEvent event : log)
        {
            if (String.valueOf(event.getMessage()).contains(message))
            {
                result.add(String.valueOf(event.getMessage()));
            }
        }

        return result;
    }


    /**
     * Log4j Appender that simply records all the Logging Events so we can
     * verify that the above logging will make it to log4j in a unit test.
     */
    private class Log4jTestAppender extends AppenderSkeleton
    {
        List<LoggingEvent> _log = new LinkedList<LoggingEvent>();

        protected void append(LoggingEvent loggingEvent)
        {
            _log.add(loggingEvent);
        }

        public void close()
        {
            _log.clear();
        }

        /**
         * @return the list of LoggingEvents that have occured in this Appender
         */
        public List<LoggingEvent> getLog()
        {
            return _log;
        }

        public boolean requiresLayout()
        {
            return false;
        }
    }
}

