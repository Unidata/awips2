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
package org.apache.qpid.util;

import org.apache.log4j.FileAppender;
import org.apache.log4j.Logger;
import org.apache.log4j.SimpleLayout;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;

/**
 * Utility to simplify the monitoring of Log4j file output
 *
 * Monitoring of a given log file can be done alternatively the Monitor will
 * add a new log4j FileAppender to the root Logger to gather all the available
 * logging for monitoring
 */
public class LogMonitor
{
    // The file that the log statements will be written to.
    private File _logfile;

    // The appender we added to the get messages
    private FileAppender _appender;

    /**
     * Create a new LogMonitor that creates a new Log4j Appender and monitors
     * all log4j output via the current configuration.
     *
     * @throws IOException if there is a problem creating the temporary file.
     */
    public LogMonitor() throws IOException
    {
        this(null);
    }

    /**
     * Create a new LogMonitor on the specified file if the file does not exist
     * or the value is null then a new Log4j appender will be added and
     * monitoring set up on that appender.
     *
     * NOTE: for the appender to receive any value the RootLogger will need to
     * have the level correctly configured.ng
     *
     * @param file the file to monitor
     *
     * @throws IOException if there is a problem creating a temporary file
     */
    public LogMonitor(File file) throws IOException
    {
        if (file != null && file.exists())
        {
            _logfile = file;
        }
        else
        {
            // This is mostly for running the test outside of the ant setup
            _logfile = File.createTempFile("LogMonitor", ".log");
            _appender = new FileAppender(new SimpleLayout(),
                                                     _logfile.getAbsolutePath());
            _appender.setFile(_logfile.getAbsolutePath());
            _appender.setImmediateFlush(true);
            Logger.getRootLogger().addAppender(_appender);
        }
    }

    /**
     * Checks the log for instances of the search string.
     *
     * The pattern parameter can take any valid argument used in String.contains()
     *
     * {@see String.contains(CharSequences)}
     *
     * @param pattern the search string
     *
     * @return a list of matching lines from the log
     *
     * @throws IOException if there is a problem with the file
     */
    public List<String> findMatches(String pattern) throws IOException
    {
        return FileUtils.searchFile(_logfile, pattern);
    }

    /**
     * Checks the log file for a given message to appear.
     *
     * @param message the message to wait for in the log
     * @param wait    the time in ms to wait for the message to occur
     *
     * @return true if the message was found
     *
     * @throws java.io.FileNotFoundException if the Log file can nolonger be found
     * @throws IOException                   thrown when reading the log file
     */
    public boolean waitForMessage(String message, long wait, boolean printFileOnFailure)
            throws FileNotFoundException, IOException
    {
        // Loop through alerts until we're done or wait ms seconds have passed,
        // just in case the logfile takes a while to flush.
        BufferedReader reader = new BufferedReader(new FileReader(_logfile));
        boolean found = false;
        long endtime = System.currentTimeMillis() + wait;
        ArrayList<String> contents = new ArrayList<String>();
        while (!found && System.currentTimeMillis() < endtime)
        {
            while (reader.ready())
            {
                String line = reader.readLine();
                contents.add(line);
                if (line.contains(message))
                {
                    found = true;
                }
            }
        }
        if (!found && printFileOnFailure)
        {
            for (String line : contents)
            {
                System.out.println(line);
            }
        }
        return found;
    }
    

    public boolean waitForMessage(String message, long alertLogWaitPeriod) throws FileNotFoundException, IOException
    {
       return waitForMessage(message, alertLogWaitPeriod, true);
    }


    /**
     * Read the log file in to memory as a String
     *
     * @return the current contents of the log file
     *
     * @throws java.io.FileNotFoundException if the Log file can nolonger be found
     * @throws IOException                   thrown when reading the log file
     */
    public String readFile() throws FileNotFoundException, IOException
    {
        return FileUtils.readFileAsString(_logfile);
    }

    /**
     * Return a File reference to the monitored file
     *
     * @return the file being monitored
     */
    public File getMonitoredFile()
    {
        return _logfile;
    }

    /**
     * Clears the log file and writes: 'Log Monitor Reset' at the start of the file
     *
     * @throws java.io.FileNotFoundException if the Log file can nolonger be found
     * @throws IOException                   thrown if there is a problem with the log file
     */
    public void reset() throws FileNotFoundException, IOException
    {
        new FileOutputStream(_logfile).getChannel().truncate(0);
    }

    /**
     * Stop monitoring this file.
     *
     * This is required to be called incase we added a new logger.
     *
     * If we don't call close then the new logger will continue to get log entries
     * after our desired test has finished.
     */
    public void close()
    {
        //Remove the custom appender we added for this logger
        if (_appender != null)
        {
            Logger.getRootLogger().removeAppender(_appender);
        }
    }

}
