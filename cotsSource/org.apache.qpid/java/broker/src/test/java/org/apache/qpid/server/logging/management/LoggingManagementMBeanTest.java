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
package org.apache.qpid.server.logging.management;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.management.JMException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.TabularDataSupport;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.qpid.management.common.mbeans.LoggingManagement;

import junit.framework.TestCase;

public class LoggingManagementMBeanTest extends TestCase
{
    private static final String TEST_LOGGER = "LoggingManagementMBeanTestLogger";
    private static final String TEST_LOGGER_CHILD1 = "LoggingManagementMBeanTestLogger.child1";
    private static final String TEST_LOGGER_CHILD2 = "LoggingManagementMBeanTestLogger.child2";

    private static final String CATEGORY_PRIORITY = "LogManMBeanTest.category.priority";
    private static final String CATEGORY_LEVEL = "LogManMBeanTest.category.level";
    private static final String LOGGER_LEVEL = "LogManMBeanTest.logger.level";
    
    private static final String NAME_INDEX = LoggingManagement.COMPOSITE_ITEM_NAMES[0];
    private static final String LEVEL_INDEX = LoggingManagement.COMPOSITE_ITEM_NAMES[1];

    private static final String NEWLINE = System.getProperty("line.separator");

    private File _testConfigFile;

    protected void setUp() throws Exception
    {
        _testConfigFile = createTempTestLog4JConfig();
    }
    
    protected void tearDown() throws Exception
    {
        File oldTestConfigFile = new File(_testConfigFile.getAbsolutePath() + ".old");
        if(oldTestConfigFile.exists())
        {
            oldTestConfigFile.delete();
        }
        
        _testConfigFile.delete();
    }

    private File createTempTestLog4JConfig()
    {
        File tmpFile = null;
        try
        {
            tmpFile = File.createTempFile("LogManMBeanTestLog4jConfig", ".tmp");
            tmpFile.deleteOnExit();

            FileWriter fstream = new FileWriter(tmpFile);
            BufferedWriter writer = new BufferedWriter(fstream);

            writer.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"+NEWLINE);
            writer.write("<!DOCTYPE log4j:configuration SYSTEM \"log4j.dtd\">"+NEWLINE);

            writer.write("<log4j:configuration xmlns:log4j=\"http://jakarta.apache.org/log4j/\" debug=\"null\" " +
            		                                                                "threshold=\"null\">"+NEWLINE);

            writer.write("  <appender class=\"org.apache.log4j.ConsoleAppender\" name=\"STDOUT\">"+NEWLINE);
            writer.write("      <layout class=\"org.apache.log4j.PatternLayout\">"+NEWLINE);
            writer.write("          <param name=\"ConversionPattern\" value=\"%d %-5p [%t] %C{2} (%F:%L) - %m%n\"/>"+NEWLINE);
            writer.write("      </layout>"+NEWLINE);
            writer.write("  </appender>"+NEWLINE);

            //Example of a 'category' with a 'priority'
            writer.write("  <category additivity=\"true\" name=\"" + CATEGORY_PRIORITY +"\">"+NEWLINE);
            writer.write("      <priority value=\"info\"/>"+NEWLINE);
            writer.write("      <appender-ref ref=\"STDOUT\"/>"+NEWLINE);
            writer.write("  </category>"+NEWLINE);

            //Example of a 'category' with a 'level'
            writer.write("  <category additivity=\"true\" name=\"" + CATEGORY_LEVEL +"\">"+NEWLINE);
            writer.write("      <level value=\"warn\"/>"+NEWLINE);
            writer.write("      <appender-ref ref=\"STDOUT\"/>"+NEWLINE);
            writer.write("  </category>"+NEWLINE);

            //Example of a 'logger' with a 'level'
            writer.write("  <logger additivity=\"true\" name=\"" + LOGGER_LEVEL + "\">"+NEWLINE);
            writer.write("      <level value=\"error\"/>"+NEWLINE);
            writer.write("      <appender-ref ref=\"STDOUT\"/>"+NEWLINE);
            writer.write("  </logger>"+NEWLINE);

            //'root' logger
            writer.write("  <root>"+NEWLINE);
            writer.write("      <priority value=\"info\"/>"+NEWLINE);
            writer.write("      <appender-ref ref=\"STDOUT\"/>"+NEWLINE);
            writer.write("  </root>"+NEWLINE);

            writer.write("</log4j:configuration>"+NEWLINE);

            writer.flush();
            writer.close();
        }
        catch (IOException e)
        {
            fail("Unable to create temporary test log4j configuration");
        }

        return tmpFile;
    }



    //******* Test Methods ******* //
   
    public void testSetRuntimeLoggerLevel()
    {
        LoggingManagementMBean lm = null;
        try
        {
            lm = new LoggingManagementMBean(_testConfigFile.getAbsolutePath(), 0);
        }
        catch (JMException e)
        {
            fail("Could not create test LoggingManagementMBean");
        }

        //create a parent test logger, set its level explicitly
        Logger log = Logger.getLogger(TEST_LOGGER);
        log.setLevel(Level.toLevel("info"));

        //create child1 test logger, check its *effective* level is the same as the parent, "info"
        Logger log1 = Logger.getLogger(TEST_LOGGER_CHILD1);
        assertTrue("Test logger's level was not the expected value", 
                    log1.getEffectiveLevel().toString().equalsIgnoreCase("info"));

        //now change its level to "warn"
        assertTrue("Failed to set logger level", lm.setRuntimeLoggerLevel(TEST_LOGGER_CHILD1, "warn"));

        //check the change, see its actual level is "warn
        assertTrue("Test logger's level was not the expected value", 
                    log1.getLevel().toString().equalsIgnoreCase("warn"));

        //try an invalid level
        assertFalse("Trying to set an invalid level succeded", lm.setRuntimeLoggerLevel(TEST_LOGGER_CHILD1, "made.up.level"));
    }
    
    public void testSetRuntimeRootLoggerLevel()
    {
        LoggingManagementMBean lm = null;
        try
        {
            lm = new LoggingManagementMBean(_testConfigFile.getAbsolutePath(), 0);
        }
        catch (JMException e)
        {
            fail("Could not create test LoggingManagementMBean");
        }

        Logger log = Logger.getRootLogger();
        
        //get current root logger level
        Level origLevel = log.getLevel();
        
        //change level twice to ensure a new level is actually selected
        
        //set root loggers level to info
        assertTrue("Failed to set root logger level", lm.setRuntimeRootLoggerLevel("debug"));
        //check it is now actually info
        Level currentLevel = log.getLevel();
        assertTrue("Logger level was not expected value", currentLevel.equals(Level.toLevel("debug")));
        
        //try an invalid level
        assertFalse("Trying to set an invalid level succeded", lm.setRuntimeRootLoggerLevel("made.up.level"));
        
        //set root loggers level to warn
        assertTrue("Failed to set logger level", lm.setRuntimeRootLoggerLevel("info"));
        //check it is now actually warn
        currentLevel = log.getLevel();
        assertTrue("Logger level was not expected value", currentLevel.equals(Level.toLevel("info")));
        
        //restore original level
        log.setLevel(origLevel);
    }

    public void testGetRuntimeRootLoggerLevel()
    {
        LoggingManagementMBean lm = null;
        try
        {
            lm = new LoggingManagementMBean(_testConfigFile.getAbsolutePath(), 0);
        }
        catch (JMException e)
        {
            fail("Could not create test LoggingManagementMBean");
        }

        Logger log = Logger.getRootLogger();
        
        //get current root logger level
        Level origLevel = log.getLevel();
        
        //change level twice to ensure a new level is actually selected
        
        //set root loggers level to debug
        log.setLevel(Level.toLevel("debug"));
        //check it is now actually debug
        assertTrue("Logger level was not expected value", lm.getRuntimeRootLoggerLevel().equalsIgnoreCase("debug"));
        
        
        //set root loggers level to warn
        log.setLevel(Level.toLevel("info"));
        //check it is now actually warn
        assertTrue("Logger level was not expected value", lm.getRuntimeRootLoggerLevel().equalsIgnoreCase("info"));

        //restore original level
        log.setLevel(origLevel);
    }
    
    public void testViewEffectiveRuntimeLoggerLevels()
    {
        LoggingManagementMBean lm = null;
        try
        {
            lm = new LoggingManagementMBean(_testConfigFile.getAbsolutePath(), 0);
        }
        catch (JMException e)
        {
            fail("Could not create test LoggingManagementMBean");
        }

        //(re)create a parent test logger, set its level explicitly
        Logger log = Logger.getLogger(TEST_LOGGER);
        log.setLevel(Level.toLevel("info"));
        
        //retrieve the current effective runtime logger level values
        TabularDataSupport levels = (TabularDataSupport) lm.viewEffectiveRuntimeLoggerLevels();
        Collection<Object> records = levels.values();
        Map<String,String> list = new HashMap<String,String>();
        for (Object o : records)
        {
            CompositeData data = (CompositeData) o;
            list.put(data.get(NAME_INDEX).toString(), data.get(LEVEL_INDEX).toString());
        }
        
        //check child2 does not exist already
        assertFalse("Did not expect this logger to exist already", list.containsKey(TEST_LOGGER_CHILD2));

        //create child2 test logger
        Logger log2 = Logger.getLogger(TEST_LOGGER_CHILD2);
        
        //retrieve the current effective runtime logger level values
        levels = (TabularDataSupport) lm.viewEffectiveRuntimeLoggerLevels();
        records = levels.values();
        list = new HashMap<String,String>();
        for (Object o : records)
        {
            CompositeData data = (CompositeData) o;
            list.put(data.get(NAME_INDEX).toString(), data.get(LEVEL_INDEX).toString());
        }

        //verify the parent and child2 loggers are present in returned values
        assertTrue(TEST_LOGGER + " logger was not in the returned list", list.containsKey(TEST_LOGGER));
        assertTrue(TEST_LOGGER_CHILD2 + " logger was not in the returned list", list.containsKey(TEST_LOGGER_CHILD2));
        
        //check child2's effective level is the same as the parent, "info"
        assertTrue("Test logger's level was not the expected value", 
                    list.get(TEST_LOGGER_CHILD2).equalsIgnoreCase("info"));

        //now change its level explicitly to "warn"
        log2.setLevel(Level.toLevel("warn"));
        
        //retrieve the current effective runtime logger level values
        levels = (TabularDataSupport) lm.viewEffectiveRuntimeLoggerLevels();
        records = levels.values();
        list = new HashMap<String,String>();
        for (Object o : records)
        {
            CompositeData data = (CompositeData) o;
            list.put(data.get(NAME_INDEX).toString(), data.get(LEVEL_INDEX).toString());
        }

        //check child2's effective level is now "warn"
        assertTrue("Test logger's level was not the expected value", 
                    list.get(TEST_LOGGER_CHILD2).equalsIgnoreCase("warn"));
    }

    public void testViewAndSetConfigFileLoggerLevel() throws Exception
    {
        LoggingManagementMBean lm =null;
        try
        {
            lm = new LoggingManagementMBean(_testConfigFile.getAbsolutePath(), 0);
        }
        catch (JMException e)
        {
            fail("Could not create test LoggingManagementMBean");
        }

        //retrieve the current values
        TabularDataSupport levels = (TabularDataSupport) lm.viewConfigFileLoggerLevels();
        Collection<Object> records = levels.values();
        Map<String,String> list = new HashMap<String,String>();
        for (Object o : records)
        {
            CompositeData data = (CompositeData) o;
            list.put(data.get(NAME_INDEX).toString(), data.get(LEVEL_INDEX).toString());
        }

        //check the 3 different types of logger definition are successfully retrieved before update
        assertTrue("Wrong number of items in returned list", list.size() == 3);
        assertTrue(CATEGORY_PRIORITY + " logger was not in the returned list", list.containsKey(CATEGORY_PRIORITY));
        assertTrue(CATEGORY_LEVEL + " logger was not in the returned list", list.containsKey(CATEGORY_LEVEL));
        assertTrue(LOGGER_LEVEL + " logger was not in the returned list", list.containsKey(LOGGER_LEVEL));

        //check that their level is as expected
        assertTrue(CATEGORY_PRIORITY + " logger's level was incorrect", list.get(CATEGORY_PRIORITY).equalsIgnoreCase("info"));
        assertTrue(CATEGORY_LEVEL + " logger's level was incorrect", list.get(CATEGORY_LEVEL).equalsIgnoreCase("warn"));
        assertTrue(LOGGER_LEVEL + " logger's level was incorrect", list.get(LOGGER_LEVEL).equalsIgnoreCase("error"));

        //increase their levels a notch to test the 3 different types of logger definition are successfully updated
        //change the category+priority to warn
        assertTrue("failed to set new level", lm.setConfigFileLoggerLevel(CATEGORY_PRIORITY, "warn"));
        //change the category+level to error
        assertTrue("failed to set new level", lm.setConfigFileLoggerLevel(CATEGORY_LEVEL, "error"));
        //change the logger+level to trace
        assertTrue("failed to set new level", lm.setConfigFileLoggerLevel(LOGGER_LEVEL, "trace"));

        //try an invalid level
        assertFalse("Use of an invalid logger level was successfull", lm.setConfigFileLoggerLevel(LOGGER_LEVEL, "made.up.level"));

        //try an invalid logger name
        assertFalse("Use of an invalid logger name was successfull", lm.setConfigFileLoggerLevel("made.up.logger.name", "info"));

        //retrieve the new values from the file and check them
        levels = (TabularDataSupport) lm.viewConfigFileLoggerLevels();
        records = levels.values();
        list = new HashMap<String,String>();
        for (Object o : records)
        {
            CompositeData data = (CompositeData) o;
            list.put(data.get(NAME_INDEX).toString(), data.get(LEVEL_INDEX).toString());
        }

        //check the 3 different types of logger definition are successfully retrieved after update
        assertTrue("Wrong number of items in returned list", list.size() == 3);
        assertTrue(CATEGORY_PRIORITY + " logger was not in the returned list", list.containsKey(CATEGORY_PRIORITY));
        assertTrue(CATEGORY_LEVEL + " logger was not in the returned list", list.containsKey(CATEGORY_LEVEL));
        assertTrue(LOGGER_LEVEL + " logger was not in the returned list", list.containsKey(LOGGER_LEVEL));

        //check that their level is as expected after the changes
        assertTrue(CATEGORY_PRIORITY + " logger's level was incorrect", list.get(CATEGORY_PRIORITY).equalsIgnoreCase("warn"));
        assertTrue(CATEGORY_LEVEL + " logger's level was incorrect", list.get(CATEGORY_LEVEL).equalsIgnoreCase("error"));
        assertTrue(LOGGER_LEVEL + " logger's level was incorrect", list.get(LOGGER_LEVEL).equalsIgnoreCase("trace"));
    }
    
    public void testGetAndSetConfigFileRootLoggerLevel() throws Exception
    {
        LoggingManagementMBean lm =null;
        try
        {
            lm = new LoggingManagementMBean(_testConfigFile.getAbsolutePath(), 0);
        }
        catch (JMException e)
        {
            fail("Could not create test LoggingManagementMBean");
        }

        //retrieve the current value
        String level = lm.getConfigFileRootLoggerLevel();

        //check the value was successfully retrieved before update
        assertTrue("Retrieved RootLogger level was incorrect", level.equalsIgnoreCase("info"));

        //try an invalid level
        assertFalse("Use of an invalid RootLogger level was successfull", lm.setConfigFileRootLoggerLevel("made.up.level"));
        
        //change the level to warn
        assertTrue("Failed to set new RootLogger level", lm.setConfigFileRootLoggerLevel("warn"));

        //retrieve the current value
        level = lm.getConfigFileRootLoggerLevel();

        //check the value was successfully retrieved after update
        assertTrue("Retrieved RootLogger level was incorrect", level.equalsIgnoreCase("warn"));
    }

    public void testGetLog4jLogWatchInterval()
    {
        LoggingManagementMBean lm =null;
        try
        {
            lm = new LoggingManagementMBean(_testConfigFile.getAbsolutePath(), 5000);
        }
        catch (JMException e)
        {
            fail("Could not create test LoggingManagementMBean");
        }

        assertTrue("Wrong value returned for logWatch period", lm.getLog4jLogWatchInterval() == 5000);
    }

}
