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
package org.apache.log4j.xml;


import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.apache.log4j.xml.QpidLog4JConfigurator.IllegalLoggerLevelException;

import junit.framework.TestCase;

public class QpidLog4JConfiguratorTest extends TestCase
{
    private static final String NEWLINE = System.getProperty("line.separator");

    private File _testConfigFile;

    private File createTempTestLog4JConfig(String loggerLevel,String rootLoggerLevel, boolean missingTagClose, boolean incorrectAttribute)
    {
        File tmpFile = null;
        try
        {
            tmpFile = File.createTempFile("QpidLog4JConfiguratorTestLog4jConfig", ".tmp");
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
            
            String closeTag="/";
            if(missingTagClose)
            {
                closeTag="";
            }

            //Example of a 'category' with a 'priority'
            writer.write("  <category additivity=\"true\" name=\"logger1\">"+NEWLINE);
            writer.write("      <priority value=\"" + loggerLevel+ "\"" + closeTag + ">"+NEWLINE);
            writer.write("      <appender-ref ref=\"STDOUT\"/>"+NEWLINE);
            writer.write("  </category>"+NEWLINE);

            String attributeName="value";
            if(incorrectAttribute)
            {
                attributeName="values";
            }
            
            //Example of a 'category' with a 'level'
            writer.write("  <category additivity=\"true\" name=\"logger2\">"+NEWLINE);
            writer.write("      <level " + attributeName + "=\"" + loggerLevel+ "\"/>"+NEWLINE);
            writer.write("      <appender-ref ref=\"STDOUT\"/>"+NEWLINE);
            writer.write("  </category>"+NEWLINE);

            //Example of a 'logger' with a 'level'
            writer.write("  <logger additivity=\"true\" name=\"logger3\">"+NEWLINE);
            writer.write("      <level value=\"" + loggerLevel+ "\"/>"+NEWLINE);
            writer.write("      <appender-ref ref=\"STDOUT\"/>"+NEWLINE);
            writer.write("  </logger>"+NEWLINE);

            //'root' logger
            writer.write("  <root>"+NEWLINE);
            writer.write("      <priority value=\"" + rootLoggerLevel+ "\"/>"+NEWLINE);
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

    public void testCheckLevelsAndStrictParser()
    {
        //try the valid logger levels
        _testConfigFile = createTempTestLog4JConfig("all", "info", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
        }
        catch (Exception e)
        {
            fail("No exception expected, valid levels and xml were used");
        }
        
        _testConfigFile = createTempTestLog4JConfig("trace", "info", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
        }
        catch (Exception e)
        {
            fail("No exception expected, valid levels and xml were used");
        }
        
        _testConfigFile = createTempTestLog4JConfig("debug", "info", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
        }
        catch (Exception e)
        {
            fail("No exception expected, valid levels and xml were used");
        }
        
        _testConfigFile = createTempTestLog4JConfig("info", "info", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
        }
        catch (Exception e)
        {
            fail("No exception expected, valid levels and xml were used");
        }
        
        _testConfigFile = createTempTestLog4JConfig("warn", "info", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
        }
        catch (Exception e)
        {
            fail("No exception expected, valid levels and xml were used");
        }
        
        _testConfigFile = createTempTestLog4JConfig("error", "info", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
        }
        catch (Exception e)
        {
            fail("No exception expected, valid levels and xml were used");
        }
        
        _testConfigFile = createTempTestLog4JConfig("fatal", "info", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
        }
        catch (Exception e)
        {
            fail("No exception expected, valid levels and xml were used");
        }
        
        _testConfigFile = createTempTestLog4JConfig("off", "info", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
        }
        catch (Exception e)
        {
            fail("No exception expected, valid levels and xml were used");
        }
        
        _testConfigFile = createTempTestLog4JConfig("null", "info", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
        }
        catch (Exception e)
        {
            fail("No exception expected, valid levels and xml were used");
        }
        
        _testConfigFile = createTempTestLog4JConfig("inherited", "info", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
        }
        catch (Exception e)
        {
            fail("No exception expected, valid levels and xml were used");
        }
        
        //now try an invalid logger level
        _testConfigFile = createTempTestLog4JConfig("madeup", "info", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
            fail("IllegalLoggerLevelException expected, invalid levels used");
        }
        catch (IllegalLoggerLevelException e)
        {
            //expected, ignore
        }
        catch (IOException e)
        {
            fail("Incorrect Exception, expected an IllegalLoggerLevelException");
        }
        
        
        
        //now try the valid rootLogger levels
        _testConfigFile = createTempTestLog4JConfig("info", "all", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
        }
        catch (Exception e)
        {
            fail("No exception expected, valid levels and xml were used");
        }
        
        _testConfigFile = createTempTestLog4JConfig("info", "trace", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
        }
        catch (Exception e)
        {
            fail("No exception expected, valid levels and xml were used");
        }
        
        _testConfigFile = createTempTestLog4JConfig("info", "debug", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
        }
        catch (Exception e)
        {
            fail("No exception expected, valid levels and xml were used");
        }
        
        _testConfigFile = createTempTestLog4JConfig("info", "info", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
        }
        catch (Exception e)
        {
            fail("No exception expected, valid levels and xml were used");
        }
        
        _testConfigFile = createTempTestLog4JConfig("info", "warn", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
        }
        catch (Exception e)
        {
            fail("No exception expected, valid levels and xml were used");
        }
        
        _testConfigFile = createTempTestLog4JConfig("info", "error", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
        }
        catch (Exception e)
        {
            fail("No exception expected, valid levels and xml were used");
        }
        
        _testConfigFile = createTempTestLog4JConfig("info", "fatal", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
        }
        catch (Exception e)
        {
            fail("No exception expected, valid levels and xml were used");
        }
        
        _testConfigFile = createTempTestLog4JConfig("info", "off", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
        }
        catch (Exception e)
        {
            fail("No exception expected, valid levels and xml were used");
        }
        
        _testConfigFile = createTempTestLog4JConfig("info", "null", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
        }
        catch (Exception e)
        {
            fail("No exception expected, valid levels and xml were used");
        }
        
        _testConfigFile = createTempTestLog4JConfig("info", "inherited", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
        }
        catch (Exception e)
        {
            fail("No exception expected, valid levels and xml were used");
        }
        
        _testConfigFile = createTempTestLog4JConfig("info", "debug", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
        }
        catch (Exception e)
        {
            fail("No exception expected, valid levels and xml were used");
        }
        
        //now try an invalid logger level
        _testConfigFile = createTempTestLog4JConfig("info", "madeup", false, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
            fail("IllegalLoggerLevelException expected, invalid levels used");
        }
        catch (IllegalLoggerLevelException e)
        {
            //expected, ignore
        }
        catch (IOException e)
        {
            fail("Incorrect Exception, expected an IllegalLoggerLevelException");
        }
        
        
        
        //now try invalid xml
        _testConfigFile = createTempTestLog4JConfig("info", "info", true, false);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
            fail("IOException expected, malformed XML used");
        }
        catch (IllegalLoggerLevelException e)
        {
            fail("Incorrect Exception, expected an IOException");
        }
        catch (IOException e)
        {
            //expected, ignore
        }
        
        _testConfigFile = createTempTestLog4JConfig("info", "info", false, true);
        try
        {
            QpidLog4JConfigurator.checkLoggerLevels(_testConfigFile.getAbsolutePath());
            fail("IOException expected, malformed XML used");
        }
        catch (IllegalLoggerLevelException e)
        {
            //expected, ignore
        }
        catch (IOException e)
        {
            fail("Incorrect Exception, expected an IllegalLoggerLevelException");
        }
    }
}
