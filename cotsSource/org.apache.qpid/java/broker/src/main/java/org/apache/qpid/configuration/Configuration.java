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
 */
package org.apache.qpid.configuration;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;

public class Configuration
{
    public static final String QPID_HOME = "QPID_HOME";

    final String QPIDHOME = System.getProperty(QPID_HOME);

    private static Logger _devlog = LoggerFactory.getLogger(Configuration.class);

    public static final String DEFAULT_LOG_CONFIG_FILENAME = "log4j.xml";
    public static final String DEFAULT_CONFIG_FILE = "etc/config.xml";

    protected final Options _options = new Options();
    protected CommandLine _commandLine;
    protected File _configFile;


    public Configuration()
    {

    }

    public void processCommandline(String[] args) throws InitException
    {
        try
        {
            _commandLine = new PosixParser().parse(_options, args);
        }
        catch (ParseException e)
        {
            throw new InitException("Unable to parse commmandline", e);
        }

        final File defaultConfigFile = new File(QPIDHOME, DEFAULT_CONFIG_FILE);
        setConfig(new File(_commandLine.getOptionValue("c", defaultConfigFile.getPath())));
    }

    public void setConfig(File file)
    {
        _configFile = file;
    }

    /**
     * @param option The option to set.
     */
    public void setOption(Option option)
    {
        _options.addOption(option);
    }

    /**
     * getOptionValue from the configuration
     * @param option variable argument, first string is option to get, second if present is the default value.
     * @return the String for the given option or null if not present (if default value not specified)
     */
    public String getOptionValue(String... option)
    {
        if (option.length == 1)
        {
            return _commandLine.getOptionValue(option[0]);
        }
        else if (option.length == 2)
        {
            return _commandLine.getOptionValue(option[0], option[1]);
        }
        return null;
    }

    public void loadConfig(File file) throws InitException
    {
        setConfig(file);
        loadConfig();
    }

    private void loadConfig() throws InitException
    {
        if (!_configFile.exists())
        {
            String error = "File " + _configFile + " could not be found. Check the file exists and is readable.";

            if (QPIDHOME == null)
            {
                error = error + "\nNote: " + QPID_HOME + " is not set.";
            }

            throw new InitException(error, null);
        }
        else
        {
            _devlog.debug("Using configuration file " + _configFile.getAbsolutePath());
        }

//        String logConfig = _commandLine.getOptionValue("l");
//        String logWatchConfig = _commandLine.getOptionValue("w", "0");
//        if (logConfig != null)
//        {
//            File logConfigFile = new File(logConfig);
//            configureLogging(logConfigFile, logWatchConfig);
//        }
//        else
//        {
//            File configFileDirectory = _configFile.getParentFile();
//            File logConfigFile = new File(configFileDirectory, DEFAULT_LOG_CONFIG_FILENAME);
//            configureLogging(logConfigFile, logWatchConfig);
//        }
    }


//    private void configureLogging(File logConfigFile, String logWatchConfig)
//    {
//        int logWatchTime = 0;
//        try
//        {
//            logWatchTime = Integer.parseInt(logWatchConfig);
//        }
//        catch (NumberFormatException e)
//        {
//            _devlog.error("Log watch configuration value of " + logWatchConfig + " is invalid. Must be "
//                          + "a non-negative integer. Using default of zero (no watching configured");
//        }
//
//        if (logConfigFile.exists() && logConfigFile.canRead())
//        {
//            _devlog.info("Configuring logger using configuration file " + logConfigFile.getAbsolutePath());
//            if (logWatchTime > 0)
//            {
//                _devlog.info("log file " + logConfigFile.getAbsolutePath() + " will be checked for changes every "
//                             + logWatchTime + " seconds");
//                // log4j expects the watch interval in milliseconds
//                DOMConfigurator.configureAndWatch(logConfigFile.getAbsolutePath(), logWatchTime * 1000);
//            }
//            else
//            {
//                DOMConfigurator.configure(logConfigFile.getAbsolutePath());
//            }
//        }
//        else
//        {
//            System.err.println("Logging configuration error: unable to read file " + logConfigFile.getAbsolutePath());
//            System.err.println("Using basic log4j configuration");
//            BasicConfigurator.configure();
//        }
//    }

    public File getConfigFile()
    {
        return _configFile;
    }


    public class InitException extends Exception
    {
        InitException(String msg, Throwable cause)
        {
            super(msg, cause);
        }
    }
}