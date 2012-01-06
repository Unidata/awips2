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
package org.apache.qpid.utils;

import java.util.Map;

public class JMXConfiguration
{
    private String hostname = "localhost";
    private String port = "8999";
    private String interval = "40000";
    private String outputpath = ".";
    private String report_file = "report.output";
    private boolean isreport_mode = false;
    private String username = null;
    private String password = null;

    public JMXConfiguration(Map map)
    {
        if (checkoptionsetting(CommandLineOptionConstants.JMXCommandLineOptionConstants.HOST_OPTION, map))
        {
            this.hostname = optionchecker(CommandLineOptionConstants.JMXCommandLineOptionConstants.HOST_OPTION, map);
        }
        if (checkoptionsetting(CommandLineOptionConstants.JMXCommandLineOptionConstants.PORT_OPTION, map))
        {
            this.port = optionchecker(CommandLineOptionConstants.JMXCommandLineOptionConstants.PORT_OPTION, map);
        }
        if (checkoptionsetting(CommandLineOptionConstants.JMXCommandLineOptionConstants.REPORT_OPTION, map))
        {

            this.report_file = optionchecker(CommandLineOptionConstants.JMXCommandLineOptionConstants.REPORT_OPTION,
                    map);
        }
        if (checkoptionsetting(CommandLineOptionConstants.JMXCommandLineOptionConstants.USER_OPTION, map))
        {

            this.setUsername(optionchecker(CommandLineOptionConstants.JMXCommandLineOptionConstants.USER_OPTION, map));
        }

        if (checkoptionsetting(CommandLineOptionConstants.JMXCommandLineOptionConstants.PASSWORD_OPTION, map))
        {
            this.setPassword(optionchecker(CommandLineOptionConstants.JMXCommandLineOptionConstants.PASSWORD_OPTION,
                    map));
        }

    }

    public void sethostname(String hostname)
    {
        this.hostname = hostname;
    }

    public void setport(String port)
    {
        this.port = port;
    }

    public void setinterval(String interval)
    {
        this.interval = interval;
    }

    public void setoutputpath(String output)
    {
        this.outputpath = output;
    }

    public String gethostname()
    {
        return this.hostname;
    }

    public String getport()
    {
        return this.port;
    }

    public String getinterval()
    {
        return this.interval;
    }

    public String getoutputpath()
    {
        return this.outputpath;
    }

    public CommandLineOption loadoption(String option, Map options)
    {
        CommandLineOption op = null;
        if (option != null)
        {
            op = (CommandLineOption) options.get(option);

        }
        return op;

    }

    public void setreportfile(String reportfile)
    {
        this.report_file = reportfile;
        this.isreport_mode = true;

    }

    public boolean isreportmode()
    {
        return this.isreport_mode;
    }

    public String getreportfile()
    {
        return this.report_file;
    }

    public String optionchecker(String option_letter, Map map)
    {

        if (map == null)
            return null;
        CommandLineOption option = (CommandLineOption) map.get(option_letter);
        if (option == null)
            return null;
        String value = option.getOptionValue();
        return value;
    }

    public boolean checkoptionsetting(String option_letter, Map map)
    {
        if (map == null)
            return false;
        CommandLineOption option = (CommandLineOption) map.get(option_letter);
        if (option == null)
            return false;
        String value = option.getOptionType();

        if (value != null)
            return true;
        else
            return false;
    }

    public void setUsername(String username)
    {
        this.username = username;
    }

    public String getUsername()
    {
        return username;
    }

    public void setPassword(String password)
    {
        this.password = password;
    }

    public String getPassword()
    {
        return password;
    }

}
