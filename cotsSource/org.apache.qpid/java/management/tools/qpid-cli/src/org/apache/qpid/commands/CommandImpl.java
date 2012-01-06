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

package org.apache.qpid.commands;

import java.util.Map;

import org.apache.qpid.Command;
import org.apache.qpid.utils.CommandLineOption;
import org.apache.qpid.utils.JMXinfo;

public abstract class CommandImpl implements Command
{
    protected JMXinfo info = null;

    private String name;
    private String virtualhost = null;
    private String object = null;

    private String outputformat = null;
    private String seperator = ",";

    public CommandImpl(JMXinfo info)
    {
        this.info = info;
    }

    public CommandImpl()
    {

    }

    protected void setName(String name)
    {
        this.name = name;
    }

    public String getName()
    {
        return this.name;
    }

    protected boolean hasName()
    {
        if (this.name == null)
            return false;

        else
            return true;
    }

    protected void setObject(String object)
    {
        this.object = object;
    }

    public String getObject()
    {
        return this.object;
    }

    protected void setOutputFormat(String outputformat)
    {
        this.outputformat = outputformat;
    }

    protected String getOutputFormat()
    {
        return outputformat;
    }

    protected void setSeperator(String seperator)
    {
        this.seperator = seperator;
    }

    protected String getSeperator()
    {
        return seperator;
    }

    protected void setVirtualhost(String virtualhost)
    {
        this.virtualhost = virtualhost;
    }

    public String getVirtualhost()
    {
        return this.virtualhost;
    }

    public String optionchecker(String option_letter)
    {
        Map map = info.getCommandLineOptionParser().getAlloptions();
        if (map == null)
            return null;
        CommandLineOption option = (CommandLineOption) map.get(option_letter);
        if (option == null)
            return null;
        String value = option.getOptionValue();
        return value;
    }

    public boolean checkoptionsetting(String option_letter)
    {
        Map map = info.getCommandLineOptionParser().getAlloptions();
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

    public void echo(String str)
    {
        System.out.println(str);
    }

    public void unrecognizeoption()
    {
        echo("list: Unrecognized option");
        echo("Try --help for more information");
    }

    public abstract void execute();

    public abstract void printusage();

}
