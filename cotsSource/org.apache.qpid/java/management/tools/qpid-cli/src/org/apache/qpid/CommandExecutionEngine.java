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
package org.apache.qpid;

import java.util.HashMap;
import java.util.Map;

import org.apache.qpid.utils.JMXinfo;

public class CommandExecutionEngine
{
    private static Map<String, Class<? extends Command>> _commands = new HashMap<String, Class<? extends Command>>();
    private Command currentcommand = null;
    private String commandname = null;
    private JMXinfo info = null;

    public CommandExecutionEngine(JMXinfo info)
    {
        this.info = info;
        this.commandname = info.getCommandLineOptionParser().getcommandname();
    }

    public boolean CommandSelector() throws Exception
    {
        Class<? extends Command> commandClass = _commands.get(this.commandname);
        if (commandClass != null)
        {
            Class<?> parameterTypes = JMXinfo.class;
            currentcommand = (Command) commandClass.getConstructor(parameterTypes).newInstance(info);
        }
        else
        {
            usage();
            return false;
        }
        return true;
    }

    public static void addCommand(String name, Class<? extends Command> newCommand)
    {
        _commands.put(name, newCommand);
    }
    
    public static Map<String, Class<? extends Command>> getCommands()
    {
        return _commands;
    }

    public void runcommand()
    {
        currentcommand.execute();
    }

    public void usage()
    {
        System.out.println(commandname + ":Command not found");
    }
}
