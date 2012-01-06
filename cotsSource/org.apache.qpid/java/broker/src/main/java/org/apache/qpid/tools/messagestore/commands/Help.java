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
package org.apache.qpid.tools.messagestore.commands;

import org.apache.qpid.tools.messagestore.MessageStoreTool;
import org.apache.qpid.tools.utils.Console;

import java.util.LinkedList;
import java.util.Map;

public class Help extends AbstractCommand
{
    public Help(MessageStoreTool tool)
    {
        super(tool);
    }

    public String help()
    {
        return "Provides detailed help on commands.";
    }

    public String getCommand()
    {
        return "help";
    }

    public String usage()
    {
        return "help [<command>]";
    }

    public void execute(String... args)
    {
        assert args.length > 0;
        assert args[0].equals(getCommand());

        if (args.length > 1)
        {
            Command command = _tool.getCommands().get(args[1]);
            if (command != null)
            {
                _console.println(command.help());
                _console.println("Usage:" + command.usage());
            }
            else
            {
                commandError("Command not found: ", args);
            }
        }
        else
        {
            java.util.List<java.util.List> data = new LinkedList<java.util.List>();

            java.util.List<String> commandName = new LinkedList<String>();
            java.util.List<String> commandDescription = new LinkedList<String>();

            data.add(commandName);
            data.add(commandDescription);

            //Set up Headers
            commandName.add("Command");
            commandDescription.add("Description");

            commandName.add(Console.ROW_DIVIDER);
            commandDescription.add(Console.ROW_DIVIDER);

            //Add current Commands with descriptions
            Map<String, Command> commands = _tool.getCommands();

            for (Command command : commands.values())
            {
                commandName.add(command.getCommand());
                commandDescription.add(command.help());
            }

            _console.printMap("Available Commands", data);
        }
    }
}
