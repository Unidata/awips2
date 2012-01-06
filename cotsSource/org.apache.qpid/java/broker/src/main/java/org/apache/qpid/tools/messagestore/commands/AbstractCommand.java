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

public abstract class AbstractCommand implements Command
{
    protected Console _console;
    protected MessageStoreTool _tool;

    public AbstractCommand(MessageStoreTool tool)
    {
        _console = tool.getConsole();
        _tool = tool;
    }

    public void setOutput(Console out)
    {
        _console = out;
    }

    protected void commandError(String message, String[] args)
    {
        _console.print(getCommand() + " : " + message);

        if (args != null)
        {
            for (int i = 1; i < args.length; i++)
            {
                _console.print(args[i]);
            }
        }
        _console.println("");
        _console.println(help());
    }


    public abstract String help();

    public abstract String usage();

    public abstract String getCommand();


    public abstract void execute(String... args);
}
