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

import org.apache.qpid.configuration.Configuration;
import org.apache.qpid.tools.messagestore.MessageStoreTool;

public class Load extends AbstractCommand
{
    public Load(MessageStoreTool tool)
    {
        super(tool);
    }

    public String help()
    {
        return "Loads specified broker configuration file.";
    }

    public String usage()
    {
        return "load <configuration file>";
    }

    public String getCommand()
    {
        return "load";
    }

    public void execute(String... args)
    {
        assert args.length > 0;
        assert args[0].equals(getCommand());

        if (args.length > 2)
        {
            _console.print("load " + args[1] + ": additional options not understood:");
            for (int i = 2; i < args.length; i++)
            {
                _console.print(args[i] + " ");
            }
            _console.println("");
        }
        else if (args.length < 2)
        {
            _console.println("Enter Configuration file.");
            String input = _console.readln();
            if (input != null)
            {
                doLoad(input);
            }
            else
            {
                _console.println("Did not recognise config file.");
            }
        }
        else
        {
            doLoad(args[1]);
        }
    }

    private void doLoad(String configfile)
    {
        _console.println("Loading Configuration:" + configfile);

        try
        {
            _tool.setConfigurationFile(configfile);
        }
        catch (Configuration.InitException e)
        {
            _console.println("Unable to open config file due to: '" + e.getMessage() + "'");
        }
    }
}
