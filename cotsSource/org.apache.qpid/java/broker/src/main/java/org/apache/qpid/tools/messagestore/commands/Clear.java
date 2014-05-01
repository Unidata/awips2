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

public class Clear extends AbstractCommand
{
    public Clear(MessageStoreTool tool)
    {
        super(tool);
    }

    public String help()
    {
        return "Clears any selection.";
    }

    public String usage()
    {
        return "clear [ all | virtualhost | exchange | queue | msgs ]";
    }

    public String getCommand()
    {
        return "clear";
    }

    public void execute(String... args)
    {
        assert args.length > 0;
        assert args[0].equals(getCommand());

        if (args.length < 1)
        {
            doClose("all");
        }
        else
        {
            doClose(args[1]);
        }
    }

    private void doClose(String type)
    {
        if (type.equals("virtualhost")
            || type.equals("all"))
        {
            _tool.getState().clearAll();
        }

        if (type.equals("exchange"))
        {
            _tool.getState().clearExchange();
        }

        if (type.equals("queue"))
        {
            _tool.getState().clearQueue();
        }

        if (type.equals("msgs"))
        {
            _tool.getState().clearMessages();
        }
    }
}
