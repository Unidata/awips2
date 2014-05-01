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
import org.apache.qpid.server.queue.AMQQueue;

public class Purge extends Move
{
    public Purge(MessageStoreTool tool)
    {
        super(tool);
    }

    public String help()
    {
        return "Purge messages from a queue.\n" +
               "The currently selected message set will be purged from the specifed queue.\n" +
               "Alternatively the values can be provided on the command line.";
    }

    public String usage()
    {
        return "purge from=<queue> [msgids=<msgids eg, 1,2,4-10>]";
    }

    public String getCommand()
    {
        return "purge";
    }


    protected boolean checkRequirements(AMQQueue fromQueue, AMQQueue toQueue, java.util.List<Long> msgids)
    {
        if (fromQueue == null)
        {
            _console.println("Source queue not specifed.");
            _console.println(usage());
            return false;
        }

        return true;
    }

    protected void doCommand(AMQQueue fromQueue, long start, long end, AMQQueue toQueue)
    {
        fromQueue.removeMessagesFromQueue(start, end);
    }
}
