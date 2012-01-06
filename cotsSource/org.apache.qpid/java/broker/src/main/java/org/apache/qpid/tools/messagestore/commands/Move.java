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

import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.queue.QueueEntry;
import org.apache.qpid.server.txn.ServerTransaction;
import org.apache.qpid.server.txn.LocalTransaction;
import org.apache.qpid.tools.messagestore.MessageStoreTool;

import java.util.LinkedList;
import java.util.List;

public class Move extends AbstractCommand
{

    public Move(MessageStoreTool tool)
    {
        super(tool);
    }

    public String help()
    {
        return "Move messages between queues.";/*\n" +
               "The currently selected message set will be moved to the specifed queue.\n" +
               "Alternatively the values can be provided on the command line.";*/
    }

    public String usage()
    {
        return "move to=<queue> [from=<queue>] [msgids=<msgids eg, 1,2,4-10>]";
    }

    public String getCommand()
    {
        return "move";
    }

    public void execute(String... args)
    {
        AMQQueue toQueue = null;
        AMQQueue fromQueue = _tool.getState().getQueue();
        java.util.List<Long> msgids = _tool.getState().getMessages();

        if (args.length >= 2)
        {
            for (String arg : args)
            {
                if (arg.startsWith("to="))
                {
                    String queueName = arg.substring(arg.indexOf("=") + 1);
                    toQueue = _tool.getState().getVhost().getQueueRegistry().getQueue(new AMQShortString(queueName));
                }

                if (arg.startsWith("from="))
                {
                    String queueName = arg.substring(arg.indexOf("=") + 1);
                    fromQueue = _tool.getState().getVhost().getQueueRegistry().getQueue(new AMQShortString(queueName));
                }

                if (arg.startsWith("msgids="))
                {
                    String msgidStr = arg.substring(arg.indexOf("=") + 1);

                    // Record the current message selection
                    java.util.List<Long> currentIDs = _tool.getState().getMessages();

                    // Use the ToolState class to perform the messasge parsing
                    _tool.getState().setMessages(msgidStr);
                    msgids = _tool.getState().getMessages();

                    // Reset the original selection of messages
                    _tool.getState().setMessages(currentIDs);
                }
            }
        }

        if (!checkRequirements(fromQueue, toQueue, msgids))
        {
            return;
        }

        processIDs(fromQueue, toQueue, msgids);
    }

    private void processIDs(AMQQueue fromQueue, AMQQueue toQueue, java.util.List<Long> msgids)
    {
        Long previous = null;
        Long start = null;

        if (msgids == null)
        {
            msgids = allMessageIDs(fromQueue);
        }

        if (msgids == null || msgids.size() == 0)
        {
            _console.println("No Messages to move.");
            return;
        }

        for (long id : msgids)
        {
            if (previous != null)
            {
                if (id == previous + 1)
                {
                    if (start == null)
                    {
                        start = previous;
                    }
                }
                else
                {
                    if (start != null)
                    {
                        //move a range of ids
                        doCommand(fromQueue, start, id, toQueue);
                        start = null;
                    }
                    else
                    {
                        //move a single id
                        doCommand(fromQueue, id, id, toQueue);
                    }
                }
            }

            previous = id;
        }

        if (start != null)
        {
            //move a range of ids
            doCommand(fromQueue, start, previous, toQueue);
        }
    }

    private List<Long> allMessageIDs(AMQQueue fromQueue)
    {
        List<Long> ids = new LinkedList<Long>();

        if (fromQueue != null)
        {
            List<QueueEntry> messages = fromQueue.getMessagesOnTheQueue();
            if (messages != null)
            {
                for (QueueEntry msg : messages)
                {
                    ids.add(msg.getMessage().getMessageNumber());
                }
            }
        }

        return ids;
    }

    protected boolean checkRequirements(AMQQueue fromQueue, AMQQueue toQueue, List<Long> msgids)
    {
        if (toQueue == null)
        {
            _console.println("Destination queue not specifed.");
            _console.println(usage());
            return false;
        }

        if (fromQueue == null)
        {
            _console.println("Source queue not specifed.");
            _console.println(usage());
            return false;
        }

        return true;
    }

    protected void doCommand(AMQQueue fromQueue, long start, long id, AMQQueue toQueue)
    {
        ServerTransaction txn = new LocalTransaction(fromQueue.getVirtualHost().getTransactionLog());
        fromQueue.moveMessagesToAnotherQueue(start, id, toQueue.getName().toString(), txn);
        txn.commit();
    }
}
