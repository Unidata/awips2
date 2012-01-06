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
import org.apache.qpid.server.txn.ServerTransaction;
import org.apache.qpid.server.txn.LocalTransaction;

public class Copy extends Move
{
    public Copy(MessageStoreTool tool)
    {
        super(tool);
    }

    public String help()
    {
        return "Copy messages between queues.";/*\n" +
               "The currently selected message set will be copied to the specifed queue.\n" +
               "Alternatively the values can be provided on the command line."; */
    }

    public String usage()
    {
        return "copy to=<queue> [from=<queue>] [msgids=<msgids eg, 1,2,4-10>]";
    }

    public String getCommand()
    {
        return "copy";
    }

    protected void doCommand(AMQQueue fromQueue, long start, long end, AMQQueue toQueue)
    {
        ServerTransaction txn = new LocalTransaction(fromQueue.getVirtualHost().getTransactionLog());
        fromQueue.copyMessagesToAnotherQueue(start, end, toQueue.getName().toString(), txn);
        txn.commit();
    }

}
