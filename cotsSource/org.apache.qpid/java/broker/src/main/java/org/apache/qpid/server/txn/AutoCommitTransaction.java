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
package org.apache.qpid.server.txn;

import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.queue.QueueEntry;
import org.apache.qpid.server.message.EnqueableMessage;
import org.apache.qpid.server.message.ServerMessage;
import org.apache.qpid.server.store.TransactionLog;
import org.apache.qpid.AMQException;

import java.util.List;
import java.util.Collection;

public class AutoCommitTransaction implements ServerTransaction
{

    private final TransactionLog _transactionLog;

    public AutoCommitTransaction(TransactionLog transactionLog)
    {
        _transactionLog = transactionLog;
    }


    public void addPostCommitAction(Action postCommitAction)
    {
        postCommitAction.postCommit();
    }

    public void dequeue(AMQQueue queue, EnqueableMessage message, Action postCommitAction)
    {

        try
        {
            if(message.isPersistent() && queue.isDurable())
            {

                TransactionLog.Transaction txn = _transactionLog.newTransaction();
                txn.dequeueMessage(queue, message.getMessageNumber());
                // store.remove enqueue
                // store.commit
                txn.commitTran();
            }
            postCommitAction.postCommit();
        }
        catch (AMQException e)
        {
            //TODO
            postCommitAction.onRollback();
            throw new RuntimeException(e);
        }
    }

    public void dequeue(Collection<QueueEntry> ackedMessages, Action postCommitAction)
    {
        try
        {
            TransactionLog.Transaction txn = null;
            for(QueueEntry entry : ackedMessages)
            {
                ServerMessage message = entry.getMessage();
                AMQQueue queue = entry.getQueue();

                if(message.isPersistent() && queue.isDurable())
                {
                    if(txn == null)
                    {
                        txn = _transactionLog.newTransaction();
                    }
                    txn.dequeueMessage(queue, message.getMessageNumber());
                }

            }
            if(txn != null)
            {
                txn.commitTran();
            }
            postCommitAction.postCommit();
        }
        catch (AMQException e)
        {
            //TODO
            postCommitAction.onRollback();
            throw new RuntimeException(e);
        }
    }


    public void enqueue(AMQQueue queue, EnqueableMessage message, Action postCommitAction)
    {
        try
        {
            if(message.isPersistent() && queue.isDurable())
            {

                TransactionLog.Transaction txn = _transactionLog.newTransaction();
                txn.enqueueMessage(queue, message.getMessageNumber());
                txn.commitTran();
            }
            postCommitAction.postCommit();
        }
        catch (AMQException e)
        {
            //TODO
            e.printStackTrace();
            postCommitAction.onRollback();
            throw new RuntimeException(e);
        }

    }

    public void enqueue(List<AMQQueue> queues, EnqueableMessage message, Action postCommitAction)
    {
        try
        {

            if(message.isPersistent())
            {
                TransactionLog.Transaction txn = _transactionLog.newTransaction();
                Long id = message.getMessageNumber();
                for(AMQQueue q : queues)
                {
                    if(q.isDurable())
                    {
                        txn.enqueueMessage(q, id);
                    }
                }
                txn.commitTran();

            }
            postCommitAction.postCommit();
        }
        catch (AMQException e)
        {
            //TODO
            postCommitAction.onRollback();
            throw new RuntimeException(e);
        }

    }

    public void commit()
    {

    }

    public void rollback()
    {

    }
}
