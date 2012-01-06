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
package org.apache.qpid.server.handler;

import org.apache.qpid.AMQException;
import org.apache.qpid.framing.QueueDeleteBody;
import org.apache.qpid.framing.QueueDeleteOkBody;
import org.apache.qpid.framing.MethodRegistry;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.qpid.server.protocol.AMQProtocolSession;
import org.apache.qpid.server.queue.QueueRegistry;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.state.AMQStateManager;
import org.apache.qpid.server.state.StateAwareMethodListener;
import org.apache.qpid.server.store.MessageStore;
import org.apache.qpid.server.store.DurableConfigurationStore;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.AMQChannel;
import org.apache.qpid.server.security.access.Permission;

public class QueueDeleteHandler implements StateAwareMethodListener<QueueDeleteBody>
{
    private static final QueueDeleteHandler _instance = new QueueDeleteHandler();

    public static QueueDeleteHandler getInstance()
    {
        return _instance;
    }

    private final boolean _failIfNotFound;

    public QueueDeleteHandler()
    {
        this(true);
    }

    public QueueDeleteHandler(boolean failIfNotFound)
    {
        _failIfNotFound = failIfNotFound;

    }

    public void methodReceived(AMQStateManager stateManager, QueueDeleteBody body, int channelId) throws AMQException
    {
        AMQProtocolSession session = stateManager.getProtocolSession();
        VirtualHost virtualHost = session.getVirtualHost();
        QueueRegistry queueRegistry = virtualHost.getQueueRegistry();
        DurableConfigurationStore store = virtualHost.getDurableConfigurationStore();

        AMQQueue queue;
        if (body.getQueue() == null)
        {
            AMQChannel channel = session.getChannel(channelId);

            if (channel == null)
            {
                throw body.getChannelNotFoundException(channelId);
            }

            //get the default queue on the channel:            
            queue = channel.getDefaultQueue();
        }
        else
        {
            queue = queueRegistry.getQueue(body.getQueue());
        }

        if (queue == null)
        {
            if (_failIfNotFound)
            {
                throw body.getChannelException(AMQConstant.NOT_FOUND, "Queue " + body.getQueue() + " does not exist.");
            }
        }
        else
        {
            if (body.getIfEmpty() && !queue.isEmpty())
            {
                throw body.getChannelException(AMQConstant.IN_USE, "Queue: " + body.getQueue() + " is not empty.");
            }
            else if (body.getIfUnused() && !queue.isUnused())
            {
                // TODO - Error code
                throw body.getChannelException(AMQConstant.IN_USE, "Queue: " + body.getQueue() + " is still used.");

            }
            else
            {
                
                //Perform ACLs
                if (!virtualHost.getAccessManager().authoriseDelete(session, queue))
                {
                    throw body.getConnectionException(AMQConstant.ACCESS_REFUSED, "Permission denied");
                }
                else if (queue.isExclusive() && !queue.isDurable() && queue.getExclusiveOwner() != session)
                {
                    throw body.getConnectionException(AMQConstant.NOT_ALLOWED,
                                                      "Queue " + queue.getName() + " is exclusive, but not created on this Connection.");
                }
                int purged = queue.delete();


                if (queue.isDurable())
                {
                    store.removeQueue(queue);
                }

                MethodRegistry methodRegistry = session.getMethodRegistry();
                QueueDeleteOkBody responseBody = methodRegistry.createQueueDeleteOkBody(purged);
                session.writeFrame(responseBody.generateFrame(channelId));
            }
        }
    }
}
