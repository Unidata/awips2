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
import org.apache.qpid.framing.BasicRejectBody;
import org.apache.qpid.server.AMQChannel;
import org.apache.qpid.server.queue.QueueEntry;
import org.apache.qpid.server.protocol.AMQProtocolSession;
import org.apache.qpid.server.state.AMQStateManager;
import org.apache.qpid.server.state.StateAwareMethodListener;
import org.apache.log4j.Logger;

public class BasicRejectMethodHandler implements StateAwareMethodListener<BasicRejectBody>
{
    private static final Logger _logger = Logger.getLogger(BasicRejectMethodHandler.class);

    private static BasicRejectMethodHandler _instance = new BasicRejectMethodHandler();

    public static BasicRejectMethodHandler getInstance()
    {
        return _instance;
    }

    private BasicRejectMethodHandler()
    {
    }

    public void methodReceived(AMQStateManager stateManager, BasicRejectBody body, int channelId) throws AMQException
    {
        AMQProtocolSession session = stateManager.getProtocolSession();

        AMQChannel channel = session.getChannel(channelId);

        if (channel == null)
        {
            throw body.getChannelNotFoundException(channelId);
        }

        if (_logger.isDebugEnabled())
        {
            _logger.debug("Rejecting:" + body.getDeliveryTag() +
                          ": Requeue:" + body.getRequeue() +
                          //": Resend:" + evt.getMethod().resend +
                          " on channel:" + channel.debugIdentity());
        }

        long deliveryTag = body.getDeliveryTag();

        QueueEntry message = channel.getUnacknowledgedMessageMap().get(deliveryTag);

        if (message == null)
        {
            _logger.warn("Dropping reject request as message is null for tag:" + deliveryTag);
//            throw evt.getMethod().getChannelException(AMQConstant.NOT_FOUND, "Delivery Tag(" + deliveryTag + ")not known");
        }                 
        else
        {
            if (message.isQueueDeleted())
            {
                _logger.warn("Message's Queue as already been purged, unable to Reject. " +
                             "Dropping message should use Dead Letter Queue");
                message = channel.getUnacknowledgedMessageMap().remove(deliveryTag);
                if(message != null)
                {
                    message.discard();
                }
                //sendtoDeadLetterQueue(msg)
                return;
            }

            if (message.getMessage() == null)
            {
                _logger.warn("Message as already been purged, unable to Reject.");
                return;
            }


            if (_logger.isDebugEnabled())
            {
                _logger.debug("Rejecting: DT:" + deliveryTag + "-" + message.getMessage() +
                              ": Requeue:" + body.getRequeue() +
                              //": Resend:" + evt.getMethod().resend +
                              " on channel:" + channel.debugIdentity());
            }

            // If we haven't requested message to be resent to this consumer then reject it from ever getting it.
            //if (!evt.getMethod().resend)
            {
                message.reject();
            }

            if (body.getRequeue())
            {
                channel.requeue(deliveryTag);
            }
            else
            {
                _logger.warn("Dropping message as requeue not required and there is no dead letter queue");
                 message = channel.getUnacknowledgedMessageMap().remove(deliveryTag);
                //sendtoDeadLetterQueue(AMQMessage message)
//                message.queue = channel.getDefaultDeadLetterQueue();
//                channel.requeue(deliveryTag);
            }
        }
    }
}
