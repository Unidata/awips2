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
package org.apache.qpid.client.handler;

import org.apache.qpid.AMQChannelClosedException;
import org.apache.qpid.AMQException;
import org.apache.qpid.AMQInvalidRoutingKeyException;
import org.apache.qpid.client.AMQNoConsumersException;
import org.apache.qpid.client.AMQNoRouteException;
import org.apache.qpid.client.protocol.AMQProtocolSession;
import org.apache.qpid.client.state.StateAwareMethodListener;
import org.apache.qpid.framing.AMQFrame;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.ChannelCloseBody;
import org.apache.qpid.framing.ChannelCloseOkBody;
import org.apache.qpid.protocol.AMQConstant;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ChannelCloseMethodHandler implements StateAwareMethodListener<ChannelCloseBody>
{
    private static final Logger _logger = LoggerFactory.getLogger(ChannelCloseMethodHandler.class);

    private static ChannelCloseMethodHandler _handler = new ChannelCloseMethodHandler();

    public static ChannelCloseMethodHandler getInstance()
    {
        return _handler;
    }

    public void methodReceived(AMQProtocolSession session, ChannelCloseBody method, int channelId)
            throws AMQException
    {
        _logger.debug("ChannelClose method received");

        AMQConstant errorCode = AMQConstant.getConstant(method.getReplyCode());
        AMQShortString reason = method.getReplyText();
        if (_logger.isDebugEnabled())
        {
            _logger.debug("Channel close reply code: " + errorCode + ", reason: " + reason);
        }

        ChannelCloseOkBody body = session.getMethodRegistry().createChannelCloseOkBody();
        AMQFrame frame = body.generateFrame(channelId);
        session.writeFrame(frame);
        try
        {
            if (errorCode != AMQConstant.REPLY_SUCCESS)
            {
                if (_logger.isDebugEnabled())
                {
                    _logger.debug("Channel close received with errorCode " + errorCode + ", and reason " + reason);
                }

                if (errorCode == AMQConstant.NO_CONSUMERS)
                {
                    throw new AMQNoConsumersException("Error: " + reason, null, null);
                }
                else if (errorCode == AMQConstant.NO_ROUTE)
                {
                    throw new AMQNoRouteException("Error: " + reason, null, null);
                }
                else if (errorCode == AMQConstant.INVALID_ARGUMENT)
                {
                    _logger.debug("Broker responded with Invalid Argument.");

                    throw new org.apache.qpid.AMQInvalidArgumentException(String.valueOf(reason), null);
                }
                else if (errorCode == AMQConstant.INVALID_ROUTING_KEY)
                {
                    _logger.debug("Broker responded with Invalid Routing Key.");

                    throw new AMQInvalidRoutingKeyException(String.valueOf(reason), null);
                }
                else
                {

                    throw new AMQChannelClosedException(errorCode, "Error: " + reason, null);
                }

            }
        }
        finally
        {
            // fixme why is this only done when the close is expected...
            // should the above forced closes not also cause a close?
            // ----------
            // Closing the session only when it is expected allows the errors to be processed
            // Calling this here will prevent failover. So we should do this for all exceptions
            // that should never cause failover. Such as authentication errors.
            // ----
            // 2009-09-07 - ritchiem
            // calling channelClosed will only close this session and will not
            // prevent failover. If we don't close the session here then we will
            // have problems during the session close as it will attempt to
            // close the session that the broker has closed, 

            session.channelClosed(channelId, errorCode, String.valueOf(reason));
        }
    }
}
