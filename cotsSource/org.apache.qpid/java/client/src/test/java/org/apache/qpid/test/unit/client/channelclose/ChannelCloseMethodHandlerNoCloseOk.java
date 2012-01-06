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
package org.apache.qpid.test.unit.client.channelclose;

import org.apache.qpid.AMQChannelClosedException;
import org.apache.qpid.AMQException;
import org.apache.qpid.AMQInvalidArgumentException;
import org.apache.qpid.AMQInvalidRoutingKeyException;
import org.apache.qpid.client.AMQNoConsumersException;
import org.apache.qpid.client.AMQNoRouteException;
import org.apache.qpid.client.protocol.AMQProtocolSession;
import org.apache.qpid.client.state.StateAwareMethodListener;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.ChannelCloseBody;
import org.apache.qpid.protocol.AMQConstant;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ChannelCloseMethodHandlerNoCloseOk implements StateAwareMethodListener<ChannelCloseBody>
{
    private static final Logger _logger = LoggerFactory.getLogger(ChannelCloseMethodHandlerNoCloseOk.class);

    private static ChannelCloseMethodHandlerNoCloseOk _handler = new ChannelCloseMethodHandlerNoCloseOk();

    public static ChannelCloseMethodHandlerNoCloseOk getInstance()
    {
        return _handler;
    }

    public void methodReceived(AMQProtocolSession session,  ChannelCloseBody method, int channelId)
        throws AMQException
    {
        _logger.debug("ChannelClose method received");

        AMQConstant errorCode = AMQConstant.getConstant(method.getReplyCode());
        AMQShortString reason = method.getReplyText();
        if (_logger.isDebugEnabled())
        {
            _logger.debug("Channel close reply code: " + errorCode + ", reason: " + reason);
        }

        // For this test Method Handler .. don't send Close-OK
        // // TODO: Be aware of possible changes to parameter order as versions change.
        // AMQFrame frame = ChannelCloseOkBody.createAMQFrame(evt.getChannelId(), method.getMajor(), method.getMinor());
        // protocolSession.writeFrame(frame);
        if (errorCode != AMQConstant.REPLY_SUCCESS)
        {
            _logger.error("Channel close received with errorCode " + errorCode + ", and reason " + reason);
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

                throw new AMQInvalidArgumentException(String.valueOf(reason), null);
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

        session.channelClosed(channelId, errorCode, String.valueOf(reason));
    }
}
