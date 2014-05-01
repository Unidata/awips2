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

import org.apache.qpid.AMQConnectionClosedException;
import org.apache.qpid.AMQException;
import org.apache.qpid.client.AMQAuthenticationException;
import org.apache.qpid.client.protocol.AMQProtocolSession;
import org.apache.qpid.client.state.AMQState;
import org.apache.qpid.client.state.StateAwareMethodListener;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.ConnectionCloseBody;
import org.apache.qpid.framing.ConnectionCloseOkBody;
import org.apache.qpid.protocol.AMQConstant;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ConnectionCloseMethodHandler implements StateAwareMethodListener<ConnectionCloseBody>
{
    private static final Logger _logger = LoggerFactory.getLogger(ConnectionCloseMethodHandler.class);

    private static ConnectionCloseMethodHandler _handler = new ConnectionCloseMethodHandler();

    public static ConnectionCloseMethodHandler getInstance()
    {
        return _handler;
    }

    private ConnectionCloseMethodHandler()
    {
    }

    public void methodReceived(AMQProtocolSession session, ConnectionCloseBody method, int channelId)
            throws AMQException
    {
        _logger.info("ConnectionClose frame received");

        // does it matter
        // stateManager.changeState(AMQState.CONNECTION_CLOSING);

        AMQConstant errorCode = AMQConstant.getConstant(method.getReplyCode());
        AMQShortString reason = method.getReplyText();

        AMQException error = null;

        try
        {

            ConnectionCloseOkBody closeOkBody = session.getMethodRegistry().createConnectionCloseOkBody();
            // TODO: check whether channel id of zero is appropriate
            // Be aware of possible changes to parameter order as versions change.
            session.writeFrame(closeOkBody.generateFrame(0));

            if (errorCode != AMQConstant.REPLY_SUCCESS)
            {
                if (errorCode == AMQConstant.NOT_ALLOWED || (errorCode == AMQConstant.ACCESS_REFUSED))
                {
                    _logger.info("Error :" + errorCode + ":" + Thread.currentThread().getName());

                    error = new AMQAuthenticationException(errorCode, reason == null ? null : reason.toString(), null);
                }
                else
                {
                    _logger.info("Connection close received with error code " + errorCode);

                    error = new AMQConnectionClosedException(errorCode, "Error: " + reason, null);
                }
            }
        }
        finally
        {

            if (error != null)
            {
                session.notifyError(error);
            }            

            // Close the protocol Session, including any open TCP connections 
            session.closeProtocolSession();

            // Closing the session should not introduce a race condition as this thread will continue to propgate any
            // exception in to the exceptionCaught method of the SessionHandler.
            // Any sessionClosed event should occur after this.
        }
    }

}
