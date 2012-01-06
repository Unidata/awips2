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

import javax.security.sasl.SaslException;
import javax.security.sasl.SaslServer;

import org.apache.log4j.Logger;
import org.apache.qpid.AMQException;
import org.apache.qpid.framing.ConnectionCloseBody;
import org.apache.qpid.framing.ConnectionSecureBody;
import org.apache.qpid.framing.ConnectionSecureOkBody;
import org.apache.qpid.framing.ConnectionTuneBody;
import org.apache.qpid.framing.MethodRegistry;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.qpid.server.protocol.AMQProtocolSession;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.security.auth.AuthenticationResult;
import org.apache.qpid.server.security.auth.manager.AuthenticationManager;
import org.apache.qpid.server.security.auth.sasl.UsernamePrincipal;
import org.apache.qpid.server.state.AMQState;
import org.apache.qpid.server.state.AMQStateManager;
import org.apache.qpid.server.state.StateAwareMethodListener;

public class ConnectionSecureOkMethodHandler implements StateAwareMethodListener<ConnectionSecureOkBody>
{
    private static final Logger _logger = Logger.getLogger(ConnectionSecureOkMethodHandler.class);

    private static ConnectionSecureOkMethodHandler _instance = new ConnectionSecureOkMethodHandler();

    public static ConnectionSecureOkMethodHandler getInstance()
    {
        return _instance;
    }

    private ConnectionSecureOkMethodHandler()
    {
    }

    public void methodReceived(AMQStateManager stateManager, ConnectionSecureOkBody body, int channelId) throws AMQException
    {
        AMQProtocolSession session = stateManager.getProtocolSession();

        AuthenticationManager authMgr = ApplicationRegistry.getInstance().getAuthenticationManager();

        SaslServer ss = session.getSaslServer();
        if (ss == null)
        {
            throw new AMQException("No SASL context set up in session");
        }
        MethodRegistry methodRegistry = session.getMethodRegistry();
        AuthenticationResult authResult = authMgr.authenticate(ss, body.getResponse());
        switch (authResult.status)
        {
            case ERROR:
                Exception cause = authResult.getCause();

                _logger.info("Authentication failed:" + (cause == null ? "" : cause.getMessage()));

                // This should be abstracted
                stateManager.changeState(AMQState.CONNECTION_CLOSING);

                ConnectionCloseBody connectionCloseBody =
                        methodRegistry.createConnectionCloseBody(AMQConstant.NOT_ALLOWED.getCode(),
                                                                 AMQConstant.NOT_ALLOWED.getName(),
                                                                 body.getClazz(),
                                                                 body.getMethod());

                session.writeFrame(connectionCloseBody.generateFrame(0));
                disposeSaslServer(session);
                break;
            case SUCCESS:
                _logger.info("Connected as: " + ss.getAuthorizationID());
                stateManager.changeState(AMQState.CONNECTION_NOT_TUNED);

                ConnectionTuneBody tuneBody =
                        methodRegistry.createConnectionTuneBody(0xFFFF,
                                                                ConnectionStartOkMethodHandler.getConfiguredFrameSize(),
                                                                ApplicationRegistry.getInstance().getConfiguration().getHeartBeatDelay());
                session.writeFrame(tuneBody.generateFrame(0));
                session.setAuthorizedID(new UsernamePrincipal(ss.getAuthorizationID()));
                disposeSaslServer(session);
                break;
            case CONTINUE:
                stateManager.changeState(AMQState.CONNECTION_NOT_AUTH);

                ConnectionSecureBody secureBody = methodRegistry.createConnectionSecureBody(authResult.challenge);
                session.writeFrame(secureBody.generateFrame(0));
        }
    }

    private void disposeSaslServer(AMQProtocolSession ps)
    {
        SaslServer ss = ps.getSaslServer();
        if (ss != null)
        {
            ps.setSaslServer(null);
            try
            {
                ss.dispose();
            }
            catch (SaslException e)
            {
                _logger.error("Error disposing of Sasl server: " + e);
            }
        }
    }
}
