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

import javax.security.sasl.SaslClient;
import javax.security.sasl.SaslException;

import org.apache.qpid.AMQException;
import org.apache.qpid.client.protocol.AMQProtocolSession;
import org.apache.qpid.client.state.StateAwareMethodListener;
import org.apache.qpid.framing.ConnectionSecureBody;
import org.apache.qpid.framing.ConnectionSecureOkBody;

public class ConnectionSecureMethodHandler implements StateAwareMethodListener<ConnectionSecureBody>
{
    private static final ConnectionSecureMethodHandler _instance = new ConnectionSecureMethodHandler();

    public static ConnectionSecureMethodHandler getInstance()
    {
        return _instance;
    }

    public void methodReceived(AMQProtocolSession session, ConnectionSecureBody body, int channelId)
                throws AMQException
    {
        SaslClient client = session.getSaslClient();
        if (client == null)
        {
            throw new AMQException(null, "No SASL client set up - cannot proceed with authentication", null);
        }



        try
        {
            // Evaluate server challenge
            byte[] response = client.evaluateChallenge(body.getChallenge());

            ConnectionSecureOkBody secureOkBody = session.getMethodRegistry().createConnectionSecureOkBody(response);

            session.writeFrame(secureOkBody.generateFrame(channelId));
        }
        catch (SaslException e)
        {
            throw new AMQException(null, "Error processing SASL challenge: " + e, e);
        }


    }


}
