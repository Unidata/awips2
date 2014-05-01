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
package org.apache.qpid.server.logging.subjects;

import org.apache.qpid.server.protocol.AMQProtocolSession;

import java.text.MessageFormat;

/** The Connection LogSubject */
public class ConnectionLogSubject extends AbstractLogSubject
{

    /**
     * 0 - Connection ID
     * 1 - Remote Address
     */
    public static String SOCKET_FORMAT = "con:{0}({1})";

    /**
     * LOG FORMAT for the ConnectionLogSubject,
     * Uses a MessageFormat call to insert the requried values according to
     * these indicies:
     *
     * 0 - Connection ID
     * 1 - User ID
     * 2 - IP
     */
    public static final String USER_FORMAT = "con:{0}({1}@{2})";

    /**
     * LOG FORMAT for the ConnectionLogSubject,
     * Uses a MessageFormat call to insert the required values according to
     * these indices:
     *
     * 0 - Connection ID
     * 1 - User ID
     * 2 - IP
     * 3 - Virtualhost
     */
    public static final String CONNECTION_FORMAT = "con:{0}({1}@{2}/{3})";


    public ConnectionLogSubject(AMQProtocolSession session)
    {
        _session = session;        
    }

    // The Session this Actor is representing
    private AMQProtocolSession _session;

    // Used to stop re-creating the _logString when we reach our final format
    private boolean _upToDate = false;

    /**
     * Update the LogString as the Connection process proceeds.
     *
     * When the Session has an authorized ID add that to the string.
     *
     * When the Session then gains a Vhost add that to the string, at this point
     * we can set upToDate = true as the _logString will not need to be updated
     * from this point onwards.
     */
    private void updateLogString()
    {
        if (!_upToDate)
        {
            if (_session.getPrincipal() != null)
            {
                if (_session.getVirtualHost() != null)
                {
                    /**
                     * LOG FORMAT used by the AMQPConnectorActor follows
                     * ConnectionLogSubject.CONNECTION_FORMAT :
                     * con:{0}({1}@{2}/{3})
                     *
                     * Uses a MessageFormat call to insert the required values according to
                     * these indices:
                     *
                     * 0 - Connection ID
                     * 1 - User ID
                     * 2 - IP
                     * 3 - Virtualhost
                     */
                    _logString = "[" + MessageFormat.format(ConnectionLogSubject.CONNECTION_FORMAT,
                                                            _session.getSessionID(),
                                                            _session.getPrincipal().getName(),
                                                            _session.getRemoteAddress(),
                                                            _session.getVirtualHost().getName())
                                 + "] ";

                    _upToDate = true;
                }
                else
                {
                    _logString = "[" + MessageFormat.format(USER_FORMAT,
                                                            _session.getSessionID(),
                                                            _session.getPrincipal().getName(),
                                                            _session.getRemoteAddress())
                                 + "] ";

                }
            }
            else
            {
                _logString = "[" + MessageFormat.format(SOCKET_FORMAT,
                                                        _session.getSessionID(),
                                                        _session.getRemoteAddress())
                             + "] ";
            }
        }
    }

    @Override
    public String toString()
    {
        updateLogString();
        return super.toString();
    }
}
