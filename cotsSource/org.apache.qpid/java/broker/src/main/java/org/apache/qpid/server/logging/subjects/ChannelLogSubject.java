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

import org.apache.qpid.server.AMQChannel;
import org.apache.qpid.server.protocol.AMQProtocolSession;

public class ChannelLogSubject extends AbstractLogSubject
{
    /**
     * LOG FORMAT for the ChannelLogSubject,
     * Uses a MessageFormat call to insert the requried values according to
     * these indicies:
     *
     * 0 - Connection ID
     * 1 - User ID
     * 2 - IP
     * 3 - Virtualhost
     * 4 - Channel ID
     */
    public static String CHANNEL_FORMAT = ConnectionLogSubject.CONNECTION_FORMAT
                                             + "/ch:{4}";

    public ChannelLogSubject(AMQChannel channel)
    {
        AMQProtocolSession session = channel.getProtocolSession();

        /**
         * LOG FORMAT used by the AMQPConnectorActor follows
         * ChannelLogSubject.CHANNEL_FORMAT :
         * con:{0}({1}@{2}/{3})/ch:{4}
         *
         * Uses a MessageFormat call to insert the required values according to
         * these indices:
         *
         * 0 - Connection ID
         * 1 - User ID
         * 2 - IP
         * 3 - Virtualhost
         * 4 - Channel ID
         */
        setLogStringWithFormat(CHANNEL_FORMAT,
                               session.getSessionID(),
                               session.getPrincipal().getName(),
                               session.getRemoteAddress(),
                               session.getVirtualHost().getName(),
                               channel.getChannelId());
    }
}
