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
package org.apache.qpid.server.security.access.plugins;

import org.apache.commons.configuration.Configuration;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.server.exchange.Exchange;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.security.access.ACLPlugin;
import org.apache.qpid.server.security.PrincipalHolder;
import org.apache.qpid.server.virtualhost.VirtualHost;

public abstract class BasicACLPlugin implements ACLPlugin
{

    // Returns true or false if the plugin should authorise or deny the request
    protected abstract AuthzResult getResult();

    public AuthzResult authoriseBind(PrincipalHolder session, Exchange exch,
            AMQQueue queue, AMQShortString routingKey)
    {
        return getResult();
    }

    public AuthzResult authoriseConnect(PrincipalHolder session,
            VirtualHost virtualHost)
    {
        return getResult();
    }

    public AuthzResult authoriseConsume(PrincipalHolder session, boolean noAck,
            AMQQueue queue)
    {
        return getResult();
    }

    public AuthzResult authoriseConsume(PrincipalHolder session,
            boolean exclusive, boolean noAck, boolean noLocal, boolean nowait,
            AMQQueue queue)
    {
        return getResult();
    }

    public AuthzResult authoriseCreateExchange(PrincipalHolder session,
            boolean autoDelete, boolean durable, AMQShortString exchangeName,
            boolean internal, boolean nowait, boolean passive,
            AMQShortString exchangeType)
    {
        return getResult();
    }

    public AuthzResult authoriseCreateQueue(PrincipalHolder session,
            boolean autoDelete, boolean durable, boolean exclusive,
            boolean nowait, boolean passive, AMQShortString queue)
    {
        return getResult();
    }

    public AuthzResult authoriseDelete(PrincipalHolder session, AMQQueue queue)
    {
        return getResult();
    }

    public AuthzResult authoriseDelete(PrincipalHolder session, Exchange exchange)
    {
        return getResult();
    }

    public AuthzResult authorisePublish(PrincipalHolder session,
            boolean immediate, boolean mandatory, AMQShortString routingKey,
            Exchange e)
    {
        return getResult();
    }

    public AuthzResult authorisePurge(PrincipalHolder session, AMQQueue queue)
    {
        return getResult();
    }

    public AuthzResult authoriseUnbind(PrincipalHolder session, Exchange exch,
            AMQShortString routingKey, AMQQueue queue)
    {
        return getResult();
    }

    public void setConfiguration(Configuration config)
    {
        // no-op
    }

}
