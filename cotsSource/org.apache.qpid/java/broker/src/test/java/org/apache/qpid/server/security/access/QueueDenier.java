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
package org.apache.qpid.server.security.access;

import org.apache.commons.configuration.Configuration;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.security.access.plugins.AllowAll;
import org.apache.qpid.server.security.PrincipalHolder;

public class QueueDenier extends AllowAll
{

    public static final ACLPluginFactory FACTORY = new ACLPluginFactory()
    {
        public boolean supportsTag(String name)
        {
            return name.equals("queueDenier");
        }

        public ACLPlugin newInstance(Configuration config)
        {
            QueueDenier plugin = new QueueDenier();
            plugin.setConfiguration(config);
            return plugin;
        }
    };

    private String _queueName = "";


    @Override
    public AuthzResult authoriseDelete(PrincipalHolder session, AMQQueue queue)
    {
        if (!(queue.getName().toString().equals(_queueName)))
        {
            return AuthzResult.ALLOWED;
        }
        else
        {
            return AuthzResult.DENIED;
        }
    }

    @Override
    public void setConfiguration(Configuration config)
    {
        _queueName = config.getString("queueDenier");
    }
}
