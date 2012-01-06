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
import org.apache.qpid.AMQConnectionException;
import org.apache.qpid.framing.AMQMethodBody;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.qpid.server.protocol.AMQProtocolSession;
import org.apache.qpid.server.security.access.ACLManager;
import org.apache.qpid.server.security.access.ACLPlugin;
import org.apache.qpid.server.security.access.ACLPluginFactory;
import org.apache.qpid.server.security.access.AccessResult;
import org.apache.qpid.server.security.access.Permission;

public class DenyAll extends BasicACLPlugin
{
    public static final ACLPluginFactory FACTORY = new ACLPluginFactory()
    {
        public boolean supportsTag(String name)
        {
            return false;
        }

        public ACLPlugin newInstance(Configuration config)
        {
            return new DenyAll();
        }
    };
    
    public AccessResult authorise(AMQProtocolSession session,
            Permission permission, AMQMethodBody body, Object... parameters)
            throws AMQConnectionException
    {

        if (ACLManager.getLogger().isInfoEnabled())
        {
            ACLManager.getLogger().info(
                    "Denying user:" + session.getPrincipal());
        }
        throw body.getConnectionException(AMQConstant.ACCESS_REFUSED,
                "DenyAll Plugin");
    }

    public String getPluginName()
    {
        return getClass().getSimpleName();
    }

    @Override 
    protected AuthzResult getResult()
    {
        // Always deny
        return AuthzResult.DENIED;
    }

}
