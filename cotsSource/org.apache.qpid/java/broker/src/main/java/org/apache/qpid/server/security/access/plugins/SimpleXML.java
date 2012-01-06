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
import org.apache.qpid.server.security.access.ACLPluginFactory;
import org.apache.qpid.server.security.access.AccessResult;
import org.apache.qpid.server.security.access.Permission;
import org.apache.qpid.server.security.access.PrincipalPermissions;
import org.apache.qpid.server.security.PrincipalHolder;
import org.apache.qpid.server.virtualhost.VirtualHost;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * This uses the default
 */
public class SimpleXML implements ACLPlugin
{
    public static final ACLPluginFactory FACTORY = new ACLPluginFactory()
    {
        public boolean supportsTag(String name)
        {
            return name.startsWith("access_control_list");
        }

        public ACLPlugin newInstance(Configuration config)
        {
            SimpleXML plugin = new SimpleXML();
            plugin.setConfiguration(config);
            return plugin;
        }
    };

    private Map<String, PrincipalPermissions> _users;
    private final AccessResult GRANTED = new AccessResult(this, AccessResult.AccessStatus.GRANTED);

    public SimpleXML()
    {
        _users = new ConcurrentHashMap<String, PrincipalPermissions>();
    }

    public void setConfiguration(Configuration config)
    {
        processConfig(config);
    }

    private void processConfig(Configuration config)
    {
        processPublish(config);

        processConsume(config);

        processCreate(config);
        
        processAccess(config);
    }

    private void processAccess(Configuration config)
    {
        Configuration accessConfig = config.subset("access_control_list.access");
        
        if(accessConfig.isEmpty())
        {
            //there is no access configuration to process
            return;
        }
        
        // Process users that have full access permission
        String[] users = accessConfig.getStringArray("users.user");

        for (String user : users)
        {
            grant(Permission.ACCESS, user);
        }
    }
    
    /**
     * Publish format takes Exchange + Routing Key Pairs
     *
     * @param config
     *            XML Configuration
     */
    private void processPublish(Configuration config)
    {
        Configuration publishConfig = config.subset("access_control_list.publish");

        // Process users that have full publish permission
        String[] users = publishConfig.getStringArray("users.user");

        for (String user : users)
        {
            grant(Permission.PUBLISH, user);
        }

        // Process exchange limited users
        int exchangeCount = 0;
        Configuration exchangeConfig = publishConfig.subset("exchanges.exchange(" + exchangeCount + ")");

        while (!exchangeConfig.isEmpty())
        {
            // Get Exchange Name
            AMQShortString exchangeName = new AMQShortString(exchangeConfig.getString("name"));

            // Get Routing Keys
            int keyCount = 0;
            Configuration routingkeyConfig = exchangeConfig.subset("routing_keys.routing_key(" + keyCount + ")");

            while (!routingkeyConfig.isEmpty())
            {
                // Get RoutingKey Value
                AMQShortString routingKeyValue = new AMQShortString(routingkeyConfig.getString("value"));

                // Apply Exchange + RoutingKey permissions to Users
                users = routingkeyConfig.getStringArray("users.user");
                for (String user : users)
                {
                    grant(Permission.PUBLISH, user, exchangeName, routingKeyValue);
                }

                // Apply permissions to Groups

                // Check for more configs
                keyCount++;
                routingkeyConfig = exchangeConfig.subset("routing_keys.routing_key(" + keyCount + ")");
            }

            // Apply Exchange wide permissions to Users
            users = exchangeConfig.getStringArray("exchange(" + exchangeCount + ").users.user");

            for (String user : users)
            {
                grant(Permission.PUBLISH, user, exchangeName);
            }

            // Apply permissions to Groups
            exchangeCount++;
            exchangeConfig = publishConfig.subset("exchanges.exchange(" + exchangeCount + ")");
        }
    }

    private void grant(Permission permission, String user, Object... parameters)
    {
        PrincipalPermissions permissions = _users.get(user);

        if (permissions == null)
        {
            permissions = new PrincipalPermissions(user);
        }

        _users.put(user, permissions);
        permissions.grant(permission, parameters);
    }

    private void processConsume(Configuration config)
    {
        boolean temporary = false;
        Configuration tempConfig = null;
        Configuration consumeConfig = config.subset("access_control_list.consume");

        tempConfig = consumeConfig.subset("queues.temporary(0)");
        if (tempConfig != null)
        {
            temporary = true;
        }

        //Permission all users who have rights to temp queues and topics
        if (tempConfig != null && !tempConfig.isEmpty())
        {
            String[] tempUsers = tempConfig.getStringArray("users.user");
            for (String user : tempUsers)
            {
                grant(Permission.CONSUME, user, temporary);
            }
        }

        // Process queue limited users
        int queueCount = 0;
        Configuration queueConfig = consumeConfig.subset("queues.queue(" + queueCount + ")");

        while (!queueConfig.isEmpty())
        {
            // Get queue Name
            AMQShortString queueName = new AMQShortString(queueConfig.getString("name"));
            // if there is no name then there may be a temporary element

            boolean ownQueues = queueConfig.containsKey("own_queues");

            // Process permissions for this queue
            String[] users = queueConfig.getStringArray("users.user");
            for (String user : users)
            {
                grant(Permission.CONSUME, user, queueName, ownQueues);
            }

            // See if we have another config
            queueCount++;
            queueConfig = consumeConfig.subset("queues.queue(" + queueCount + ")");
        }

        // Process users that have full consume permission
        String[] users = consumeConfig.getStringArray("users.user");

        for (String user : users)
        {
            //NOTE: this call does not appear to do anything inside the grant section for consume
            grant(Permission.CONSUME, user);
        }
    }

    private void processCreate(Configuration config)
    {
        boolean temporary = false;
        Configuration tempConfig = null;

        Configuration createConfig = config.subset("access_control_list.create");

        tempConfig = createConfig.subset("queues.temporary(0)");
        if (tempConfig != null)
        {
            temporary = true;
        }

        //Permission all users who have rights to temp queues and topics
        if (tempConfig != null && !tempConfig.isEmpty())
        {
            String[] tempUsers = tempConfig.getStringArray("users.user");
            for (String user : tempUsers)
            {
                grant(Permission.CREATEQUEUE, user, temporary);
            }
        }
        // Process create permissions for queue creation
        int queueCount = 0;
        Configuration queueConfig = createConfig.subset("queues.queue(" + queueCount + ")");

        while (!queueConfig.isEmpty())
        {
            // Get queue Name
            AMQShortString queueName = new AMQShortString(queueConfig.getString("name"));

            int exchangeCount = 0;
            Configuration exchangeConfig = queueConfig.subset("exchanges.exchange(" + exchangeCount + ")");

            while (!exchangeConfig.isEmpty())
            {

                AMQShortString exchange = new AMQShortString(exchangeConfig.getString("name"));
                AMQShortString routingKey = new AMQShortString(exchangeConfig.getString("routing_key"));
               
                // Process permissions for this queue
                String[] users = exchangeConfig.getStringArray("users.user");
                for (String user : users)
                {
                    //This is broken as the user name is not stored
                    grant(Permission.CREATEEXCHANGE, user, exchange);

                    //This call could be cleaned up as temporary is now being set earlier (above) 
                    grant(Permission.CREATEQUEUE, user, temporary, (queueName.equals("") ? null : queueName), (exchange
                            .equals("") ? null : exchange), (routingKey.equals("") ? null : routingKey));
                }

                // See if we have another config
                exchangeCount++;
                exchangeConfig = queueConfig.subset("exchanges.exchange(" + exchangeCount + ")");
            }

            // Process users that are not bound to an exchange
            String[] users = queueConfig.getStringArray("users.user");

            for (String user : users)
            {
                grant(Permission.CREATEQUEUE, user, temporary, queueName);
            }

            // See if we have another config
            queueCount++;
            queueConfig = createConfig.subset("queues.queue(" + queueCount + ")");
        }

        // Process create permissions for exchange creation
        int exchangeCount = 0;
        Configuration exchangeConfig = createConfig.subset("exchanges.exchange(" + exchangeCount + ")");

        while (!exchangeConfig.isEmpty())
        {
            AMQShortString exchange = new AMQShortString(exchangeConfig.getString("name"));
            AMQShortString clazz = new AMQShortString(exchangeConfig.getString("class"));

            // Process permissions for this queue
            String[] users = exchangeConfig.getStringArray("users.user");
            for (String user : users)
            {
                //And this is broken too 
                grant(Permission.CREATEEXCHANGE, user, exchange, clazz);
            }

            // See if we have another config
            exchangeCount++;
            exchangeConfig = queueConfig.subset("exchanges.exchange(" + exchangeCount + ")");
        }

        // Process users that have full create permission
        String[] users = createConfig.getStringArray("users.user");

        for (String user : users)
        {
            grant(Permission.CREATEEXCHANGE, user);
            grant(Permission.CREATEQUEUE, user);
        }

    }

    public String getPluginName()
    {
        return "Simple";
    }

    public AuthzResult authoriseBind(PrincipalHolder session, Exchange exch, AMQQueue queue, AMQShortString routingKey)
    {
        PrincipalPermissions principalPermissions = _users.get(session.getPrincipal().getName());
        if (principalPermissions == null)
        {
            return AuthzResult.DENIED;
        }
        else
        {
            return principalPermissions.authorise(Permission.BIND, null, exch, queue, routingKey);
        }
    }

    public AuthzResult authoriseConnect(PrincipalHolder session, VirtualHost virtualHost)
    {
        PrincipalPermissions principalPermissions = _users.get(session.getPrincipal().getName());
        if (principalPermissions == null)
        {
            return AuthzResult.DENIED;
        }
        else
        {
            return principalPermissions.authorise(Permission.ACCESS);
        }
    }

    public AuthzResult authoriseConsume(PrincipalHolder session, boolean noAck, AMQQueue queue)
    {
        PrincipalPermissions principalPermissions = _users.get(session.getPrincipal().getName());
        if (principalPermissions == null)
        {
            return AuthzResult.DENIED;
        }
        else
        {
            return principalPermissions.authorise(Permission.CONSUME, queue);
        }
    }

    public AuthzResult authoriseConsume(PrincipalHolder session, boolean exclusive, boolean noAck, boolean noLocal,
            boolean nowait, AMQQueue queue)
    {
        return authoriseConsume(session, noAck, queue);
    }

    public AuthzResult authoriseCreateExchange(PrincipalHolder session, boolean autoDelete, boolean durable,
            AMQShortString exchangeName, boolean internal, boolean nowait, boolean passive, AMQShortString exchangeType)
    {
        PrincipalPermissions principalPermissions = _users.get(session.getPrincipal().getName());
        if (principalPermissions == null)
        {
            return AuthzResult.DENIED;
        }
        else
        {
            return principalPermissions.authorise(Permission.CREATEEXCHANGE, exchangeName);
        }
    }

    public AuthzResult authoriseCreateQueue(PrincipalHolder session, boolean autoDelete, boolean durable, boolean exclusive,
            boolean nowait, boolean passive, AMQShortString queue)
    {
        PrincipalPermissions principalPermissions = _users.get(session.getPrincipal().getName());
        if (principalPermissions == null)
        {
            return AuthzResult.DENIED;
        }
        else
        {
            return principalPermissions.authorise(Permission.CREATEQUEUE, autoDelete, queue);
        }
    }

    public AuthzResult authoriseDelete(PrincipalHolder session, AMQQueue queue)
    {
        PrincipalPermissions principalPermissions = _users.get(session.getPrincipal().getName());
        if (principalPermissions == null)
        {
            return AuthzResult.DENIED;
        }
        else
        {
            return principalPermissions.authorise(Permission.DELETE);
        }
    }

    public AuthzResult authoriseDelete(PrincipalHolder session, Exchange exchange)
    {
        PrincipalPermissions principalPermissions = _users.get(session.getPrincipal().getName());
        if (principalPermissions == null)
        {
            return AuthzResult.DENIED;
        }
        else
        {
            return principalPermissions.authorise(Permission.DELETE);
        }
    }

    public AuthzResult authorisePublish(PrincipalHolder session, boolean immediate, boolean mandatory,
            AMQShortString routingKey, Exchange e)
    {
        PrincipalPermissions principalPermissions = _users.get(session.getPrincipal().getName());
        if (principalPermissions == null)
        {
            return AuthzResult.DENIED;
        }
        else
        {
            return principalPermissions.authorise(Permission.PUBLISH, e, routingKey);
        }
    }

    public AuthzResult authorisePurge(PrincipalHolder session, AMQQueue queue)
    {
        PrincipalPermissions principalPermissions = _users.get(session.getPrincipal().getName());
        if (principalPermissions == null)
        {
            return AuthzResult.DENIED;
        }
        else
        {
            return principalPermissions.authorise(Permission.PURGE);
        }
    }

    public AuthzResult authoriseUnbind(PrincipalHolder session, Exchange exch, AMQShortString routingKey, AMQQueue queue)
    {
        PrincipalPermissions principalPermissions = _users.get(session.getPrincipal().getName());
        if (principalPermissions == null)
        {
            return AuthzResult.DENIED;
        }
        else
        {
            return principalPermissions.authorise(Permission.UNBIND);
        }
    }

}
