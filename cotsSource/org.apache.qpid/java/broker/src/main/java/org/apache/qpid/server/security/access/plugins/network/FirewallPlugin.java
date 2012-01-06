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
package org.apache.qpid.server.security.access.plugins.network;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Pattern;

import org.apache.commons.configuration.CompositeConfiguration;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.qpid.protocol.ProtocolEngine;
import org.apache.qpid.server.security.access.ACLPlugin;
import org.apache.qpid.server.security.access.ACLPluginFactory;
import org.apache.qpid.server.security.access.plugins.AbstractACLPlugin;
import org.apache.qpid.server.security.PrincipalHolder;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.util.NetMatcher;

public class FirewallPlugin extends AbstractACLPlugin
{

    public class FirewallPluginException extends Exception {}

    public static final ACLPluginFactory FACTORY = new ACLPluginFactory()
    {
        public boolean supportsTag(String name)
        {
            return name.startsWith("firewall");
        }

        public ACLPlugin newInstance(Configuration config) throws ConfigurationException
        {
            FirewallPlugin plugin = new FirewallPlugin();
            plugin.setConfiguration(config.subset("firewall"));
            return plugin;
        }
    };

    public class FirewallRule
    {

        private static final long DNS_TIMEOUT = 30000;
        private AuthzResult _access;
        private NetMatcher _network;
        private Pattern[] _hostnamePatterns;

        public FirewallRule(String access, List networks, List hostnames)
        {
            _access = (access.equals("allow")) ? AuthzResult.ALLOWED : AuthzResult.DENIED;

            if (networks != null && networks.size() > 0)
            {
                String[] networkStrings = objListToStringArray(networks);
                _network = new NetMatcher(networkStrings);
            }

            if (hostnames != null && hostnames.size() > 0)
            {
                int i = 0;
                _hostnamePatterns = new Pattern[hostnames.size()];
                for (String hostname : objListToStringArray(hostnames))
                {
                    _hostnamePatterns[i++] = Pattern.compile(hostname);
                }
            }

        }

        private String[] objListToStringArray(List objList)
        {
            String[] networkStrings = new String[objList.size()];
            int i = 0;
            for (Object network : objList)
            {
                networkStrings[i++] = (String) network;
            }
            return networkStrings;
        }

        public boolean match(InetAddress remote) throws FirewallPluginException
        {
            if (_hostnamePatterns != null)
            {
                String hostname = getHostname(remote);
                if (hostname == null)
                {
                    throw new FirewallPluginException();
                }
                for (Pattern pattern : _hostnamePatterns)
                {
                    if (pattern.matcher(hostname).matches())
                    {
                        return true;
                    }
                }
                return false;
            }
            else
            {
                return _network.matchInetNetwork(remote);
            }
        }

        /**
         * @param remote the InetAddress to look up
         * @return the hostname, null if not found or takes longer than 30s to find
         */
        private String getHostname(final InetAddress remote)
        {
            final String[] hostname = new String[]{null};
            final AtomicBoolean done = new AtomicBoolean(false);
            // Spawn thread
            Thread thread = new Thread(new Runnable()
            {
               public void run()
               {
                   hostname[0] = remote.getCanonicalHostName();
                   done.getAndSet(true);
                   synchronized (done)
                   {
                       done.notifyAll();
                   }
               }
            });

            thread.run();
            long endTime = System.currentTimeMillis() + DNS_TIMEOUT;

            while (System.currentTimeMillis() < endTime && !done.get())
            {
                try
                {
                    synchronized (done)
                    {
                        done.wait(endTime - System.currentTimeMillis());
                    }
                }
                catch (InterruptedException e)
                {
                    // Check the time and if necessary sleep for a bit longer
                }
            }
            return hostname[0];
        }

        public AuthzResult getAccess()
        {
            return _access;
        }

    }

    private AuthzResult _default = AuthzResult.ABSTAIN;
    private FirewallRule[] _rules;

    @Override
    public AuthzResult authoriseConnect(PrincipalHolder principalHolder, VirtualHost virtualHost)
    {
        if(!(principalHolder instanceof ProtocolEngine))
        {
            return AuthzResult.ABSTAIN; // We only deal with tcp sessions
        }

        ProtocolEngine session = (ProtocolEngine) principalHolder;

        SocketAddress sockAddr = session.getRemoteAddress();
        if (!(sockAddr instanceof InetSocketAddress))
        {
            return AuthzResult.ABSTAIN; // We only deal with tcp sessions
        }

        InetAddress addr = ((InetSocketAddress) sockAddr).getAddress();

        if (addr == null)
        {
            return AuthzResult.ABSTAIN; // Not an Inet socket on the other end
        }

        boolean match = false;
        for (FirewallRule rule : _rules)
        {
            try
            {
                match = rule.match(addr);
            }
            catch (FirewallPluginException e)
            {
                return AuthzResult.DENIED;
            }
            if (match)
            {
                return rule.getAccess();
            }
        }
        return _default;

    }

    public void setConfiguration(Configuration config) throws ConfigurationException
    {
        // Get default action
        String defaultAction = config.getString("[@default-action]");
        if (defaultAction == null)
        {
            _default = AuthzResult.ABSTAIN;
        }
        else if (defaultAction.toLowerCase().equals("allow"))
        {
            _default = AuthzResult.ALLOWED;
        }
        else
        {
            _default = AuthzResult.DENIED;
        }
        CompositeConfiguration finalConfig = new CompositeConfiguration(config);

        List subFiles = config.getList("xml[@fileName]");
        for (Object subFile : subFiles)
        {
            finalConfig.addConfiguration(new XMLConfiguration((String) subFile));
        }

        // all rules must have an access attribute
        int numRules = finalConfig.getList("rule[@access]").size();
        _rules = new FirewallRule[numRules];
        for (int i = 0; i < numRules; i++)
        {
            FirewallRule rule = new FirewallRule(finalConfig.getString("rule(" + i + ")[@access]"), finalConfig.getList("rule("
                    + i + ")[@network]"), finalConfig.getList("rule(" + i + ")[@hostname]"));
            _rules[i] = rule;
        }
    }
}
