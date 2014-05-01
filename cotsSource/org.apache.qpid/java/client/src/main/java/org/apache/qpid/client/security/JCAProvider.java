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
package org.apache.qpid.client.security;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.security.sasl.SaslClientFactory;

import java.security.Provider;
import java.security.Security;
import java.util.Map;

/**
 * JCAProvider is a security provider for SASL client factories that is configured from a map of SASL mechanism names
 * to client factories implementation class names. It is intended that the map of client factories can be read from a
 * configuration file or other application configuration mechanism.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Register SASL mechanism implementations.
 * </table>
 */
public class JCAProvider extends Provider
{
    private static final Logger log = LoggerFactory.getLogger(JCAProvider.class);

    /**
     * Creates the security provider with a map from SASL mechanisms to implementing factories.
     *
     * @param providerMap The map from SASL mechanims to implementing factory classes.
     */
    public JCAProvider(Map<String, Class<? extends SaslClientFactory>> providerMap)
    {
        super("AMQSASLProvider-Client", 1.0, "A JCA provider that registers all "
            + "AMQ SASL providers that want to be registered");
        register(providerMap);
//        Security.addProvider(this);
    }

    /**
     * Registers client factory classes for a map of mechanism names to client factory classes.
     *
     * @param providerMap The map from SASL mechanims to implementing factory classes.
     */
    private void register(Map<String, Class<? extends SaslClientFactory>> providerMap)
    {
        for (Map.Entry<String, Class<? extends SaslClientFactory>> me : providerMap.entrySet())
        {
            put( "SaslClientFactory."+me.getKey(), me.getValue().getName());
            log.debug("Registered SASL Client factory for " + me.getKey() + " as " + me.getValue().getName());
        }
    }
}
