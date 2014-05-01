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
package org.apache.qpid.server.security.auth.manager;

import org.apache.log4j.Logger;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.qpid.server.configuration.VirtualHostConfiguration;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.security.auth.manager.AuthenticationManager;
import org.apache.qpid.server.security.auth.database.PrincipalDatabase;
import org.apache.qpid.server.security.auth.sasl.JCAProvider;
import org.apache.qpid.server.security.auth.sasl.AuthenticationProviderInitialiser;
import org.apache.qpid.server.security.auth.AuthenticationResult;

import javax.security.auth.callback.CallbackHandler;
import javax.security.sasl.SaslServerFactory;
import javax.security.sasl.SaslServer;
import javax.security.sasl.SaslException;
import javax.security.sasl.Sasl;
import java.util.Map;
import java.util.HashMap;
import java.util.TreeMap;
import java.security.Security;

public class PrincipalDatabaseAuthenticationManager implements AuthenticationManager
{
    private static final Logger _logger = Logger.getLogger(PrincipalDatabaseAuthenticationManager.class);

    /** The list of mechanisms, in the order in which they are configured (i.e. preferred order) */
    private String _mechanisms;

    /** Maps from the mechanism to the callback handler to use for handling those requests */
    private Map<String, CallbackHandler> _callbackHandlerMap = new HashMap<String, CallbackHandler>();

    /**
     * Maps from the mechanism to the properties used to initialise the server. See the method Sasl.createSaslServer for
     * details of the use of these properties. This map is populated during initialisation of each provider.
     */
    private Map<String, Map<String, ?>> _serverCreationProperties = new HashMap<String, Map<String, ?>>();

    private AuthenticationManager _default = null;
    /** The name for the required SASL Server mechanisms */
    public static final String PROVIDER_NAME= "AMQSASLProvider-Server";

    public PrincipalDatabaseAuthenticationManager(String name, VirtualHostConfiguration hostConfig) throws Exception
    {
        _logger.info("Initialising " + (name == null ? "Default" : "'" + name + "'")
                     + " PrincipleDatabase authentication manager.");

        // Fixme This should be done per Vhost but allowing global hack isn't right but ...
        // required as authentication is done before Vhost selection

        Map<String, Class<? extends SaslServerFactory>> providerMap = new TreeMap<String, Class<? extends SaslServerFactory>>();


        if (name == null || hostConfig == null)
        {
            initialiseAuthenticationMechanisms(providerMap, ApplicationRegistry.getInstance().getDatabaseManager().getDatabases());
        }
        else
        {
            String databaseName = hostConfig.getAuthenticationDatabase();

            if (databaseName == null)
            {

                _default = ApplicationRegistry.getInstance().getAuthenticationManager();
                return;
            }
            else
            {
                PrincipalDatabase database = ApplicationRegistry.getInstance().getDatabaseManager().getDatabases().get(databaseName);

                if (database == null)
                {
                    throw new ConfigurationException("Requested database:" + databaseName + " was not found");
                }

                initialiseAuthenticationMechanisms(providerMap, database);
            }
        }

        if (providerMap.size() > 0)
        {
            // Ensure we are used before the defaults
            if (Security.insertProviderAt(new JCAProvider(PROVIDER_NAME, providerMap), 1) == -1)
            {
                _logger.error("Unable to load custom SASL providers. Qpid custom SASL authenticators unavailable.");
            }
            else
            {
                _logger.info("Additional SASL providers successfully registered.");
            }

        }
        else
        {
            _logger.warn("No additional SASL providers registered.");
        }

    }


    private void initialiseAuthenticationMechanisms(Map<String, Class<? extends SaslServerFactory>> providerMap, Map<String, PrincipalDatabase> databases) throws Exception
    {
        if (databases.size() > 1)
        {
            _logger.warn("More than one principle database provided currently authentication mechanism will override each other.");
        }

        for (Map.Entry<String, PrincipalDatabase> entry : databases.entrySet())
        {
            // fixme As the database now provide the mechanisms they support, they will ...
            // overwrite each other in the map. There should only be one database per vhost.
            // But currently we must have authentication before vhost definition.
            initialiseAuthenticationMechanisms(providerMap, entry.getValue());
        }
    }

    private void initialiseAuthenticationMechanisms(Map<String, Class<? extends SaslServerFactory>> providerMap, PrincipalDatabase database) throws Exception
    {
        if (database == null || database.getMechanisms().size() == 0)
        {
            _logger.warn("No Database or no mechanisms to initialise authentication");
            return;
        }

        for (Map.Entry<String, AuthenticationProviderInitialiser> mechanism : database.getMechanisms().entrySet())
        {
            initialiseAuthenticationMechanism(mechanism.getKey(), mechanism.getValue(), providerMap);
        }
    }

    private void initialiseAuthenticationMechanism(String mechanism, AuthenticationProviderInitialiser initialiser,
                                                   Map<String, Class<? extends SaslServerFactory>> providerMap)
            throws Exception
    {
        if (_mechanisms == null)
        {
            _mechanisms = mechanism;
        }
        else
        {
            // simple append should be fine since the number of mechanisms is small and this is a one time initialisation
            _mechanisms = _mechanisms + " " + mechanism;
        }
        _callbackHandlerMap.put(mechanism, initialiser.getCallbackHandler());
        _serverCreationProperties.put(mechanism, initialiser.getProperties());
        Class<? extends SaslServerFactory> factory = initialiser.getServerFactoryClassForJCARegistration();
        if (factory != null)
        {
            providerMap.put(mechanism, factory);
        }
        _logger.info("Initialised " + mechanism + " SASL provider successfully");
    }

    public String getMechanisms()
    {
        if (_default != null)
        {
            // Use the default AuthenticationManager if present
            return _default.getMechanisms();
        }
        else
        {
            return _mechanisms;
        }
    }

    public SaslServer createSaslServer(String mechanism, String localFQDN) throws SaslException
    {
        if (_default != null)
        {
            // Use the default AuthenticationManager if present
            return _default.createSaslServer(mechanism, localFQDN);
        }
        else
        {
            return Sasl.createSaslServer(mechanism, "AMQP", localFQDN, _serverCreationProperties.get(mechanism),
                                         _callbackHandlerMap.get(mechanism));
        }

    }

    public AuthenticationResult authenticate(SaslServer server, byte[] response)
    {
        // Use the default AuthenticationManager if present
        if (_default != null)
        {
            return _default.authenticate(server, response);
        }


        try
        {
            // Process response from the client
            byte[] challenge = server.evaluateResponse(response != null ? response : new byte[0]);

            if (server.isComplete())
            {
                return new AuthenticationResult(challenge, AuthenticationResult.AuthenticationStatus.SUCCESS);
            }
            else
            {
                return new AuthenticationResult(challenge, AuthenticationResult.AuthenticationStatus.CONTINUE);
            }
        }
        catch (SaslException e)
        {
            return new AuthenticationResult(AuthenticationResult.AuthenticationStatus.ERROR, e);
        }
    }

    public void close()
    {
        Security.removeProvider(PROVIDER_NAME);
    }
}
