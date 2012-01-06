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
package org.apache.qpid.server.configuration;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.qpid.test.utils.QpidTestCase;

/**
 * This system test ensures that when loading our default system-test
 * configuration file the configuration is correctly loaded.
 *
 * All configuration values should be set in the systest config file so that
 * the ability to load them can be validated.
 */
public class ServerConfigurationFileTest extends QpidTestCase
{
    ServerConfiguration _serverConfig;

    public void setUp() throws ConfigurationException
    {
        if (!_configFile.exists())
        {
            fail("Unable to test without config file:" + _configFile);
        }

        saveTestConfiguration();
        _serverConfig = new ServerConfiguration(_configFile);
    }

    /**
     * This helper method ensures that when we attempt to read a value that is
     * set in the configuration file we do actualy read a value and not
     * simply get a defaulted value from the ServerConfiguration.get*() methods.
     *
     * @param property the propert to test
     */
    private void validatePropertyDefinedInFile(String property)
    {
        //Verify that we are not just picking up the the default value from the getBoolean
        assertNotNull("The value set in the configuration file is not being read for property:" + property,
                      _serverConfig.getConfig().getProperty(property));
    }

    public void testProtectIOEnabled() throws ConfigurationException
    {
        validatePropertyDefinedInFile(ServerConfiguration.CONNECTOR_PROTECTIO_ENABLED);
    }

    public void testProtectIOReadBufferLimitSize() throws ConfigurationException
    {
        validatePropertyDefinedInFile(ServerConfiguration.CONNECTOR_PROTECTIO_READ_BUFFER_LIMIT_SIZE);
    }

    public void testProtectIOWriteBufferLimitSize() throws ConfigurationException
    {
        validatePropertyDefinedInFile(ServerConfiguration.CONNECTOR_PROTECTIO_WRITE_BUFFER_LIMIT_SIZE);
    }

    public void testStatusUpdates() throws ConfigurationException
    {
        validatePropertyDefinedInFile(ServerConfiguration.STATUS_UPDATES);
    }

    public void testLocale() throws ConfigurationException
    {
        validatePropertyDefinedInFile(ServerConfiguration.ADVANCED_LOCALE);
    }

}
