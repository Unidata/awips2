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

import org.apache.qpid.util.FileUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

/**
 * CallbackHandlerRegistry is a registry for call back handlers for user authentication and interaction during user
 * authentication. It is capable of reading its configuration from a properties file containing call back handler
 * implementing class names for different SASL mechanism names. Instantiating this registry also has the effect of
 * configuring and registering the SASL client factory implementations using {@link DynamicSaslRegistrar}.
 *
 * <p/>The callback configuration should be specified in a properties file, refered to by the System property
 * "amp.callbackhandler.properties". The format of the properties file is:
 *
 * <p/><pre>
 * CallbackHanlder.mechanism=fully.qualified.class.name
 * </pre>
 *
 * <p/>Where mechanism is an IANA-registered mechanism name and the fully qualified class name refers to a
 * class that implements org.apache.qpid.client.security.AMQCallbackHanlder and provides a call back handler for the
 * specified mechanism.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Parse callback properties.
 * <tr><td> Provide mapping from SASL mechanisms to callback implementations.
 * </table>
 */
public class CallbackHandlerRegistry
{
    private static final Logger _logger = LoggerFactory.getLogger(CallbackHandlerRegistry.class);

    /** The name of the system property that holds the name of the callback handler properties file. */
    private static final String FILE_PROPERTY = "amq.callbackhandler.properties";

    /** The default name of the callback handler properties resource. */
    public static final String DEFAULT_RESOURCE_NAME = "org/apache/qpid/client/security/CallbackHandlerRegistry.properties";

    /** A static reference to the singleton instance of this registry. */
    private static CallbackHandlerRegistry _instance = new CallbackHandlerRegistry();

    /** Holds a map from SASL mechanism names to call back handlers. */
    private Map<String, Class> _mechanismToHandlerClassMap = new HashMap<String, Class>();

    /** Holds a space delimited list of mechanisms that callback handlers exist for. */
    private String _mechanisms;

    /**
     * Gets the singleton instance of this registry.
     *
     * @return The singleton instance of this registry.
     */
    public static CallbackHandlerRegistry getInstance()
    {
        return _instance;
    }

    /**
     * Gets the callback handler class for a given SASL mechanism name.
     *
     * @param mechanism The SASL mechanism name.
     *
     * @return The callback handler class for the mechanism, or null if none is configured for that mechanism.
     */
    public Class getCallbackHandlerClass(String mechanism)
    {
        return (Class) _mechanismToHandlerClassMap.get(mechanism);
    }

    /**
     * Gets a space delimited list of supported SASL mechanisms.
     *
     * @return A space delimited list of supported SASL mechanisms.
     */
    public String getMechanisms()
    {
        return _mechanisms;
    }

    /**
     * Creates the call back handler registry from its configuration resource or file. This also has the side effect
     * of configuring and registering the SASL client factory implementations using {@link DynamicSaslRegistrar}.
     */
    private CallbackHandlerRegistry()
    {
        // Register any configured SASL client factories.
        DynamicSaslRegistrar.registerSaslProviders();

        String filename = System.getProperty(FILE_PROPERTY);
        InputStream is =
            FileUtils.openFileOrDefaultResource(filename, DEFAULT_RESOURCE_NAME,
                CallbackHandlerRegistry.class.getClassLoader());

        try
        {
            Properties props = new Properties();
            props.load(is);
            parseProperties(props);
            _logger.info("Callback handlers available for SASL mechanisms: " + _mechanisms);
        }
        catch (IOException e)
        {
            _logger.error("Error reading properties: " + e, e);
        }
        finally
        {
            if (is != null)
            {
                try
                {
                    is.close();

                }
                catch (IOException e)
                {
                    _logger.error("Unable to close properties stream: " + e, e);
                }
            }
        }
    }

    /*private InputStream openPropertiesInputStream(String filename)
    {
        boolean useDefault = true;
        InputStream is = null;
        if (filename != null)
        {
            try
            {
                is = new BufferedInputStream(new FileInputStream(new File(filename)));
                useDefault = false;
            }
            catch (FileNotFoundException e)
            {
                _logger.error("Unable to read from file " + filename + ": " + e, e);
            }
        }

        if (useDefault)
        {
            is = CallbackHandlerRegistry.class.getResourceAsStream(DEFAULT_RESOURCE_NAME);
        }

        return is;
    }*/

    /**
     * Scans the specified properties as a mapping from IANA registered SASL mechanism to call back handler
     * implementations, that provide the necessary call back handling for obtaining user log in credentials
     * during authentication for the specified mechanism, and builds a map from mechanism names to handler
     * classes.
     *
     * @param props
     */
    private void parseProperties(Properties props)
    {
        Enumeration e = props.propertyNames();
        while (e.hasMoreElements())
        {
            String propertyName = (String) e.nextElement();
            int period = propertyName.indexOf(".");
            if (period < 0)
            {
                _logger.warn("Unable to parse property " + propertyName + " when configuring SASL providers");

                continue;
            }

            String mechanism = propertyName.substring(period + 1);
            String className = props.getProperty(propertyName);
            Class clazz = null;
            try
            {
                clazz = Class.forName(className);
                if (!AMQCallbackHandler.class.isAssignableFrom(clazz))
                {
                    _logger.warn("SASL provider " + clazz + " does not implement " + AMQCallbackHandler.class
                        + ". Skipping");

                    continue;
                }

                _mechanismToHandlerClassMap.put(mechanism, clazz);
                if (_mechanisms == null)
                {
                    _mechanisms = mechanism;
                }
                else
                {
                    // one time cost
                    _mechanisms = _mechanisms + " " + mechanism;
                }
            }
            catch (ClassNotFoundException ex)
            {
                _logger.warn("Unable to load class " + className + ". Skipping that SASL provider");

                continue;
            }
        }
    }
}
