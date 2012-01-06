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
package org.apache.qpid.junit.extensions.util;

import java.util.Properties;

/**
 * TestContextProperties is an extension of {@link ParsedProperties} that keeps track of property key/value pairs
 * that are used by tests being run under the {@link org.apache.qpid.junit.extensions.TKTestRunner}. To keep the
 * test runner notified of configurable test parameters, tests should establish their required property values by
 * initiliazing fields or statics or in the constructor, through this class. The tk test runner automatically places
 * any additional properties specified on the command line into the this class, and these are held statically.
 *
 * <p/>Here is an example:
 *
 * <pre>
 * public class MyTestClass extends TestCase {
 *     ParsedProperties testProps = TestContextProperties.getInstance();
 *     private int testParam = testProps.setPropertyIfNull("testParam", 1);
 * ...
 * </pre>
 *
 * <p/>This has the effect of setting up the field testParam with the default value of 1, unless it is overridden
 * by values passed to the tk test runner. It also notifies the tk test runner of the name and value of the test
 * parameter actually used for the test, so that this can be logged in the test output file.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Log all name/value pairs read or written.
 * </table>
 *
 * @author Rupert Smith
 */
public class TestContextProperties extends ParsedProperties
{
    /** Used for debugging. */
    // Logger log = Logger.getLogger(TestContextProperties.class);

    /** Holds all properties set or read through this property extension class. */
    private Properties accessedProps = new Properties();

    /** The singleton instance of the test context properties. */
    private static TestContextProperties singleton = null;

    /**
     * Default constructor that builds a ContextualProperties that uses environment defaults.
     */
    private TestContextProperties()
    {
        super();
    }

    /**
     * Gets the singleton instance of the test context properties.
     *
     * @return The singleton instance of the test context properties.
     */
    public static synchronized ParsedProperties getInstance()
    {
        if (singleton == null)
        {
            singleton = new TestContextProperties();
        }

        return singleton;
    }

    /**
     * Gets the singleton instance of the test context properties, applying a specified set of default properties to
     * it, if they are not already set.
     *
     * @param defaults The defaults to apply for properties not already set.
     *
     * @return The singleton instance of the test context properties.
     */
    public static synchronized ParsedProperties getInstance(Properties defaults)
    {
        ParsedProperties props = getInstance();

        for (Object key : defaults.keySet())
        {
            String stringKey = (String) key;
            String value = defaults.getProperty(stringKey);

            props.setPropertyIfNull(stringKey, value);
        }

        return props;
    }

    /*
     * Creates a ContextualProperties that uses environment defaults and is initialized with the specified properties.
     *
     * @param props The properties to initialize this with.
     */
    /*public TestContextProperties(Properties props)
    {
        super();
    }*/

    /**
     * Gets all of the properties (with their most recent values) that have been set or read through this class.
     *
     * @return All of the properties accessed through this class.
     */
    public static Properties getAccessedProps()
    {
        return (singleton == null) ? new Properties() : singleton;
            // return accessedProps;
    }

    /**
     * Looks up a property value relative to the environment, callers class and method. The default environment will be
     * checked for a matching property if defaults are being used. The property key/value pair is remembered and made
     * available to {@link org.apache.qpid.junit.extensions.TKTestRunner}.
     *
     * @param key The property key.
     *
     * @return The value of this property searching from the most specific definition (environment, class, method, key)
     *         to the most general (key only), unless use of default environments is turned off in which case the most general
     *         proeprty searched is (environment, key).
     */
    public String getProperty(String key)
    {
        // log.debug("public String getProperty(String key = " + key + "): called");

        String value = super.getProperty(key);

        if (value != null)
        {
            accessedProps.setProperty(key, value);
        }

        // log.debug("value = " + value);

        return value;
    }

    /**
     * Calls the <tt>Hashtable</tt> method <code>put</code>. Provided for parallelism with the <tt>getProperty</tt>
     * method. Enforces use of strings for property keys and values. The value returned is the result of the
     * <tt>Hashtable</tt> call to <code>put</code>. The property key/value pair is remembered and made
     * available to {@link org.apache.qpid.junit.extensions.TKTestRunner}.
     *
     * @param key   The key to be placed into this property list.
     * @param value The value corresponding to <tt>key</tt>.
     *
     * @return The previous value of the specified key in this property list, or <code>null</code> if it did not have one.
     */
    public synchronized Object setProperty(String key, String value)
    {
        // log.debug("public synchronized Object setProperty(String key = " + key + ", String value = " + value + "): called");

        Object result = super.setProperty(key, value);
        accessedProps.setProperty(key, value);

        return result;
    }

    /**
     * Helper method for setting properties to defaults when they are not already set. The property key/value pair is
     * remembered and made available to {@link org.apache.qpid.junit.extensions.TKTestRunner}.
     *
     * @param key      The name of the system property to set.
     * @param value    The value to set it to.
     *
     * @return The value of the property, which will be the value passed in if it was null, or the existing value otherwise.
     */
    public String setPropertyIfNull(String key, String value)
    {
        // log.debug("public String setPropertyIfNull(String key = " + key + ", String value = " + value + "): called");

        String result = super.setPropertyIfNull(key, value);

        if (value != null)
        {
            accessedProps.setProperty(key, result);
        }

        // log.debug("result = " + result);

        return result;
    }
}
