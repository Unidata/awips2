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
 * ParsedProperties extends the basic Properties class with methods to extract properties, not as strings but as strings
 * parsed into basic types.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * </table>
 *
 * @author Rupert Smith
 */
public class ParsedProperties extends Properties
{
    /**
     * Creates an empty ParsedProperties.
     */
    public ParsedProperties()
    {
        super();
    }

    /**
     * Creates a ParsedProperties initialized with the specified properties.
     *
     * @param props The properties to initialize this with.
     */
    public ParsedProperties(Properties props)
    {
        super(props);
    }

    /**
     * Helper method for setting system properties to defaults when they are not already set.
     *
     * @param propname The name of the system property to set.
     * @param value    The value to set it to.
     *
     * @return The value of the property after this method call.
     */
    public static boolean setSysPropertyIfNull(String propname, boolean value)
    {
        return Boolean.parseBoolean(setSysPropertyIfNull(propname, Boolean.toString(value)));
    }

    /**
     * Helper method for setting system properties to defaults when they are not already set.
     *
     * @param propname The name of the system property to set.
     * @param value    The value to set it to.
     *
     * @return The value of the property after this method call.
     */
    public static short setSysPropertyIfNull(String propname, short value)
    {
        return Short.parseShort(setSysPropertyIfNull(propname, Short.toString(value)));
    }

    /**
     * Helper method for setting system properties to defaults when they are not already set.
     *
     * @param propname The name of the system property to set.
     * @param value    The value to set it to.
     *
     * @return The value of the property after this method call.
     */
    public static int setSysPropertyIfNull(String propname, int value)
    {
        return Integer.parseInt(setSysPropertyIfNull(propname, Integer.toString(value)));
    }

    /**
     * Helper method for setting system properties to defaults when they are not already set.
     *
     * @param propname The name of the system property to set.
     * @param value    The value to set it to.
     *
     * @return The value of the property after this method call.
     */
    public static long setSysPropertyIfNull(String propname, long value)
    {
        return Long.parseLong(setSysPropertyIfNull(propname, Long.toString(value)));
    }

    /**
     * Helper method for setting system properties to defaults when they are not already set.
     *
     * @param propname The name of the system property to set.
     * @param value    The value to set it to.
     *
     * @return The value of the property after this method call.
     */
    public static float setSysPropertyIfNull(String propname, float value)
    {
        return Float.parseFloat(setSysPropertyIfNull(propname, Float.toString(value)));
    }

    /**
     * Helper method for setting system properties to defaults when they are not already set.
     *
     * @param propname The name of the system property to set.
     * @param value    The value to set it to.
     *
     * @return The value of the property after this method call.
     */
    public static double setSysPropertyIfNull(String propname, double value)
    {
        return Double.parseDouble(setSysPropertyIfNull(propname, Double.toString(value)));
    }

    /**
     * Helper method for setting system properties to defaults when they are not already set.
     *
     * @param propname The name of the system property to set.
     * @param value    The value to set it to.
     *
     * @return The value of the system property after this method call.
     */
    public static String setSysPropertyIfNull(String propname, String value)
    {
        String property = System.getProperty(propname);

        if (property == null)
        {
            System.setProperty(propname, value);

            return value;
        }
        else
        {
            return property;
        }
    }

    /**
     * Helper method for setting properties to defaults when they are not already set.
     *
     * @param propname The name of the system property to set.
     * @param value    The value to set it to.
     *
     * @return The value of the property after this method call.
     */
    public boolean setPropertyIfNull(String propname, boolean value)
    {
        return Boolean.parseBoolean(setPropertyIfNull(propname, Boolean.toString(value)));
    }

    /**
     * Helper method for setting properties to defaults when they are not already set.
     *
     * @param propname The name of the system property to set.
     * @param value    The value to set it to.
     *
     * @return The value of the property after this method call.
     */
    public short setPropertyIfNull(String propname, short value)
    {
        return Short.parseShort(setPropertyIfNull(propname, Short.toString(value)));
    }

    /**
     * Helper method for setting properties to defaults when they are not already set.
     *
     * @param propname The name of the system property to set.
     * @param value    The value to set it to.
     *
     * @return The value of the property after this method call.
     */
    public int setPropertyIfNull(String propname, int value)
    {
        return Integer.parseInt(setPropertyIfNull(propname, Integer.toString(value)));
    }

    /**
     * Helper method for setting properties to defaults when they are not already set.
     *
     * @param propname The name of the system property to set.
     * @param value    The value to set it to.
     *
     * @return The value of the property after this method call.
     */
    public long setPropertyIfNull(String propname, long value)
    {
        return Long.parseLong(setPropertyIfNull(propname, Long.toString(value)));
    }

    /**
     * Helper method for setting properties to defaults when they are not already set.
     *
     * @param propname The name of the system property to set.
     * @param value    The value to set it to.
     *
     * @return The value of the property after this method call.
     */
    public float setPropertyIfNull(String propname, float value)
    {
        return Float.parseFloat(setPropertyIfNull(propname, Float.toString(value)));
    }

    /**
     * Helper method for setting properties to defaults when they are not already set.
     *
     * @param propname The name of the system property to set.
     * @param value    The value to set it to.
     *
     * @return The value of the property after this method call.
     */
    public double setPropertyIfNull(String propname, double value)
    {
        return Double.parseDouble(setPropertyIfNull(propname, Double.toString(value)));
    }

    /**
     * Helper method for setting properties to defaults when they are not already set.
     *
     * @param propname The name of the system property to set.
     * @param value    The value to set it to.
     *
     * @return The value of the property after this method call.
     */
    public String setPropertyIfNull(String propname, String value)
    {
        String property = super.getProperty(propname);

        if (property == null)
        {
            super.setProperty(propname, value);

            return value;
        }
        else
        {
            return property;
        }
    }

    /**
     * Helper method for setting properties.
     *
     * @param propname The name of the system property to set.
     * @param value    The value to set it to.
     *
     * @return The value of the property after this method call.
     */
    public boolean setProperty(String propname, boolean value)
    {
        setProperty(propname, Boolean.toString(value));

        return value;
    }

    /**
     * Helper method for setting properties.
     *
     * @param propname The name of the system property to set.
     * @param value    The value to set it to.
     *
     * @return The value of the property after this method call.
     */
    public short setProperty(String propname, short value)
    {
        setProperty(propname, Short.toString(value));

        return value;
    }

    /**
     * Helper method for setting properties.
     *
     * @param propname The name of the system property to set.
     * @param value    The value to set it to.
     *
     * @return The value of the property after this method call.
     */
    public int setProperty(String propname, int value)
    {
        setProperty(propname, Integer.toString(value));

        return value;
    }

    /**
     * Helper method for setting properties.
     *
     * @param propname The name of the system property to set.
     * @param value    The value to set it to.
     *
     * @return The value of the property after this method call.
     */
    public long setProperty(String propname, long value)
    {
        setProperty(propname, Long.toString(value));

        return value;
    }

    /**
     * Helper method for setting properties.
     *
     * @param propname The name of the system property to set.
     * @param value    The value to set it to.
     *
     * @return The value of the property after this method call.
     */
    public float setProperty(String propname, float value)
    {
        setProperty(propname, Float.toString(value));

        return value;
    }

    /**
     * Helper method for setting properties.
     *
     * @param propname The name of the system property to set.
     * @param value    The value to set it to.
     *
     * @return The value of the property after this method call.
     */
    public double setProperty(String propname, double value)
    {
        setProperty(propname, Double.toString(value));

        return value;
    }

    /**
     * Parses a property as a boolean.
     *
     * @param propName The property.
     *
     * @return The property as a boolean, or false if it does not exist.
     */
    public boolean getPropertyAsBoolean(String propName)
    {
        String prop = getProperty(propName);

        return (prop != null) && Boolean.parseBoolean(prop);
    }

    /**
     * Parses a property as an integer.
     *
     * @param propName The property.
     *
     * @return The property as a integer, or null if it does not exist.
     */
    public Integer getPropertyAsInteger(String propName)
    {
        String prop = getProperty(propName);

        return (prop != null) ? new Integer(prop) : null;
    }

    /**
     * Parses a property as a long.
     *
     * @param propName The property.
     *
     * @return The property as a long, or null if it does not exist.
     */
    public Long getPropertyAsLong(String propName)
    {
        String prop = getProperty(propName);

        return (prop != null) ? new Long(prop) : null;
    }
}
