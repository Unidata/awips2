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
package org.apache.qpid.common;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import java.util.Properties;

/**
 * QpidProperties captures the project name, version number, and source code repository revision number from a properties
 * file which is generated as part of the build process. Normally, the name and version number are pulled from the module
 * name and version number of the Maven build POM, but could come from other sources if the build system is changed. The
 * idea behind this, is that every build has these values incorporated directly into its jar file, so that code in the
 * wild can be identified, should its origination be forgotten.
 *
 * <p/>To get the build version of any Qpid code call the {@link #main} method. This version string is usually also
 * printed to the console on broker start up.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><td>Load build versioning information into the runtime, for code identification purposes.
 * </table>
 *
 * @todo Code to locate/load/log properties can be factored into a reusable properties utils class. Avoid having this
 *       same snippet of loading code scattered in many places.
 *
 * @todo Could also add a build number property for a sequential build number assigned by an automated build system, for
 *       build reproducability purposes.
 */
public class QpidProperties
{
    /** Used for debugging purposes. */
    private static final Logger _logger = LoggerFactory.getLogger(QpidProperties.class);

    /** The name of the version properties file to load from the class path. */
    public static final String VERSION_RESOURCE = "qpidversion.properties";

    /** Defines the name of the product property. */
    public static final String PRODUCT_NAME_PROPERTY = "qpid.name";

    /** Defines the name of the version property. */
    public static final String RELEASE_VERSION_PROPERTY = "qpid.version";

    /** Defines the name of the source code revision property. */
    public static final String BUILD_VERSION_PROPERTY = "qpid.svnversion";

    /** Defines the default value for all properties that cannot be loaded. */
    private static final String DEFAULT = "unknown";

    /** Holds the product name. */
    private static String productName = DEFAULT;

    /** Holds the product version. */
    private static String releaseVersion = DEFAULT;

    /** Holds the source code revision. */
    private static String buildVersion = DEFAULT;

    // Loads the values from the version properties file.
    static
    {
        Properties props = new Properties();

        try
        {
            InputStream propertyStream = QpidProperties.class.getClassLoader().getResourceAsStream(VERSION_RESOURCE);
            if (propertyStream == null)
            {
                _logger.warn("Unable to find resource " + VERSION_RESOURCE + " from classloader");
            }
            else
            {
                props.load(propertyStream);

                if (_logger.isDebugEnabled())
                {
                    _logger.debug("Dumping QpidProperties");
                    for (Map.Entry<Object, Object> entry : props.entrySet())
                    {
                        _logger.debug("Property: " + entry.getKey() + " Value: " + entry.getValue());
                    }

                    _logger.debug("End of property dump");
                }

                productName = readPropertyValue(props, PRODUCT_NAME_PROPERTY);
                releaseVersion = readPropertyValue(props, RELEASE_VERSION_PROPERTY);
                buildVersion = readPropertyValue(props, BUILD_VERSION_PROPERTY);
            }
        }
        catch (IOException e)
        {
            // Log a warning about this and leave the values initialized to unknown.
            _logger.error("Could not load version.properties resource: " + e, e);
        }
    }

    /**
     * Gets the product name.
     *
     * @return The product name.
     */
    public static String getProductName()
    {
        return productName;
    }

    /**
     * Gets the product version.
     *
     * @return The product version.
     */
    public static String getReleaseVersion()
    {
        return releaseVersion;
    }

    /**
     * Gets the source code revision.
     *
     * @return The source code revision.
     */
    public static String getBuildVersion()
    {
        return buildVersion;
    }

    /**
     * Extracts all of the version information as a printable string.
     *
     * @return All of the version information as a printable string.
     */
    public static String getVersionString()
    {
        return getProductName() + " - " + getReleaseVersion() + " build: " + getBuildVersion();
    }

    /**
     * Helper method to extract a named property from properties.
     *
     * @param props        The properties.
     * @param propertyName The named property to extract.
     *
     * @return The extracted property or a default value if the properties do not contain the named property.
     *
     * @todo A bit pointless.
     */
    private static String readPropertyValue(Properties props, String propertyName)
    {
        String retVal = (String) props.get(propertyName);
        if (retVal == null)
        {
            retVal = DEFAULT;
        }

        return retVal;
    }

    /**
     * Prints the versioning information to the console. This is extremely usefull for identifying Qpid code in the
     * wild, where the origination of the code has been forgotten.
     *
     * @param args Does not require any arguments.
     */
    public static void main(String[] args)
    {
        System.out.println(getVersionString());
    }
}
