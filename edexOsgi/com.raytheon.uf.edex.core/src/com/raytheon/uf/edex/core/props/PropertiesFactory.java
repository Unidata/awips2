/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.uf.edex.core.props;

import java.io.File;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.jar.JarFile;

import org.apache.commons.configuration.CompositeConfiguration;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.ConfigurationFactory;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.commons.configuration.interpol.ConfigurationInterpolator;
import org.apache.commons.lang.text.StrLookup;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.util.JarUtil;

/**
 * Factory for retrieving uFrame properties
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2006                     garmenda    Initial creation
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class PropertiesFactory {

    private final String ENVIRONMENT = "ENVIRONMENT";

    private final String ATTRIBUTE_NAMES = "ATTRIBUTE_NAMES";

    private static final String JAR_CONFIG_FILE = System
            .getProperty("edex.home") + "/conf/res/config.xml";

    private static final String JAR_CONFIG_DIR = "res/conf/";

    private static final String NAME = "Name";

    private EnvProperties theEnvProperties;

    private Map<String, Properties> thePropertiesMap;

    private Map<String, Configuration> theConfMap;

    /** The logger */
    private Log theLogger = LogFactory.getLog(getClass());

    /**
     * The instance of the Configuration factory. Used to load the config.xml
     * file
     */
    private ConfigurationFactory theConfFactory;

    /**
     * The composite configuration instance, which has access to each property
     * type
     */
    private CompositeConfiguration theConfiguration;

    private static PropertiesFactory theFactory = null;

    static {
        ConfigurationInterpolator.registerGlobalLookup("env", new StrLookup() {
            @Override
            public String lookup(String varName) {
                return System.getProperty(varName);
            }
        });
    }

    private PropertiesFactory() {
        thePropertiesMap = new HashMap<String, Properties>();
        theConfMap = new HashMap<String, Configuration>();
    }

    public synchronized static PropertiesFactory getInstance() {

        if (theFactory == null) {
            theFactory = new PropertiesFactory();
            theFactory.theConfFactory = new ConfigurationFactory(
                    JAR_CONFIG_FILE);
            try {
                theFactory.theConfiguration = (CompositeConfiguration) theFactory.theConfFactory
                        .getConfiguration();
                try {
                    addConfigFromDirectory("pluginDirectory");
                    addConfigFromDirectory("servicesDirectory");
                } catch (Exception e) {
                    System.out.println("PF: error reading config - "
                            + e.getMessage());
                }
                for (int counter = 0; counter < theFactory.theConfiguration
                        .getNumberOfConfigurations(); counter++) {
                    String pluginName = (String) theFactory.theConfiguration
                            .getConfiguration(counter).getProperty(NAME);
                    if (pluginName != null) {
                        if (theFactory.theConfMap.containsKey(pluginName)) {
                            // System.out.println("PF: conf["+counter+"]
                            // appending configuration for " + pluginName);
                            /*
                             * if the configuration already exists, we append
                             * the new properties to the existing configuration,
                             * then replace it in the conf map. The assumption
                             * here is that the new configuration is the smaller
                             * of the two.
                             */
                            Configuration config = theFactory.theConfMap
                                    .get(pluginName);
                            Configuration toCopy = theFactory.theConfiguration
                                    .getConfiguration(counter);
                            Iterator<?> iter = toCopy.getKeys();
                            while (iter.hasNext()) {
                                String next = (String) iter.next();
                                config.addProperty(next,
                                        toCopy.getProperty(next));
                            }
                            theFactory.theConfMap.put(pluginName, config);

                        } else {
                            // System.out.println("PF: conf["+counter+"] -
                            // initial configuration for " + pluginName);
                            theFactory.theConfMap.put(pluginName,
                                    theFactory.theConfiguration
                                            .getConfiguration(counter));
                        }
                    }
                }

                theFactory.thePropertiesMap = new HashMap<String, Properties>();

            } catch (ConfigurationException e) {
                theFactory.theLogger.error("Unable to retrieve configuration",
                        e);
            }

        }

        return theFactory;

    }

    public synchronized Properties getPluginProperties(String aPlugin) {

        Properties properties = null;

        // first check for a valid properties instance
        properties = thePropertiesMap.get(aPlugin);
        if (properties == null) {
            if (theConfMap.get(ENVIRONMENT) != null
                    && theConfMap.get(aPlugin.toUpperCase()) != null
                    && theConfMap.get(ATTRIBUTE_NAMES) != null) {
                properties = new Properties(theConfMap.get(aPlugin),
                        theConfMap.get(ATTRIBUTE_NAMES),
                        theConfMap.get(ENVIRONMENT));
            }
            if (properties != null)
                thePropertiesMap.put(aPlugin, properties);
        }

        return properties;
    }

    public synchronized EnvProperties getEnvProperties() {

        if (theEnvProperties == null) {
            if (theConfMap.get(ENVIRONMENT) != null
                    && theConfMap.get(ATTRIBUTE_NAMES) != null) {

                theEnvProperties = new EnvProperties(
                        theConfMap.get(ATTRIBUTE_NAMES),
                        theConfMap.get(ENVIRONMENT));
            }
        }

        return theEnvProperties;
    }

    public synchronized Configuration getConfiguration(String aConfiguration)
            throws PropertiesException {

        Configuration configuration = null;

        // first check for a valid properties instance
        configuration = theConfMap.get(aConfiguration.toUpperCase());

        // then, check if the properties object is not null, else thrown an
        // exception
        if (configuration == null)
            throw new PropertiesException(
                    "Unable to retrieve configuration for " + aConfiguration);

        return configuration;
    }

    private static void addConfigFromJar(CompositeConfiguration config,
            String jarPath, String configPath) throws Exception {
        String regex = "^" + configPath + ".*\\.xml$";
        String[] files = JarUtil.getFileListFromJar(new File(jarPath), regex);
        if (files != null && files.length != 0) {
            for (String name : files) {
                // System.out.println("PF: reading " + name);
                XMLConfiguration xconf = new XMLConfiguration();
                JarFile jarFile = new JarFile(jarPath);
                InputStream jarStream = jarFile.getInputStream(jarFile
                        .getEntry(name));
                xconf.load(jarStream);
                config.addConfiguration(xconf);
            }
        }
    }

    private static void addConfigFromDirectory(String service) throws Exception {
        String pluginDir = theFactory.theConfFactory.getConfiguration()
                .getString(service);
        File[] jars = JarUtil.getJarFiles(new File(pluginDir));
        for (File jar : jars) {
            if (JarUtil.isFileInJar(jar, JAR_CONFIG_DIR)) {
                // System.out.println("PF: reading " +
                // jar.getPath());
                addConfigFromJar(theFactory.theConfiguration, jar.getPath(),
                        JAR_CONFIG_DIR);
            }
        }

    }
}
