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

package com.raytheon.edex.plugin.factory;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.IMessageDecoder;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.core.props.EnvProperties;
import com.raytheon.uf.edex.core.props.Properties;
import com.raytheon.uf.edex.core.props.PropertiesFactory;

/**
 * Base implementation for object factory classes. This class can be extended to
 * provide factories for retrieving plugin data types (ie decoders, separators,
 * or daos) <br>
 * Factories extending this class must implement the singleton pattern.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 7/24/07		353			bphillip	Initial check-in
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public abstract class AbstractFactory {

    protected enum FactoryType {
        DECODER, SEPARATOR
    }

    private static final String HTTPPROXYSET = "HTTPPROXYSET";

    private static final String HTTPPROXYHOST = "HTTPPROXYHOST";

    private static final String HTTPPROXYPORT = "HTTPPROXYPORT";

    protected FactoryType factoryType;

    /** The logger */
    protected final Log logger = LogFactory.getLog(getClass());

    /** The environment properties */
    private EnvProperties envProperties;

    /**
     * Constructs a new object factory
     * 
     * @param factoryType
     *            The type of object loaded by the factory
     * @throws PluginException
     *             If errors occur during instantiation of the factory
     */
    protected AbstractFactory(FactoryType factoryType) throws PluginException {

        this.factoryType = factoryType;
        envProperties = PropertiesFactory.getInstance().getEnvProperties();

        if (envProperties == null)
            throw new PluginException(
                    "Unable to retrieve environment properties");
        // set the proxy
        loadProxyProperties();
    }

    /**
     * Gets the object of the type implied by the factory for the specified
     * plugin name. This method is wrapped by the public get() method.
     * 
     * @param pluginName
     * @return The object of the type implied by the factory
     * @throws PluginException
     *             If instantiation of the object fails
     */
    public abstract Object get(String pluginName) throws PluginException;

    /**
     * Gets an instance of the specified plug-in component for the specified
     * data type.
     * 
     * @param pluginName
     * @param pluginComponentName
     * @return an instance of the class for the specified interface for the
     *         plug-in
     * @throws PluginException
     */
    protected Object getInstance(String pluginName) throws PluginException {
        String className = null;
        Class<?> aClass = null;
        Object instance = null;
        Properties properties = null;

        // retrieve the the properties for this plugin
        properties = PropertiesFactory.getInstance().getPluginProperties(
                pluginName);

        if (properties == null)
            throw new PluginException("Unable to load properties for "
                    + pluginName);
        // get the name of the class for the component
        className = properties.getPluginValue(factoryType.toString());
        if (className == null) {
            throw new PluginException("Unable to load class for service = "
                    + pluginName + " property = " + factoryType);
        }

        try {
            aClass = this.getClass().getClassLoader().loadClass(className);
        } catch (ClassNotFoundException e) {
            this.logger.error("Unable to create a Class object for: '"
                    + className + "' with the URL class loader.");
            throw new PluginException("Unable to create a Class object for: '"
                    + className + "' with the URL class loader.");
        }

        // if there's a class, try to load it
        if (aClass != null) {
            // logger.info("Creating instance of " + className);
            try {
                instance = aClass.newInstance();
                if (factoryType == FactoryType.DECODER) {
                    IMessageDecoder dc = (IMessageDecoder) instance;
                    dc.setPluginName(pluginName);
                    dc.setProperties(properties);
                }
            } catch (Exception e) {
                this.logger.error("Unable to create an instance of: '"
                        + className + "'.", e);
                throw new PluginException("Unable to create an instance of: '"
                        + className + "'.", e);
            }
        }

        return instance;
    }

    /**
     * Loads the proxy settings from the thePluginConfigurationuration
     * properties.
     * 
     */
    private void loadProxyProperties() throws PluginException {

        String proxySet = null;
        String proxyHost = null;
        String proxyPort = null;

        // set the proxy for these calls
        proxySet = this.envProperties.getEnvValue(HTTPPROXYSET);
        proxyHost = this.envProperties.getEnvValue(HTTPPROXYHOST);
        proxyPort = this.envProperties.getEnvValue(HTTPPROXYPORT);

        // if all properties are set, set the system values
        if ((proxySet != null) && (proxyHost != null) && (proxyPort != null)) {
            System.getProperties().put("http.proxySet", proxySet);
            System.getProperties().put("http.proxyHost", proxyHost);
            System.getProperties().put("http.proxyPort", proxyPort);
        }

    }

}
