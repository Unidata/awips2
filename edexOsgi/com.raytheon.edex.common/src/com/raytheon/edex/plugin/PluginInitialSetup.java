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
package com.raytheon.edex.plugin;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.IPluginRegistryChanged;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * Initializes a plugin that is added to the registry.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 20, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PluginInitialSetup implements IPluginRegistryChanged {

    /** The logger */
    protected transient Log logger = LogFactory.getLog(getClass());

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataplugin.PluginRegistyChanged#pluginAdded(java
     * .lang.String)
     */
    @Override
    public void pluginAdded(String pluginName) {
        try {
            PluginFactory.getInstance().getPluginInitializer(pluginName)
                    .initializePlugin();
        } catch (PluginException e) {
            logger.error("Error initializing " + pluginName + " plugin!", e);
        } catch (Throwable e) {
            logger.error("Uncaught exception occurred while initializing "
                    + pluginName + " plugin.", e);
        }
    }

    /**
     * Cleans any plugins out of the plugin_info table if plugins have been
     * removed since last startup
     * 
     * @throws PluginException
     *             If problems occur interacting with the database
     */
    private void cleanPluginInfoTable() throws PluginException {
        // TODO This was taken out of InitializerBean and commented out
        // because the logic won't work here or there because
        // we don't know the entire list of available plugins at this time.
        // What we should do is add to the listener a method for pluginRemoved
        // and have that remove the plugin_info row.

        // List<String> registeredPlugins = new ArrayList<String>();
        // try {
        // registeredPlugins = pvd.loadAllPluginNames();
        // } catch (DataAccessLayerException e) {
        // throw new PluginException(
        // "Cannot get list of available plugins from the database", e);
        // }
        // List<String> plugins = Arrays.asList(EDEXUtil.getAvailablePlugins());
        // for (String pluginName : registeredPlugins) {
        // if (!plugins.contains(pluginName)) {
        // try {
        // pvd.deletePluginVersionByName(pluginName);
        // } catch (DataAccessLayerException e) {
        // logger.error("Unable to remove " + pluginName
        // + " plugin from plugin_info table", e);
        // } catch (Throwable e) {
        // logger.error("Uncaught error:: unable to remove "
        // + pluginName + " plugin from plugin_info table", e);
        // }
        // }
        // }
    }

}
