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
package com.raytheon.uf.edex.database;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.IPluginRegistryChanged;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.util.registry.GenericRegistry;
import com.raytheon.uf.common.util.registry.RegistryException;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;

/**
 * Registry of EDEX database plugins
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 28, 2010 #5050      rjpeter     Initial creation
 * May 29, 2014 2726       rjpeter     Added initial listeners and properties for easier spring dependency management.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class DatabasePluginRegistry extends
        GenericRegistry<String, DatabasePluginProperties> implements
        IPluginRegistryChanged {
    /** The logger */
    protected transient Log logger = LogFactory.getLog(getClass());

    private static DatabasePluginRegistry instance = new DatabasePluginRegistry();

    private final List<IDatabasePluginRegistryChanged> listeners = new ArrayList<IDatabasePluginRegistryChanged>();

    private List<DatabasePluginProperties> initialProperties;

    private DatabasePluginRegistry() {
        super();
    }

    public static DatabasePluginRegistry getInstance() {
        return instance;
    }

    /**
     * Called by spring to initialize the registry. Mainly used to ensure the
     * base database plugin is always loaded.
     */
    public void init() throws RegistryException {
        if ((initialProperties != null) && !initialProperties.isEmpty()) {
            for (DatabasePluginProperties dbProp : initialProperties) {
                register(dbProp.getPluginFQN(), dbProp);
            }
        }
    }

    @Override
    public Object register(String pluginFQN,
            DatabasePluginProperties pluginProperties) throws RegistryException {
        if (!registry.containsKey(pluginFQN)) {
            try {
                super.register(pluginFQN, pluginProperties);
                for (IDatabasePluginRegistryChanged iprc : listeners) {
                    iprc.pluginAdded(pluginFQN);
                }
            } catch (PluginException e) {
                throw new RegistryException("Plugin " + pluginFQN
                        + " failed during registration", e);
            }
            return this;
        } else {
            throw new RegistryException("Duplicate pluginFQN " + pluginFQN
                    + " registered in Spring XML configuration.");
        }
    }

    public Object addListener(IDatabasePluginRegistryChanged listener) {
        listeners.add(listener);
        return this;
    }

    @Override
    public void pluginAdded(String pluginName) {
        PluginProperties props = PluginRegistry.getInstance()
                .getRegisteredObject(pluginName);
        if ((props.getPluginFQN() != null)
                && (props.getPluginFQN().length() > 0)) {
            // multiple plugins may use the same jar file.
            if (!registry.containsKey(props.getPluginFQN())) {
                try {
                    DatabasePluginProperties dprops = new DatabasePluginProperties(
                            props);
                    this.register(dprops.getPluginFQN(), dprops);
                } catch (Exception e) {
                    logger.error("Failed to register dataplugin [" + pluginName
                            + "]", e);
                }
            }
        }
    }

    public void setInitialListeners(
            List<IDatabasePluginRegistryChanged> listeners) {
        this.listeners.addAll(listeners);
    }

    public void setInitialProperties(
            List<DatabasePluginProperties> initialProperties) {
        this.initialProperties = initialProperties;
    }
}
