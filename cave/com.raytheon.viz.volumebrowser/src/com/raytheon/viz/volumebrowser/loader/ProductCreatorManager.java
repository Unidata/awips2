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
package com.raytheon.viz.volumebrowser.loader;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.rsc.ResourceType;

/**
 * 
 * Class for managing registered {@link ProductCreator}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 03, 2015  3861     bsteffen  Initial Creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class ProductCreatorManager {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProductCreatorManager.class);

    private static final String EXTENSION_POINT_ID = "com.raytheon.viz.volumebrowser.productcreator";

    private static final ProductCreatorManager instance = new ProductCreatorManager();

    private final Map<Key, ProductCreator> creators = new HashMap<>();

    private ProductCreatorManager() {
        loadFromExtensionPoint();
    }

    private void loadFromExtensionPoint() {
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry.getExtensionPoint(EXTENSION_POINT_ID);
        IExtension[] extensions = point.getExtensions();

        for (IExtension ext : extensions) {
            IConfigurationElement[] config = ext.getConfigurationElements();

            for (IConfigurationElement cfg : config) {
                String resourceType = cfg.getAttribute("resourceType");
                String plugins = cfg.getAttribute("plugins");
                try {
                    ProductCreator creator = (ProductCreator) cfg
                            .createExecutableExtension("creatorClass");
                    for (String plugin : plugins.split(",")) {
                        addCreator(plugin, resourceType, creator);
                    }
                } catch (CoreException e) {
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "Unable to load a Volume Browser product creator, some products will not load.",
                                    e);
                }
            }
        }
    }

    public void addCreator(String pluginName, String resourceType,
            ProductCreator creator) {
        Key key = new Key(pluginName, resourceType);
        synchronized (creators) {
            creators.put(key, creator);
        }
    }

    public ProductCreator getCreator(String pluginName, String resourceType) {
        Key key = new Key(pluginName, resourceType);
        synchronized (creators) {
            return creators.get(key);
        }
    }

    public ProductCreator getCreator(String pluginName,
            ResourceType resourceType) {
        return getCreator(pluginName, resourceType.toString().toLowerCase());
    }

    public static ProductCreatorManager getInstance() {
        return instance;
    }

    private static class Key {

        public final String pluginName;

        public final String resourceType;

        private final int hashCode;

        public Key(String pluginName, String resourceType) {
            this.pluginName = pluginName;
            this.resourceType = resourceType;

            int prime = 31;
            int hashCode = 1;
            hashCode = prime * hashCode
                    + ((pluginName == null) ? 0 : pluginName.hashCode());
            hashCode = prime * hashCode
                    + ((resourceType == null) ? 0 : resourceType.hashCode());
            this.hashCode = hashCode;
        }

        @Override
        public int hashCode() {
            return hashCode;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            Key other = (Key) obj;
            if (pluginName == null) {
                if (other.pluginName != null)
                    return false;
            } else if (!pluginName.equals(other.pluginName))
                return false;
            if (resourceType == null) {
                if (other.resourceType != null)
                    return false;
            } else if (!resourceType.equals(other.resourceType))
                return false;
            return true;
        }

        @Override
        public String toString() {
            return "Key [pluginName=" + pluginName + ", resourceType="
                    + resourceType + "]";
        }
    }
}
