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
package com.raytheon.uf.viz.core.rsc;

import java.util.LinkedHashMap;
import java.util.Map;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.uf.viz.core.status.StatusConstants;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Mar 25, 2008				randerso	Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class RenderingOrderFactory {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(RenderingOrderFactory.class);

    /** The extension point where rendering order is defined */
    private static final String RENDERING_ORDER_EXTENSION = "com.raytheon.uf.viz.core.renderingOrder";

    /** The extension point where rendering order is defined */
    private static final String RESOURCE_EXTENSION = "com.raytheon.uf.viz.core.resource";

    /** The map of ID to rendering order */
    private static Map<String, Integer> idMap;

    /** The map of Resource to rendering order id */
    private static Map<String, String> rscMap;

    public static class ResourceOrder {
        public static ResourceOrder LOWEST = new ResourceOrder("LOWEST", 0);

        public static ResourceOrder HIGHEST = new ResourceOrder("HIGHEST",
                Integer.MAX_VALUE - 1);

        public static ResourceOrder UNKNOWN = new ResourceOrder("UNKNOWN",
                Integer.MAX_VALUE - 2);

        public static ResourceOrder NOT_SET = new ResourceOrder("NOT_SET",
                Integer.MAX_VALUE);

        public String id;

        public int value;

        private ResourceOrder(String id, int value) {
            this.id = id;
            this.value = value;
        }

        public static ResourceOrder[] values() {
            return new ResourceOrder[] { LOWEST, HIGHEST, UNKNOWN, NOT_SET };
        }
    }

    /**
     * returns the singleton instance of the map
     * 
     * @return
     */
    private static synchronized Map<String, Integer> getIdMap() {
        if (idMap == null) {
            idMap = new LinkedHashMap<String, Integer>();

            IExtensionRegistry registry = Platform.getExtensionRegistry();
            if (registry == null)
                return idMap;

            IExtensionPoint point = registry
                    .getExtensionPoint(RENDERING_ORDER_EXTENSION);
            if (point == null)
                return null;
            IExtension[] extensions = point.getExtensions();

            for (IExtension ext : extensions) {
                IConfigurationElement[] config = ext.getConfigurationElements();

                for (IConfigurationElement cfg : config) {
                    String id = cfg.getAttribute("id");
                    int order = Integer.parseInt(cfg.getAttribute("order"));
                    if (idMap.put(id, order) != null) {
                        Activator
                                .getDefault()
                                .getLog()
                                .log(
                                        new Status(
                                                Status.ERROR,
                                                Activator.PLUGIN_ID,
                                                "Duplicate resourceOrdering id: \""
                                                        + id
                                                        + "\" defined in "
                                                        + ext
                                                                .getNamespaceIdentifier()));
                    }
                }
            }

            // Now add the defaults, TODO: Should plugin.xml be allowed to
            // override defaults or should defaults always override xml.. I
            // think defaults should never be overridden so that is why they are
            // added last
            for (ResourceOrder order : ResourceOrder.values()) {
                idMap.put(order.id, order.value);
            }
        }

        return idMap;
    }

    /**
     * returns the singleton instance of the map
     * 
     * @return
     */
    private static synchronized Map<String, String> getRscMap() {
        if (rscMap == null) {
            rscMap = new LinkedHashMap<String, String>();

            IExtensionRegistry registry = Platform.getExtensionRegistry();
            if (registry == null)
                return rscMap;

            IExtensionPoint point = registry
                    .getExtensionPoint(RESOURCE_EXTENSION);
            if (point == null)
                return null;
            IExtension[] extensions = point.getExtensions();

            for (IExtension ext : extensions) {
                IConfigurationElement[] config = ext.getConfigurationElements();

                for (IConfigurationElement cfg : config) {
                    String className = cfg.getAttribute("class");
                    String id = cfg.getAttribute("renderingOrderId");
                    rscMap.put(className, id);
                }
            }
        }

        return rscMap;
    }

    /**
     * Looks up the rendering order by ID
     * 
     * @param id
     * @return the rendering order
     */
    public static ResourceOrder getRenderingOrder(String id) {
        Integer order = getIdMap().get(id);
        if (order == null) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unknown resource ordering: " + id);

            return ResourceOrder.UNKNOWN;
        }
        return new ResourceOrder(id, order);
    }

    public static ResourceOrder getRenderingOrder(AbstractVizResource<?, ?> rsc) {
        Class<?> clazz = rsc.getClass();
        while (clazz.equals(Object.class) == false) {
            if (getRscMap().get(clazz.getName()) != null) {
                return getRenderingOrder(getRscMap().get(clazz.getName()));
            } else {
                clazz = clazz.getSuperclass();
            }
        }

        statusHandler.handle(Priority.VERBOSE,
                "Could not find order id for class heirarchy of: "
                        + rsc.getClass());

        return ResourceOrder.UNKNOWN;
    }
}
