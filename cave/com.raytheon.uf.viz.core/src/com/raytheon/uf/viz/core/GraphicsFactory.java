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

package com.raytheon.uf.viz.core;

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
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * Factory for getting graphics factory adapters
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 19, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class GraphicsFactory {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GraphicsFactory.class);

    private static final String DEFAULT = "default";

    private static Map<String, AbstractGraphicsFactoryAdapter> adapters;

    static {
        adapters = new HashMap<String, AbstractGraphicsFactoryAdapter>();

        // Construct the resource mapping from Eclipse plugins
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry
                .getExtensionPoint("com.raytheon.uf.viz.core.graphicsFactory");
        if (point != null) {
            IExtension[] extensions = point.getExtensions();

            for (IExtension ext : extensions) {
                IConfigurationElement[] config = ext.getConfigurationElements();
                for (IConfigurationElement ele : config) {
                    String id = ele.getAttribute("id");
                    try {
                        AbstractGraphicsFactoryAdapter adapter = (AbstractGraphicsFactoryAdapter) ele
                                .createExecutableExtension("factoryClass");
                        adapters.put(id, adapter);
                    } catch (CoreException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Could not instantiate graphics adapter: "
                                        + ele.getAttribute("factoryClass"));

                    }
                }
            }

            if (adapters.containsKey(DEFAULT) == false) {
                statusHandler.handle(Priority.PROBLEM,
                        "No default graphics factory adapter found");
            }

        }
    }

    /**
     * Get the default graphics adapter using type 2D
     * 
     * @return
     * @throws VizException
     */
    public static AbstractGraphicsFactoryAdapter getGraphicsAdapter() {
        return getGraphicsAdapter(DEFAULT);
    }

    /**
     * Get the graphics adapter specified by the id and type
     * 
     * @param id
     * @return
     * @throws VizException
     */
    public static AbstractGraphicsFactoryAdapter getGraphicsAdapter(String id) {
        AbstractGraphicsFactoryAdapter adapter = adapters.get(id);
        if (adapter == null) {
            throw new RuntimeException(
                    "Could not find graphics adapter with id: " + id);
        }
        return adapter;
    }

}
