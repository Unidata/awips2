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
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.status.StatusConstants;

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
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(GraphicsFactory.class);

    public static enum GraphicsType {
        TWO_D("2D"), THREE_D("3D");

        public String id;

        private GraphicsType(String type) {
            this.id = type;
        }

        public static GraphicsType getType(String id) {
            for (GraphicsType type : GraphicsType.values()) {
                if (type.id.equals(id)) {
                    return type;
                }
            }
            return null;
        }
    }

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
                    String type = ele.getAttribute("type");
                    String[] types = type == null ? new String[] {
                            GraphicsType.TWO_D.id, GraphicsType.THREE_D.id }
                            : new String[] { type };
                    for (String tp : types) {
                        GraphicsType gt = GraphicsType.getType(tp);
                        if (gt != null) {
                            AbstractGraphicsFactoryAdapter adapter;
                            try {
                                adapter = (AbstractGraphicsFactoryAdapter) ele
                                        .createExecutableExtension("factoryClass");
                                adapter.setType(gt);
                                adapters.put(id + ":" + tp, adapter);
                            } catch (CoreException e) {
                                statusHandler.handle(
                                        Priority.PROBLEM,
                                        "Could not instantiate graphics adapter: "
                                                + ele.getAttribute("factoryClass"));

                            }
                        } else {
                            statusHandler.handle(Priority.PROBLEM,
                                    "Graphics type, " + type
                                            + " is not a valid type");
                        }
                    }
                }
            }

            if (adapters.containsKey(DEFAULT + ":" + GraphicsType.TWO_D.id) == false) {
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
    public static AbstractGraphicsFactoryAdapter getGraphicsAdapter()
            throws VizException {
        return getGraphicsAdapter(DEFAULT, GraphicsType.TWO_D.id);
    }

    /**
     * Get the default graphics adapter given the graphics type. A null type
     * will default to 2D
     * 
     * @return
     * @throws VizException
     */
    public static AbstractGraphicsFactoryAdapter getGraphicsAdapter(String type)
            throws VizException {
        return getGraphicsAdapter(DEFAULT, type != null ? type
                : GraphicsType.TWO_D.id);
    }

    /**
     * Get the graphics adapter specified by the id and type
     * 
     * @param id
     * @return
     * @throws VizException
     */
    public static AbstractGraphicsFactoryAdapter getGraphicsAdapter(String id,
            String type) throws VizException {
        AbstractGraphicsFactoryAdapter adapter = adapters.get(id + ":" + type);
        if (adapter == null) {
            throw new VizException("Could not find graphics adapter with id: "
                    + id + " and type: " + type);
        }
        return adapter;
    }

    /**
     * Get the graphics type string for the descriptor (2D or 3D)
     * 
     * @param descriptor
     * @return
     */
    public static String getType(IDescriptor descriptor) {
        String type = GraphicsType.TWO_D.id;
        if (descriptor.getGridGeometry().getDimension() == 3) {
            type = GraphicsType.THREE_D.id;
        }
        return type;
    }
}
