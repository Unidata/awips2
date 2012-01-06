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
package com.raytheon.uf.viz.core.maps;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.Validate;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.Capabilities;
import com.raytheon.viz.ui.VizWorkbenchManager;

/**
 * This class maintains a list of overlay and/or background maps loaded in the
 * associated map descriptor and provides interfaces to load and unload maps.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 12, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class MapManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MapManager.class);

    private static Map<IMapDescriptor, MapManager> instanceMap;

    public static synchronized MapManager getInstance(
            IMapDescriptor mapDescriptor) {

        if (instanceMap == null) {
            instanceMap = new HashMap<IMapDescriptor, MapManager>();
        }

        MapManager mapMgr = instanceMap.get(mapDescriptor);
        if (mapMgr == null) {
            mapMgr = new MapManager(mapDescriptor);
            instanceMap.put(mapDescriptor, mapMgr);
        }
        return mapMgr;
    }

    private IMapDescriptor mapDescriptor;

    /**
     * Constructor - it is package private as it should only be called from
     * classes implementing IMapDescriptor
     * 
     * @param mapDescriptor
     */
    private MapManager(IMapDescriptor mapDescriptor) {
        this.mapDescriptor = mapDescriptor;
    }

    /**
     * Load a map into the specified target by name
     * 
     * @param mapName
     *            name of map to be loaded
     * 
     * @return the map resource pair that was added to the descriptor and
     *         initialized or null if no map found with the specified name
     */
    public ResourcePair loadMapByName(final String mapName) {
        String mapPath = MapStore.findMapPath(mapName);
        if (mapPath != null) {
            return loadMap(mapPath);
        } else {
            statusHandler.handle(Priority.PROBLEM, "Map \"" + mapName
                    + "\" not found.");
            return null;
        }
    }

    /**
     * Load a map into the specified target by bundle name
     * 
     * @param mapBundleName
     *            name of map bundle to be loaded
     * 
     * @return the map resource pair that was added to the descriptor and
     *         initialized or null if no map found with the specified name
     */
    public ResourcePair loadMapByBundleName(final String mapBundleName) {
        String mapPath = MapStore.findBundlePath(mapBundleName);
        if (mapPath != null) {
            return loadMap(mapPath);
        } else {
            statusHandler.handle(Priority.PROBLEM, "Map bundle \""
                    + mapBundleName + "\" not found.");
            return null;
        }
    }

    public ResourcePair loadMap(final String mapPath,
            IDisplayPaneContainer container) {
        Validate.notNull(mapPath);

        // check to see if map already loaded
        String mapName = MapStore.findMapName(mapPath);
        ResourcePair rpAll = findMapInAllPanes(mapName, container);
        ResourcePair rp = findMap(mapName);
        if (rp != null) {
            return rp;
        }

        try {
            rp = MapStore.retrieveMap(mapPath);

            // pull out the bundle resources
            // this assumes a map bundle has only a single descriptor
            rp.instantiateResource(mapDescriptor);
            AbstractVizResource<?, ?> rsc = rp.getResource();

            // retrieve map style preferences
            String perspective = getPerspective();
            Capabilities capabilities = Activator.getDefault()
                    .getStylePreferences().get(perspective, mapName);

            for (AbstractCapability capability : capabilities) {
                rsc.getCapabilities().addCapability(capability);
            }

            mapDescriptor.getResourceList().add(rp);

            if (rpAll != null) {
                rp.getLoadProperties().setCapabilities(
                        rpAll.getLoadProperties().getCapabilities());
            }

            rsc.issueRefresh();

        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Error loading bundle", e);
        }
        return rp;
    }

    /**
     * Load a map into the specified target
     * 
     * @param mapPath
     *            name of map to be loaded
     * @return the map resource pair that was added to the descriptor and
     *         initialized
     */
    public ResourcePair loadMap(final String mapPath) {
        return loadMap(mapPath, null);
    }

    public String getPerspective() {
        IWorkbenchWindow window = VizWorkbenchManager.getInstance()
                .getCurrentWindow();
        if (window != null) {
            IWorkbenchPage page = window.getActivePage();
            if (page != null) {
                IPerspectiveDescriptor desc = page.getPerspective();
                if (desc != null) {
                    return desc.getId();
                }
            }
        }
        return null;
    }

    /**
     * Unload a map from the associated map descriptor
     * 
     * @param mapName
     *            name of map to be unloaded
     */
    public void unloadMap(String mapName) {
        ResourcePair map = findMap(mapName);

        if (map == null) {
            return;
        }

        // unload map from the current mapDescriptor
        AbstractVizResource<?, ?> rsc = map.getResource();
        mapDescriptor.getResourceList().removeRsc(rsc);
    }

    /**
     * Toggle the state of (load or unload) the specified map
     * 
     * @param mapName
     *            name of map to be loaded/unloaded
     */
    public void toggleMap(final String mapName, final String mapPath,
            IDisplayPaneContainer container) {
        if (isMapLoaded(mapName)) {
            unloadMap(mapName);
        } else {
            loadMap(mapPath, container);
        }
    }

    /**
     * Determine if the specified map is loaded in the specified map descriptor
     * 
     * @param mapName
     *            name of map in question
     * @return true if map is currently loaded in the associated map descriptor
     */
    public boolean isMapLoaded(final String mapName) {
        return findMap(mapName) != null;
    }

    /**
     * @param mapName
     * @return
     */
    private ResourcePair findMap(final String mapName) {
        ResourcePair map = null;
        for (ResourcePair rp : mapDescriptor.getResourceList()) {
            if (rp.getResource() != null && rp.getResource().getName() != null
                    && rp.getResource().getName().equals(mapName)) {
                map = rp;
                break;
            }
        }
        return map;
    }

    /**
     * @param mapName
     * @return
     */
    private ResourcePair findMapInAllPanes(final String mapName,
            IDisplayPaneContainer container) {
        if (container == null) {
            return null;
        }
        ResourcePair map = null;
        // search through the panes
        for (IDisplayPane pane : container.getDisplayPanes()) {
            IDescriptor desc = pane.getDescriptor();
            if (desc instanceof IMapDescriptor) {
                IMapDescriptor mapDesc = (IMapDescriptor) desc;
                if (instanceMap.get(mapDesc) != null) {
                    for (ResourcePair rp : mapDesc.getResourceList()) {
                        if (rp.getResource() != null
                                && rp.getResource().getName() != null
                                && rp.getResource().getName().equals(mapName)) {
                            map = rp;
                            break;
                        }
                    }
                }
            }
        }
        return map;
    }

    /**
     * @param prefs
     * @param name
     */
    public void saveStylePreferences(String mapName, Capabilities prefs) {
        Activator.getDefault().getStylePreferences()
                .put(getPerspective(), mapName, prefs);
    }
}
