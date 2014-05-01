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

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.ResourceList;

/**
 * This class encapsulates the storage of overlay and/or background maps
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

public class MapStore {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MapStore.class);

    public static class MapNode {
        private final String name;

        private final String path;

        private List<MapNode> subTree;

        public MapNode(String name, String path) {
            this.name = name;
            this.path = path;
        }

        private void add(MapNode node) {
            if (this.subTree == null) {
                subTree = new ArrayList<MapNode>();
            }
            subTree.add(node);
        }

        /**
         * @return the name
         */
        public String getName() {
            return name;
        }

        /**
         * @return the path
         */
        public String getPath() {
            return path;
        }

        /**
         * Searches the tree for the specified map name
         * 
         * @param name
         *            name of map to be found
         * @return path of desired map or null if not found
         */
        public String findPath(String name) {
            if (this.name.equals(name)) {
                return path;

            } else if (subTree != null) {
                for (MapNode node : subTree) {
                    String path = node.findPath(name);
                    if (path != null) {
                        return path;
                    }
                }
            }
            return null;
        }

        /**
         * Searches the tree for the map path
         * 
         * @param path
         *            path of the bundle
         * @return name of the specified map
         */
        public String findName(String path) {
            if (this.path.equals(path)) {
                return this.name;

            } else if (subTree != null) {
                for (MapNode node : subTree) {
                    String name = node.findName(path);
                    if (name != null) {
                        return name;
                    }
                }
            }
            return null;
        }

        /**
         * Searches the tree for the specified map bundle name
         * 
         * @param bundleName
         *            name of map to be found
         * @return path of desired map bundle or null if not found
         */
        public String findBundlePath(String bundleName) {
            if (this.path.endsWith(bundleName)) {
                return path;

            } else if (subTree != null) {
                for (MapNode node : subTree) {
                    String path = node.findBundlePath(bundleName);
                    if (path != null) {
                        return path;
                    }
                }
            }
            return null;
        }

        /**
         * @return the subTree
         */
        public List<MapNode> getSubTree() {
            return subTree;
        }
    }

    private static MapNode mapTree;

    private static MapNode loadMaps(String name, String path) {

        MapNode maps = new MapNode(name, path);

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile[] files = pm.listStaticFiles(path,
                new String[] { ".xml" }, false, false);

        Map<String, String> variables = getVariables();
        for (LocalizationFile lf : files) {
            File file = lf.getFile();
            if (file.isFile() && file.exists() && file.canRead()) {
                try {
                    Bundle bundle = Bundle.unmarshalBundle(file, variables);
                    AbstractRenderableDisplay[] displays = bundle.getDisplays();
                    if (displays.length > 0) {
                        ResourceList resourceList = displays[0].getDescriptor()
                                .getResourceList();
                        for (ResourcePair rp : resourceList) {
                            if (rp.getResourceData().getNameGenerator() != null) {
                                String mapName = rp.getResourceData()
                                        .getNameGenerator().getName(null);
                                maps.add(new MapNode(mapName, lf.getName()));
                            }
                        }
                    }
                } catch (Throwable e) {
                    statusHandler.handle(
                            Priority.PROBLEM,
                            "Error loading map bundle: "
                                    + file.getAbsolutePath(), e);
                }
            } else if (file.isDirectory()) {
                if (!path.equals(lf.getName())) {
                    maps.add(loadMaps(file.getName(), lf.getName()));
                }
            }
        }

        if (maps.getSubTree() != null) {
            Collections.sort(maps.getSubTree(), new Comparator<MapNode>() {

                @Override
                public int compare(MapNode node1, MapNode node2) {
                    return node1.getName().compareTo(node2.getName());
                }

            });
        }

        return maps;
    }

    /**
     * Finds the map path given the specified name
     * 
     * @param mapName
     *            map to be found
     * @return localization path of desired map or null if not found
     */
    public static String findMapPath(String mapName) {
        return getMapTree().findPath(mapName);
    }

    /**
     * Finds the map path given the specified bundle name
     * 
     * @param mapBundleName
     * @return localization path of desired map bundle or null if not found
     */
    public static String findBundlePath(String mapBundleName) {
        String bundleName = File.separatorChar
                + mapBundleName.replace("<site>", "site");
        if (bundleName.endsWith(".xml")) {
            bundleName = bundleName.substring(0, bundleName.length() - 4);
        }

        String suffix = "_" + getVariables().get("site");
        if (bundleName.endsWith(suffix)) {
            bundleName = bundleName.substring(0,
                    bundleName.length() - suffix.length())
                    + "_site";
        }
        bundleName += ".xml";
        return getMapTree().findBundlePath(bundleName);
    }

    /**
     * Finds the map name given the map path
     * 
     * @param path
     *            the path of the desired map
     * @return the name of the map
     */
    public static String findMapName(String path) {
        return getMapTree().findName(path);
    }

    /**
     * Returns a tree structured list of maps suitable for constructing a map
     * menu
     * 
     * @return the map tree
     */
    public static synchronized MapNode getMapTree() {
        if (mapTree == null) {
            // retrieve all available maps from the bundles/maps directory
            mapTree = loadMaps("", "bundles/maps");

        }
        return mapTree;
    }

    /**
     * Retrieve the desired map
     * 
     * @param mapPath
     *            path of map to be retrieved
     * @return ResourcePair containing the map
     * @throws VizException
     */
    public static ResourcePair retrieveMap(String mapPath) throws VizException {
        Bundle bundle = Bundle.unmarshalBundle(PathManagerFactory
                .getPathManager().getStaticFile(mapPath), getVariables());

        if ((bundle.getDisplays().length > 0)
                && (bundle.getDisplays()[0].getDescriptor().getResourceList()
                        .size() > 0)) {
            for (ResourcePair rp : bundle.getDisplays()[0].getDescriptor()
                    .getResourceList()) {
                // return first non system resource
                if (!rp.getProperties().isSystemResource()) {
                    return rp;
                }
            }
        }
        return null;
    }

    private static Map<String, String> getVariables() {
        String site = LocalizationManager.getInstance().getSite();

        Map<String, String> vars = new HashMap<String, String>();
        vars.put("site", site);
        return vars;
    }
}
