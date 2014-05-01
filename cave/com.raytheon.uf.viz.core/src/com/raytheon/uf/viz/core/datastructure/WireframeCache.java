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

package com.raytheon.uf.viz.core.datastructure;

import java.util.HashMap;
import java.util.Map;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.viz.core.drawables.IWireframeShape;

/**
 * WireframeCache is a simple internal cache to reduce duplicate vector loading.
 * 
 * Wireframe sets are considered to be equivalent if they came from the same
 * file name and were generated against the same grid geometry (native
 * projection).
 * 
 * <B>Typical Lifecycle:</B>
 * <OL>
 * <LI>Call checkWireframe(...) to determine if the vector set has already been
 * compiled. If it has, this will return the vector set.
 * <LI>If no vector set is returned, create one, and register it with
 * registerWireframe(...)
 * <LI>When ready to dispose, call unregisterWireframe(). <B>NOTE:</B> Never
 * dispose of wireframes directly, Use the unregisterWireframe(...) method for
 * disposal.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2007            chammack    Initial Creation.	
 * Nov 14, 2007            chammack    Refactored for general usage
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class WireframeCache {

    /** The singleton instance */
    private static WireframeCache instance;

    /**
     * Contains a count of the number of displays currently viewing the vectors
     */
    private final Map<Key, Integer> countMap;

    /** Contains a mapping to an actual renderable object */
    private final Map<Key, IWireframeShape> refMap;

    /**
     * Internal key structure representation
     * 
     */
    private static class Key {
        public GeneralGridGeometry gridGeometry;

        public String shapefile;

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result
                    + ((gridGeometry == null) ? 0 : gridGeometry.hashCode());
            result = prime * result
                    + ((shapefile == null) ? 0 : shapefile.hashCode());
            return result;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            Key other = (Key) obj;
            if (gridGeometry == null) {
                if (other.gridGeometry != null) {
                    return false;
                }
            } else if (!gridGeometry.equals(other.gridGeometry)) {
                return false;
            }
            if (shapefile == null) {
                if (other.shapefile != null) {
                    return false;
                }
            } else if (!shapefile.equals(other.shapefile)) {
                return false;
            }
            return true;
        }

    }

    /**
     * Get the singleton instance of the cache
     * 
     * @return
     */
    public static synchronized WireframeCache getInstance() {
        if (instance == null) {
            instance = new WireframeCache();
        }

        return instance;

    }

    /**
     * Private constructor
     */
    private WireframeCache() {
        countMap = new HashMap<Key, Integer>();
        refMap = new HashMap<Key, IWireframeShape>();
    }

    /**
     * Check to see if a wireframe shape is currently in use for a shapefile
     * with a specified grid geometry (projection)
     * 
     * @param shapefile
     *            the shapefile's name
     * @param geom
     *            the geometry/projection used to generate the vectors
     * @return a wireframe shape, if available, else null.
     */
    public IWireframeShape checkWireframe(String shapefile,
            GeneralGridGeometry geom) {
        synchronized (this) {
            Key key = new Key();
            key.gridGeometry = geom;
            key.shapefile = shapefile;
            Integer count = countMap.get(key);

            if (count != null) {
                count = count + 1;

                countMap.put(key, count);
                return refMap.get(key);
            }

            return null;
        }
    }

    /**
     * Register a wireframe shape associated with a shapefile and grid
     * geometry/projection
     * 
     * @param shapefile
     *            the shapefile used to generate the vectors
     * @param geom
     *            the geometry used to generate the vectors
     * @param shape
     *            the generated vector shape
     */
    public void registerWireframe(String shapefile, GeneralGridGeometry geom,
            IWireframeShape shape) {
        synchronized (this) {
            Key key = new Key();
            key.gridGeometry = geom;
            key.shapefile = shapefile;
            Integer count = countMap.get(key);
            if (count == null) {
                count = new Integer(1);
            } else {
                count = count + 1;
            }

            countMap.put(key, count);
            refMap.put(key, shape);
        }
    }

    /**
     * Release the handle to a given wireframe vector set.
     * 
     * If the vectors are determined to no longer be in use, it will be
     * disposed.
     * 
     * NOTE: Never dispose of wireframe shapes in cache directly, always
     * unregister them.
     * 
     * @param shapefile
     *            the shapefile
     * @param geom
     *            the grid geometry/projection
     */
    public void unregisterWireframe(String shapefile, GeneralGridGeometry geom) {
        synchronized (this) {
            Key key = new Key();
            key.gridGeometry = geom;
            key.shapefile = shapefile;
            Integer count = countMap.get(key);
            if (count == null) {
                return;
            }

            if (count == 1) {
                countMap.remove(key);
                IWireframeShape shape = refMap.get(key);
                shape.dispose();
                return;
            }

            count = count - 1;
            countMap.put(key, count);

        }
    }
}
