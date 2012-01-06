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
package com.raytheon.viz.core.rsc.hdf5;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.collections.keyvalue.MultiKey;
import org.geotools.coverage.grid.GeneralGridGeometry;

/**
 * Factory for ImageTileLists.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 2, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TileListFactory {

    private static Map<MultiKey, ImageTileList> cacheMap = new HashMap<MultiKey, ImageTileList>();

    /**
     * Get the ImageTileList for the levels/size and geometry.
     * ImageTileList.use() will be called automatically when using this method
     * 
     * @param levels
     * @param tileSize
     * @param geometry
     * @return
     */
    public static synchronized ImageTileList getTileList(int levels,
            int tileSize, GeneralGridGeometry dataGeom,
            GeneralGridGeometry targetGeom) {
        final MultiKey key = new MultiKey(levels, tileSize, dataGeom,
                targetGeom);
        ImageTileList list = cacheMap.get(key);
        if (list == null) {
            list = new ImageTileList();
            cacheMap.put(key, list);
        } else {
            list.use();
        }
        return list;
    }

}
