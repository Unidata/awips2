package com.raytheon.uf.viz.monitor.ffmp.ui.rsc;

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

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension.IColormapShadedShape;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;

/**
 * Shaded shape container
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 15 Sept, 2011 10899     dhladky     Initial creation
 * 27 June, 2013 2152      njensen     Added dispose()
 * Sep 23,  2014 3009      njensen     Cleaned up
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class FFMPShapeContainer {

    /** always the same vertexes, one for each CWA **/
    private ConcurrentMap<String, ConcurrentMap<String, IColormapShadedShape>> shadedShapes = null;

    // public constructor
    public FFMPShapeContainer() {
        shadedShapes = new ConcurrentHashMap<String, ConcurrentMap<String, IColormapShadedShape>>();
    }

    /**
     * Retrieves the shaded shape if it exists, or builds and caches it if it
     * does not exist
     * 
     * @param cwa
     * @param huc
     * @param target
     * @param descriptor
     * @return the shaded shape
     */
    public IColormapShadedShape getShape(String cwa, String huc,
            IGraphicsTarget target, MapDescriptor descriptor)
            throws VizException {

        IColormapShadedShape shape = getDrawableShape(cwa, huc);

        if (shape == null) {
            shape = target.getExtension(IColormapShadedShapeExtension.class)
                    .createColormapShadedShape(descriptor.getGridGeometry(),
                            true);

            ConcurrentMap<String, IColormapShadedShape> cwaShapes = shadedShapes
                    .get(cwa);

            if (cwaShapes == null) {
                cwaShapes = new ConcurrentHashMap<String, IColormapShadedShape>();
                shadedShapes.put(cwa, cwaShapes);
            }

            cwaShapes.put(huc, shape);
        }

        return shape;
    }

    /**
     * Retrieves the shaded shape from the cache
     * 
     * @param cwa
     * @param huc
     * @return the shape if it exists, otherwise null
     */
    public IColormapShadedShape getDrawableShape(String cwa, String huc) {
        IColormapShadedShape shape = null;
        if (cwa != null) {
            ConcurrentMap<String, IColormapShadedShape> innerMap = shadedShapes
                    .get(cwa);
            if (innerMap != null && huc != null) {
                shape = innerMap.get(huc);
            }
        }

        return shape;
    }

    /**
     * Disposes of each of the shaded shapes and clears the maps
     */
    public void dispose() {
        for (ConcurrentMap<String, IColormapShadedShape> innerMap : shadedShapes
                .values()) {
            for (IColormapShadedShape shp : innerMap.values()) {
                shp.dispose();
            }
            innerMap.clear();
        }
        shadedShapes.clear();
    }

}
