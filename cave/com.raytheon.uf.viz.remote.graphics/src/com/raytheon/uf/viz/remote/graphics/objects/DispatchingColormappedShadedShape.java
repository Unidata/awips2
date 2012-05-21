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
package com.raytheon.uf.viz.remote.graphics.objects;

import java.util.Collection;

import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension.IColormapShadedShape;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.DispatchingObject;
import com.raytheon.uf.viz.remote.graphics.events.DisposeObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 18, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DispatchingColormappedShadedShape extends
        DispatchingObject<IColormapShadedShape> implements IColormapShadedShape {

    private boolean dirty = true;

    /**
     * @param targetObject
     * @param dispatcher
     * @param targetGeometry
     */
    public DispatchingColormappedShadedShape(IColormapShadedShape targetObject,
            Dispatcher dispatcher) {
        super(targetObject, dispatcher);
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension.IColormapShadedShape#getColorKeys()
     */
    public Collection<Object> getColorKeys() {
        return wrappedObject.getColorKeys();
    }

    /**
     * @param lineString
     * @param colorKey
     * @see com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension.IColormapShadedShape#addPolygon(com.vividsolutions.jts.geom.LineString[],
     *      java.lang.Object)
     */
    public void addPolygon(LineString[] lineString, Object colorKey) {
        wrappedObject.addPolygon(lineString, colorKey);
    }

    /**
     * @param contours
     * @param colorKey
     * @see com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension.IColormapShadedShape#addPolygonPixelSpace(com.vividsolutions.jts.geom.LineString[],
     *      java.lang.Object)
     */
    public void addPolygonPixelSpace(LineString[] contours, Object colorKey) {
        wrappedObject.addPolygonPixelSpace(contours, colorKey);
    }

    /**
     * @param geometry
     * @param colorKey
     * @see com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension.IColormapShadedShape#addGeometry(com.vividsolutions.jts.geom.Geometry,
     *      java.lang.Object)
     */
    public void addGeometry(Geometry geometry, Object colorKey) {
        wrappedObject.addGeometry(geometry, colorKey);
    }

    /**
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension.IColormapShadedShape#dispose()
     */
    public void dispose() {
        wrappedObject.dispose();
        dispatch(RemoteGraphicsEventFactory.createEvent(
                DisposeObjectEvent.class, this));
    }

}
