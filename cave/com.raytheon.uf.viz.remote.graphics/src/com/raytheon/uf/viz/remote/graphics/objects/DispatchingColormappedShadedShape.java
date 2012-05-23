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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension.IColormapShadedShape;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.events.shapes.AbstractShadedShapeData.DataSpace;
import com.raytheon.uf.viz.remote.graphics.events.shapes.ColormappedShadedShapeDataEvent;
import com.raytheon.uf.viz.remote.graphics.events.shapes.ColormappedShadedShapeDataEvent.ColormappedShadedGeometryData;
import com.raytheon.uf.viz.remote.graphics.events.shapes.ColormappedShadedShapeDataEvent.ColormappedShadedShapeData;
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
        AbstractDispatchingShape<IColormapShadedShape> implements
        IColormapShadedShape {

    private int currentId = 0;

    private Map<Object, Integer> keyMap = new HashMap<Object, Integer>();

    private ColormappedShadedShapeDataEvent updateEvent;

    /**
     * @param targetObject
     * @param dispatcher
     * @param targetGeometry
     */
    public DispatchingColormappedShadedShape(IColormapShadedShape targetObject,
            Dispatcher dispatcher) {
        super(targetObject, dispatcher);
        updateEvent = createNewUpdateEvent();
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
        if (updateEvent != null) {
            updateEvent.addShapeData(DataSpace.WORLD, lineString,
                    getKey(colorKey));
            markDirty();
        }
    }

    /**
     * @param contours
     * @param colorKey
     * @see com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension.IColormapShadedShape#addPolygonPixelSpace(com.vividsolutions.jts.geom.LineString[],
     *      java.lang.Object)
     */
    public void addPolygonPixelSpace(LineString[] contours, Object colorKey) {
        wrappedObject.addPolygonPixelSpace(contours, colorKey);
        if (updateEvent != null) {
            updateEvent.addShapeData(DataSpace.PIXEL, contours,
                    getKey(colorKey));
            markDirty();
        }
    }

    /**
     * @param geometry
     * @param colorKey
     * @see com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension.IColormapShadedShape#addGeometry(com.vividsolutions.jts.geom.Geometry,
     *      java.lang.Object)
     */
    public void addGeometry(Geometry geometry, Object colorKey) {
        wrappedObject.addGeometry(geometry, colorKey);
        if (updateEvent != null) {
            updateEvent.addGeometryData(geometry, getKey(colorKey));
            markDirty();
        }
    }

    private Integer getKey(Object colorKey) {
        synchronized (keyMap) {
            Integer keyId = keyMap.get(colorKey);
            if (keyId == null) {
                keyId = currentId++;
                keyMap.put(colorKey, keyId);
            }
            return keyId;
        }
    }

    public Map<Object, Integer> getKeyMap() {
        synchronized (keyMap) {
            return new HashMap<Object, Integer>(keyMap);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.remote.graphics.objects.AbstractDispatchingShape#
     * compile()
     */
    @Override
    public void compile() {
        updateEvent.setCompile(true);
        // super.compile() will flush the state
        super.compile();
        // Event if original shape was mutable, once compiled we should not
        // accept more data
        updateEvent = null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.remote.graphics.objects.AbstractDispatchingShape#
     * reset()
     */
    @Override
    public void reset() {
        super.reset();
        updateEvent = createNewUpdateEvent();
        markDirty();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.remote.graphics.objects.AbstractDispatchingShape#
     * dispose()
     */
    @Override
    public void dispose() {
        super.dispose();
        keyMap.clear();
        updateEvent = null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.remote.graphics.objects.AbstractDispatchingShape#
     * flushInternalState()
     */
    @Override
    protected void flushInternalState() {
        if (updateEvent != null) {
            ColormappedShadedShapeDataEvent toSend = createNewUpdateEvent();
            toSend.setShapeData(new ArrayList<ColormappedShadedShapeData>(
                    updateEvent.getShapeData()));
            toSend.setGeometryData(new ArrayList<ColormappedShadedGeometryData>(
                    updateEvent.getGeometryData()));
            dispatch(toSend);
        }
    }

    private ColormappedShadedShapeDataEvent createNewUpdateEvent() {
        return RemoteGraphicsEventFactory.createEvent(
                ColormappedShadedShapeDataEvent.class, this);
    }

}
