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

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.events.shapes.SetShadedShapeFillPattern;
import com.raytheon.uf.viz.remote.graphics.events.shapes.ShadedShapeDataEvent;
import com.raytheon.uf.viz.remote.graphics.events.shapes.ShadedShapeDataEvent.DataSpace;
import com.raytheon.uf.viz.remote.graphics.events.shapes.ShadedShapeDataEvent.ShadedShapeData;
import com.vividsolutions.jts.geom.LineString;

/**
 * Object that wraps an IShadedShape and dispatches key events
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 15, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DispatchingShadedShape extends
        AbstractDispatchingShape<IShadedShape> implements IShadedShape {

    private ShadedShapeDataEvent updateEvent;

    /**
     * @param targetObject
     * @param dispatcher
     */
    public DispatchingShadedShape(IShadedShape targetObject,
            Dispatcher dispatcher) {
        super(targetObject, dispatcher);
        updateEvent = createNewUpdateEvent();
    }

    /**
     * @param lineString
     * @param color
     * @see com.raytheon.uf.viz.core.drawables.IShadedShape#addPolygon(com.vividsolutions.jts.geom.LineString[],
     *      org.eclipse.swt.graphics.RGB)
     */
    public void addPolygon(LineString[] lineString, RGB color) {
        wrappedObject.addPolygon(lineString, color);
        if (updateEvent != null) {
            updateEvent.addShapeData(DataSpace.WORLD, lineString, color);
            markDirty();
        }
    }

    /**
     * @param contours
     * @param color
     * @see com.raytheon.uf.viz.core.drawables.IShadedShape#addPolygonPixelSpace(com.vividsolutions.jts.geom.LineString[],
     *      org.eclipse.swt.graphics.RGB)
     */
    public void addPolygonPixelSpace(LineString[] contours, RGB color) {
        wrappedObject.addPolygonPixelSpace(contours, color);
        if (updateEvent != null) {
            updateEvent.addShapeData(DataSpace.PIXEL, contours, color);
            markDirty();
        }
    }

    /**
     * @param pattern
     * @see com.raytheon.uf.viz.core.drawables.IShadedShape#setFillPattern(byte[])
     */
    public void setFillPattern(byte[] pattern) {
        wrappedObject.setFillPattern(pattern);
        SetShadedShapeFillPattern event = RemoteGraphicsEventFactory
                .createEvent(SetShadedShapeFillPattern.class, this);
        event.setFillPattern(pattern);
        dispatch(event);
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
            ShadedShapeDataEvent toSend = updateEvent;
            if (wrappedObject.isMutable()) {
                toSend = createNewUpdateEvent();
                toSend.setShapeData(new ArrayList<ShadedShapeData>(updateEvent
                        .getShapeData()));
            } else {
                updateEvent = null;
            }
            dispatch(toSend);
        }
    }

    private ShadedShapeDataEvent createNewUpdateEvent() {
        return RemoteGraphicsEventFactory.createEvent(
                ShadedShapeDataEvent.class, this);
    }
}
