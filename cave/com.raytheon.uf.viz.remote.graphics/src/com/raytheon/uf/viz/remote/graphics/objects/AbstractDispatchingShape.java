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

import java.util.Arrays;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.TransformFactory;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.IShape;
import com.raytheon.uf.viz.remote.graphics.Activator;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.DispatchingObject;
import com.raytheon.uf.viz.remote.graphics.events.DisposeObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;

/**
 * Abstract IShape object
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

public abstract class AbstractDispatchingShape<T extends IShape> extends
        DispatchingObject<T> implements IShape {

    private MathTransform worldToTargetGrid;

    private boolean dirty = true;

    /**
     * @param targetObject
     * @param dispatcher
     */
    public AbstractDispatchingShape(T targetObject, Dispatcher dispatcher,
            GeneralGridGeometry targetGeometry) {
        super(targetObject, dispatcher);
        try {
            worldToTargetGrid = TransformFactory.worldToGrid(targetGeometry,
                    PixelInCell.CELL_CENTER);
        } catch (FactoryException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    "Error getting transform from base crs to target grid", e);
        }
    }

    /**
     * 
     * @see com.raytheon.uf.viz.core.drawables.IShape#compile()
     */
    public void compile() {
        wrappedObject.compile();
        // flush data
        flushState();
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.drawables.IShape#isMutable()
     */
    public boolean isMutable() {
        return wrappedObject.isMutable();
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.drawables.IShape#isDrawable()
     */
    public boolean isDrawable() {
        return wrappedObject.isDrawable();
    }

    /**
     * 
     * @see com.raytheon.uf.viz.core.drawables.IShape#dispose()
     */
    public void dispose() {
        wrappedObject.dispose();
        // Send dispose event
        dispatch(RemoteGraphicsEventFactory.createEvent(
                DisposeObjectEvent.class, this));
    }

    /**
     * 
     * @see com.raytheon.uf.viz.core.drawables.IShape#reset()
     */
    public void reset() {
        wrappedObject.reset();
    }

    /**
     * Mark that the state of the object is dirty and needs to be flushed
     */
    protected void markDirty() {
        dirty = true;
    }

    /**
     * Flushes the internal state of the object if it is dirty
     */
    public final void flushState() {
        if (dirty) {
            flushInternalState();
            dirty = false;
        }
    }

    /**
     * Extending classes should flush their internal state here by sending an
     * event that contains the updated state
     */
    protected abstract void flushInternalState();

    /**
     * Convert a "world" pixel to the target geometry grid space
     * 
     * @param world
     * @return
     * @throws TransformException
     */
    protected double[] worldToPixel(double[] world) throws TransformException {
        if (worldToTargetGrid != null) {
            double[] out = new double[world.length];
            worldToTargetGrid.transform(world, 0, out, 0, 1);
            return out;
        } else {
            return Arrays.copyOf(world, world.length);
        }

    }
}
