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

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.remote.graphics.DispatchGraphicsTarget;

/**
 * View that wraps a graphics view. This class exists because we can't pass in a
 * DispatchGraphcisTarget to a view expecting a different type. We must pass in
 * the dispatching targets wrapped target
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 7, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ViewWrapper implements IView {

    private IView delegate;

    public ViewWrapper(IView delegate) {
        this.delegate = delegate;
    }

    private IGraphicsTarget getDelegateTarget(IGraphicsTarget target) {
        if (target instanceof DispatchGraphicsTarget) {
            return ((DispatchGraphicsTarget) target).getWrappedObject();
        }
        return target;
    }

    /**
     * @param target
     * @see com.raytheon.uf.viz.core.IView#setupView(com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    public void setupView(IGraphicsTarget target) {
        // Any view functions that need a target must take the delegate target.
        // We need to figure out a better way to do this so this class is not
        // needed
        delegate.setupView(getDelegateTarget(target));
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.IView#getZoom()
     */
    public double getZoom() {
        return delegate.getZoom();
    }

    /**
     * @param dimensions
     * @return
     * @see com.raytheon.uf.viz.core.IView#recalcZoomLevel(int[])
     */
    public double recalcZoomLevel(int[] dimensions) {
        return delegate.recalcZoomLevel(dimensions);
    }

    /**
     * @param zoomLevel
     * @see com.raytheon.uf.viz.core.IView#zoom(double)
     */
    public void zoom(double zoomLevel) {
        delegate.zoom(zoomLevel);
    }

    /**
     * @param factor
     * @param screenX
     * @param screenY
     * @param target
     * @see com.raytheon.uf.viz.core.IView#scaleAndBias(double, double, double,
     *      com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    public void scaleAndBias(double factor, double screenX, double screenY,
            IGraphicsTarget target) {
        delegate.scaleAndBias(factor, screenX, screenY,
                getDelegateTarget(target));
    }

    /**
     * @param pixel
     * @return
     * @see com.raytheon.uf.viz.core.IView#isVisible(double[])
     */
    public boolean isVisible(double[] pixel) {
        return delegate.isVisible(pixel);
    }

    /**
     * @param pe
     * @return
     * @see com.raytheon.uf.viz.core.IView#isVisible(com.raytheon.uf.viz.core.IExtent)
     */
    public boolean isVisible(IExtent pe) {
        return delegate.isVisible(pe);
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.IView#getExtent()
     */
    public IExtent getExtent() {
        return delegate.getExtent();
    }

    /**
     * @param pe
     * @see com.raytheon.uf.viz.core.IView#setExtent(com.raytheon.uf.viz.core.IExtent)
     */
    public void setExtent(IExtent pe) {
        delegate.setExtent(pe);
    }

    /**
     * @param screenCoordinate
     * @param target
     * @return
     * @see com.raytheon.uf.viz.core.IView#getDisplayCoords(double[],
     *      com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    public double[] getDisplayCoords(double[] screenCoordinate,
            IGraphicsTarget target) {
        return delegate.getDisplayCoords(screenCoordinate,
                getDelegateTarget(target));
    }

    /**
     * @param pc
     * @return
     * @see com.raytheon.uf.viz.core.IView#createExtent(com.raytheon.uf.viz.core.PixelCoverage)
     */
    public IExtent createExtent(PixelCoverage pc) {
        return delegate.createExtent(pc);
    }

    /**
     * @param startScreen
     * @param endScreen
     * @param target
     * @see com.raytheon.uf.viz.core.IView#shiftExtent(double[], double[],
     *      com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    public void shiftExtent(double[] startScreen, double[] endScreen,
            IGraphicsTarget target) {
        delegate.shiftExtent(startScreen, endScreen, getDelegateTarget(target));
    }

    /**
     * @param x
     * @param y
     * @param depth
     * @param target
     * @return
     * @see com.raytheon.uf.viz.core.IView#screenToGrid(double, double, double,
     *      com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    public double[] screenToGrid(double x, double y, double depth,
            IGraphicsTarget target) {
        return delegate.screenToGrid(x, y, depth, getDelegateTarget(target));
    }

    /**
     * @param grid
     * @param target
     * @return
     * @see com.raytheon.uf.viz.core.IView#gridToScreen(double[],
     *      com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    public double[] gridToScreen(double[] grid, IGraphicsTarget target) {
        return delegate.gridToScreen(grid, getDelegateTarget(target));
    }

    /**
     * @param clientArea
     * @param dimensions
     * @see com.raytheon.uf.viz.core.IView#scaleToClientArea(org.eclipse.swt.graphics.Rectangle,
     *      int[])
     */
    public void scaleToClientArea(Rectangle clientArea, int[] dimensions) {
        delegate.scaleToClientArea(clientArea, dimensions);
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.IView#clone()
     */
    public Object clone() {
        return new ViewWrapper((IView) delegate.clone());
    }

}
