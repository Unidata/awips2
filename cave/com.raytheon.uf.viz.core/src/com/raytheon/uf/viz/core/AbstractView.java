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
package com.raytheon.uf.viz.core;

import javax.vecmath.Vector3d;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.geom.Plane;
import com.raytheon.uf.viz.core.geom.Ray;

/**
 * 
 * Provides basic view functionality that may be useful to any view object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 25, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class AbstractView implements IView {

    private static final Plane mapPlane = new Plane(0.0, 0.0, 1.0, 0.0, false);

    protected IExtent extent;

    public AbstractView(IExtent pe) {
        this.extent = pe;
    }

    @Override
    public void scaleAndBias(double factor, double screenX, double screenY,
            IGraphicsTarget target) {
        double[] grid = screenToGrid(screenX, screenY, 0, target);
        this.extent.scaleAndBias(factor, grid[0], grid[1]);
    }

    @Override
    public double getZoom() {
        return extent.getScale();
    }

    @Override
    public double recalcZoomLevel(int[] dimensions) {
        int worldWidth = dimensions[0];
        int worldHeight = dimensions[1];
        return Math.min((extent.getMaxX() - extent.getMinX()) / worldWidth,
                (extent.getMaxY() - extent.getMinY()) / worldHeight);
    }

    @Override
    public void zoom(double zoomLevel) {
        this.extent.scale(zoomLevel);
    }

    @Override
    public boolean isVisible(double[] pixel) {
        return extent.contains(pixel);
    }

    @Override
    public boolean isVisible(IExtent pe) {
        return extent.intersects(pe);
    }

    @Override
    public IExtent getExtent() {
        return extent;
    }

    @Override
    public void setExtent(IExtent e) {
        this.extent = e;
    }

    @Override
    public double[] getDisplayCoords(double[] screenCoordinate,
            IGraphicsTarget target) {

        Ray r = computeRay(screenCoordinate, target);
        if (r == null) {
            return null;
        }

        Vector3d i = mapPlane.intersection(r);
        if (i == null) {
            return null;
        }
        return new double[] { i.x, i.y, i.z };
    }

    /**
     * Create a Ray starting a the near plane with direction towards the far
     * 
     * @param mouse
     *            mouse x,y
     * @return Ray
     */
    public Ray computeRay(double[] mouse, IGraphicsTarget target) {
        Vector3d far = new Vector3d(screenToGrid(mouse[0], mouse[1], 1, target));
        Vector3d near = new Vector3d(
                screenToGrid(mouse[0], mouse[1], 0, target));
        if (near == null || far == null) {
            return null;
        }

        far.sub(near);
        far.normalize();

        return new Ray(near, far);
    }

    @Override
    public IExtent createExtent(PixelCoverage pc) {
        return new PixelExtent(pc.getMinX(), pc.getMaxX(), pc.getMinY(),
                pc.getMaxY());
    }

    @Override
    public void shiftExtent(double[] startScreen, double[] endScreen,
            IGraphicsTarget target) {
        double[] start = screenToGrid(startScreen[0], startScreen[1], 0, target);
        double[] end = screenToGrid(endScreen[0], endScreen[1], 0, target);

        this.extent.shift(end[0] - start[0], end[1] - start[1]);
    }

    @Override
    public void scaleToClientArea(Rectangle clientArea, int[] dims) {
        double screenRatio;

        if ((clientArea.width == 0) || (clientArea.height == 0)) {
            screenRatio = 1.0;
        } else {
            screenRatio = (double) clientArea.width
                    / (double) clientArea.height;
        }
        int f_worldWidth = dims[0];
        int f_worldHeight = dims[1];

        double worldRatio = (double) f_worldWidth / (double) f_worldHeight;

        // set pixel extent to show entire map
        if (screenRatio > worldRatio) {
            this.extent = new PixelExtent(0, f_worldHeight * screenRatio, 0,
                    f_worldHeight);
        } else {
            this.extent = new PixelExtent(0, f_worldWidth, 0, f_worldWidth
                    / screenRatio);
        }

        this.extent.shift((f_worldWidth - extent.getWidth()) / 2,
                (f_worldHeight - extent.getHeight()) / 2);
    }

    @Override
    public double[] screenToGrid(double x, double y, double depth,
            IGraphicsTarget target) {
        double correctedX = (x * (extent.getMaxX() - extent.getMinX()) / getCanvasBounds(target).width)
                + extent.getMinX();
        double correctedY = (y * (extent.getMaxY() - extent.getMinY()) / getCanvasBounds(target).height)
                + extent.getMinY();
        // z bounds are 0 to 1
        double correctedZ = (depth * (extent.getMax().z - extent.getMin().z))
                + extent.getMin().z;
        return new double[] { correctedX, correctedY, correctedZ };
    }

    @Override
    public double[] gridToScreen(double[] grid, IGraphicsTarget target) {
        double x = ((grid[0] - extent.getMinX()) * getCanvasBounds(target).width)
                / (extent.getMaxX() - extent.getMinX());
        double y = ((grid[1] - extent.getMinY()) * getCanvasBounds(target).height)
                / (extent.getMaxY() - extent.getMinY());
        // z bounds are 0 to 1
        double z = (grid[2] - extent.getMin().z)
                / (extent.getMax().z - extent.getMin().z);

        return new double[] { x, y, z };
    }

    public abstract Rectangle getCanvasBounds(IGraphicsTarget target);

    public abstract void setupView(IGraphicsTarget target);

    public abstract AbstractView clone();

}
