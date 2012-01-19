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

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.geom.Plane;

/**
 * Interface that defines a view area. Methods include determining what is in
 * view and interacting the view.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 
 * </pre>
 * 
 * @author estrabal
 * @version 1.0
 */

public interface IView {

    public static enum POVShiftType {
        EYE, FOCUS, BOTH
    }

    /**
     * @param GLTarget
     */
    public abstract void setupView(IGraphicsTarget target);

    public abstract String getDisplayType();

    /**
     * Get Zoom
     * 
     * @return zoom
     */
    public abstract double getZoom();

    public double recalcZoomLevel(int[] dimensions);

    /**
     * Set elevation exaggeration factor
     * 
     * @param factor
     */
    public abstract void setElevationExaggeration(double factor);

    /**
     * Get the elevation exaggeration
     * 
     * @return
     */
    public abstract double getElevationExaggeration();

    /**
     * Zoom the display to the desired level
     * 
     * @param zoomLevel
     *            Desired zoom level. Must be > 0 and <= 1.0 where 1.0 = full
     *            display viewable in client area
     */
    public abstract void zoom(double zoomLevel);

    /**
     * 
     * @param factor
     * @param screenX
     * @param screenY
     * @param target
     */
    public abstract void scaleAndBias(double factor, double screenX,
            double screenY, IGraphicsTarget target);

    /**
     * Determine if pixel is in view area
     * 
     * @param pixel
     * @return
     */
    public boolean isVisible(double[] pixel);

    /**
     * Determine if any part of is in view area
     * 
     * @param pe
     * @return
     */
    public boolean isVisible(IExtent pe);

    /**
     * Get the pixelExtent for the view area
     * 
     * @return
     */
    public abstract IExtent getExtent();

    /**
     * 
     * @param pe
     */
    public abstract void setExtent(IExtent pe);

    /**
     * Set the center of the view area
     * 
     * @param point
     */
    public abstract void setCenter(Vector3d point);

    /**
     * Get the display grid coordinates under the screen point
     * 
     * @param screenCoordinate
     * @param target
     * @return grid coordinates of the display
     */
    public abstract double[] getDisplayCoords(double[] screenCoordinate,
            IGraphicsTarget target);

    /**
     * Change the point of view based on mouse position change
     * 
     * @param shiftType
     * 
     * @param last
     * @param current
     */
    public abstract boolean shiftPOV(double[] lastMouse, double[] currentMouse,
            POVShiftType shiftType, IGraphicsTarget target);

    public abstract void setTilt(double delta);

    public abstract double getTilt();

    /**
     * Set the focus point on the map from the mouse position
     * 
     * @param currentMouse
     * @return
     */
    public abstract boolean setFocalPoint(double[] currentMouse,
            IGraphicsTarget target);

    /**
     * Get the clipping planes
     * 
     * @return
     */
    abstract public Plane[] getClippingPlanes();

    /**
     * 
     * @param center
     * @return distance from the eye to the point
     */
    abstract public double getEyeDistance(Vector3d point);

    /**
     * Get eye in grid space
     * 
     * @return
     */
    abstract public double[] getEye();

    /**
     * Set eye in grid space
     * 
     * @param eye
     */
    abstract public void setEye(double[] eye);

    /**
     * 
     * @return
     */
    abstract public double[] getFocalPoint();

    /**
     * Create a specific extent for this type of view
     * 
     * @param pc
     * @return
     */
    abstract public IExtent createExtent(PixelCoverage pc);

    /**
     * Set up clipping planes to remove objects outside the view area.
     * 
     * @throws VizException
     *             TODO
     */
    abstract public void setClippingPlanes() throws VizException;

    /**
     * Shift the extent by the delta
     * 
     * @param startScreen
     * @param endScreen
     * @param target
     */
    abstract public void shiftExtent(double[] startScreen, double[] endScreen,
            IGraphicsTarget target);

    /**
     * Convert screen space to grid space
     * 
     * @param x
     *            screen coordinate
     * @param y
     *            screen coordinate
     * @param depth
     *            range [0.0 - 1.0] where 0.0 is the near plane and 1.0 is the
     *            far plane
     * @param target
     *            the target used to perform the calculations.
     * @return value in grid space
     */
    abstract public double[] screenToGrid(double x, double y, double depth,
            IGraphicsTarget target);

    /**
     * Convert grid space to screen space
     * 
     * @param grid
     * @param target
     * @return screen coordinate
     */
    abstract public double[] gridToScreen(double[] grid, IGraphicsTarget target);

    /**
     * Scale the pixel extent to show the full display in the client area
     * 
     * @param clientArea
     *            client area bounding rectangle
     * @param display
     *            the descriptor that will be drawn
     */
    public abstract void scaleToClientArea(Rectangle clientArea,
            int[] dimensions);

    public Object clone();

}
