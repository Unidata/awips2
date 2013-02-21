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
package com.raytheon.uf.viz.core.drawables;

import java.util.Map;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.AbstractGraphicsFactoryAdapter;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IView;

/**
 * This interface defines the combination of a renderable object and the area
 * over which it is displayed over.
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Jan 30, 2007             chammack    Initial Creation.
 *  
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
/**
 * @author randerso
 * 
 */
public interface IRenderableDisplay extends IRenderable {

    public static final double MIN_ZOOM_LEVEL = 0.001;

    public static final double MAX_ZOOM_LEVEL = 1.0;

    /**
     * Center the desired point in the client area
     * 
     * @param center
     *            desired center point in world coordinates
     */
    public abstract void recenter(double[] center);

    /**
     * Adjust the pixel extent to the client area by scaling, recentering, and
     * zooming as applicable
     * 
     * @param clientArea
     */
    public abstract void calcPixelExtent(Rectangle clientArea);

    /**
     * Get the pixel width of the world
     * 
     * @return the world width
     */
    public abstract int getWorldWidth();

    /**
     * Get the pixel height of the world
     * 
     * @return the world height
     */
    public abstract int getWorldHeight();

    /**
     * Get the X,Y,Z coordinates for the display
     * 
     * @return
     */
    public abstract int[] getDimensions();

    /**
     * Returns true if the display contains blinking elements
     * 
     * This is required to notify the draw scheduler
     * 
     * @return true if the display contains blinking elements
     */
    public abstract boolean isBlinking();

    /**
     * Returns true if the display has blinking elements that have changed state
     * 
     * This is required to notify the draw scheduler
     * 
     * @return true if the display contains blinking elements that have changed
     *         state
     */
    public abstract boolean isBlinkStateChanged();

    /**
     * Get the descriptor of the renderable display
     * 
     * @return the descriptor
     */
    public IDescriptor getDescriptor();

    /**
     * Set the descriptor for the renderable display to use
     * 
     * @param desc
     */
    public void setDescriptor(IDescriptor desc);

    /**
     * Get the internal view representation from this display. Note, this is
     * really only for use in initializing a PaintProperties object and for
     * child classes which need to access their internal view representation,
     * and should not be used otherwise.
     * 
     * @return internal view representation
     */
    public abstract IView getView();

    /**
     * <pre>
     * ===================================
     * Functionality migrated from view.
     * ===================================
     * </pre>
     */

    /**
     * Get the pixelExtent for the view area
     * 
     * @return
     */
    public abstract IExtent getExtent();

    /**
     * use the renderable display dimensions
     * 
     * @param dimensions
     * @return
     */
    public double recalcZoomLevel(int[] dimensions);

    /**
     * Set the canvas bounds
     * 
     * @param bounds
     */
    public abstract void setBounds(Rectangle bounds);

    /**
     * Get the canvas bounds
     * 
     * @param bounds
     */
    public abstract Rectangle getBounds();

    /**
     * Zoom the display to the desired level
     * 
     * @param zoomLevel
     *            Desired zoom level. Must be > 0 and <= 1.0 where 1.0 = full
     *            display viewable in client area
     */
    public abstract void zoom(double zoomLevel);

    /**
     * Get Zoom
     * 
     * @return zoom
     */
    public abstract double getZoom();

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
     * 
     * @param pe
     */
    public abstract void setExtent(IExtent pe);

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
     * Scale the pixel extent to show the full display in the client area
     * 
     * @param clientArea
     *            client area bounding rectangle
     */
    public abstract void scaleToClientArea(Rectangle clientArea);

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
     * Set up the display with the given target
     * 
     * @param target
     */
    public void setup(IGraphicsTarget target);

    /**
     * Dispose the renderable display
     */
    public void dispose();

    /**
     * Triggers the clearing of the RenderableDisplay
     */
    public void clear();

    /**
     * Set the background color of the display
     * 
     * @param color
     */
    public void setBackgroundColor(RGB color);

    /**
     * Get the background color of the display
     * 
     * @return color
     */
    public RGB getBackgroundColor();

    /**
     * create a new renderable display based on the display
     * 
     * @return the new renderable display
     */
    public IRenderableDisplay createNewDisplay();

    /**
     * Set whether we are swapping out the display
     * 
     * @param swapping
     */
    public void setSwapping(boolean swapping);

    /**
     * Is the display being swapped
     * 
     * @return
     */
    public boolean isSwapping();

    /**
     * get the globals object
     * 
     * @return
     */
    public Map<String, Object> getGlobalsMap();

    /**
     * Get the IDisplayPaneContainer the renderable display is loaded to
     * 
     * @return
     */
    public IDisplayPaneContainer getContainer();

    /**
     * Set the renderable displays IDisplayPaneContainer. NOTE: One of the panes
     * in the container's pane list should have a renderable display == to this
     * renderable display
     * 
     * @param container
     */
    public void setContainer(IDisplayPaneContainer container);

    /**
     * Notify the display to refresh
     */
    public void refresh();

    /**
     * Get the graphics adapter for the display
     * 
     * @return
     */
    public AbstractGraphicsFactoryAdapter getGraphicsAdapter();

    /**
     * Set the graphics adapter for the display
     * 
     * @param adapter
     */
    public void setGraphicsAdapter(AbstractGraphicsFactoryAdapter adapter);
}
