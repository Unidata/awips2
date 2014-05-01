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

import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener.DisplayChangeType;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * IDisplayPaneContainer
 * 
 * Describes a view, etc that contains one or more displays
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Jul 30, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public interface IDisplayPaneContainer {

    /**
     * Returns the display panes
     * 
     * @return the display pane
     */
    public IDisplayPane[] getDisplayPanes();

    /**
     * @return the loopProperties
     */
    public LoopProperties getLoopProperties();

    /**
     * @param loopProperties
     *            the loopProperties to set
     */
    public void setLoopProperties(LoopProperties loopProperties);

    /**
     * @return the active display pane
     */
    public abstract IDisplayPane getActiveDisplayPane();

    /**
     * Refresh all panes
     */
    public abstract void refresh();

    /**
     * Translate a click in screen space into "world" coordinates
     * 
     * @param x
     *            the x coordinate
     * @param y
     *            the y coordinate
     * @return the corresponding world coordinate
     */
    public abstract Coordinate translateClick(double x, double y);

    /**
     * Translate a world screen coordinate to screen (x,y) coordinates.
     * 
     * 
     * @param c
     *            world coordinate of the click
     * @return the visible screen pixel value
     */
    public abstract double[] translateInverseClick(Coordinate c);

    /**
     * Add a renderable display change listener
     * 
     * @param displayChangedListener
     */
    public void addRenderableDisplayChangedListener(
            IRenderableDisplayChangedListener displayChangedListener);

    /**
     * Remove a renderable display change listener
     * 
     * @param displayChangedListener
     */
    public void removeRenderableDisplayChangedListener(
            IRenderableDisplayChangedListener displayChangedListener);

    /**
     * Notify the renderable display listeners that the display has changed
     * 
     * @param pane
     * @param display
     */
    public void notifyRenderableDisplayChangedListeners(IDisplayPane pane,
            IRenderableDisplay display, DisplayChangeType type);

    /**
     * Register a input handler on the container at the given priority
     * 
     * @param handler
     * @param priority
     */
    public void registerMouseHandler(IInputHandler handler,
            InputPriority priority);

    /**
     * Register a input handler on the container at the default priority
     * 
     * @param handler
     **/
    public void registerMouseHandler(IInputHandler handler);

    /**
     * Unregister the input handler
     * 
     * @param handler
     */
    public void unregisterMouseHandler(IInputHandler handler);
}
