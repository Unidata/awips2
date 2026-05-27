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
package com.raytheon.uf.viz.xy.graph;

import java.util.List;

import org.locationtech.jts.geom.Coordinate;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.xy.map.rsc.IGraphableResource;
import com.raytheon.uf.viz.xy.util.AbstractGraphZoomHandler;

/**
 * Interface for a graphing object
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 29, 2009            mschenke     Initial creation
 * Feb 10, 2011 8244       bkowal       Keep track of the current
 *                                      magnification settings.
 * Jul 21, 2015 4220       mapeters     Added getZoomHandler/setZoomHandler
 * Apr 22, 2022 8791       mapeters     Added setDescriptor()
 *
 * </pre>
 *
 * @author mschenke
 */
public interface IGraph extends IRenderable {

    /**
     * Plot an x,y value on the graph to actual screen location
     *
     * @param x
     *            the x value, ie time, distance, etc
     * @param y
     *            the y value, similar to x but y
     * @return
     */
    double[] getGridLocation(double x, double y);

    /**
     * Plot a grid coordinate to graph value
     *
     * @param x
     * @param y
     * @return
     */
    double[] getVirtualLocation(double x, double y);

    /**
     * Update the graphs extent, should only be called by the graph resource
     *
     * @param extent
     *            the new graph extent
     */
    void updateExtent(IExtent extent);

    /**
     * Update the graphs virtual extent
     *
     * @param extent
     */
    void updateVirtualExtent();

    /**
     * Add a graphable resource to the graph
     *
     * @param rsc
     *            the graphable resource to add
     */
    void addGraphResource(IGraphableResource<?, ?> rsc);

    /**
     * Remove the graph resource from the graph
     *
     * @param rsc
     */
    void removeGraphResource(IGraphableResource<?, ?> rsc);

    /**
     * Return the extent of the graph, a subset of the display's extent
     *
     * @return the graph's extent
     */
    IExtent getExtent();

    /**
     * Tells whether the graph is ready to be plotted to yet, use this method in
     * the resoures paint method to determine if execution should continue or
     * not. Currently a hack to make sure the resources are waiting for their
     * graphs to be constructed before attempting to plot to them
     *
     * @return if the graph is ready
     */
    boolean isReady();

    /**
     * Get the number of resources loaded in the graph
     *
     * @return
     */
    int getResourceCount();

    /**
     * check to see if the graph is being drawn or not
     *
     * @return true if drawn, false if not
     */
    boolean isDisplayed();

    /**
     * Set if the graph should be drawn or not
     *
     * @param displayed
     */
    void setDisplayed(boolean displayed);

    /**
     * Zoom in the graph, 0 indexed
     *
     * @param index
     */
    void zoom(int index, Coordinate gridCoord);

    /**
     * Pan to the given extent
     *
     * @param panToExtent
     */
    void pan(double xDist, double yDist, boolean panning);

    /**
     * Get the loaded resources on the graph
     *
     * @return
     */
    List<?> getResources();

    /**
     * Dispose of the graph
     */
    void dispose();

    /**
     * Reconstructs the graph
     */
    void reconstruct();

    void setCurrentMagnification(Double currentMagnification);

    AbstractGraphZoomHandler getZoomHandler();

    void setZoomHandler(AbstractGraphZoomHandler zoomHandler);

    void setDescriptor(XyGraphDescriptor descriptor);
}