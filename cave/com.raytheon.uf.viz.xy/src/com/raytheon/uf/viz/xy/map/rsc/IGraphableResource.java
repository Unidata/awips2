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
package com.raytheon.uf.viz.xy.map.rsc;

import com.raytheon.uf.viz.xy.graph.labeling.IGraphLabel;

/**
 * If a resource is able to be displayed on a graph it should implement this
 * interface. The resources should be careful about getGraphKey, this key is
 * used to map similar resources to the same graph. Some graphs will support
 * multiple graphs on the display and others will support multiple data on a
 * single graph and some will support both. A non tested idea for a key would be
 * to use the title of the graph as resources that could be plotted to the same
 * graph will most likely have the same graph titles
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 30, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IGraphableResource<T, V> {

    /**
     * This should return an object that will be constructable by all resources
     * needing to be plotted on the same graph.
     * 
     * @return
     */
    public Object getGraphKey();

    /**
     * Returns the x-data for the graph, since the graph is a function of x,
     * this data is the concrete values that are present like dataTimes or
     * distances. This data will have toString() called on it for displaying
     * labels on the x-axis
     * 
     * @return the x range data
     */
    public IGraphLabel<T>[] getXRangeData();

    /**
     * Returns the y-data range for the graph. y data might be 2, 4, 5, 3, 7,
     * 10. in that order, but this function should return the labels that need
     * to be displayed on the y-axis so in that case it could be 0, 2, 4, 6, 8,
     * 10, 12 or something similar. Remember toString() will be called on these
     * objects for the labels
     * 
     * @return the y range data
     */
    public IGraphLabel<V>[] getYRangeData();

    /**
     * Issue a redraw on the resource, this function should recalculate where
     * the data needs to be on the screen
     */
    public void redraw();
}
