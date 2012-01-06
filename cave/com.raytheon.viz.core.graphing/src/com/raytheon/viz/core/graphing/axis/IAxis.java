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
package com.raytheon.viz.core.graphing.axis;

import java.util.ArrayList;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.rsc.capabilities.IColorableResource;
import com.raytheon.viz.core.graphing.DataAxisInfo;

/**
 * Interface for axes
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 23, 2007            njensen     Initial creation	
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public interface IAxis extends IRenderable, IColorableResource {

    public static enum Orientation {
        HORIZONTAL, VERTICAL
    }

    /**
     * Converts a data value to a coordinate on the axis
     * 
     * @param aValue
     *            the data to convert
     * @return the coordinate on the axis
     */
    public double valueToCoordinate(Object aValue);

    /**
     * Update the axis labeling based on the number of graphs displayed
     * 
     * @param numberOfGraphs
     *            the number of graphs in the overall display
     */
    public void updateLabeling(int numberOfGraphs);

    /**
     * Zooms an axis to a specific coordinate and zoom amount
     * 
     * @param x
     *            the x coordinate of the zoom (only used for horizontal axes)
     * @param y
     *            the y coordinate of the zoom (only used for vertical axes)
     * @param zoom
     *            the amount to zoom where 1 is no zoom, > 1 is zoom in, and < 0
     *            is zoom out
     * @return if the axis successfully zoomed
     */
    public boolean zoom(double x, double y, double zoom);

    /**
     * Sets the possible range of data values on the axis
     * 
     * @param min
     *            the minimum point on the axis
     * @param max
     *            the maximum point on the axis
     */
    public void setRange(Double min, Double max);

    /**
     * Adds a title to the axis
     * 
     * @param aTitle
     *            the title
     * @param aColor
     *            the color of the title
     */
    public void addTitle(String aTitle, RGB aColor);

    /**
     * Gets the titles
     * 
     * @return the titles
     */
    public ArrayList<String> getTitles();

    /**
     * Gets the minimum value of the axis
     * 
     * @return the min val
     */
    public Double getMinVal();

    /**
     * Gets the maximum value of the axis
     * 
     * @return the max val
     */
    public Double getMaxVal();

    /**
     * Gets the orientation
     * 
     * @return the orientation
     */
    public Orientation getOrientation();

    /**
     * Sets the orientation
     * 
     * @param orientation
     *            the orientation to set
     */
    public void setOrientation(Orientation orientation);

    /**
     * Gets the graph area (area contained within two axes)
     * 
     * @return the graphArea
     */
    public Rectangle getGraphArea();

    /**
     * Gets the data axis info
     * 
     * @return the info
     */
    public DataAxisInfo getInfo();

    /**
     * Sets the data axis info
     * 
     * @param info
     *            the info
     */
    public void setInfo(DataAxisInfo info);

    /**
     * Gets the axis labeling
     * 
     * @return the axis labeling
     */
    public AxisLabeling getLabeling();

    /**
     * Sets the titles
     * 
     * @param titles
     *            the titles
     * @param titleColors
     *            the color of the titles
     */
    public void setTitles(ArrayList<String> titles, ArrayList<RGB> titleColors);

}
