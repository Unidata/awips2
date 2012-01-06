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
package com.raytheon.uf.viz.xy.graph.axis;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Interface for an axis, an axis has a start/end coordinate, a line style
 * (dashed, solid, dotted), a value at the axis and can be drawn or not
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 29, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IAxis {

    /**
     * Set the start location of this axis (not in lat/lon)
     * 
     * @param start
     */
    public void setStartLoc(Coordinate start);

    /**
     * Set the end location of this axis (not in lat/lon)
     * 
     * @param end
     */
    public void setEndLoc(Coordinate end);

    /**
     * Set the line style of the axis
     * 
     * @param style
     */
    public void setLineStyle(LineStyle style);

    /**
     * Get the LineStyle
     */
    public LineStyle getLineStyle();

    /**
     * Returns the JTS LineString type
     * 
     * @return
     */
    public Coordinate[] getCoordinates();

    /**
     * Tells the axis whether or not it should be drawn
     * 
     * @param drawAxis
     */
    public void setDrawAxis(boolean drawAxis);

    /**
     * should the axis be drawn
     * 
     * @return
     */
    public boolean isDrawAxis();

    /**
     * Get the value at the axis
     * 
     * @return
     */
    public double getDiscreteValue();

    /**
     * Set the discrete value at the axis
     * 
     * @param val
     */
    public void setDiscreteValue(double val);
}
