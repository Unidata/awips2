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

import com.raytheon.uf.viz.xy.graph.labeling.IGraphLabel;

/**
 * An IAxisPlacer is responsible for determining where labels go on the axis, it
 * bases it off of offsets so the caller can determine where to place based on
 * offset
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

public interface IAxisPlacer {

    /**
     * Determines where to place the axes, should set the start/end coords in
     * the axis
     * 
     * @param axes
     */
    public double[] placeAxes(IAxis[] axes);

    /**
     * 
     * @param labels
     */
    public double[] placeLabels(IGraphLabel<?>[] labels);

    /**
     * Get the offset on the axis where this value belongs
     * 
     * @param discreteValue
     * @return the offset on the axis
     */
    public double getPixelLoc(double dataValue);

    /**
     * Get the value at the offset, reverse of getOffset
     * 
     * @param offset
     * @return
     */
    public double getDataValue(double pixelLoc);

    /**
     * Get the size of this axis placer
     * 
     * @return
     */
    public double getDataWidth();

    public void pan(double distance);

    public void zoom(double offset, double zoomLevel);

    public double getMinDataValue();

    public double getMaxDataValue();

    public void setPixelWidth(double pixelWidth);

}
