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

import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Represents a line-path based shape
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------	----------	-----------	--------------------------
 *    7/1/06                    chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * 
 */
public interface IWireframeShape extends IShape {

    /**
     * Add a sequence of latitude and longitudes which form a line
     * 
     * @param latLong
     *            the points
     */
    public abstract void addLineSegment(Coordinate[] latLong);

    /**
     * Add a sequence of screen coordinates that form a line
     * 
     * 
     * @param screenCoordinates
     *            the screen coordinates
     * 
     */
    public abstract void addLineSegment(double[][] screenCoordinates);

    public abstract void addLabel(String label, double[] screenCoordinate);

    public abstract void clearLabels();

    /**
     * Allocate enough space for the specified number of points
     * 
     * @param points
     */
    public abstract void allocate(int points);
}
