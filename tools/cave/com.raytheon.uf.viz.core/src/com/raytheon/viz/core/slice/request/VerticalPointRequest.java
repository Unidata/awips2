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
package com.raytheon.viz.core.slice.request;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Slice request for data at a single point at multiple heights and times.
 * Slices returned from this contain grids where y represents the height in the
 * atmosphere and x represents successive data times.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 11, 2007            njensen     Initial creation	
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class VerticalPointRequest extends VerticalRequest {

    public enum TimeDirection {
        RIGHT_TO_LEFT, LEFT_TO_RIGHT
    };

    protected Coordinate coordinate;

    protected TimeDirection timeDirection = TimeDirection.LEFT_TO_RIGHT;

    /**
     * @return the coordinate
     */
    public Coordinate getCoordinate() {
        return coordinate;
    }

    /**
     * @param coordinate
     *            the coordinate to set
     */
    public void setCoordinate(Coordinate coordinate) {
        this.coordinate = coordinate;
    }

    /**
     * @return the timeDirection
     */
    public TimeDirection getTimeDirection() {
        return timeDirection;
    }

    /**
     * @param timeDirection
     *            the timeDirection to set
     */
    public void setTimeDirection(TimeDirection timeDirection) {
        this.timeDirection = timeDirection;
    }

}
