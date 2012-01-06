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
 * Slice request for a vertical plane (i.e. cross section). The results returned
 * from this slice contain grids where y represents the height in the atmosphere
 * and x represents point on a line on the earth.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 28, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class VerticalPlaneRequest extends VerticalRequest {

    protected Coordinate[] coordinates;

    /**
     * @return the startCoordinate
     */
    public Coordinate[] getCoordinates() {
        return coordinates;
    }

    /**
     * @param startCoordinate
     *            the startCoordinate to set
     */
    public void setCoordinates(Coordinate[] coordinates) {
        this.coordinates = coordinates;
    }

}
