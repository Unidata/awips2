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
package com.raytheon.viz.hydro.stationprofile;

/**
 * This class contains plot points used to draw station data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation 
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 *
 */
public class StationPlotPoints implements Comparable<StationPlotPoints>
{
    /**
     * Location ID.
     */
    private String lid;
    
    /**
     * River miles.
     */
    private double riverMiles;
    
    /**
     * Zero datum for the gage.
     */
    private double zeroDatum;
    
    /**
     * Constructor.
     * @param lid Location Id.
     * @param riverMiles River miles.
     * @param elevationFt Elevation in feet.
     */
    public StationPlotPoints(String lid, double riverMiles, double zeroDatum)
    {
        this.lid = lid;
        this.riverMiles = riverMiles;
        this.zeroDatum = zeroDatum;
    }
    
    /**
     * Get the river miles.
     * @return The river miles.
     */
    public double getRiverMiles()
    {
        return riverMiles;
    }

    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * @param lid the lid to set
     */
    public void setLid(String lid) {
        this.lid = lid;
    }  
    /**
     * @return the zeroDatum
     */
    public double getZeroDatum() {
        return zeroDatum;
    }

    /**
     * compareTo method for comparing river mile values.
     * @param obj Object containing river miles to compare to.
     * @return -1, 0, 1 if the river miles is less then, equal to,
     *         or greater then the river miles passed in. 
     */
    public int compareTo(StationPlotPoints obj)
    {
        StationPlotPoints otherObject = obj;
        
        if (riverMiles < otherObject.riverMiles)
        {
            return 1;
        }
        else if (riverMiles > otherObject.riverMiles)
        {
            return -1;
        }
        
        return 0;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(getLid() + " " + getRiverMiles() + " " + getZeroDatum());
        return sb.toString();
    }
}
