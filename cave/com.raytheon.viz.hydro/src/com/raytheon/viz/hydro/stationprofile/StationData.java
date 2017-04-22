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

import java.util.Date;


/**
 * This class contains station data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation 
 * 17 NOV 2008  1628       dhladky     Little Update.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 *
 */
public class StationData
{
    /**
     * Station name.
     */
    private String name;
    
    /**
     * Reach information.
     */
    private String reach;
    
    /**
     * Action value.
     */
    private double action;
    
    /**
     * Flood value.
     */
    private double flood;
     
    /**
     * Station plot point used for graphing the data.
     */
    private StationPlotPoints stationPlotPoints;
    
    /**
     * The max value for the station, obs or fcst.
     */
    private double maxValue;
    
    /**
     * The time of the max value.
     */
    private Date validTime;
    
    /**
     * Constructor.
     * @param name Station name.
     * @param reach Reach information.
     * @param action Action value.
     * @param flood Flood value.
     * @param stationPlotPoints Station plot points for graphing.
     */
    public StationData(String name, String reach, double action, double flood,
            StationPlotPoints stationPlotPoints)
    {
        this.name = name;
        this.reach = reach;
        this.action = action;
        this.flood = flood;
        this.stationPlotPoints = stationPlotPoints;
    }
   
    /**
     * Get the station name.
     * @return The station name.
     */
    public String getName()
    {
        return name;
    }

    /**
     * Get the reach information.
     * @return The reach information.
     */
    public String getReach()
    {
        return reach;
    }

    /**
     * Get the action information.
     * @return The action information.
     */
    public double getAction()
    {
        return action;
    }

    /**
     * Get the flood value.
     * @return The flood value.
     */
    public double getFlood()
    {
        return flood;
    }
    
    /**
     * Get the station plot points.
     * @return The station plot points.
     */
    public StationPlotPoints getStationPlotPoints()
    {
        return stationPlotPoints;
    }

    /**
     * @return the maxValue
     */
    public double getMaxValue() {
        return maxValue;
    }

    /**
     * @param maxValue the maxValue to set
     */
    public void setMaxValue(double maxValue) {
        this.maxValue = maxValue;
    }

    /**
     * @return the validTime
     */
    public Date getValidTime() {
        return validTime;
    }

    /**
     * @param validTime the validTime to set
     */
    public void setValidTime(Date validTime) {
        this.validTime = validTime;
    }
}
