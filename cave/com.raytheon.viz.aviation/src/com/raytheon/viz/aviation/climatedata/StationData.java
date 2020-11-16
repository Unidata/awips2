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
package com.raytheon.viz.aviation.climatedata;

import java.util.ArrayList;

/**
 * Class containing the USAF-WBAN Id and an array of observation data (year and number
 * of observations).
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 15, 2010 #4549      lvenable    Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class StationData
{
    /**
     * USAF-WBAN Identification.
     */
    private String usafWbanId;
    
    /**
     * Array of Observation data.
     */
    private ArrayList<ObsGraphData> obsGraphDataArray;
    
    /**
     * Constructor.
     * @param usafWbanId USAF-WBAN Identification. 
     */
    public StationData(String usafWbanId)
    {
        this.usafWbanId = usafWbanId;        
        obsGraphDataArray = new ArrayList<ObsGraphData>();
    }
    
    /**
     * Constructor.
     * @param usafWbanId USAF-WBAN Identification.
     * @param obsArray Array of Observation data.
     */
    public StationData(String usafWbanId, ArrayList<ObsGraphData> obsArray)
    {
        this.usafWbanId = usafWbanId;        
        obsGraphDataArray = obsArray;
    }

    /**
     * Get the array of observation data.
     * @return Array of observation data.
     */
    public ArrayList<ObsGraphData> getObsGraphDataArray()
    {
        return obsGraphDataArray;
    }

    /**
     * Set the observation data array.
     * @param obsGraphDataArray Array of observation data.
     */
    public void setObsGraphDataArray(ArrayList<ObsGraphData> obsGraphDataArray)
    {
        this.obsGraphDataArray = obsGraphDataArray;
    }

    /**
     * Get the USAF-WBAN Identification.
     * @return The USAF-WBAN Identification.
     */
    public String getUsafWbanId()
    {
        return usafWbanId;
    }   
    
    /**
     * Add observation graph data. 
     * @param graphData Observation graph data.
     */
    public void addObsGraphData(ObsGraphData graphData)
    {
        if (obsGraphDataArray == null)
        {
            obsGraphDataArray = new ArrayList<ObsGraphData>();
        }
        
        obsGraphDataArray.add(graphData);
    }
}
