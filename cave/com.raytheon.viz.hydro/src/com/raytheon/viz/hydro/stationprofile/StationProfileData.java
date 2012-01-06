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

import java.util.Set;
import java.util.TreeMap;

import com.raytheon.uf.common.dataplugin.shef.tables.Statprof;

/**
 * This class contains data for the station profile.
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
public class StationProfileData
{
    /**
     * Stream name.
     */
    private String streamName;
    
    /**
     * Maximum elevation in feet.
     */
    private int elevationFtMax = 0;
    
    /**
     * Minimum elevation in feet.
     */
    private int elevationFtMin = 0;
    
    /**
     * Map of station names (key) and the associated station data (value).
     */
    private TreeMap<String, Statprof> stationDataMap;
    
    /**
     * Constructor.
     * @param streamName Stream name.
     * @param elevationFtMax Maximum elevation in feet.
     * @param elevationFtMin Minimum elevation in feet.
     */
    public StationProfileData(String streamName, int elevationFtMax, int elevationFtMin)
    {
        this.streamName = streamName;
        this.elevationFtMax = elevationFtMax;
        this.elevationFtMin = elevationFtMin;
        
        stationDataMap = new TreeMap<String, Statprof>();
    }
    
    /**
     * Get the stream name.
     * @return The stream name.
     */
    public String getStreamName()
    {
        return streamName;
    }

    /**
     * Get the maximum elevation in feet.
     * @return The maximum elevation in feet.
     */
    public int getElevationFtMax()
    {
        return elevationFtMax;
    }

    /**
     * Get the minimum elevation in feet.
     * @return The minimum elevation in feet.
     */
    public int getElevationFtMin()
    {
        return elevationFtMin;
    }
    
    /**
     * Add station data to the map.
     * @param stationName Station name (key).
     * @param data Station data (value).
     */
    public void addStationData(String stationName, Statprof data)
    {
        stationDataMap.put(stationName, data);
    }
    
    /**
     * Get the station data.
     * @param key Map key (station name).
     * @return The station data.
     */
    public Statprof getStationData(String key)
    {
        return stationDataMap.get(key);
    }
    
    /**
     * Get the set of station names.
     * @return Station names in a Set.
     */
    public Set<String> getStations()
    {
        return stationDataMap.keySet();
    }
}
