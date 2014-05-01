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
package com.raytheon.uf.viz.monitor.data;

import java.util.Date;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Keep Areas with Containers of ObReports by Time in Hash Template For Silver
 * Springs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 12/07/09                  dhladky    Initial Creation.
 * Nov. 1, 2012 #1297       skorolev    Changed ArrayList to List
 * 
 * </pre>
 * 
 * @author dhladky
 * 
 */

public class AreaContainer {

    /** List of stations **/
    private List<String> stations = null;

    /** Zone ID **/
    private String areaId = null;

    /** Map zone - stations **/
    private ConcurrentHashMap<String, StationContainer> container = null;

    /**
     * Keeps station list for every zone.
     * 
     * @param stations
     * @param areaId
     */
    public AreaContainer(List<String> stations, String areaId) {
        this.setStations(stations);
        this.areaId = areaId;
        this.container = new ConcurrentHashMap<String, StationContainer>();
        // creates the stations list for this area
        for (String stationId : stations) {
            StationContainer sc = new StationContainer(stationId);
            container.put(stationId, sc);
        }
    }

    /**
     * Gets ObReports for station
     * 
     * @param stationId
     * @return StationContainer
     */
    public StationContainer getStation(String stationId) {
        StationContainer sc = null;
        if (container.containsKey(stationId)) {
            sc = container.get(stationId);
        }
        return sc;
    }

    /**
     * Gets the container
     * 
     * @return
     */
    public ConcurrentHashMap<String, StationContainer> getContainer() {
        return container;
    }

    /**
     * Gets the Area ID
     * 
     * @return
     */
    public String getAreaId() {
        return areaId;
    }

    /**
     * Removes zone and it's stations. If it's ever needed
     * 
     * @param stationId
     */
    public void removeStation(String stationId) {
        if (container.containsKey(stationId)) {
            container.remove(stationId);
        }
    }

    /**
     * Get the best obReport (time)
     * 
     * @param key
     * @return
     */
    public ObReport getBestAreaReport(Date key) {
        ObReport report = new ObReport();
        for (String station : container.keySet()) {
            if (key != null) {
                report = container.get(station).getMostRecent(key);
            }
        }
        if (report == null) {
            report = new ObReport();
        }
        return report;
    }

    /**
     * Adds a report for this area.
     * 
     * @param date
     * @param report
     */
    public void addReport(Date date, ObReport report) {
        if (container.containsKey(report.getPlatformId())) {
            container.get(report.getPlatformId()).addReport(date, report);
        }
    }

    /**
     * Sets station list
     * 
     * @param stations
     */
    public void setStations(List<String> stations) {
        this.stations = stations;
    }

    /**
     * Gets station list
     * 
     * @return stations
     */
    public List<String> getStations() {
        return stations;
    }
}
