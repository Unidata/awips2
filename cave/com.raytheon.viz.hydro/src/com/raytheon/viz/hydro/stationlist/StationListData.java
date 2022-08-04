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
package com.raytheon.viz.hydro.stationlist;

/**
 * This class contains station list data. This class also implements Comparable
 * so the data can be sorted by ID.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation.
 * 10/2/2008    1555       grichard    Support station selection.
 * 11/19/2008   1662       grichard    Support pt. precip. accum.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class StationListData implements Comparable<StationListData> {
    /**
     * Location (Station) ID.
     */
    private String lid;

    /**
     * Station name.
     */
    private String name;

    /**
     * Station longitude.
     */
    private double lon;

    /**
     * Station latitude.
     */
    private double lat;

    /**
     * HSA.
     */
    private String hsa;

    /**
     * Constructor.
     * 
     * @param lid
     *            Location (Station) ID.
     * @param name
     *            Station name.
     * @param lon
     *            Station longitude.
     * @param lat
     *            Station latitude.
     */
    public StationListData(String lid, String name, double lon, double lat,
            String hsa) {
        this.lid = lid;
        this.name = name;
        this.lon = lon;
        this.lat = lat;
        this.hsa = hsa;
    }

    /**
     * compareTo method to compare location (station) IDs.
     * 
     * @param obj
     *            Object to compare to.
     * @return -1 if ID is less than, 0 if ID is equal to, or 1 id ID is greater
     *         than the object ID that was passed in.
     */
    @Override
    public int compareTo(StationListData obj) {
        return lid.compareTo(obj.lid);
    }

    /**
     * Get the data in a formatted string.
     * 
     * @return The formatted data in a string format.
     */
    @Override
    public String toString() {
        return String.format("%5s %s  [%3.2f %3.2f]", lid, name, lon, lat);
    }

    /**
     * Get the station ID.
     * 
     * @return The location (station) ID.
     */
    public String getLid() {
        return lid;
    }

    /**
     * Get the station name.
     * 
     * @return The station name.
     */
    public String getName() {
        return name;
    }

    /**
     * Get the station Longitude.
     * 
     * @return The station Longitude.
     */
    public double getLon() {
        return lon;
    }

    /**
     * Get the station Latitude.
     * 
     * @return The station Latitude.
     */
    public double getLat() {
        return lat;
    }

    /**
     * Get the HSA.
     * 
     * @return The HSA.
     */
    public String getHsa() {
        return hsa;
    }

}