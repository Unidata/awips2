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
package com.raytheon.viz.hydrobase;

import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.IGetSortType;

/**
 * this class contains the Hydro Station data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 4, 2008				lvenable	Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class HydroStationData implements Comparable<HydroStationData> {
    /**
     * Station.
     */
    private String station = "";

    /**
     * Name.
     */
    private String name = "";

    /**
     * State, County.
     */
    private String stateCounty = "";

    /**
     * Basin.
     */
    private String basin = "";

    /**
     * Stream.
     */
    private String stream = "";

    /**
     * Latitude.
     */
    private double lat = 0;

    /**
     * Longitude.
     */
    private double lon = 0;

    /**
     * Callback used to get the sort type.
     */
    private IGetSortType sortCB;

    /**
     * Constructor.
     * 
     * @param sortCB
     *            Sort callback.
     * @param station
     *            Station.
     * @param name
     *            Name.
     * @param stateCounty
     *            State and County.
     * @param basin
     *            Basin.
     * @param stream
     *            Stream.
     */
    public HydroStationData(IGetSortType sortCB, String station, String name,
            String stateCounty, String basin, String stream, double lat,
            double lon) {
        this.sortCB = sortCB;

        // Set the local variable to the arguments passed in.
        // I am calling the setter methods because they handle
        // null arguments.
        setStation(station);
        setName(name);
        setStateCounty(stateCounty);
        setBasin(basin);
        setStream(stream);
        setLat(lat);
        setLon(lon);
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param sortCB
     *            Sort callback.
     * @param data
     *            The raw data from the database
     */
    public HydroStationData(IGetSortType sortCB, Object[] data) {
        this(sortCB, (String) data[0], (String) data[1], (String) data[5] + ","
                + (String) data[6], (String) data[4], (String) data[7],
                (data[2] != null) ? (Double) data[2]
                        : HydroConstants.MISSING_VALUE,
                (data[3] != null) ? (Double) data[3]
                        : HydroConstants.MISSING_VALUE);
    }

    /**
     * Get the latitude
     * 
     * @return The latitude
     */
    public double getLat() {
        return lat;
    }

    /**
     * Set the latitude
     * 
     * @param lat
     *            The new latitude
     */
    public void setLat(Double lat) {
        this.lat = (lat != null) ? lat : HydroConstants.MISSING_VALUE;
    }

    /**
     * Get the longitude
     * 
     * @return The longitude
     */
    public double getLon() {
        return lon;
    }

    /**
     * Set the longitude
     * 
     * @param lon
     *            The new longitude
     */
    public void setLon(Double lon) {
        this.lon = (lon != null) ? lon : HydroConstants.MISSING_VALUE;
    }

    /**
     * Get the station.
     * 
     * @return The station.
     */
    public String getStation() {
        return station;
    }

    /**
     * Set the station.
     * 
     * @param station
     *            The station.
     */
    public void setStation(String station) {
        if (station == null) {
            this.station = "";
            return;
        }

        this.station = station;
    }

    /**
     * Get the name. The name.
     * 
     * @return
     */
    public String getName() {
        return name;
    }

    /**
     * Set the name.
     * 
     * @param name
     *            The name.
     */
    public void setName(String name) {
        if (name == null) {
            this.name = "";
            return;
        }

        this.name = name;
    }

    /**
     * Get the state and county.
     * 
     * @return
     */
    public String getStateCounty() {
        return stateCounty;
    }

    /**
     * Set the state and county.
     * 
     * @param stateCounty
     *            The state and county.
     */
    public void setStateCounty(String stateCounty) {
        if (stateCounty == null) {
            this.stateCounty = "";
            return;
        }

        this.stateCounty = stateCounty;
    }

    /**
     * Get the basin.
     * 
     * @return The basin.
     */
    public String getBasin() {
        return basin;
    }

    /**
     * Set the basin.
     * 
     * @param basin
     *            The basin.
     */
    public void setBasin(String basin) {
        if (basin == null) {
            this.basin = "";
            return;
        }

        this.basin = basin;
    }

    /**
     * Get the stream.
     * 
     * @return The stream.
     */
    public String getStream() {
        return stream;
    }

    /**
     * Set the stream.
     * 
     * @param stream
     *            The stream.
     */
    public void setStream(String stream) {
        if (stream == null) {
            this.stream = "";
            return;
        }

        this.stream = stream;
    }

    /**
     * Get the data in a formatted string for displaying.
     * 
     * @return The formatted data.
     */
    public String getDisplayString() {
        String tmpStr = String.format("%-8S %-25S %-23S %-25S %-25S %-9S %-9S",
                station, name, stateCounty, basin, stream, lat, lon);

        return tmpStr;
    }

    /**
     * Returns the formated string with the LID and location name
     * 
     * @return
     */
    public String getLocationDisplayString() {
        return String.format("%-8S %-25.25S", station, name);
    }

    /**
     * Returns the formated string with the State and County
     * 
     * @return
     */
    public String getStateCountyDisplayString() {
        return String.format(" %-24.24S", stateCounty);
    }

    /**
     * Returns the formated string with the Basin
     * 
     * @return
     */
    public String getBasinDisplayString() {
        return String.format(" %-25.25S", basin);
    }

    /**
     * Returns the formated string with the Stream
     * 
     * @return
     */
    public String getStreamDisplayString() {
        return String.format(" %-25.25S", stream);
    }

    /**
     * Returns the formated string with the Lat/Lon
     * 
     * @return
     */
    public String getLatLonDisplayString() {
        return String.format(" %-9.9S %-9.9S", lat, lon);
    }

    /**
     * Compare method used for sorting the data.
     * 
     * @param obj
     *            Hydro station data object.
     */
    public int compareTo(HydroStationData obj) {
        HydroStationData otherObject = obj;

        String sortType = sortCB.getSortType();

        if (sortType.compareTo("State,County") == 0) {
            return stateCounty.compareTo(otherObject.getStateCounty());
        } else if (sortType.compareTo("Name") == 0) {
            return name.compareTo(otherObject.getName());
        } else // Sort by station is the default
        {
            return station.compareTo(otherObject.getStation());
        }
    }
}
