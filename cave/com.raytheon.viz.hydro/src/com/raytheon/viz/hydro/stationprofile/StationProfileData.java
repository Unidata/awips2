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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.shef.tables.Statprof;

/**
 * This class contains data for the station profile.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation
 * 18 Jan 2018  6822       mduff       Moved some data specific information into this data container.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 *
 */
public class StationProfileData {
    private static final int SCALE_ROUNDING = 5;

    private static final int MIN_RIVER_MILES = 200;

    /**
     * Stream name.
     */
    private String streamName;

    /**
     * Maximum elevation in feet.
     */
    private int elevationScaleMax = 0;

    /**
     * Minimum elevation in feet.
     */
    private int elevationScaleMin = 0;

    private double maxMile = -9999;

    private double minMile = 9999;

    private double mileRange;

    private List<Statprof> stationList;

    /**
     * Map of station names (key) and the associated station data (value).
     */
    private Map<String, Statprof> stationDataMap;

    /**
     * Constructor.
     * 
     * @param streamName
     *            Stream name.
     * @param elevationFtMax
     *            Maximum elevation in feet.
     * @param elevationFtMin
     *            Minimum elevation in feet.
     */
    public StationProfileData(String streamName, List<Statprof> stationList) {
        this.streamName = streamName;
        this.stationList = stationList;
        populate(stationList);
    }

    private void populate(List<Statprof> stationList) {
        stationDataMap = new HashMap<>();
        for (Statprof station : stationList) {
            stationDataMap.put(station.getId().getLid(), station);
        }

        setDataFields();
    }

    /**
     * This method sets the data fields needed for the plot drawing. minElev and
     * maxElev are the min and max values for the y-axis and set to
     * elevationScaleMin and elevationScaleMax. minMile and maxMile are the min
     * and max mile for the x axis.
     */
    private void setDataFields() {
        double maxElev = -9999;
        double minElev = 9999;
        for (Statprof station : stationList) {
            double zd = station.getId().getZd();
            double fs = station.getId().getFs();
            if (zd + fs > maxElev) {
                maxElev = zd;
            }

            if (zd + fs < minElev) {
                minElev = zd;
            }

            double mile = station.getId().getMile();
            if (mile > maxMile) {
                maxMile = mile;
            }
            if (mile < minMile) {
                minMile = mile;
            }
        }

        long lmax = (long) maxElev / SCALE_ROUNDING;
        maxElev = (lmax + 100) * SCALE_ROUNDING;

        if (maxElev < 0) {
            maxElev = 0;
        }

        long lmin = (long) minElev / SCALE_ROUNDING;
        minElev = (lmin - 100) * SCALE_ROUNDING;

        if (minElev > 19_999) {
            minElev = 0;
        }

        elevationScaleMax = (int) maxElev;
        elevationScaleMin = (int) minElev;

        /*
         * Create the maxMile to be a multiple of 50.
         */
        maxMile = 50 + Math.floor(maxMile);
        while (maxMile % 50 != 0) {
            maxMile--;
        }

        lmin = (long) minMile / SCALE_ROUNDING;
        minMile = (lmin - 10) * SCALE_ROUNDING;
        minMile += (long) minMile % 10;

        if (minMile < 0) {
            minMile = 0;
        }

        /*
         * Add 50 so the max range can be the next multiple of 50 that is larger
         * than the value.
         */
        mileRange = (maxMile - minMile) + 50;
        if (mileRange < (MIN_RIVER_MILES)) {
            mileRange = MIN_RIVER_MILES;
        }
    }

    /**
     * Get the stream name.
     * 
     * @return The stream name.
     */
    public String getStreamName() {
        return streamName;
    }

    /**
     * Get the maximum scale elevation.
     * 
     * @return The maximum scale elevation.
     */
    public int getElevationScaleMax() {
        return elevationScaleMax;
    }

    /**
     * Get the minimum scale elevation.
     * 
     * @return The minimum elevation.
     */
    public int getElevationScaleMin() {
        return elevationScaleMin;
    }

    /**
     * Get the station data.
     * 
     * @param key
     *            Map key (station name).
     * @return The station data.
     */
    public Statprof getStationData(String key) {
        return stationDataMap.get(key);
    }

    /**
     * Get the set of station data objects.
     * 
     * @return List of station data
     */
    public List<Statprof> getStations() {
        return stationList;
    }

    public double getMileRange() {
        return mileRange;
    }

    public double getMaxMile() {
        return maxMile;
    }

    public double getMinMile() {
        return minMile;
    }
}
