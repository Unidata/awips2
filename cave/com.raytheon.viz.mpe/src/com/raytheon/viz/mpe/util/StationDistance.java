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
package com.raytheon.viz.mpe.util;

import com.raytheon.viz.mpe.util.DailyQcUtils.Station;

/**
 * Objects in this class contain enough information to determine the nearest
 * neighbors to other Station objects. This class is used by class
 * StationListManager in method computeClosestNeighbors. Also included are
 * methods to compute distance 1) between two Stations and 2) between two
 * lat/lon coordinates.
 * 
 * @author daniel.stein
 * 
 *         <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 7, 2011            snaples     Initial creation
 * 
 * </pre>
 * 
 * 
 * @version 1.0
 */

public class StationDistance {
    private double distance;

    private int stationIndex; // This station's position in the station list.

    private static float conversionConstant = 0.0174f;

    public StationDistance() {
        distance = -99.0;
        stationIndex = -99;
    }

    public double getDistance() {
        return distance;
    }

    public int getStationIndex() {
        return stationIndex;
    }

    public StationDistance(double distance, int stationIndex) {
        this.distance = distance;
        this.stationIndex = stationIndex;
    }

    public static double computeDistance(Station station1, Station station2) {
        return computeDistance(station1.lat, station1.lon, station2.lat,
                station2.lon);
    }

    public static double computeDistance(float lat1, float lon1, float lat2,
            float lon2) {
        float deltaLat = lat1 - lat2;
        float deltaLon = (float) ((lon1 - lon2) * Math.cos(lon1 + lon2) / 2 * conversionConstant);
        return ((deltaLat * deltaLat) + (deltaLon * deltaLon));
    }

} /* class StationDistance */
