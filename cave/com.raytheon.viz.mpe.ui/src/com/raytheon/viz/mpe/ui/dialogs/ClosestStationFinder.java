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
package com.raytheon.viz.mpe.ui.dialogs;

import java.util.List;

import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 7, 2011            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class ClosestStationFinder {
    private StationFilter _stationFilter = null;

    // --------------------------------------------------------

    public ClosestStationFinder(StationFilter stationFilter) {
        _stationFilter = stationFilter;

    }

    // --------------------------------------------------------

    public int findClosestStation(List<Station> stationList,
            Coordinate clickedLatLonCoord) {
        // String header = "EditPrecipStationsDialog.findClosestStation(): ";
        // Coordinate clickedPixelCoordinate =
        // getPixelCoordinateFromLatLonCoordinate(clickedLatLonCoord);

        double maxdist = Double.MAX_VALUE; // large number to start with

        int index = -1;
        // for each station
        for (int i = 0; i < stationList.size(); i++) {
            Station currentStation = stationList.get(i);

            if (_stationFilter.shouldFilterOut(i)) {
                continue;
            }

            double distance = getDistanceFromStation(clickedLatLonCoord,
                    currentStation);

            if (distance < maxdist) {
                index = i;
                maxdist = distance;
            }
        } // end for i < max_stations

        return index;
    }

    // --------------------------------------------------------

    private double getDistanceFromStation(Coordinate clickedLatLonCoordinate,
            Station station) {
        // String header = "ClosestStationFinderg.getDistanceFromStation(): ";

        double distance = Double.MAX_VALUE;

        // the smallFactor is used to distinguish 2 stations or 1 stations with
        // multiple type sources at the same location
        // this code piggybacks on the feature that allows nearby stations to
        // have their associated
        // text displayed in one of 4 quadrants
        // if 2 stations are at the same location, then selecting the first one
        // allows the user to
        // set its text location to be a different quadrant. From then on,
        // either station can be selected by clicking
        // closer to its label
        double smallFactor = 0.0000001;
        double apparent_x = station.lon + (station.xadd * smallFactor);
        double apparent_y = station.lat + (station.yadd * -smallFactor);

        double x_diff = clickedLatLonCoordinate.x - apparent_x; // lon
        double y_diff = clickedLatLonCoordinate.y - apparent_y; // lat

        double distanceSquared = (x_diff * x_diff) + (y_diff * y_diff);
        // double distance = Math.pow(distanceSquared, .5);

        // it is not necessary to get the exact distance,
        // distanceSquared is faster and works
        distance = distanceSquared;

        return distance;

    }
}
