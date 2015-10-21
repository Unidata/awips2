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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * This class combines code from 1) ReadPrecipStationLists, 2)
 * ReadTemperatureStationLists, and 3) ReadFreezingStationLists. Most of the
 * code in those 3 classes were duplicated in each. The above 3 classes assumed
 * that the stations were grouped together and in a particular order in the
 * station list file. This new class reads and parses each line individually and
 * makes no assumptions about grouping or order.
 * 
 * Lines with a '#' character as the first character on a line are treated as a
 * comment and ignored. Anything after a '!' character in the middle of a line
 * is similarly treated as a comment and ignored.
 * 
 * The first 3 characters of the second field on a given line (Station.parm?) is
 * used to detemine the type of Station - PPD = Daily Precip, TAI = Temperature,
 * HZI = Freezing.
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
 * Oct 14, 2015 17977     snaples     Updated readStationLists to recognize 
 *                                    sub area as a new area and not use masterFileFlag.
 * 
 * </pre>
 * 
 * @version 1.0
 */

public class StationListManager {
    private static final int MONTHS_PER_YEAR = 12;

    private final AppsDefaults appsDefaults = AppsDefaults.getInstance();

    private static StationListManager instance;

    private final String station_dir = appsDefaults
            .getToken("mpe_station_list_dir");

    private final String sitename = "";

    private final DailyQcUtils dqc = DailyQcUtils.getInstance();

    private static long previousStationsModifiedTime = -99L;

    private static long previousLabelModifiedTime = -99L;

    /**
     * Retrieve singleton instance
     * 
     * @return singleton instance of StationListManager
     */
    public static synchronized StationListManager getInstance() {
        if (instance == null) {
            instance = new StationListManager();
        }

        return instance;
    }

    public void getStationInfo(String qcArea, boolean newarea,
            ArrayList<Station> freezingStationList,
            ArrayList<Station> temperatureStationList,
            ArrayList<Station> precipStationList) throws FileNotFoundException {

        // String header = "StationListManager.getStationInfo(): ";

        String pathName = getStationListPath(qcArea);

        if (pathName == null) {
            throw new FileNotFoundException("In getStationInfo, pathName "
                    + "missing.  qcArea is: " + qcArea);
        }

        long currentLabelModifiedTime;
        String labelPathName = (pathName + "_label_positions");
        File labelPositionFile = new File(labelPathName);
        long currentStationsModifiedTime;
        File stationListFile = new File(pathName);

        currentLabelModifiedTime = labelPositionFile.lastModified();
        currentStationsModifiedTime = stationListFile.lastModified();

        if ((newarea == true) || (((currentLabelModifiedTime != previousLabelModifiedTime)
                || (currentStationsModifiedTime != previousStationsModifiedTime)))) {
            LabelPositionManager labelPositionManager = new LabelPositionManager();

            previousLabelModifiedTime = currentLabelModifiedTime;

            try {
                if (labelPositionFile.exists()) {
                    FileReader labelPositionFileReader = new FileReader(
                            labelPositionFile);
                    labelPositionManager
                            .readLabelPositions(labelPositionFileReader);
                }

                previousStationsModifiedTime = currentStationsModifiedTime;

                FileReader stationListFileReader = new FileReader(pathName);

                readStationLists(stationListFileReader, 
                        labelPositionManager, freezingStationList,
                        temperatureStationList, precipStationList);

                computeClosestNeighbors(precipStationList);
                computeClosestNeighbors(temperatureStationList);

            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }

    } /* getStationInfo */

    /**
     * Method readStationLists opens the station list file, reads one line at a
     * time, and adds each entry's information to the appropriate station list.
     * This method will throw an IOException if the pathName is null.
     * 
     * @param qcArea
     * @param masterFileFlag
     * @param freezingStationList
     * @param temperatureStationList
     * @param precipStationList
     */

    private void readStationLists(FileReader stationListFileReader,
            LabelPositionManager labelPositionManager,
            ArrayList<Station> freezingStationList,
            ArrayList<Station> temperatureStationList,
            ArrayList<Station> precipStationList) throws IOException {
        String header = "StationListManager.readStationLists(): ";

        String inputLine; // one line of input
        BufferedReader inputReader = new BufferedReader(stationListFileReader);

            try {
                String[] commentTokens; // tokens broken into input and comments
                String[] stationTokens; // tokens from line of input (actual
                                        // Station data)

                inputLine = inputReader.readLine();

                while (inputLine != null) {
                    stationTokens = null;

                    if (inputLine.charAt(0) != '#') // not a comment
                    {
                        // Strip away anything after a '!' (in-line comment)
                        // character
                        commentTokens = inputLine.split("!");
                        stationTokens = commentTokens[0].split("\\s+", 7);
                    }

                    if ((stationTokens != null) && // If we got input and
                            (stationTokens.length == 7) && // input is valid and
                            (inputLine.charAt(0) != '#')) // not a comment
                    {
                        Station aStation = dqc.new Station();
                        aStation.hb5 = stationTokens[0].toString().trim(); // Location
                                                                           // ID
                        aStation.parm = stationTokens[1].toString().trim();
                        double lat = Double.parseDouble(stationTokens[2]);
                        double lon = -Double.parseDouble(stationTokens[3]);
                        aStation.lat = (float) lat;
                        aStation.lon = (float) lon;
                        int elev;

                        elev = Integer.parseInt(stationTokens[4]);

                        if (elev == 0) // Don't know why this is done djsiii
                        {
                            elev = 1;
                        }

                        aStation.elev = elev;
                        aStation.tip = Integer.parseInt(stationTokens[5]);
                        Coordinate hrap_point;
                        Coordinate latlon = new Coordinate();

                        latlon.x = lon;
                        latlon.y = lat;
                        hrap_point = dqc.getLatLontoHrap(latlon);
                        aStation.hrap_x = (float) hrap_point.x;
                        aStation.hrap_y = (float) hrap_point.y;
                        aStation.name = stationTokens[6].toString().trim();
                        aStation.isoh = new float[24]; // 24 hrs in a day?
                        aStation.min = new float[MONTHS_PER_YEAR];
                        aStation.max = new float[MONTHS_PER_YEAR];

                        Arrays.fill(aStation.isoh, -99.0f); // Initialize
                        Arrays.fill(aStation.min, -99.0f); // Initialize
                        Arrays.fill(aStation.max, -99.0f); // Initialize

                        // At this point, we've built a Station object. Store
                        // label positions coordinates (screen coordinates) in
                        // the Station object
                        labelPositionManager.setLabelPosition(aStation);

                        if (aStation.parm.regionMatches(true, 0, "PPD", 0, 3)) {
                            precipStationList.add(aStation);
                        } else if (aStation.parm.regionMatches(true, 0, "TAI",
                                0, 3)) {
                            temperatureStationList.add(aStation);
                        } else if (aStation.parm.regionMatches(true, 0, "HZI",
                                0, 3)) {
                            freezingStationList.add(aStation);
                        }

                    } /* if we have valid, processible input */

                    inputLine = inputReader.readLine(); // Get the next line of
                                                        // input for parsing

                } /* while (inputLine != null) */

                System.out.println(header + "precipStationList.size() = "
                        + precipStationList.size());

            } /* try */

            catch (IOException e) {
                System.out.println("Exception in: 'readStationLists'.");
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

            finally {
                try {
                    if (inputReader != null) {
                        inputReader.close();
                    }
                } /* try */
                catch (IOException e) {
                    System.out.println("UNKNOWN exception in: "
                            + "'readStationLists', " + "finally block.");
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                } /* catch */

            } /* finally */

    } /* readStationLists */

    private String getStationListPath(String qcArea) {
        String stationListPath;

        if (qcArea != null) {
            if (station_dir.length() > 0) {
                stationListPath = station_dir + "/" + qcArea + "_station_list";
            } else {
                stationListPath = qcArea;
            }
        } else {
            if (station_dir.length() > 0) {
                stationListPath = station_dir + "/" + sitename
                        + "_station_list";
            } else {
                stationListPath = sitename;
            }
        }

        return stationListPath;

    } /* getStationListPath */

    private void addDistanceToSortedList(
            ArrayList<StationDistance> sortedDistanceList,
            StationDistance currentDistance) {

        StationDistanceComparator comparator = new StationDistanceComparator();

        int index = Collections.binarySearch(sortedDistanceList,
                currentDistance, comparator);

        int insertIndex = -1;
        if (index >= 0) // value was in list already
        {
            insertIndex = index;
        } else // value was not in list already
        {
            // when not present, index = ( -(insertIndex) - 1)
            insertIndex = -(index + 1);
        }

        sortedDistanceList.add(insertIndex, currentDistance);
    }

    /*
     * Compute the list of closest neighbor's. For each station, this list will
     * include a list of the indexes of the closest gages.
     */

    public void computeClosestNeighbors(ArrayList<Station> stationList) {
        // String header = "StationListManager.computeClosestNeighbors(): ";

        final int maxNeighborListSize = DailyQcUtils.mpe_dqc_max_precip_neighbors;
        // System.out.println(header + "max_neighbor_list_size = " +
        // maxNeighborListSize);

        int stationCount = stationList.size();
        int thisStationIndex;
        int otherStationIndex; // Saves repeated declarations inside loop

        for (thisStationIndex = 0; thisStationIndex < stationCount; ++thisStationIndex) {

            Station thisStation = stationList.get(thisStationIndex);

            ArrayList<StationDistance> sortedDistanceList = new ArrayList<StationDistance>();

            for (otherStationIndex = 0; otherStationIndex < stationCount; ++otherStationIndex) {
                Station otherStation = stationList.get(otherStationIndex);

                if (thisStation != otherStation) {
                    double distance = getDistance(thisStation, otherStation);

                    StationDistance currentDistance = new StationDistance(
                            distance, otherStationIndex);

                    // System.out.println(header +
                    // "before adding to sortedList, size() = " +
                    // sortedDistanceList.size());
                    addDistanceToSortedList(sortedDistanceList, currentDistance);
                    // System.out.println(header +
                    // "after adding to sortedList, size() = " +
                    // sortedDistanceList.size());

                    // remove any extra neighbors
                    if (sortedDistanceList.size() > maxNeighborListSize) {
                        sortedDistanceList
                                .remove(sortedDistanceList.size() - 1);
                    }
                    // System.out.println(header +
                    // "after trimming from sortedList, size() = " +
                    // sortedDistanceList.size());
                }
            }

            /*
             * At this point the arrayList should contain the desired distances
             * to all other stations. Copy them to thisStation's index[] array.
             */
            // System.out.println(header + "sortedDistanceList.size() = " +
            // sortedDistanceList.size());

            thisStation.index = new short[maxNeighborListSize];
            for (int i = 0; i < sortedDistanceList.size(); ++i) {
                StationDistance stationDistance = sortedDistanceList.get(i);
                thisStation.index[i] = (short) stationDistance
                        .getStationIndex();
            }
        }

        /*
         * 
         * for (i = 0; i < maxPStations; i++) { statI = stations.get(i);
         * 
         * if (statI.index == null) { statI.index = new short[qcn]; }
         * 
         * for (int l = 0; l < qcn; l++) { sorted[l] = 9999999; }
         * 
         * for (int m = 0; m < maxPStations; ++m) { if (i == m) { continue; }
         * 
         * statM = stations.get(m);
         * 
         * if (statM.index == null) { statM.index = new short[qcn]; }
         * 
         * deltaLat = statI.lat - statM.lat; deltaLon = ((statI.lon - statM.lon)
         * Math.cos(statI.lat + statM.lat) / 2 * conv); dist = (deltaLat *
         * deltaLat) + (deltaLon * deltaLon);
         * 
         * for (int l = 0; l < qcn; l++) { if (dist < sorted[l]) { for (int h =
         * qcn - 1; h > l; h--) { sorted[h] = sorted[h - 1]; statI.index[h] =
         * statI.index[h - 1]; }
         * 
         * sorted[l] = dist; statI.index[l] = (short) m;
         * 
         * break; } } }
         * 
         * stations.set(i, statI); }
         */

    } /* computeClosestNeighbors */

    private double getDistance(Station station1, Station station2) {

        float conv = .0174f;

        double deltaLat = station1.lat - station2.lat;
        double deltaLon = ((((station1.lon - station2.lon) * Math
                .cos(station1.lat + station2.lat)) / 2) * conv);

        double distSquared = (deltaLat * deltaLat) + (deltaLon * deltaLon);

        double distance = Math.sqrt(distSquared);

        return distance;
    }

    private class StationDistanceComparator implements
            Comparator<StationDistance> {

        @Override
        public int compare(StationDistance stationDistance1,
                StationDistance stationDistance2) {
            return Double.compare(stationDistance1.getDistance(),
                    stationDistance2.getDistance());
        }

    };

} /* class StationListManager */
