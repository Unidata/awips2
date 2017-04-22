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
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Scanner;
import java.util.TimeZone;
import java.util.TreeSet;

import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypc;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypp;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.hydro.CommonHydroConstants;
import com.raytheon.uf.common.hydro.data.PrecipTotal;
import com.raytheon.uf.common.mpe.util.PrecipUtil;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.util.DailyQcUtils.QCHRAP;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.raytheon.viz.mpe.util.Disagg6Hr.Dist;
import com.raytheon.viz.mpe.util.Disagg6Hr.Values_1hr;
import com.raytheon.viz.mpe.util.MeanMonthlyPrecip.Isoh;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Compute 1 Hour Neighbor List
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2009            snaples     Initial creation
 * Mar  2, 2015  15660     snaples     Fixed issues with the file writing and creating the new files.
 * Apr 05, 2016  18350     snaples     Changed call to DailyQC Utils getInstance.
 * Jul 18, 2016   4600     njensen     Some cleanup
 * Jul 25, 2016   4623     skorolev    Replaced TotalPrecip with PrecipTotal. Cleanup.
 * 
 * </pre>
 * 
 * @author snaples
 */

public class Compute1HrNeighborList {

    public static Values_1hr[] valuesReadIn = null;

    public static int[] sortedList1hr = null;

    public static int sortedArraySize = 0;

    public static Station[] disaggStation1hr;

    private final IUFStatusHandler logger = UFStatus
            .getHandler(this.getClass());

    private Station[] disaggStation6hr = null;

    private Dist[] dist6hrTo1hr = null;

    private Date stationListTimeStamp6hr = TimeUtil.newDate();

    private String neighborListFile = null;

    private int retNeighbors = Disagg6Hr.ret_neighbors;

    private int numRecords = -1;

    private int mpeDqcMaxPrecipNeighbors = DailyQcUtils.mpe_dqc_max_precip_neighbors;

    private PrecipUtil pu = PrecipUtil.getInstance();

    public Compute1HrNeighborList() {
        // default constructor
    }

    public void free_1hr_station_list() {
        if (disaggStation1hr != null) {
            disaggStation1hr = null;
        }
    }

    /**
     * Computes 1hr Station List
     */
    public void compute1hrStationList() {
        MeanMonthlyPrecip mmp = new MeanMonthlyPrecip();
        Isoh isoh = mmp.getIsoh();
        int smonth = DailyQcUtils.smonth;
        int emonth = DailyQcUtils.emonth;

        String hb5 = "";
        double dist1, dist2, dist;
        double[] sorted = new double[mpeDqcMaxPrecipNeighbors];
        int ind, h, i, ier, l, m, k;
        QCHRAP hrapPoint = new QCHRAP();
        float lat, lon;
        double conv = .0174533;
        disaggStation6hr = Disagg6Hr.disagg_station_6hr;
        dist6hrTo1hr = Disagg6Hr.dist_6hr_to_1hr;
        neighborListFile = Disagg6Hr.neighbor_list_file;
        int numDisaggStations = Disagg6Hr.num_disagg_stations;

        boolean fileError = false;
        String areaId;
        String fname = "_station_list";
        String disagg1hrStationsDir;
        String disagg1hrStationsPath;
        int numDistinct1hrs = 0;

        int xor, yor, maxx, maxy;

        Date timestampPreviousNeighbors = new Date();
        int numPreviousNeighbors = 0;
        boolean generate = true;
        Boolean duplicateStation = false;
        String tempBuf = "";
        AppsDefaults appsDefaults = AppsDefaults.getInstance();

        disagg1hrStationsDir = appsDefaults.getToken("mpe_station_list_dir");
        areaId = appsDefaults.getToken("mpe_site_id");

        try (BufferedWriter out = Disagg6Hr.getDisagg_log_fd()) {
            if (areaId.isEmpty()) {
                out.write("token mpe_site_id not defined\n");
                return;
            } else {
                disagg1hrStationsPath = String.format("%s/%s%s",
                        disagg1hrStationsDir, areaId, fname);
            }
            try (BufferedReader in = new BufferedReader(new FileReader(
                    disagg1hrStationsPath))) {
                out.write(String.format("mpe_gage_locations filename = %s\n",
                        disagg1hrStationsPath));

                /* Read the record containing the number of hourly stations. */
                String hstat = in.readLine().trim();
                int p = hstat.length();

                if (p == -1) {
                    out.write("could not read number of hourly stations from file\n");
                    return;
                }

                numRecords = Integer.parseInt(hstat);

                if (numRecords < 1) {
                    out.write("could not read number of hourly stations from file\n");
                    return;
                }

                if (numRecords <= 0) {
                    out.write("number of hourly stations le 0\n");
                    return;
                }

                /* Allocate space for the stations array. */
                disaggStation1hr = new Station[numRecords];

                xor = (int) MPEDataManager.getInstance().getHRAPExtent()
                        .getMinX();
                yor = (int) MPEDataManager.getInstance().getHRAPExtent()
                        .getMinY();
                maxx = (int) MPEDataManager.getInstance().getHRAPExtent()
                        .getMaxX();
                maxy = (int) MPEDataManager.getInstance().getHRAPExtent()
                        .getMaxY();

                /* Read the PPH stations. */
                i = 0;
                for (ind = 0; ind < numRecords; ++ind) {
                    String pp = in.readLine().trim();

                    if (pp == null) {
                        out.write(String
                                .format(" error reading 1hr station list -- ind = %d numRecords = %d\n",
                                        ind, numRecords));
                        break;
                    }

                    Scanner s = new Scanner(pp);
                    hb5 = s.next();
                    s.next(); // unused string
                    lat = s.nextFloat();
                    lon = s.nextFloat();

                    DailyQcUtils dqc = DailyQcUtils.getInstance();
                    disaggStation1hr[i] = dqc.new Station();
                    disaggStation1hr[i].isoh = new float[24];
                    disaggStation1hr[i].hb5 = "";
                    disaggStation1hr[i].index = new short[DailyQcUtils.mpe_dqc_max_precip_neighbors];
                    disaggStation1hr[i].parm = "";
                    disaggStation1hr[i].cparm = "";

                    // eliminate duplicate stations from the station list
                    // file
                    for (k = 0; k < numDistinct1hrs; k++) {
                        if (hb5.equalsIgnoreCase(disaggStation1hr[k].hb5)) {
                            // we found a duplicate station
                            duplicateStation = true;
                            break;
                        }

                    }

                    if (duplicateStation) {
                        duplicateStation = false;
                        continue;
                    } else {
                        numDistinct1hrs++;
                    }

                    disaggStation1hr[i].hb5 = hb5;
                    disaggStation1hr[i].lat = lat;
                    float testlon = lon;
                    if (testlon > 0) {
                        testlon = testlon * -1;
                    }
                    disaggStation1hr[i].lon = testlon;

                    /*
                     * Set elev to 1 ft. The value of 0 creates problems with
                     * subsequent computations in DailyQC.
                     */

                    disaggStation1hr[i].elev = 1;

                    lat = disaggStation1hr[i].lat;
                    lon = disaggStation1hr[i].lon;

                    /* Store the station's coordinates in HRAP. */

                    Coordinate ll = new Coordinate();
                    ll.x = disaggStation1hr[i].lon;
                    ll.y = disaggStation1hr[i].lat;
                    ReferencedCoordinate rc = new ReferencedCoordinate(ll);
                    Coordinate gridCell = new Coordinate();
                    try {
                        gridCell = rc.asGridCell(
                                com.raytheon.uf.common.xmrg.hrap.HRAP
                                        .getInstance().getGridGeometry(),
                                PixelInCell.CELL_CORNER);
                    } catch (Exception e) {
                        logger.error(
                                "Error determining grid cell for referenced coordinate "
                                        + ll.x + ", " + ll.y, e);
                        // TODO should this return/exit early?
                    }
                    hrapPoint.x = (short) gridCell.x;
                    hrapPoint.y = (short) gridCell.y;

                    disaggStation1hr[i].hrap_x = hrapPoint.x;
                    disaggStation1hr[i].hrap_y = hrapPoint.y;
                    MeanMonthlyTemp mmt = new MeanMonthlyTemp();

                    for (k = 0; k < 12; k++) {
                        disaggStation1hr[i].isoh[k] = -1;

                        ier = mmt.is_good(k, smonth, emonth);
                        if (ier == -1) {
                            continue;
                        }

                        if (((int) hrapPoint.x < maxx)
                                && ((int) hrapPoint.y < maxy)
                                && ((int) hrapPoint.x >= xor)
                                && ((int) hrapPoint.y >= yor)) {
                            disaggStation1hr[i].isoh[k] = isoh.value[k][(int) hrapPoint.y
                                    - yor][(int) hrapPoint.x - xor];
                        }
                    }

                    i++;
                    s.close();
                }

                numRecords = numDistinct1hrs;
            } catch (Exception e) {
                logger.error("Error opening file " + disagg1hrStationsPath
                        + " ", e);
            }

            /*
             * code to find if 1hr nearest neighbors file already exists if
             * exists, then check if either of, the number of neighbors token or
             * the last modification time of the 6hr station lists, both of
             * which are stored in the nearest neighbors file, have changed. if
             * neither of them have changed, just read the file instead of
             * generating the file, else re-generate.
             */
            if (retNeighbors == 0) {
                try (BufferedReader in2 = new BufferedReader(new FileReader(
                        neighborListFile))) {
                    String lines = in2.readLine();
                    Scanner s = new Scanner(lines);
                    if (lines != null) {
                        numPreviousNeighbors = s.nextInt();
                    } else {
                        generate = false;

                    }
                    lines = in2.readLine();
                    if (lines != null) {
                        timestampPreviousNeighbors.setTime(s.nextInt());

                    } else {
                        generate = false;
                    }

                    if (mpeDqcMaxPrecipNeighbors != numPreviousNeighbors
                            || timestampPreviousNeighbors
                                    .before(stationListTimeStamp6hr)) {
                        generate = false;
                    } else {
                        out.write(String.format(
                                "Reading %d nearest neighbor 1hr stations\n",
                                mpeDqcMaxPrecipNeighbors));

                        for (i = 0; i < Disagg6Hr.num_disagg_stations; i++) {
                            lines = in2.readLine();
                            for (l = 0; l < mpeDqcMaxPrecipNeighbors; l++) {
                                /*
                                 * read from neighbor list file read the 1hr
                                 * station info for each 6hrstation lines =
                                 * in.readLine();
                                 */
                                if (lines != null) {
                                    disaggStation6hr[i].index[l] = s
                                            .nextShort();
                                    disaggStation1hr[disaggStation6hr[i].index[l]].hb5 = s
                                            .next();
                                    disaggStation1hr[disaggStation6hr[i].index[l]].lat = s
                                            .nextFloat();
                                    disaggStation1hr[disaggStation6hr[i].index[l]].lon = s
                                            .nextFloat();
                                    dist6hrTo1hr[i].distances_to_neighbors[l] = s
                                            .nextDouble();
                                } else {
                                    out.write("Error reading neighbor 1hr stations...file corrupted...generating 1hr neighbours\n");
                                    fileError = true;
                                }
                            }
                            if (fileError) {
                                break;
                            }
                        }
                        generate = true;
                    }
                    s.close();
                } catch (Exception e) {
                    logger.error(
                            "Error opening file " + neighborListFile + " ", e);
                }
            } else {
                generate = false;
            }
            if (fileError) {
                generate = false;
            }
            if (!generate) {
                /*
                 * Logic to find 1hr nearest neighbors for the 6hr disagg
                 * stations
                 */
                out.write(String.format(
                        "Generating %d nearest neighbor 1hr stations\n",
                        mpeDqcMaxPrecipNeighbors));
                for (i = 0; i < Disagg6Hr.num_disagg_stations; i++) {
                    for (l = 0; l < mpeDqcMaxPrecipNeighbors; l++) {
                        sorted[l] = 9999999;
                    }
                    for (m = 0; m < numRecords; ++m) {
                        // do not use 1hr station with same id as 6hr
                        // station
                        if (disaggStation1hr[m].hb5
                                .equalsIgnoreCase(disaggStation6hr[i].hb5)) {
                            continue;

                        }
                        dist1 = disaggStation6hr[i].lat
                                - disaggStation1hr[m].lat;
                        dist2 = (disaggStation6hr[i].lon - disaggStation1hr[m].lon)
                                * Math.cos((disaggStation6hr[i].lat + disaggStation1hr[m].lat)
                                        * conv / 2.);
                        dist = Math.pow(dist1, 2) + Math.pow(dist2, 2);

                        /*
                         * In the following logic we calculate if the current
                         * distance between the 6hr and 1hr stations is shorter
                         * than one of the gages in the 'sorted' array. If it is
                         * we add it and remove the farthest distance from the
                         * array. Note that the 'sorted' array is sorted
                         * literally!
                         */

                        for (l = 0; l < mpeDqcMaxPrecipNeighbors; l++) {
                            if (dist < sorted[l]) {
                                for (h = mpeDqcMaxPrecipNeighbors - 1; h > l; h--) {
                                    sorted[h] = sorted[h - 1];
                                    disaggStation6hr[i].index[h] = disaggStation6hr[i].index[h - 1];
                                }

                                /*
                                 * add this distance into the array of current
                                 * "num neighbors" shortest distances
                                 */
                                sorted[l] = dist;
                                /*
                                 * add the 1hr station as a nearest neighbor to
                                 * he 6hr station
                                 */
                                disaggStation6hr[i].index[l] = (short) m;
                                /*
                                 * storing the distance of this 1hr gage from
                                 * the 6hr gage
                                 */
                                dist6hrTo1hr[i].distances_to_neighbors[l] = dist;
                                break;
                            }
                        }
                    }
                }
                // ----------------------------------------------------------------
            }
            try (BufferedWriter out2 = new BufferedWriter(new FileWriter(
                    neighborListFile))) {
                if (!generate) {
                    out2.write(String.format("%d\n", mpeDqcMaxPrecipNeighbors));
                    File nlf = new File(neighborListFile);
                    stationListTimeStamp6hr.setTime(nlf.lastModified());
                    out2.write(String.format("%d\n",
                            (int) stationListTimeStamp6hr.getTime()));
                }

                /* print out list of 1hr nearest neighbor stations */
                for (i = 0; i < numDisaggStations; i++) {
                    out.write(String.format("---%s---\n",
                            disaggStation6hr[i].hb5));
                    if (!generate) {
                        out2.write(String.format("%s\n",
                                disaggStation6hr[i].hb5));
                    }

                    for (l = 0; l < mpeDqcMaxPrecipNeighbors; l++) {
                        tempBuf = String
                                .format("%d\t%s\t%5.2f\t%5.2f\t%6.2f",
                                        disaggStation6hr[i].index[l],
                                        disaggStation1hr[disaggStation6hr[i].index[l]].hb5,
                                        disaggStation1hr[disaggStation6hr[i].index[l]].lat,
                                        disaggStation1hr[disaggStation6hr[i].index[l]].lon,
                                        dist6hrTo1hr[i].distances_to_neighbors[l] * 60.);
                        if (!generate) {
                            out2.write(String.format("%s\n", tempBuf));
                        }
                        out.write(String.format("%s\n", tempBuf));
                    }
                }
            } catch (Exception e) {
                logger.error("Error opening file " + neighborListFile + " ", e);
            }
        } catch (Exception e) {
            logger.error("Error computing 1 hr station list ", e);
        }
    }

    /**
     * Reads 1hr Gage Values
     */
    public void read1hrGageVals() {
        int[] pp_rec_cnt = new int[1];
        int[] pc_rec_cnt = new int[1];
        int duration = 1; /* The duration in hours. */
        PrecipTotal totalPrecip;
        Date endtime_disagg = Disagg6Hr.endtime_disagg;
        Date starttime_disagg = Disagg6Hr.starttime_disagg;
        Date end_time_temp;
        List<Hourlypp> pOrigHourlyPP = null;
        Hourlypp pHourlyPP = null;
        List<Hourlypc> pOrigHourlyPC = null;
        Hourlypc pHourlyPC = null;
        double min_percent = 0.0;
        boolean advance = false;
        int day = 0;
        int i, j, k;
        int num_disagg_stations = Disagg6Hr.num_disagg_stations;
        int[] temp = new int[num_disagg_stations * mpeDqcMaxPrecipNeighbors];
        BufferedWriter out = Disagg6Hr.getDisagg_log_fd();
        disaggStation6hr = Disagg6Hr.disagg_station_6hr;
        int qc_days = DailyQcUtils.qcDays;

        try {
            out.write(" Reading 1hr Precip Gage Values \n");

            sortedList1hr = new int[num_disagg_stations
                    * mpeDqcMaxPrecipNeighbors];
            /* sorted_list_1hr array is array of indexes of 1hr gages */
            /* defined in compute_1hr_station_list function */

            for (i = 0; i < num_disagg_stations * mpeDqcMaxPrecipNeighbors; i++) {
                sortedList1hr[i] = -1;
            }
            for (i = 0; i < num_disagg_stations; i++) {
                for (j = 0; j < mpeDqcMaxPrecipNeighbors; j++) {
                    sortedList1hr[mpeDqcMaxPrecipNeighbors * i + j] = disaggStation6hr[i].index[j];
                }
            }

            // remove duplicates (if any) from among sets of 20 nearest-neighbor
            // stations
            temp = sortedList1hr;
            TreeSet<Integer> set = new TreeSet<>();
            for (int v : temp) {
                set.add(v);
            }

            Integer[] foo = new Integer[0];
            set.toArray(foo);
            sortedList1hr = new int[foo.length];

            for (i = 0; i < foo.length; i++) {
                sortedList1hr[i] = foo[i].intValue();
            }
            Arrays.sort(sortedList1hr);
            sortedArraySize = sortedList1hr.length;
            out.write(String
                    .format(" number of non-duplicate 1hr nearest-neighbor stations = %d\n",
                            sortedArraySize));

            valuesReadIn = new Values_1hr[qc_days * sortedArraySize];

            out.write(String.format(
                    "endtime_disagg = %10.0f  starttime_disagg = %10.0f\n",
                    (float) endtime_disagg.getTime(),
                    (float) starttime_disagg.getTime()));

            // get 1hr precip gage values using precip totalling routines
            // loop on the number of unique (non-duplicate) 1hr stations
            MPEDataManager mpeDM = MPEDataManager.getInstance();
            for (i = 0; i < sortedArraySize; i++) {
                day = 0;
                if (sortedList1hr[i] != -1) {
                    out.write(String.format("station = %s\n",
                            disaggStation1hr[sortedList1hr[i]].hb5));

                    /* load precip data for all hours for this station */
                    pOrigHourlyPP = mpeDM.loadPPHourly(starttime_disagg,
                            endtime_disagg,
                            disaggStation1hr[sortedList1hr[i]].hb5, null);
                    pOrigHourlyPC = mpeDM.loadPCHourly(starttime_disagg,
                            endtime_disagg,
                            disaggStation1hr[sortedList1hr[i]].hb5, null);

                    pHourlyPP = pOrigHourlyPP.get(0);
                    pp_rec_cnt[i] = pOrigHourlyPP.size();
                    pHourlyPC = pOrigHourlyPC.get(0);
                    pc_rec_cnt[i] = pOrigHourlyPC.size();

                    if (pHourlyPP == null && pHourlyPC == null) {
                        out.write(String
                                .format("precip totalling routines found no data for gage %s and missing data substituted \n",
                                        disaggStation1hr[sortedList1hr[i]].hb5));
                        end_time_temp = endtime_disagg;
                        for (k = 0; k < qc_days; k++) {
                            valuesReadIn[k * sortedArraySize + i].index_in_1hr_list = sortedList1hr[i];
                            valuesReadIn[k * sortedArraySize + i].ID = disaggStation1hr[sortedList1hr[i]].hb5;
                            for (j = 0; j < 24; j++) {
                                Calendar nt = Calendar.getInstance(TimeZone
                                        .getTimeZone("GMT"));
                                nt.setTime(end_time_temp);
                                nt.add(Calendar.SECOND, -3600);
                                end_time_temp = nt.getTime();
                                valuesReadIn[k * sortedArraySize + i].HourlyValues[j] = -9999.f;
                                if (j == 23) {
                                    valuesReadIn[k * sortedArraySize + i].dqc_day = day++;
                                }
                            }
                        }
                    } else {
                        out.write("hour  day  value\n");
                        int[] pHourlyPPIdx = new int[] { 0 };
                        int[] pHourlyPCIdx = new int[] { 0 };
                        end_time_temp = endtime_disagg;
                        Calendar nt = Calendar.getInstance(TimeZone
                                .getTimeZone("GMT"));

                        for (k = 0; k < qc_days; k++) {
                            valuesReadIn[k * sortedArraySize + i].ID = disaggStation1hr[sortedList1hr[i]].hb5;

                            for (j = 0; j < 24; j++) {
                                nt.setTime(end_time_temp);
                                nt.add(Calendar.SECOND, -3600);
                                end_time_temp = nt.getTime();

                                totalPrecip = pu.getTotalHourlyPrecip(
                                        pOrigHourlyPC, pHourlyPCIdx,
                                        pOrigHourlyPP, pHourlyPPIdx,
                                        end_time_temp, duration,
                                        (float) min_percent,
                                        CommonHydroConstants.PRECIP_TS_RANK,
                                        advance, pc_rec_cnt, pp_rec_cnt);

                                valuesReadIn[k * sortedArraySize + i].HourlyValues[j] = totalPrecip
                                        .getValue();
                                if (j == 23) {
                                    valuesReadIn[k * sortedArraySize + i].dqc_day = day++;
                                }

                                out.write(String.format("   %d  %d  %6.2f\n",
                                        j, k, totalPrecip.getValue()));
                            }
                        } /* end for (k=0 ... */
                    } /* if(pHourlyPP == NULL) */
                } /* end if(sorted_list_1hr ... */
            } /* end for (i=0 ... */

            out.flush();
        } catch (IOException e) {
            logger.error("Error writing output", e);
            return;
        }
    }
}
