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
import java.nio.file.Paths;
import java.util.ArrayList;
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
import com.raytheon.uf.common.mpe.constants.FilePermissionConstants;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.file.IOPermissionsHelper;
import com.raytheon.viz.hydrocommon.whfslib.PrecipUtil;
import com.raytheon.viz.hydrocommon.whfslib.PrecipUtil.total_precip;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.util.DailyQcUtils.QCHRAP;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.raytheon.viz.mpe.util.Disagg6Hr.Dist;
import com.raytheon.viz.mpe.util.Disagg6Hr.Values_1hr;
import com.raytheon.viz.mpe.util.MeanMonthlyPrecip.Isoh;
import org.locationtech.jts.geom.Coordinate;

/**
 * Computes and writes the MPE 1 Hour Station List. Based on:
 * pproc_lib/src/Disagg/TEXT/compute_1hr_neighbor_list.c.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2009            snaples     Initial creation
 * Mar  2, 2015  15660     snaples     Fixed issues with the file writing and creating the new files.
 * Apr 05, 2016  18350     snaples     Changed call to DailyQC Utils getInstance.
 * Jul 18, 2016  4600      njensen     Some cleanup
 * Aug 07, 2017  6334      bkowal      Directories are now created with 770 permissions and files 660.
 * Oct 03, 2017  6407      bkowal      Eliminated warnings.
 * 
 * </pre>
 * 
 * @author snaples
 */

public class Compute1HrNeighborList {

    public static Values_1hr[] valuesReadIn = null;

    public static int[] sorted_list_1hr = null;

    public static int sorted_array_size = 0;

    public static Station[] disagg_station_1hr;

    private final IUFStatusHandler logger = UFStatus
            .getHandler(this.getClass());

    private Station[] disagg_station_6hr = null;

    private Date station_list_time_stamp_6hr = TimeUtil.newDate();

    private int ret_neighbors = Disagg6Hr.ret_neighbors;

    private int duplicate_station = 0;

    private int mpe_dqc_max_precip_neighbors = DailyQcUtils.mpe_dqc_max_precip_neighbors;

    public Compute1HrNeighborList() {
        // default constructor
    }

    public void free_1hr_station_list() {
        if (disagg_station_1hr != null) {
            disagg_station_1hr = null;
        }
    }

    public void compute1hrStationList() {
        MeanMonthlyPrecip mmp = new MeanMonthlyPrecip();
        Isoh isoh = mmp.getIsoh();
        int smonth = DailyQcUtils.smonth;
        int emonth = DailyQcUtils.emonth;

        String hb5 = "";
        double dist1, dist2, dist;
        double[] sorted = new double[mpe_dqc_max_precip_neighbors];
        int ind, h, i, l, m, k;
        QCHRAP hrap_point = new QCHRAP();
        float lat, lon;
        double conv = .0174533;
        disagg_station_6hr = Disagg6Hr.disagg_station_6hr;
        Dist[] dist_6hr_to_1hr = Disagg6Hr.dist_6hr_to_1hr;
        String neighbor_list_file = Disagg6Hr.neighbor_list_file;
        int num_disagg_stations = Disagg6Hr.num_disagg_stations;

        int file_error = 0;
        String area_id;
        String fname = "_station_list";
        String disagg_1hr_stations_dir;
        String disagg_1hr_stations_path;
        int num_distinct_1hrs = 0;

        int xor, yor, maxx, maxy;

        Date timestamp_previous_neighbors = new Date();
        int num_previous_neighbors = 0;
        int generate = 0;
        String temp_buf = "";
        AppsDefaults appsDefaults = AppsDefaults.getInstance();

        disagg_1hr_stations_dir = appsDefaults.getToken("mpe_station_list_dir");
        area_id = appsDefaults.getToken("mpe_site_id");
        /*
         * TODO: need to greatly rewrite the following and other associated
         * classes such that: 1) there is not a static BufferedWriter 2) Cleanup
         * up the conditional statements such that all BufferedReaders can be
         * opened within try with resources statements and closed automatically.
         */
        BufferedReader in = null;
        BufferedWriter out = Disagg6Hr.getDisagg_log_fd();
        BufferedWriter out2 = null;

        try {
            if (area_id.length() > 0) {
                disagg_1hr_stations_path = String.format("%s/%s%s",
                        disagg_1hr_stations_dir, area_id, fname);
                out.write(String.format("mpe_gage_locations filename = %s\n",
                        disagg_1hr_stations_path));
                in = new BufferedReader(
                        new FileReader(disagg_1hr_stations_path));
            } else {
                out.write("token mpe_site_id not defined\n");
                return;
            }

            /* Read the record containing the number of hourly stations. */
            String hstat = in.readLine().trim();
            int p = hstat.length();

            if (p == -1) {
                out.write(
                        "could not read number of hourly stations from file\n");
                in.close();
                return;
            }

            int num_records = Integer.parseInt(hstat);

            if (num_records < 1) {
                out.write(
                        "could not read number of hourly stations from file\n");
                in.close();
                return;
            }

            if (num_records <= 0) {
                out.write("number of hourly stations le 0\n");
                in.close();
                return;
            }

            /* Allocate space for the stations array. */
            disagg_station_1hr = new Station[num_records];

            xor = (int) MPEDataManager.getInstance().getHRAPExtent().getMinX();
            yor = (int) MPEDataManager.getInstance().getHRAPExtent().getMinY();
            maxx = (int) MPEDataManager.getInstance().getHRAPExtent().getMaxX();
            maxy = (int) MPEDataManager.getInstance().getHRAPExtent().getMaxY();

            /* Read the PPH stations. */
            i = 0;
            for (ind = 0; ind < num_records; ++ind) {
                String pp = in.readLine().trim();

                if (pp == null) {
                    out.write(String.format(
                            " error reading 1hr station list -- ind = %d num_records = %d\n",
                            ind, num_records));
                    break;
                }

                Scanner s = new Scanner(pp);
                hb5 = s.next();
                s.next(); // unused string
                lat = s.nextFloat();
                lon = s.nextFloat();

                DailyQcUtils dqc = DailyQcUtils.getInstance();
                disagg_station_1hr[i] = dqc.new Station();
                disagg_station_1hr[i].isoh = new float[24];
                disagg_station_1hr[i].hb5 = "";
                disagg_station_1hr[i].index = new short[DailyQcUtils.mpe_dqc_max_precip_neighbors];
                disagg_station_1hr[i].parm = "";
                disagg_station_1hr[i].cparm = "";

                // eliminate duplicate stations from the station list file
                for (k = 0; k < num_distinct_1hrs; k++) {
                    if (hb5.equalsIgnoreCase(disagg_station_1hr[k].hb5)) {
                        // we found a duplicate station
                        duplicate_station = 1;
                        break;

                    }

                }

                if (duplicate_station == 1) {
                    duplicate_station = 0;
                    continue;
                } else {
                    num_distinct_1hrs++;
                }

                disagg_station_1hr[i].hb5 = hb5;
                disagg_station_1hr[i].lat = lat;
                float testlon = lon;
                if (testlon > 0) {
                    testlon = testlon * -1;
                }
                disagg_station_1hr[i].lon = testlon;

                /*
                 * Set elev to 1 ft. The value of 0 creates problems with
                 * subsequent computations in DailyQC.
                 */

                disagg_station_1hr[i].elev = 1;

                lat = disagg_station_1hr[i].lat;
                lon = disagg_station_1hr[i].lon;

                /* Store the station's coordinates in HRAP. */

                Coordinate ll = new Coordinate();
                ll.x = disagg_station_1hr[i].lon;
                ll.y = disagg_station_1hr[i].lat;
                ReferencedCoordinate rc = new ReferencedCoordinate(ll);
                Coordinate gridCell = new Coordinate();
                try {
                    gridCell = rc.asGridCell(
                            com.raytheon.uf.common.xmrg.hrap.HRAP.getInstance()
                                    .getGridGeometry(),
                            PixelInCell.CELL_CORNER);
                } catch (Exception e) {
                    logger.error(
                            "Error determining grid cell for referenced coordinate "
                                    + ll.x + ", " + ll.y,
                            e);
                    // TODO should this return/exit early?
                }
                hrap_point.x = (short) gridCell.x;
                hrap_point.y = (short) gridCell.y;

                disagg_station_1hr[i].hrap_x = hrap_point.x;
                disagg_station_1hr[i].hrap_y = hrap_point.y;
                MeanMonthlyTemp mmt = new MeanMonthlyTemp();

                for (k = 0; k < 12; k++) {
                    disagg_station_1hr[i].isoh[k] = -1;

                    if (!mmt.isWithinRange(k, smonth, emonth)) {
                        continue;
                    }

                    if (((int) hrap_point.x < maxx)
                            && ((int) hrap_point.y < maxy)
                            && ((int) hrap_point.x >= xor)
                            && ((int) hrap_point.y >= yor)) {
                        disagg_station_1hr[i].isoh[k] = isoh.value[k][(int) hrap_point.y
                                - yor][(int) hrap_point.x - xor];
                    }
                }

                i++;
                s.close();
            }

            num_records = num_distinct_1hrs;

            /* Close the stations file. */
            in.close();

            /*
             * code to find if 1hr nearest neighbors file already exists if
             * exists, then check if either of, the number of neighbors token or
             * the last modification time of the 6hr station lists, both of
             * which are stored in the nearest neighbors file, have changed. if
             * neither of them have changed, just read the file instead of
             * generating the file, else re-generate.
             */
            if (ret_neighbors == 0) {
                in = new BufferedReader(new FileReader(neighbor_list_file));
                String lines = in.readLine();
                Scanner s = new Scanner(lines);
                if (lines != null) {
                    num_previous_neighbors = s.nextInt();
                } else {
                    generate = 1;

                }
                lines = in.readLine();
                if (lines != null) {
                    timestamp_previous_neighbors.setTime(s.nextInt());

                } else {
                    generate = 1;
                }

                if (mpe_dqc_max_precip_neighbors != num_previous_neighbors
                        || timestamp_previous_neighbors
                                .before(station_list_time_stamp_6hr)) {
                    generate = 1;
                } else {
                    out.write(String.format(
                            "Reading %d nearest neighbor 1hr stations\n",
                            mpe_dqc_max_precip_neighbors));

                    for (i = 0; i < Disagg6Hr.num_disagg_stations; i++) {
                        lines = in.readLine();
                        for (l = 0; l < mpe_dqc_max_precip_neighbors; l++) {
                            // read from neighbor list file
                            // read the 1hr station info for each 6hr station
                            lines = in.readLine();
                            if (lines != null) {
                                // sscanf(line, "%hd%s%f%f%lf"
                                disagg_station_6hr[i].index[l] = s.nextShort();
                                disagg_station_1hr[disagg_station_6hr[i].index[l]].hb5 = s
                                        .next();
                                disagg_station_1hr[disagg_station_6hr[i].index[l]].lat = s
                                        .nextFloat();
                                disagg_station_1hr[disagg_station_6hr[i].index[l]].lon = s
                                        .nextFloat();
                                dist_6hr_to_1hr[i].distances_to_neighbors[l] = s
                                        .nextDouble();
                            } else {
                                out.write(
                                        "Error reading neighbor 1hr stations...file corrupted...generating 1hr neighbours\n");
                                file_error = 1;
                            }
                        }
                        if (file_error == 1) {
                            break;
                        }
                    }
                    generate = 0;
                }
                s.close();
            } else {
                generate = 1;
            }
            if (file_error == 1) {
                generate = 1;
            }
            if (generate == 1) {
                // Logic to find 1hr nearest neighbors for the 6hr disagg
                // stations
                // ---------------------------------------------------------------
                out.write(String.format(
                        "Generating %d nearest neighbor 1hr stations\n",
                        mpe_dqc_max_precip_neighbors));
                for (i = 0; i < Disagg6Hr.num_disagg_stations; i++) {
                    for (l = 0; l < mpe_dqc_max_precip_neighbors; l++) {
                        sorted[l] = 9_999_999;
                    }
                    for (m = 0; m < num_records; ++m) {
                        // do not use 1hr station with same id as 6hr station
                        if (disagg_station_1hr[m].hb5
                                .equalsIgnoreCase(disagg_station_6hr[i].hb5)) {
                            continue;

                        }
                        dist1 = disagg_station_6hr[i].lat
                                - disagg_station_1hr[m].lat;
                        dist2 = (disagg_station_6hr[i].lon
                                - disagg_station_1hr[m].lon)
                                * Math.cos((disagg_station_6hr[i].lat
                                        + disagg_station_1hr[m].lat) * conv
                                        / 2.);
                        dist = Math.pow(dist1, 2) + Math.pow(dist2, 2);

                        /*
                         * In the following logic we calculate if the current
                         * distance between the 6hr and 1hr stations is shorter
                         * than one of the gages in the 'sorted' array. If it is
                         * we add it and remove the farthest distance from the
                         * array. Note that the 'sorted' array is sorted
                         * literally!
                         */

                        for (l = 0; l < mpe_dqc_max_precip_neighbors; l++) {
                            if (dist < sorted[l]) {
                                for (h = mpe_dqc_max_precip_neighbors
                                        - 1; h > l; h--) {
                                    sorted[h] = sorted[h - 1];
                                    disagg_station_6hr[i].index[h] = disagg_station_6hr[i].index[h
                                            - 1];
                                }

                                // add this distance into the array of current
                                // "num neighbors" shortest distances
                                sorted[l] = dist;

                                // add the 1hr station as a nearest neighbor to
                                // the 6hr station
                                disagg_station_6hr[i].index[l] = (short) m;

                                // storing the distance of this 1hr gage from
                                // the 6hr gage
                                dist_6hr_to_1hr[i].distances_to_neighbors[l] = dist;
                                break;
                            }
                        }
                    }
                }
                // ----------------------------------------------------------------
            }
            out2 = new BufferedWriter(new FileWriter(neighbor_list_file));
            if (generate == 1) {
                in.close();
                out2.write(String.format("%d\n", mpe_dqc_max_precip_neighbors));
                File nlf = new File(neighbor_list_file);
                station_list_time_stamp_6hr.setTime(nlf.lastModified());
                out2.write(String.format("%d\n",
                        (int) station_list_time_stamp_6hr.getTime()));
            }

            /* print out list of 1hr nearest neighbor stations */
            for (i = 0; i < num_disagg_stations; i++) {
                out.write(
                        String.format("---%s---\n", disagg_station_6hr[i].hb5));
                if (generate == 1) {
                    out2.write(
                            String.format("%s\n", disagg_station_6hr[i].hb5));
                }

                for (l = 0; l < mpe_dqc_max_precip_neighbors; l++) {
                    temp_buf = String.format("%d\t%s\t%5.2f\t%5.2f\t%6.2f",
                            disagg_station_6hr[i].index[l],
                            disagg_station_1hr[disagg_station_6hr[i].index[l]].hb5,
                            disagg_station_1hr[disagg_station_6hr[i].index[l]].lat,
                            disagg_station_1hr[disagg_station_6hr[i].index[l]].lon,
                            dist_6hr_to_1hr[i].distances_to_neighbors[l] * 60.);
                    if (generate == 1) {
                        out2.write(String.format("%s\n", temp_buf));
                    }
                    out.write(String.format("%s\n", temp_buf));
                }
            }
            out2.close();
            // This updates the permissions of "out2".
            IOPermissionsHelper.applyFilePermissions(
                    Paths.get(neighbor_list_file),
                    FilePermissionConstants.POSIX_FILE_SET);
        } catch (Exception e) {
            logger.error("Error computing 1 hr station list", e);
        } finally {
            try {
                /*
                 * TODO: this will not be necessary when the necessary changes
                 * have been made to transition to FILE I/O via try with
                 * resources.
                 */
                if (out2 != null) {
                    out2.close();
                }
            } catch (IOException e) {
                // ignore
                logger.warn("Failed to close the neighbor list file: "
                        + neighbor_list_file + ".", e);
            }
        }
    }

    public void read1hrGageVals() {
        int[] pp_rec_cnt = new int[1];
        int[] pc_rec_cnt = new int[1];
        /* The duration in hours. */
        int duration = 1;
        total_precip total_precip;
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
        int[] temp = new int[num_disagg_stations
                * mpe_dqc_max_precip_neighbors];
        BufferedWriter out = Disagg6Hr.getDisagg_log_fd();
        disagg_station_6hr = Disagg6Hr.disagg_station_6hr;
        int qc_days = DailyQcUtils.qcDays;

        try {
            out.write(" Reading 1hr Precip Gage Values \n");

            sorted_list_1hr = new int[num_disagg_stations
                    * mpe_dqc_max_precip_neighbors];
            /* sorted_list_1hr array is array of indexes of 1hr gages */
            /* defined in compute_1hr_station_list function */

            for (i = 0; i < num_disagg_stations
                    * mpe_dqc_max_precip_neighbors; i++) {
                sorted_list_1hr[i] = -1;
            }
            for (i = 0; i < num_disagg_stations; i++) {
                for (j = 0; j < mpe_dqc_max_precip_neighbors; j++) {
                    sorted_list_1hr[mpe_dqc_max_precip_neighbors * i
                            + j] = disagg_station_6hr[i].index[j];
                }
            }

            // remove duplicates (if any) from among sets of 20 nearest-neighbor
            // stations
            temp = sorted_list_1hr;
            TreeSet<Integer> set = new TreeSet<>();
            for (int v : temp) {
                set.add(v);
            }

            Integer[] foo = new Integer[0];
            set.toArray(foo);
            sorted_list_1hr = new int[foo.length];

            for (i = 0; i < foo.length; i++) {
                sorted_list_1hr[i] = foo[i].intValue();
            }
            Arrays.sort(sorted_list_1hr);
            sorted_array_size = sorted_list_1hr.length;
            out.write(String.format(
                    " number of non-duplicate 1hr nearest-neighbor stations = %d\n",
                    sorted_array_size));

            valuesReadIn = new Values_1hr[qc_days * sorted_array_size];

            out.write(String.format(
                    "endtime_disagg = %10.0f  starttime_disagg = %10.0f\n",
                    (float) endtime_disagg.getTime(),
                    (float) starttime_disagg.getTime()));

            // get 1hr precip gage values using precip totalling routines
            // loop on the number of unique (non-duplicate) 1hr stations
            PrecipUtil pu = PrecipUtil.getInstance();
            for (i = 0; i < sorted_array_size; i++) {
                day = 0;
                if (sorted_list_1hr[i] != -1) {
                    out.write(String.format("station = %s\n",
                            disagg_station_1hr[sorted_list_1hr[i]].hb5));

                    /* load precip data for all hours for this station */
                    pOrigHourlyPP = pu.load_PP_hourly(starttime_disagg,
                            endtime_disagg,
                            disagg_station_1hr[sorted_list_1hr[i]].hb5, null);
                    pOrigHourlyPC = pu.load_PC_hourly(starttime_disagg,
                            endtime_disagg,
                            disagg_station_1hr[sorted_list_1hr[i]].hb5, null);

                    pHourlyPP = pOrigHourlyPP.get(0);
                    pp_rec_cnt[i] = pOrigHourlyPP.size();
                    pHourlyPC = pOrigHourlyPC.get(0);
                    pc_rec_cnt[i] = pOrigHourlyPC.size();

                    if (pHourlyPP == null && pHourlyPC == null) {
                        out.write(String.format(
                                "precip totalling routines found no data for gage %s and missing data substituted \n",
                                disagg_station_1hr[sorted_list_1hr[i]].hb5));
                        end_time_temp = endtime_disagg;
                        for (k = 0; k < qc_days; k++) {
                            valuesReadIn[k * sorted_array_size
                                    + i].index_in_1hr_list = sorted_list_1hr[i];
                            valuesReadIn[k * sorted_array_size
                                    + i].ID = disagg_station_1hr[sorted_list_1hr[i]].hb5;
                            for (j = 0; j < 24; j++) {
                                Calendar nt = Calendar.getInstance(
                                        TimeZone.getTimeZone("GMT"));
                                nt.setTime(end_time_temp);
                                nt.add(Calendar.SECOND, -3600);
                                end_time_temp = nt.getTime();
                                valuesReadIn[k * sorted_array_size
                                        + i].HourlyValues[j] = -9999.f;
                                if (j == 23) {
                                    valuesReadIn[k * sorted_array_size
                                            + i].dqc_day = day++;
                                }
                            }
                        }
                    } else {
                        out.write("hour  day  value\n");
                        int[] pHourlyPPIdx = new int[] { 0 };
                        int[] pHourlyPCIdx = new int[] { 0 };
                        end_time_temp = endtime_disagg;
                        Calendar nt = Calendar
                                .getInstance(TimeZone.getTimeZone("GMT"));
                        for (k = 0; k < qc_days; k++) {
                            valuesReadIn[k * sorted_array_size
                                    + i].ID = disagg_station_1hr[sorted_list_1hr[i]].hb5;

                            for (j = 0; j < 24; j++) {
                                nt.setTime(end_time_temp);
                                nt.add(Calendar.SECOND, -3600);
                                end_time_temp = nt.getTime();
                                total_precip = pu.get_total_hourly_precip(
                                        (ArrayList<Hourlypc>) pOrigHourlyPC,
                                        pHourlyPCIdx,
                                        (ArrayList<Hourlypp>) pOrigHourlyPP,
                                        pHourlyPPIdx, end_time_temp, duration,
                                        (float) min_percent,
                                        PrecipUtil.PRECIP_TS_RANK, advance,
                                        pc_rec_cnt, pp_rec_cnt);

                                valuesReadIn[k * sorted_array_size
                                        + i].HourlyValues[j] = total_precip.value;
                                if (j == 23) {
                                    valuesReadIn[k * sorted_array_size
                                            + i].dqc_day = day++;
                                }

                                out.write(String.format("   %d  %d  %6.2f\n", j,
                                        k, total_precip.value));
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
