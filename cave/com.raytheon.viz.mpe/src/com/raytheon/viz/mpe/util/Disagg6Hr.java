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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Scanner;
import java.util.TimeZone;

import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.mpe.constants.FilePermissionConstants;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.util.file.IOPermissionsHelper;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pdata;
import com.raytheon.viz.mpe.util.DailyQcUtils.QCHRAP;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.raytheon.viz.mpe.util.MeanMonthlyPrecip.Isoh;
import org.locationtech.jts.geom.Coordinate;

/**
 * Based on: pproc_lib/src/Disagg/TEXT/Disagg6hr.c.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2009            snaples     Initial creation
 * Mar 2, 2015  15660      snaples     Fixed the disagg logging and corrected the procedure.
 * Jul 16, 2015 17561      snaples     Updated to fix issues with QPE Grid.
 * Aug 07, 2017 6334       bkowal      Directories are now created with 770 permissions and files 660.
 * Oct 03, 2017 6407       bkowal      The method isWithinRange provided by prism data reading now returns a boolean.
 * Oct 12, 2018 7099       tgurney     Fix checking of the mpe_disagg_execute setting
 * Oct 15, 2018 7099       tgurney     Fix log file opening issue
 *
 * </pre>
 *
 * @author snaples
 */

public class Disagg6Hr {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private DailyQcUtils dqc = DailyQcUtils.getInstance();

    public static class Values_1hr {
        public int dqc_day;

        public int index_in_1hr_list;

        public String ID = "";

        public float[] HourlyValues = new float[24];
    }

    public static class Values_6hr {
        public String ID = "";

        public float value[] = new float[4];

        public int hrapx_local;

        public int hrapy_local;
    }

    public static class Dist {
        public double[] distances_to_neighbors;
    }

    public static Values_1hr[] disaggValues;

    public static Values_6hr[] values6hr;

    public static Station[] disagg_station_6hr;

    public static int disagg_db_factor = 100;

    public static Dist[] dist_6hr_to_1hr;

    public static BufferedWriter disagg_log_fd = null;

    public static String disagg_log_file = null;

    public static String disagg_1hr_neighbors_fd = null;

    public static String neighbor_list_file;

    public static int disagg_maxx, disagg_maxy;

    private Date station_list_time_stamp_6hr;

    public static int ret_neighbors = -1;

    private String disagg_method = "";

    private String disagg_stations_fd = null;

    public static String[] obsdate;

    public static Date[] obsdate_date_t;

    /* global variables for disagg */
    public static String qpe_files_dir, date_form;

    public static double[][][] QPEgrids;

    public static int val6hreq0, val6hrgt0;

    public static Date endtime_disagg;

    public static Date starttime_disagg;

    public static Date end_time_temp;

    private Date btim = DailyQcUtils.btime.getTime();

    private Values_1hr valuesReadIn;

    public static int[] sorted_list_1hr;

    public static int num_disagg_stations = 0;

    private boolean first = true;

    private int mpe_dqc_max_precip_neighbors = DailyQcUtils.mpe_dqc_max_precip_neighbors;

    private DisaggGridMethod dgm = new DisaggGridMethod();

    private Compute1HrNeighborList c1n = new Compute1HrNeighborList();

    /* DISAGG_MISSING_STATION_SYMBOL signifies a station */
    /* is in disagg station list but not in DailyQC station list */
    public static final String DISAGG_MISSING_STATION_SYMBOL = "XXXXXX";

    public void disagg_cleanup() {
        int i, j = -1;

        if (valuesReadIn != null) {
            valuesReadIn = null;
        }

        if (dist_6hr_to_1hr != null) {
            dist_6hr_to_1hr = null;
        }

        if (disaggValues != null) {
            disaggValues = null;
        }

        if (values6hr != null) {
            values6hr = null;
        }

        if (disagg_stations_fd != null) {
            disagg_stations_fd = null;
        }

        if (obsdate != null) {
            obsdate = null;
        }
        if (obsdate_date_t != null) {
            obsdate_date_t = null;
        }

        if (sorted_list_1hr != null) {
            sorted_list_1hr = null;
        }

        if ((disagg_method.compareTo("GRID") == 0)) {
            for (i = 0; i < 6; i++) {
                for (j = 0; j < disagg_maxy; j++) {
                    if (QPEgrids[i][j] != null) {
                        QPEgrids[i][j] = null;
                    }
                }

                if (QPEgrids[i] != null) {
                    QPEgrids[i] = null;
                }
            }

            if (QPEgrids != null) {
                QPEgrids = null;
            }

            dgm.grid_cleanup();
        }

        c1n.free_1hr_station_list();

        if (disagg_station_6hr != null) {
            disagg_station_6hr = null;
        }
    }

    public void disagg6hr() throws IOException {
        long start_time, end_time;
        String logdir;
        String station_list_dir = DailyQcUtils.mpe_station_list_dir;
        String gridmask_dir = DailyQcUtils.mpe_gridmasks;
        String station_list_file;
        String buf = "";
        String cval6hr = "";

        station_list_time_stamp_6hr = SimulatedTime.getSystemTime().getTime();
        Date currentTime = SimulatedTime.getSystemTime().getTime();
        String datestring = "";
        final SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        final SimpleDateFormat ddf = new SimpleDateFormat("yyyyMMddHHMM");
        ddf.setTimeZone(TimeZone.getTimeZone("GMT"));
        int max_stations = DailyQcUtils.precip_stations.size();
        List<Station> station = new ArrayList<>();
        station = DailyQcUtils.precip_stations;
        int num_days_to_qc = DailyQcUtils.qcDays;

        int k = 0, j, i, ii;
        int emonth = DailyQcUtils.emonth;
        int smonth = DailyQcUtils.smonth;
        MeanMonthlyPrecip mmp = new MeanMonthlyPrecip();
        Isoh isoh = mmp.getIsoh();
        String area_val_local = DailyQcUtils.currentQcArea;
        Pdata pdata[] = DailyQcUtils.pdata;
        AppsDefaults appsDefaults = AppsDefaults.getInstance();

        QCHRAP hrap_point = new QCHRAP();
        int xor, yor, maxx, maxy;
        int index = -1;
        Date tt = SimulatedTime.getSystemTime().getTime();

        start_time = tt.getTime();

        currentTime.getTime();
        datestring = ddf.format(currentTime);

        // read disagg station list
        // this is not in the "first" block because
        // we have to "stat" this file every time to
        // see if stations have been added or deleted.
        station_list_file = String.format("%s/%s_disagg_station_list",
                station_list_dir, area_val_local);
        neighbor_list_file = String.format("%s/%s_disagg_1hr_neighbors",
                gridmask_dir, area_val_local);
        disagg_stations_fd = station_list_file;
        BufferedReader in = null;
        BufferedWriter out = null;
        FileWriter fd_log = null;

        if (!appsDefaults
                .consideredTrue(appsDefaults.getToken("mpe_disagg_execute"))) {
            statusHandler.info("Disagg token is OFF");
            statusHandler.info(
                    "exiting from disagg routine..mpe_editor continuing...");
            return;
        }

        /*---------------------------------------------*/
        /* define log file name */
        /* open log file */
        /*---------------------------------------------*/
        logdir = appsDefaults.getToken("mpe_editor_logs_dir");
        disagg_log_file = String.format("%s/disagg_%sz", logdir, datestring);

        try {
            if (disagg_log_fd != null) {
                disagg_log_fd.close();
            }
            fd_log = new FileWriter(disagg_log_file);
            out = new BufferedWriter(fd_log);
            disagg_log_fd = out;
            if (first) {
                disagg_log_fd.write("\t\t-- 6hr to 1hr Disaggregation -- \n");
                disagg_log_fd.write("\t\t-- Version AWIPS II 11.9.0-1 -- \n");
                disagg_log_fd.write("hydrologic day = 12z - 12z\n");
            }

            first = false;
        } catch (IOException e) {
            statusHandler.handle(Priority.WARN,
                    "Warning: Could not open disagg log file: "
                            + disagg_stations_fd
                            + ". Exiting from disagg routine..mpe_editor continuing...\n",
                    e);
            return;
        }

        disagg_method = appsDefaults.getToken("mpe_disagg_method");

        String delete_values = appsDefaults.getToken("mpe_disagg_delete_1hr",
                "on");

        try {
            disagg_log_fd.write(
                    String.format("6hr disagg station list file name: %s\n",
                            station_list_file));
        } catch (IOException e) {
            statusHandler.error(
                    "Failed to write to log file: " + disagg_log_file + ".", e);
        }

        xor = (int) MPEDataManager.getInstance().getHRAPExtent().getMinX();
        yor = (int) MPEDataManager.getInstance().getHRAPExtent().getMinY();
        maxx = (int) MPEDataManager.getInstance().getHRAPExtent().getMaxX();
        maxy = (int) MPEDataManager.getInstance().getHRAPExtent().getMaxY();
        disagg_maxx = maxx;
        disagg_maxy = maxy;

        endtime_disagg = btim;
        Calendar nt = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        nt.setTime(endtime_disagg);
        nt.add(Calendar.SECOND, -(num_days_to_qc * 86400));
        starttime_disagg = nt.getTime();
        end_time_temp = endtime_disagg;

        try {
            disagg_log_fd.write(
                    String.format(" endtime = %10.0f  starttime = %10.0f\n",
                            (float) endtime_disagg.getTime(),
                            (float) starttime_disagg.getTime()));
        } catch (IOException e) {
            statusHandler.error(
                    "Failed to write to log file: " + disagg_log_file + ".", e);
        }

        in = new BufferedReader(new FileReader(disagg_stations_fd));
        buf = in.readLine();
        num_disagg_stations = Integer.parseInt(buf);
        if (num_disagg_stations <= 0) {
            disagg_log_fd.write("WARNING: No 6hr gages to disagg\n");
            disagg_log_fd.write(
                    "exiting from disagg routine..mpe_editor continuing...\n");
            in.close();
            return;
        }

        disagg_station_6hr = new Station[(num_days_to_qc
                * num_disagg_stations)];
        disaggValues = new Values_1hr[(num_days_to_qc * num_disagg_stations)];
        values6hr = new Values_6hr[(num_days_to_qc * num_disagg_stations)];

        for (int z = 0; z < (num_days_to_qc * num_disagg_stations); z++) {
            disagg_station_6hr[z] = dqc.new Station();
            disaggValues[z] = new Values_1hr();
            values6hr[z] = new Values_6hr();
        }
        dist_6hr_to_1hr = new Dist[num_disagg_stations];

        for (int z = 0; z < num_disagg_stations; z++) {
            dist_6hr_to_1hr[z] = new Dist();
        }

        obsdate = new String[(num_days_to_qc + 1)];
        obsdate_date_t = new Date[num_days_to_qc + 1];

        disagg_log_fd.write(" 6hr Disagg Station List\n");

        for (j = 0; j < num_days_to_qc; j++) {
            for (i = 0; i < num_disagg_stations; i++) {
                index = (j * num_disagg_stations) + i;

                disagg_station_6hr[index].isoh = null;
                disagg_station_6hr[index].max = null;
                disagg_station_6hr[index].min = null;
                disagg_station_6hr[index].hb5 = null;
                disagg_station_6hr[index].name = null;
                disagg_station_6hr[index].parm = null;
                disagg_station_6hr[index].cparm = null;
                disagg_station_6hr[index].index = null;
                disagg_station_6hr[index].zindex = null;

                dist_6hr_to_1hr[i].distances_to_neighbors = new double[mpe_dqc_max_precip_neighbors];

                disagg_station_6hr[index].isoh = new float[12];
                disagg_station_6hr[index].hb5 = "";
                disagg_station_6hr[index].index = new short[mpe_dqc_max_precip_neighbors];
                disagg_station_6hr[index].parm = "";
                disagg_station_6hr[index].cparm = "";

                if (j == 0) {
                    if (in.ready()) {
                        buf = in.readLine();
                    }

                    Scanner s = new Scanner(buf);
                    disagg_station_6hr[i].hb5 = s.next();
                    disagg_station_6hr[i].lat = s.nextFloat();
                    float testlon = s.nextFloat();
                    if (testlon > 0) {
                        testlon = testlon * -1;
                    }
                    disagg_station_6hr[i].lon = testlon;

                    Coordinate ll = new Coordinate();
                    ll.x = disagg_station_6hr[i].lon;
                    ll.y = disagg_station_6hr[i].lat;
                    ReferencedCoordinate rc = new ReferencedCoordinate(ll);
                    Coordinate gridCell = null;
                    try {
                        gridCell = rc.asGridCell(
                                com.raytheon.uf.common.xmrg.hrap.HRAP
                                        .getInstance().getGridGeometry(),
                                PixelInCell.CELL_CORNER);
                    } catch (Exception e) {
                        statusHandler.error(
                                "Failed to convert Referenced Coordinate: "
                                        + rc.toString() + " to a grid cell.",
                                e);
                        continue;
                    }
                    hrap_point.x = (short) gridCell.x;
                    hrap_point.y = (short) gridCell.y;

                    values6hr[index].hrapx_local = (int) hrap_point.x - xor;
                    values6hr[index].hrapy_local = (int) hrap_point.y - yor;

                    disagg_log_fd.write(String.format(
                            " %s   %5.2f  %5.2f;    hrapx = %d hrapy = %d\n",
                            disagg_station_6hr[i].hb5,
                            disagg_station_6hr[i].lat,
                            disagg_station_6hr[i].lon,
                            values6hr[index].hrapx_local,
                            values6hr[index].hrapy_local));

                    MeanMonthlyTemp mmt = new MeanMonthlyTemp();
                    for (k = 0; k < 12; k++) {
                        disagg_station_6hr[i].isoh[k] = -1;
                        if (!mmt.isWithinRange(k, smonth, emonth)) {
                            continue;
                        }

                        if ((((int) hrap_point.x) < maxx)
                                && (((int) hrap_point.y) < maxy)
                                && (((int) hrap_point.x >= xor))
                                && (((int) hrap_point.y >= yor))) {
                            disagg_station_6hr[i].isoh[k] = isoh.value[k][(int) hrap_point.y
                                    - yor][(int) hrap_point.x - xor];

                        }

                    }
                    s.close();
                } else {
                    values6hr[index].hrapx_local = values6hr[i].hrapx_local;
                    values6hr[index].hrapy_local = values6hr[i].hrapy_local;

                    for (k = 0; k < 12; k++) {
                        disagg_station_6hr[index].isoh[k] = disagg_station_6hr[i].isoh[k];
                    }
                }

                disagg_station_6hr[index].hb5 = disagg_station_6hr[i].hb5;
                disagg_station_6hr[index].lat = disagg_station_6hr[i].lat;
                disagg_station_6hr[index].lon = disagg_station_6hr[i].lon;

                for (k = 0; k < max_stations; k++) {
                    if (disagg_station_6hr[index].hb5
                            .compareToIgnoreCase(station.get(k).hb5) == 0) // fixed
                                                                           // for
                                                                           // DR
                                                                           // 17561
                    {
                        values6hr[index].ID = disagg_station_6hr[index].hb5;
                        for (ii = 0; ii < 4; ii++) // for 4 6hr values
                        {
                            // we treat "bad" 6hr stations as missing in Disagg
                            // calculations
                            // note that "bad" stations have a quality code of
                            // '1'
                            if ((pdata[j].stn[k].frain[ii].data >= 0)
                                    && (pdata[j].stn[k].frain[ii].data != 1)) {
                                values6hr[index].value[ii] = pdata[j].stn[k].frain[ii].data;
                            } else {
                                values6hr[index].value[ii] = -9999.f;
                            }
                        }
                        break;

                    }

                } /* end for(k = 0 ... */

                /*------------------------------------------------------------------------------*/
                /*
                 * case of station in disagg list which does not appear in
                 * DailyQC station list
                 */
                /*------------------------------------------------------------------------------*/

                if (k == max_stations) {
                    disagg_log_fd.write(String.format(
                            " station id = %s does not appear in DailyQC PPD list -- ignore\n",
                            disagg_station_6hr[index].hb5));

                    values6hr[index].ID = DISAGG_MISSING_STATION_SYMBOL;
                    values6hr[index].value[0] = -9999.f;
                    values6hr[index].value[1] = -9999.f;
                    values6hr[index].value[2] = -9999.f;
                    values6hr[index].value[3] = -9999.f;

                }

            }

            obsdate[j] = "";
            obsdate[j] = sdf.format(end_time_temp);
            obsdate_date_t[j] = end_time_temp;
            disagg_log_fd.write(String.format(
                    "datestring for disagg day %d = %s\n", j, obsdate[j]));
            nt.setTime(end_time_temp);
            nt.add(Calendar.SECOND, -86_400);
            end_time_temp = nt.getTime();

        }

        obsdate[num_days_to_qc] = "";

        // note that end_time_temp is not being decremented
        // as in the above loop because it will be decremented
        // one extra time in the loop already.
        obsdate[num_days_to_qc] = sdf.format(end_time_temp);
        disagg_log_fd.write(String.format("datestring for disagg day %d = %s\n",
                num_days_to_qc, obsdate[num_days_to_qc]));
        obsdate_date_t[num_days_to_qc] = end_time_temp;

        /* print 6hr values to log */
        disagg_log_fd.write("\n");
        disagg_log_fd.write("6hr Values\n");
        disagg_log_fd.write("Day #\tPeriod #\tValue \n");

        index = -1;

        for (i = 0; i < num_disagg_stations; i++) {
            for (j = 0; j < num_days_to_qc; j++) {
                index = (j * num_disagg_stations) + i;

                if (values6hr[index].ID.equals(DISAGG_MISSING_STATION_SYMBOL)) {
                    continue;
                }

                if (j == 0) {
                    disagg_log_fd.write(String.format("%s \n",
                            disagg_station_6hr[index].hb5));
                }

                for (ii = 0; ii < 4; ii++) {
                    disagg_log_fd.write(String.format("  %d\t%d\t%5.2f\n", j,
                            ii, values6hr[index].value[ii]));

                }
            }
        }

        disagg_log_fd.write(
                String.format("6hr-1hr disagg method: %s\n", disagg_method));

        disagg_log_fd.flush();

        if ("grid".equalsIgnoreCase(disagg_method)) {
            qpe_files_dir = appsDefaults.getToken("mpe_qpe_dir");
            date_form = appsDefaults.getToken("st3_date_form");

            disagg_log_fd.write(String.format("dir containing QPE files: %s\n",
                    qpe_files_dir));
            disagg_log_fd
                    .write(String.format("xmrg date format: %s\n", date_form));

            cval6hr = appsDefaults.getToken("mpe_disagg_6hreq_0");

            if (cval6hr.length() > 0) {
                val6hreq0 = Integer.parseInt(cval6hr);
                disagg_log_fd
                        .write(String.format("val6hreq0= %d\n", val6hreq0));

            } else {
                val6hreq0 = 1;
                disagg_log_fd.write(
                        "mpe_disagg_6hreq_0 token not defined -- value = 1 used\n");

            }

            cval6hr = appsDefaults.getToken("mpe_disagg_6hrgt_0");
            if (cval6hr.length() > 0) {
                val6hrgt0 = Integer.parseInt(cval6hr);
                disagg_log_fd
                        .write(String.format("val6hrgt0= %d\n", val6hrgt0));

            } else {
                val6hrgt0 = 1;
                disagg_log_fd.write(
                        "mpe_disagg_6hrgt_0 token not defined -- value = 1 used\n");

            }

            /*
             * malloc space for 6 arrays each of size (maxy x maxx) to hold QPE
             * grids
             */
            /* QPEgrids[k][i][j] array */
            /* k = 0,1,2,3,4,5 */
            /* i = num of columns */
            /* j = num of rows */
            QPEgrids = new double[6][disagg_maxy][disagg_maxx];

            // fixed for DR 17561
            for (k = 0; k < 6; k++) {
                for (i = 0; i < disagg_maxy; i++) {
                    for (j = 0; j < disagg_maxx; j++) {
                        QPEgrids[k][i][j] = -9.0;
                    }
                }
            }
        } else {
            /*----------------------------------------------------------*/
            /* generate and read list of surrounding stations */
            /*----------------------------------------------------------*/
            File st = new File(station_list_file);
            long stm = st.lastModified();
            Date std = SimulatedTime.getSystemTime().getTime();
            std.setTime(stm);
            station_list_time_stamp_6hr.setTime(std.getTime());
            st = new File(neighbor_list_file);
            ret_neighbors = (int) st.lastModified();
            c1n.compute1hrStationList();
            c1n.read1hrGageVals();
        }
        disagg_log_fd.write("---------------------\n");

        /*---------------------------------------------*/
        /* disagg 6hr to 1hr values */
        /*---------------------------------------------*/
        if ("grid".equalsIgnoreCase(disagg_method)) {
            /*---------------------------------------------*/
            /* grid method */
            /*---------------------------------------------*/
            DisaggGridMethod dgm = new DisaggGridMethod();
            dgm.disaggGridMethod();
        } else {
            /*---------------------------------------------*/
            /* nearest neighbor (point) method */
            /*---------------------------------------------*/
            DisaggPointMethod dpm = new DisaggPointMethod();
            dpm.disaggPointMethod();
        }

        /*----------------------------------------------------------------------*/
        /*
         * delete 1hr values from previous disagg run from HourlyPP table
         */
        /*
         * mpe_disagg_delete_1hr token controls this /*------------------
         * ----------------------------------------------------
         */
        if (appsDefaults.consideredTrue(delete_values)) {
            Delete1hrDisaggValues ddv = new Delete1hrDisaggValues();
            ddv.delete1hrDisaggValues();
        }

        /*---------------------------------------------*/
        /* write 1hr values to HourlyPP table */
        /*---------------------------------------------*/
        Write1hrVals6HrGages w1v = new Write1hrVals6HrGages();
        w1v.write1hrValuesFor6hrGages();

        end_time = tt.getTime();
        disagg_log_fd.write(
                String.format("Disagg exit --- elapsed time = %d second(s)\n",
                        ((end_time - start_time) * 1000)));

        // cleanup/free all the allocated structures in
        // this file and other globals and file descriptors
        disagg_cleanup();

        disagg_log_fd.close();
        in.close();
        IOPermissionsHelper.applyFilePermissions(Paths.get(disagg_log_file),
                FilePermissionConstants.POSIX_FILE_SET);
    }

    public static BufferedWriter getDisagg_log_fd() {
        return disagg_log_fd;
    }
}
