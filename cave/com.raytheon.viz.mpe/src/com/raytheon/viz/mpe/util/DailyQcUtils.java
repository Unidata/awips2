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
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Random;
import java.util.Scanner;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.mpe.constants.FilePermissionConstants;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.file.IOPermissionsHelper;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.util.MapPrecipGagesGrid.Topo;
import org.locationtech.jts.geom.Coordinate;

/**
 * Daily QC Utilities
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 16, 2009            snaples     Initial creation
 * Dec 04, 2012 15544      wkwock      fix missing 12z-18z after 12
 * Sep 11, 2013 #2353      lvenable    Fixed cursor memory leaks and Scanner resource leak.
 * Nov 24, 2014 16911      xwei        The day of Hydrologic Date is set to the next day
 *                                     if hour is greater than 18Z.
 * Mar 10, 2015 14575      snaples     Added additional status flags.
 * Oct 14, 2015 17977      snaples     Fixed loadData to read station
 *                                     lists when new area, which means it needs to read some tokens also.
 * Nov 25, 2015 17986      snaples     Updated array func to adjust QC codes for update to dialogs.
 * Apr 05, 2016 18350      snaples     Initialized station arrays inside of loadDataSet, was only on
 *                                     class initialization.
 * Apr 11, 2016 2373       skorolev    Added constant MISSED value.
 * Sep 06, 2016 19303      lbousaidi   Check if scanner's next token is a valid float value.
 * Mar 02, 2017 6147       dgilling    Correct order of values with in variable
 *                                     func.
 * Apr 17, 2017 6148       bkowal      Added {@link #after18Z()}. Retrieve an additional day worth of
 *                                     data if it is after 18Z.
 * Apr 25, 2017 6148       bkowal      A maximum of 11 days of QC data can now be retrieved after 18Z.
 * Jun 29, 2017 6147       bkowal      Restored the original order of variables in func because labels
 *                                     were in the wrong order.
 * Jul 13, 2017 6148       bkowal      No longer estimate partial days.                                   
 * Aug 07, 2017 6334       bkowal      Directories are now created with 770 permissions and files 660.
 * Oct 03, 2017 6407       bkowal      Cleanup. Prism data loading now returns meaningful errors.
 * Oct 04, 2017 19908      snaples     Updated initialization of station arrays to fix blank array data
 *                                     after closing and reopening DQC.
 * Dec 15, 2017 6547       bkowal      Remove unnecessary adjustment now that the larger underlying problem
 *                                     has been resolved.
 * Jan 24, 2018 6547       bkowal      Adjust station quality function index order.
 * Mar 05, 2018 7232       bkowal      Eliminated methods checking for/handling partial days. The input data
 *                                     now correctly reflects whether it is for a partial day or not.
 * May 10, 2018 7184       bkowal      Check the selected date/time when making the partial day determination.
 * May 21, 2018 7131       mduff       Updated to accommodate changes to other classes and cleanup.
 * Sep 26, 2018 7482       smanoj      Fix the issue with Month in the filenames
 * Feb  8, 2019 7131       tgurney     Precip reverse filter value default to 20.0
 * Mar 11, 2020 19533   mgamazaychikov Removed references to mpe_basin
 *
 * </pre>
 *
 * @author snaples
 */

public class DailyQcUtils {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DailyQcUtils.class);

    /* Define timefile file extensions. */
    public static final String TIME_FILE[][] = {
            { "00_06", "06_12", "12_18", "18_00", "00_00" },
            { "06_12", "12_18", "18_00", "00_06", "06_06" },
            { "12_18", "18_00", "00_06", "06_12", "12_12" },
            { "18_00", "00_06", "06_12", "12_18", "18_18" } };

    public static final String T_TIME_FILE[][] = {
            { "00", "06", "12", "18", "max", "min" },
            { "06", "12", "18", "00", "max", "min" },
            { "12", "18", "00", "06", "max", "min" },
            { "18", "00", "06", "12", "max", "min" } };

    public static final String Z_TIME_FILE[][] = { { "00", "06", "12", "18" },
            { "06", "12", "18", "00" }, { "12", "18", "00", "06" },
            { "18", "00", "06", "12" } };

    public static final double MOSAIC_DEFAULT = -9.0;

    private static final int MAX_TYPE_SOURCES = 8;

    public static final int MISSED = -9999;

    private static final int SIX_HR_OF_SECONDS = 6 * TimeUtil.SECONDS_PER_HOUR;

    private static final int TWELVE_HR_OF_SECONDS = 12
            * TimeUtil.SECONDS_PER_HOUR;

    private static final int EIGHTEEN_HR_OF_SECONDS = 18
            * TimeUtil.SECONDS_PER_HOUR;

    private static DailyQcUtils instance;

    public double[][][] QPEaccum24hr;

    public short[][] QPEgrid1hr;

    private String lastQcArea = "";

    public static String currentQcArea;

    private Date currDate;

    private Date selDate;

    public static int qcDays;

    private int firstTok = 1;

    public boolean isohyets_used = false;

    public boolean maxmin_used = false;

    public boolean init_maxmin = false;

    public static int pcp_in_use[] = new int[500];

    private boolean newarea = false;

    private static AppsDefaults appsDefaults = AppsDefaults.getInstance();

    private String station_climo_file;

    private String hrap_gage_file;

    private String hrap_zgage_file;

    private String hrap_tgage_file;

    private String hrap_grid_mask_file;

    private String hrap_file;

    private String pcpn_file;

    public String proc_pcpn_file;

    public String pcpn_qc_file;

    public String pcpn_dev_file;

    public String pcpn_bad_file;

    private String snow_file;

    private String zpoint1_file;

    public String zpoint2_file;

    private String tpoint1_file;

    public String tpoint2_file;

    public String temp_bad_file;

    public String temp_dev_file;

    private String ngrid_file;

    public String grid_file;

    public String zgrid_file;

    public String tgrid_file;

    private String bad_snow_file;

    public String rsel_file;

    public String mat_file;

    public String map_file;

    public int prev_smonth;

    public int prev_emonth;

    public boolean gageqc_performed = false;

    public int method = 2;

    public static boolean wfo_all = false;

    public boolean render_all = true;

    public int wfo_orig;

    public static int[] wfo_in_use = new int[20];

    private String[] tag = new String[20];

    public static String mpe_gridmasks;

    public String mpe_rfc_name;

    public int mpe_td_new_algorithm_set;

    public int mpe_td_details_set;

    private String mpe_point_precip_dir;

    private String mpe_grid_precip_dir;

    private String mpe_map_precip_dir;

    private String mpe_dev_precip_dir;

    private String mpe_bad_precip_dir;

    private String mpe_point_freezing_dir;

    private String mpe_grid_freezing_dir;

    private String mpe_maz_freezing_dir;

    private String mpe_point_temperature_dir;

    private String mpe_grid_temperature_dir;

    private String mpe_mat_temperature_dir;

    private String mpe_bad_temperature_dir;

    private String mpe_dev_temperature_dir;

    public static String mpe_station_list_dir;

    private String mpe_climo_list_dir;

    private String mpe_prism_dir;

    public static int mpe_dqc_max_precip_neighbors;

    public static int mpe_dqc_max_temp_neighbors;

    private float mpe_dqc_precip_deviation;

    private float mpe_dqc_temperature_deviation;

    public int mpe_dqc_min_good_stations;

    public boolean mpe_dqc_warningpopup_flag;

    public int mpe_dqc_grid_max_dist;

    public boolean mpe_dqc_save_netcdf;

    public boolean mpe_dqc_save_grib;

    public int dqc_ending_6hour_obstime = -1;

    public int dqcTimeStringIndex;

    public String mpe_show_missing_gage;

    public static int isom = -1;

    public static ArrayList<Station> precip_stations;

    public static ArrayList<Station> temperature_stations;

    public static ArrayList<Station> freezing_stations;

    public static Pdata pdata[];

    public static Tdata tdata[];

    public static Zdata zdata[];

    private static Hrap_Grid hrap_grid = new Hrap_Grid();

    public static Ts[] ts;

    public static int tsmax = 0;

    public Maps mean_areal_precip_global[];

    public static Pcp pcp = new Pcp();

    public static Pcp spf = new Pcp();

    public static Pcp tpf = new Pcp();

    public Bad_Daily_Values bad_values[];

    public Bad_Daily_Values bad_tvalues[];

    public int pid = newPid();

    public static int emonth;

    public static int smonth;

    private int max_basins;

    public static Calendar btime = Calendar
            .getInstance(TimeZone.getTimeZone("GMT"));

    /**
     * The default ending obs time for temperature and freezing level data.
     */
    private static final int DEFAULT_ENDING_6HOUR_OBS_TIME = 6;

    /**
     * The maximum number of days DailyQC may be run for.
     */
    public static final int MAX_GAGEQC_DAYS = 10;

    private static int DAILYQC_FAILED = 0;

    private static int new_area_flag = 0;

    public int func[] = { 8, 3, 0, 1, 2 };

    public static int[] dflag = new int[10];

    public static int[] qflag = new int[10];

    public float pxtemp = 1.0f;

    public int dmvalue = (int) ((1.0 * 100 * 3.28) / .55);

    public int temperature_filter_value = -50;

    private static final String dqc_ending_6hour_obstime_tok = "dqc_ending_6hour_obstime";

    public int curHr12_18 = -1;

    public int curHr18_00 = -1;

    public int curHr00_06 = -1;

    public int curHr06_12 = -1;

    /* Function which associates the Gage QC edit levels with a value. */
    public int funct[] = { 8, 0, 6, 2, 3, 4, 5, 1, 7, 9 };

    public static int gage_char[] = new int[2];

    public static int plot_view = 0;

    public boolean frzlvl_flag = true;

    public int find_station_flag = -1;

    public static int pcpn_time = 0;

    public static int pcp_flag = -1;

    public static int pcpn_day = 0;

    public static int contour_flag = -1;

    public static int points_flag = 1;

    public static int grids_flag = -1;

    public static int map_flag = -1;

    public static boolean qpf_flag = false;

    public static boolean maxmin_flag = false;

    public static boolean z_flag = false;

    private static int curHrMinSec = -1;

    private static int hydro_curDay;

    private boolean post_analysis_calc;

    private boolean auto_dailyqc_flag;

    private File td_fp;

    public BufferedWriter td_fpwr;

    public Topo topo;

    /** QC Precip filter value */
    private float precipFilterVal;

    /** QC Precip reverse filter value */
    private float precipReverseFilterVal = 20.0f;

    private int pointElevationFilterValue = 0;

    private float pxTempFilterValue;

    private DailyQcUtils() {
        // empty constructor
    }

    public class Ts {
        public String abr;

        public String name;
    }

    public static class Pcp {

        public int value[][];

    }

    public class Maps {
        public String hb5;

        public int basin_points;

        public int zones[] = new int[4];

        public String bchar;

        public Lcoord basin[];

        public int hrap_points;

        public Hrap_Data hrap_data[];

        public int owner;

        public int maps_done[];

        public int zmaps_done[];

        public int tmaps_done[];

        public float gz[];

        public float uz[];

        public float mz[];

        public float lz[];

        public float gzc[];

        public float uzc[];

        public float mzc[];

        public float lzc[];

        public float zgz[];

        public float zuz[];

        public float zmz[];

        public float zlz[];

        public float tgz[];

        public float tuz[];

        public float tmz[];

        public float tlz[];
    }

    public class Lcoord {
        public float lon; /* x */

        public float lat; /* y */
    }

    public class Hrap_Data {

        public int x;

        public int y;

        public int zone[] = new int[4];

    }

    public static class QCHRAP {
        public float x;

        public float y;
    }

    public static class Hrap_Grid {

        public int maxi;

        public int maxj;

        public int hrap_minx;

        public int hrap_miny;

        public Coord coord[][];

        public Gagem gage[][];

        public int owner[][];

        public int isoh[][][];

        public int max[][][];

        public int min[][][];

        public int elev[][];

    }

    public static class Coord {

        public float lat;

        public float lon;

        /**
         *
         * @param ilat
         * @param ilon
         */
        public Coord(float ilat, float ilon) {
            lat = ilat;
            lon = ilon;
        }
    }

    public static class Gagem {

        public int index[] = new int[30];

        public int zindex[] = new int[30];

        public int tindex[] = new int[30];
    }

    public class Tdata {

        public Date data_time;

        public String ztime;

        public int used[] = new int[6];

        public float stddev;

        public int level[] = new int[6];

        public Ttn tstn[];
    }

    public class Pdata {

        public Date data_time;

        public String ztime;

        public int used[] = new int[5];

        public float stddev;

        public int level;

        public Stn[] stn;
    }

    public class Zdata {

        public Date data_time;

        public String ztime;

        public int used[] = new int[5];

        public int level[] = new int[5];

        public Ztn zstn[];

    }

    public class Stn {
        public short tcons;

        public short scons[];

        public Rain rrain[];

        public Rain frain[];

        public int frzlvl[];

        public short snoflag[];

        public short sflag[];

        public Rain srain[];
    }

    public class Ttn {
        public Tlevel tlevel1[];

        public Tlevel tlevel2[];
    }

    public class Ztn {
        public Zlevel zlevel1[];

        public Zlevel zlevel2[];
    }

    public class Tlevel {
        public float data;

        public short qual;

        public short estimate;

        public float a;
    }

    public class Zlevel {

        public float data;

        public short qual;
    }

    public class Rain {

        public float data;

        public short qual;

        public float estimate;

        public float stddev;
    }

    public class Bad_Daily_Values {

        public int used;

        public String hb5;

        public String parm;

        public int day;

        public int quart;

        public float fvalue;
    }

    public class Station {
        public float isoh[];

        public float max[];

        public float min[];

        public String hb5;

        public String name;

        public String parm;

        public String cparm;

        public short index[];

        public short zindex[];

        public float lat;

        public float lon;

        public int elev;

        public int tip;

        public int xadd;

        public int yadd;

        public float hrap_x;

        public float hrap_y;
    }

    /**
     * Retrieve singleton instance
     *
     * @return singleton instance of DailyQcUtils
     */
    public static synchronized DailyQcUtils getInstance() {
        if (instance == null) {
            instance = new DailyQcUtils();
        }

        return instance;
    }

    public static boolean after18Z() {
        Calendar current = TimeUtil.newCalendar(TimeUtil.GMT_TIME_ZONE);
        return current.get(Calendar.HOUR_OF_DAY) >= 18;
    }

    /**
     * Determines if the current GMT time is after 12z and before 18z. This
     * knowledge is important because it determines whether or not the user is
     * able to view a full day of data for a Precipitation point.
     *
     * @return {@code true} when the current time is after 12z and before 18z;
     *         {@code false}, otherwise.
     */
    public static boolean after12ZBefore18Z() {
        Calendar current = TimeUtil.newCalendar(TimeUtil.GMT_TIME_ZONE);
        final int hourOfDay = current.get(Calendar.HOUR_OF_DAY);
        return (hourOfDay >= 12 && hourOfDay < 18);
    }

    public int newPid() {
        Random r = new Random();

        // Let's create a hand-made pid for default
        this.pid = r.nextInt() & 0x7fffffff;
        return pid;
    }

    public int getPid() {
        return pid;
    }

    public int qcDataReload(Date currentDate, String currntQcArea, int days,
            boolean autoqc) {
        currentQcArea = currntQcArea;
        currDate = currentDate;
        auto_dailyqc_flag = autoqc;
        qcDays = MPEDataManager.getInstance().getDQCDays();
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        Cursor prevCursor = shell.getCursor();
        shell.setCursor(Display.getDefault().getSystemCursor(SWT.CURSOR_WAIT));
        new_area_flag = 1;
        int retval = loadDataSet();
        lastQcArea = currentQcArea;
        shell.setCursor(prevCursor);
        return retval;
    }

    public static int getEnding6HourObsTime() {
        String s = appsDefaults.getToken(dqc_ending_6hour_obstime_tok);
        int value = ((!(null == s)) ? Integer.parseInt(s) : -1);

        if (value == -1) {
            value = DEFAULT_ENDING_6HOUR_OBS_TIME;
        }
        return value;
    }

    public int qcDataHasChanged(Date prevDate, Date currentDate,
            String currntQcArea, int days, boolean autoqc) {
        currentQcArea = currntQcArea;
        currDate = currentDate;
        if (prevDate == null) {
            prevDate = currDate;
        }
        selDate = prevDate;
        auto_dailyqc_flag = autoqc;
        qcDays = MPEDataManager.getInstance().getDQCDays();
        if (lastQcArea.isEmpty()) {
            newarea = true;
            lastQcArea = currentQcArea;
            firstTok = 1;
        }
        curHrMinSec = getCurrentHrMinSec(selDate);
        if (curHrMinSec != -1) {
            if ((curHrMinSec >= 0) && (curHrMinSec < SIX_HR_OF_SECONDS)) {
                curHr00_06 = 1;
            } else if ((curHrMinSec >= SIX_HR_OF_SECONDS)
                    && (curHrMinSec < TWELVE_HR_OF_SECONDS)) {
                curHr06_12 = 1;
            } else if ((curHrMinSec >= TWELVE_HR_OF_SECONDS)
                    && (curHrMinSec < EIGHTEEN_HR_OF_SECONDS)) {
                curHr12_18 = 1;
            } else {
                curHr18_00 = 1;
            }
        } else {
            curHr00_06 = -1;
            curHr06_12 = -1;
            curHr12_18 = -1;
            curHr18_00 = -1;
        }
        if ((!currDate.equals(selDate)) || (!currentQcArea.equals(lastQcArea))
                || newarea) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            Cursor prevCursor = shell.getCursor();
            shell.setCursor(
                    Display.getDefault().getSystemCursor(SWT.CURSOR_WAIT));
            firstTok = 1;
            int retval = loadDataSet();
            lastQcArea = currentQcArea;
            shell.setCursor(prevCursor);
            return retval;
        } else {
            new_area_flag = 1;
            lastQcArea = currentQcArea;
            return new_area_flag;
        }
    }

    public static int getCurrentHrMinSec(Date currentDate) {

        Calendar cal = TimeUtil.newGmtCalendar(currentDate);
        int curYear = cal.get(Calendar.YEAR);
        int curMonth = 1 + cal.get(Calendar.MONTH);
        int curDay = cal.get(Calendar.DAY_OF_MONTH);
        int curHour = cal.get(Calendar.HOUR_OF_DAY);
        int curMin = cal.get(Calendar.MINUTE);
        int curSec = cal.get(Calendar.SECOND);

        Calendar system = TimeUtil.newGmtCalendar();
        /* decide the current time's hydrologic day */
        system.add(Calendar.HOUR_OF_DAY, 6);

        int hydro_curYear = system.get(Calendar.YEAR);
        int hydro_curMonth = 1 + system.get(Calendar.MONTH);
        hydro_curDay = system.get(Calendar.DAY_OF_MONTH);

        if ((hydro_curYear == curYear) && (hydro_curMonth == curMonth)
                && (hydro_curDay == curDay)) {
            curHrMinSec = (curHour * 3600) + (curMin * 60) + curSec;
        }

        return curHrMinSec;
    }

    private int loadDataSet() {
        int num;
        int kk, mk;

        bad_values = new Bad_Daily_Values[6000];
        bad_tvalues = new Bad_Daily_Values[6000];
        for (int kd = 0; kd < 6000; kd++) {
            bad_values[kd] = new Bad_Daily_Values();
            bad_tvalues[kd] = new Bad_Daily_Values();
        }
        String dbuf = "";
        ReadQPFGrids rqg = new ReadQPFGrids();
        WriteQPFGrids wqg = new WriteQPFGrids();

        // Initialize the station list arrays
        precip_stations = new ArrayList<>();
        temperature_stations = new ArrayList<>();
        freezing_stations = new ArrayList<>();

        if ((pdata == null) || (pdata.length < 1)) {
            new_area_flag = 1;
        }

        if (newarea || (new_area_flag == 1)) {
            pdata = new Pdata[11];
            tdata = new Tdata[11];
            zdata = new Zdata[11];
            ts = new Ts[20];
            mean_areal_precip_global = new Maps[2000];
            init_maxmin = false;
        }

        btime.setTime(selDate);
        int begin_day = btime.get(Calendar.DAY_OF_MONTH);

        if ((begin_day == hydro_curDay) && (curHr12_18 == 1)) {
            btime.set(Calendar.HOUR_OF_DAY, 12);
            btime.set(Calendar.MINUTE, 0);
            btime.set(Calendar.SECOND, 0);
        } else if ((begin_day == hydro_curDay) && ((curHr00_06 == 1)
                || (curHr06_12 == 1) || (curHr18_00 == 1))) {
            btime.set(Calendar.HOUR_OF_DAY, 12);
            btime.set(Calendar.MINUTE, 0);
            btime.set(Calendar.SECOND, 0);
        } else {
            btime.set(Calendar.HOUR_OF_DAY, 12);
            btime.set(Calendar.MINUTE, 0);
            btime.set(Calendar.SECOND, 0);
        }

        emonth = btime.get(Calendar.MONTH);
        Calendar otime = TimeUtil.newGmtCalendar();
        otime.setTime(btime.getTime());
        Calendar stime = TimeUtil.newGmtCalendar();
        long dayoffset = (24L * 3600L * qcDays);
        otime.add(Calendar.SECOND, -(int) dayoffset);
        smonth = otime.get(Calendar.MONTH);
        isom = emonth;
        statusHandler.handle(Priority.DEBUG,
                "Current qc time is " + btime.getTime());
        if (!newarea && (new_area_flag == 1)) {
            newarea = true;
        }

        final String mpe_gridmasks_tok = "mpe_gridmask_dir";
        final String mpe_rfc_name_tok = "mpe_site_id";

        final String mpe_point_precip_dir_tok = "mpe_point_precip_dir";
        final String mpe_grid_precip_dir_tok = "mpe_grid_precip_dir";
        final String mpe_map_precip_dir_tok = "mpe_map_dir";
        final String mpe_bad_precip_dir_tok = "mpe_bad_precip_dir";
        final String mpe_dev_precip_dir_tok = "mpe_dev_precip_dir";

        final String mpe_point_freezing_dir_tok = "mpe_point_freezing_dir";
        final String mpe_grid_freezing_dir_tok = "mpe_grid_freezing_dir";
        final String mpe_maz_freezing_dir_tok = "mpe_maz_dir";

        final String mpe_point_temperature_dir_tok = "mpe_point_temperature_dir";
        final String mpe_grid_temperature_dir_tok = "mpe_grid_temperature_dir";
        final String mpe_mat_temperature_dir_tok = "mpe_mat_dir";
        final String mpe_bad_temperature_dir_tok = "mpe_bad_temperature_dir";
        final String mpe_dev_temperature_dir_tok = "mpe_dev_temperature_dir";
        final String mpe_station_list_dir_tok = "mpe_station_list_dir";
        final String mpe_climo_list_dir_tok = "mpe_climo_dir";
        final String mpe_prism_dir_tok = "mpe_prism_dir";
        final String mpe_type_source_tok = "mpe_type_source";

        /*
         * Read tokens which control the how the DailyQC algorithms work.
         */
        final String mpe_dqc_max_precip_neighbors_tok = "mpe_dqc_max_precip_neighbors";
        final String mpe_dqc_max_temp_neighbors_tok = "mpe_dqc_max_temp_neighbors";
        final String mpe_dqc_precip_deviation_tok = "mpe_dqc_precip_deviation";
        final String mpe_dqc_temperature_deviation_tok = "mpe_dqc_temperature_deviation";
        final String mpe_dqc_min_good_stations_tok = "mpe_dqc_min_good_stations";
        final String mpe_show_missing_gage_tok = "mpe_show_missing_gage";
        final String mpe_dqc_warningpopup_tok = "mpe_dqc_warningpopup";
        final String mpe_dqc_save_netcdf_tok = "mpe_dqc_save_netcdf";
        final String mpe_dqc_save_grib_tok = "mpe_dqc_save_grib";

        if (firstTok == 1) {
            // read tokens
            mpe_gridmasks = appsDefaults.getToken(mpe_gridmasks_tok);
            mpe_rfc_name = appsDefaults.getToken(mpe_rfc_name_tok);
            mpe_td_new_algorithm_set = 0;
            String mpe_td_details_set_tok_val = appsDefaults
                    .getToken("mpe_td_details_set");
            if ((mpe_td_details_set_tok_val != null)
                    && (mpe_td_details_set_tok_val.length() > 0)) {
                if (appsDefaults.consideredTrue(mpe_td_details_set_tok_val)) {
                    mpe_td_details_set = 1;
                } else {
                    mpe_td_details_set = 0;
                }
            } else {
                mpe_td_details_set = 0;
            }
            String mpe_td_new_algorithm_tok_val = appsDefaults
                    .getToken("mpe_td_new_algorithm");
            if ((mpe_td_new_algorithm_tok_val != null)
                    && (mpe_td_new_algorithm_tok_val.length() > 0)) {
                if (appsDefaults.consideredTrue(mpe_td_new_algorithm_tok_val)) {
                    mpe_td_new_algorithm_set = 1;
                } else {
                    mpe_td_new_algorithm_set = 0;
                }
            } else {
                mpe_td_new_algorithm_set = 0;
            }
            mpe_point_precip_dir = appsDefaults
                    .getToken(mpe_point_precip_dir_tok);
            mpe_grid_precip_dir = appsDefaults
                    .getToken(mpe_grid_precip_dir_tok);
            mpe_map_precip_dir = appsDefaults.getToken(mpe_map_precip_dir_tok);
            mpe_dev_precip_dir = appsDefaults.getToken(mpe_dev_precip_dir_tok);
            mpe_bad_precip_dir = appsDefaults.getToken(mpe_bad_precip_dir_tok);
            mpe_show_missing_gage = appsDefaults
                    .getToken(mpe_show_missing_gage_tok);
            mpe_point_freezing_dir = appsDefaults
                    .getToken(mpe_point_freezing_dir_tok);
            mpe_grid_freezing_dir = appsDefaults
                    .getToken(mpe_grid_freezing_dir_tok);
            mpe_maz_freezing_dir = appsDefaults
                    .getToken(mpe_maz_freezing_dir_tok);
            mpe_point_temperature_dir = appsDefaults
                    .getToken(mpe_point_temperature_dir_tok);
            mpe_grid_temperature_dir = appsDefaults
                    .getToken(mpe_grid_temperature_dir_tok);
            mpe_mat_temperature_dir = appsDefaults
                    .getToken(mpe_mat_temperature_dir_tok);
            mpe_bad_temperature_dir = appsDefaults
                    .getToken(mpe_bad_temperature_dir_tok);
            mpe_dev_temperature_dir = appsDefaults
                    .getToken(mpe_dev_temperature_dir_tok);
            mpe_station_list_dir = appsDefaults
                    .getToken(mpe_station_list_dir_tok);
            mpe_climo_list_dir = appsDefaults.getToken(mpe_climo_list_dir_tok);
            mpe_prism_dir = appsDefaults.getToken(mpe_prism_dir_tok);
            mpe_dqc_max_precip_neighbors = Integer.parseInt(
                    appsDefaults.getToken(mpe_dqc_max_precip_neighbors_tok));
            mpe_dqc_max_temp_neighbors = Integer.parseInt(
                    appsDefaults.getToken(mpe_dqc_max_temp_neighbors_tok));
            mpe_dqc_precip_deviation = Float.parseFloat(
                    appsDefaults.getToken(mpe_dqc_precip_deviation_tok));
            mpe_dqc_temperature_deviation = Float.parseFloat(
                    appsDefaults.getToken(mpe_dqc_temperature_deviation_tok));
            mpe_dqc_min_good_stations = Integer.parseInt(
                    appsDefaults.getToken(mpe_dqc_min_good_stations_tok));

            String save_netcdf = appsDefaults.getToken(mpe_dqc_save_netcdf_tok);
            mpe_dqc_save_netcdf = appsDefaults.consideredTrue(save_netcdf);

            String save_grib = appsDefaults.getToken(mpe_dqc_save_grib_tok);
            mpe_dqc_save_grib = appsDefaults.consideredTrue(save_grib);

            dqc_ending_6hour_obstime = getEnding6HourObsTime();
            String gridMax = appsDefaults.getToken("mpe_dqc_grid_max_dist");
            if ((gridMax != null) && (gridMax.length() > 0)) {
                /*
                 * TODO: what if this is not a valid integer value?
                 * Apps_defaults should really be validated before it can be
                 * saved / used.
                 */
                mpe_dqc_grid_max_dist = Integer.parseInt(gridMax);
            }
            String mpe_post_analysis_tok_val = appsDefaults
                    .getToken("mpe_post_analysis");
            post_analysis_calc = appsDefaults
                    .consideredTrue(mpe_post_analysis_tok_val);

            dqcTimeStringIndex = (dqc_ending_6hour_obstime / 6) + 1;
            String mpe_dqc_warningpopup_val = ((appsDefaults
                    .getToken(mpe_dqc_warningpopup_tok)));
            mpe_dqc_warningpopup_flag = appsDefaults
                    .consideredTrue(mpe_dqc_warningpopup_val);
        }

        if (firstTok == 1) {
            station_climo_file = mpe_climo_list_dir + "/" + mpe_rfc_name
                    + "_station_climo_list";
            hrap_gage_file = mpe_gridmasks + "/precip_neighbor_list_"
                    + currentQcArea;
            hrap_zgage_file = mpe_gridmasks + "/freezing_neighbor_list_"
                    + currentQcArea;
            hrap_tgage_file = mpe_gridmasks + "/temperature_neighbor_list_"
                    + currentQcArea;
            hrap_file = mpe_gridmasks + "/basin_to_grid_" + mpe_rfc_name;
            hrap_grid_mask_file = mpe_gridmasks + "/hsa_to_grid_mask_"
                    + mpe_rfc_name;

            // Create the filenames for the precipitation, freezing level and
            // and temperature data.
            pcpn_file = mpe_point_precip_dir + "/precip_1_" + currentQcArea
                    + "_point_";
            proc_pcpn_file = mpe_point_precip_dir + "/precip_2_" + currentQcArea
                    + "_point_";
            pcpn_qc_file = mpe_point_precip_dir + "/precip_qc_" + currentQcArea
                    + "_point_";
            snow_file = mpe_point_precip_dir + "/snow_" + currentQcArea
                    + "_point_";
            bad_snow_file = mpe_bad_precip_dir + "/bad_snow_" + currentQcArea
                    + "_point";
            pcpn_dev_file = mpe_dev_precip_dir + "/precip_" + currentQcArea
                    + "_stddev_";
            pcpn_bad_file = mpe_bad_precip_dir + "/precip_" + currentQcArea
                    + "_bad_";
            map_file = mpe_map_precip_dir + "/map_" + currentQcArea + "_";
            grid_file = mpe_grid_precip_dir + "/precip_" + currentQcArea
                    + "_grid_";
            ngrid_file = mpe_grid_precip_dir + "/nexrad_grid_";

            // Define the paths to the Freezing Level directories.
            zpoint1_file = mpe_point_freezing_dir + "/freezing_1_"
                    + currentQcArea + "_point_";
            zpoint2_file = mpe_point_freezing_dir + "/freezing_2_"
                    + currentQcArea + "_point_";
            zgrid_file = mpe_grid_freezing_dir + "/freezing_" + currentQcArea
                    + "_grid_";
            rsel_file = mpe_maz_freezing_dir + "/maz_" + currentQcArea + "_";

            // Define the paths to the Temperature directories.
            tpoint1_file = mpe_point_temperature_dir + "/temperature_1_"
                    + currentQcArea + "_point_";
            tpoint2_file = mpe_point_temperature_dir + "/temperature_2_"
                    + currentQcArea + "_point_";
            tgrid_file = mpe_grid_temperature_dir + "/temperature_"
                    + currentQcArea + "_grid_";
            mat_file = mpe_mat_temperature_dir + "/mat_" + currentQcArea + "_";
            temp_dev_file = mpe_dev_temperature_dir + "/temperature_"
                    + currentQcArea + "_stddev_";
            temp_bad_file = mpe_bad_temperature_dir + "/temperature_"
                    + currentQcArea + "_bad_";
            String mpe_editor_logs_dir = appsDefaults
                    .getToken("mpe_editor_logs_dir");
            /*
             * create mpe_td_details_file to store the details of Time
             * Distributed estimates if the token mpe_td_details_set is set as
             * ON or on
             */

            if (mpe_td_details_set == 1) {
                String mpe_td_details_file = String
                        .format("%s/mpe_td_details_file", mpe_editor_logs_dir);

                td_fp = new File(mpe_td_details_file);
                td_fp.setReadable(true, false);
                td_fp.setWritable(true, false);
                if (td_fp == null) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Could not open mpe_td_details_file in load_gage_data().");
                    return 0;
                }
                try {
                    td_fpwr = new BufferedWriter(new FileWriter(td_fp, true));
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Could not open mpe_td_details_file: "
                                    + mpe_td_details_file
                                    + " for writing, in load_gage_data().",
                            e);
                }
            }

        }
        statusHandler.handle(Priority.DEBUG,
                "DQC: DailyQcUtils finished loading tokens. ");
        if (newarea || (new_area_flag == 1)) {
            String type_sources = appsDefaults.getToken(mpe_type_source_tok);
            if (type_sources.length() <= 0) {
                statusHandler.handle(Priority.ERROR,
                        "mpe_type_source_token token not defined.");
            } else {
                String regx = "(\\w{2}):(\\w{2,})";
                Pattern p = Pattern.compile(regx);
                Matcher mt = p.matcher(type_sources);
                tsmax = 0;
                while (mt.find()) {
                    ts[tsmax] = new Ts();
                    ts[tsmax].abr = mt.group(1);
                    ts[tsmax].name = mt.group(2);
                    ++tsmax;
                }

                if (tsmax == 0) {
                    statusHandler.handle(Priority.ERROR,
                            "Could not parse any typesources from %s token:"
                                    + mpe_type_source_tok);
                }
                // added by zhan for DR # 8973
                else if (tsmax > MAX_TYPE_SOURCES) {
                    statusHandler.handle(Priority.WARN,
                            "Warning !!! Number of typesources read from mpe_type_source token exceeds MAX_TYPE_SOURCES !!!");

                    tsmax = MAX_TYPE_SOURCES;
                }
            }

        }

        if (newarea) {
            StationListManager slm = StationListManager.getInstance();
            statusHandler.handle(Priority.DEBUG,
                    "DQC: Reading Freezing Stations List. ");

            try {
                slm.getStationInfo(currentQcArea, newarea, freezing_stations,
                        temperature_stations, precip_stations);
            } catch (FileNotFoundException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "File for " + currentQcArea + " does not exist:" + e);
            }
            if (freezing_stations == null) {
                statusHandler.handle(Priority.PROBLEM,
                        "ERROR, Could not read freezing level station list file. DailyQC stopped.");
                return DAILYQC_FAILED;
            }
            statusHandler.handle(Priority.DEBUG,
                    "DQC: Reading Temperature Stations List. ");

            if (temperature_stations == null) {
                statusHandler.handle(Priority.ERROR,
                        "ERROR, Could not read temperature station list file. DailyQC stopped.");
                return DAILYQC_FAILED;
            }
            statusHandler.handle(Priority.DEBUG,
                    "DQC: Reading Precip Stations List. ");

            if (precip_stations == null) {
                statusHandler.handle(Priority.ERROR,
                        "ERROR, Could not read precip station list file. DailyQC stopped.");
                return DAILYQC_FAILED;
            }

            InitPrecipClimo ipc = new InitPrecipClimo();
            boolean status = ipc.initPrecip_climo(station_climo_file,
                    precip_stations, precip_stations.size());
            if (!status) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not read precipitation Climo file. DailyQC stopped.");
                return DAILYQC_FAILED;
            }

            InitTempClimo itc = new InitTempClimo();
            status = itc.initTemp_climo(station_climo_file,
                    temperature_stations, temperature_stations.size());
            if (!status) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not read temperature Climo file. DailyQC stopped.");
                return DAILYQC_FAILED;
            }
        }

        boolean status = false;
        MPEPrismDataLoadFailed mpePrismDataLoadFailed = new MeanMonthlyPrecip()
                .read_mean_monthly_precip(mpe_prism_dir, mpe_rfc_name, smonth,
                        emonth);
        if (mpePrismDataLoadFailed != null) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not read the precipitation PRISM files. "
                            + mpePrismDataLoadFailed.getMessage()
                            + " DailyQC stopped.",
                    mpePrismDataLoadFailed.getEx());
            return DAILYQC_FAILED;
        }

        mpePrismDataLoadFailed = new MeanMonthlyTemp().read_mean_monthly_temp(
                mpe_prism_dir, mpe_rfc_name, smonth, emonth);
        if (mpePrismDataLoadFailed != null) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not read the temperature PRISM files. "
                            + mpePrismDataLoadFailed.getMessage()
                            + " DailyQC stopped.",
                    mpePrismDataLoadFailed.getEx());
            return DAILYQC_FAILED;
        }
        statusHandler.handle(Priority.DEBUG,
                "DQC: Finished loading Climo data. ");

        // Create the predefined gage, freezing level and temperature
        // HRAP grids which will later be used when rendering grids.
        MapPrecipGagesGrid mpg = new MapPrecipGagesGrid();
        statusHandler.handle(Priority.DEBUG,
                "DQC: Mapping precip gages to grid. ");
        long start = System.currentTimeMillis();
        statusHandler.handle(Priority.DEBUG, "Starting to map gages to grid.");
        hrap_grid = mpg.map_precip_gages_to_grid(smonth, emonth, hrap_gage_file,
                currentQcArea, precip_stations, precip_stations.size());
        if (hrap_grid == null) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not map precipitation gages to grid. DailyQC stopped.\n");
            return DAILYQC_FAILED;
        }
        statusHandler.handle(Priority.DEBUG,
                "DQC: Mapping freezing gages to grid. ");
        MapFreezingGagesGrid mfg = new MapFreezingGagesGrid();
        status = mfg.map_freeze_gages_to_grid(smonth, emonth, hrap_grid,
                hrap_zgage_file, currentQcArea, freezing_stations,
                precip_stations, freezing_stations.size(),
                precip_stations.size());
        if (!status) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not map freezing level points to the HRAP grid. DailyQC stopped.");
            return DAILYQC_FAILED;
        }
        statusHandler.handle(Priority.DEBUG,
                "DQC: Mapping temperature gages to grid. ");
        MapTempGagesGrid mtg = new MapTempGagesGrid();
        status = mtg.map_temp_gages_to_grid(smonth, emonth, hrap_grid,
                hrap_tgage_file, currentQcArea, temperature_stations,
                temperature_stations.size());
        if (!status) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not map temp level points to the HRAP grid. DailyQC stopped.");
            return DAILYQC_FAILED;
        }
        long elapsed = System.currentTimeMillis() - start;
        statusHandler.handle(Priority.DEBUG,
                "Finished mapping gages to grid, elapsed time: " + elapsed
                        + " ms");
        if (newarea) {
            // Create the HSA mask. This maps each HRAP grid bin to an HSA.
            // If no file is supplied, then the default grid_mask is set to all
            // 1's.
            GetHrapMask ghm = new GetHrapMask();
            start = System.currentTimeMillis();
            statusHandler.handle(Priority.DEBUG, "Starting HSA to Grid mask");
            ghm.get_hsa_to_grid_mask(hrap_grid, tag, wfo_all,
                    hrap_grid_mask_file);
            statusHandler.handle(Priority.DEBUG,
                    "Finished HSA to Grid mask, elapsed time: "
                            + (System.currentTimeMillis() - start) + " ms");
            /* Was the precipitation climatology available? */
            if (!isohyets_used) {
                method = 1;
            }

            GetBasinData gbd = new GetBasinData();
            status = gbd.getBasinData(hrap_file, mean_areal_precip_global, tag);
            if (!status) {
                statusHandler.handle(Priority.ERROR,
                        "Error retrieving basin data.  DailyQC Stopped.");
                return DAILYQC_FAILED;
            }
        }
        // Allocate memory for the precip, freezing level and
        // temperature data in the pdata array.
        for (int zz = 0; zz < pdata.length; zz++) {
            pdata[zz] = new Pdata();
        }
        for (int l = 0; l < pdata.length; ++l) {
            pdata[l].stn = new Stn[precip_stations.size()];

            for (int m = 0; m < precip_stations.size(); ++m) {
                pdata[l].stn[m] = new Stn();
                pdata[l].stn[m].scons = new short[5];
                pdata[l].stn[m].rrain = new Rain[5];
                pdata[l].stn[m].frain = new Rain[5];
                pdata[l].stn[m].srain = new Rain[5];

                for (int zj = 0; zj < 5; zj++) {
                    pdata[l].stn[m].rrain[zj] = new Rain();
                    pdata[l].stn[m].frain[zj] = new Rain();
                    pdata[l].stn[m].srain[zj] = new Rain();
                }

                pdata[l].stn[m].frzlvl = new int[5];
                pdata[l].stn[m].snoflag = new short[5];
                pdata[l].stn[m].sflag = new short[5];
            }
        }

        for (int zz = 0; zz < tdata.length; zz++) {
            tdata[zz] = new Tdata();
        }
        for (int l = 0; l < pdata.length; ++l) {
            tdata[l].tstn = new Ttn[temperature_stations.size()];

            for (int m = 0; m < temperature_stations.size(); ++m) {
                tdata[l].tstn[m] = new Ttn();
                tdata[l].tstn[m].tlevel1 = new Tlevel[6];
                tdata[l].tstn[m].tlevel2 = new Tlevel[6];
                for (int zj = 0; zj < 6; zj++) {
                    tdata[l].tstn[m].tlevel1[zj] = new Tlevel();
                    tdata[l].tstn[m].tlevel2[zj] = new Tlevel();
                }
            }
        }

        for (int zz = 0; zz < zdata.length; zz++) {
            zdata[zz] = new Zdata();
        }
        for (int l = 0; l < pdata.length; l++) {
            zdata[l].zstn = new Ztn[freezing_stations.size()];

            for (int m = 0; m < freezing_stations.size(); m++) {
                zdata[l].zstn[m] = new Ztn();
                zdata[l].zstn[m].zlevel1 = new Zlevel[5];
                zdata[l].zstn[m].zlevel2 = new Zlevel[5];
                for (int zj = 0; zj < 5; zj++) {
                    zdata[l].zstn[m].zlevel1[zj] = new Zlevel();
                    zdata[l].zstn[m].zlevel2[zj] = new Zlevel();
                }
            }
        }

        for (int m = 0; m < qcDays; ++m) {
            otime.setTime(btime.getTime());
            dayoffset = (24L * 3600L * m);
            otime.add(Calendar.SECOND, -(int) dayoffset);

            // Read the level 1 precipitation data.
            pdata[m].stddev = mpe_dqc_precip_deviation;
            tdata[m].stddev = mpe_dqc_temperature_deviation;

            // Complete the names of the files containing the precipitation,
            // freezing level, and temperature data. This amounts to adding
            // the current datetime being processed to each file being
            // processed.
            String preca = String.format("%s%04d%02d%02d", pcpn_file,
                    otime.get(Calendar.YEAR), otime.get(Calendar.MONTH) + 1,
                    otime.get(Calendar.DAY_OF_MONTH));
            String precb = String.format("%s%04d%02d%02d", proc_pcpn_file,
                    otime.get(Calendar.YEAR), otime.get(Calendar.MONTH) + 1,
                    otime.get(Calendar.DAY_OF_MONTH));
            String precc = String.format("%s%04d%02d%02d", pcpn_dev_file,
                    otime.get(Calendar.YEAR), otime.get(Calendar.MONTH) + 1,
                    otime.get(Calendar.DAY_OF_MONTH));
            String precd = String.format("%s%04d%02d%02d", pcpn_bad_file,
                    otime.get(Calendar.YEAR), otime.get(Calendar.MONTH) + 1,
                    otime.get(Calendar.DAY_OF_MONTH));
            String prece = String.format("%s%04d%02d%02d", snow_file,
                    otime.get(Calendar.YEAR), otime.get(Calendar.MONTH) + 1,
                    otime.get(Calendar.DAY_OF_MONTH));
            String zpointa = String.format("%s%04d%02d%02d", zpoint1_file,
                    otime.get(Calendar.YEAR), otime.get(Calendar.MONTH) + 1,
                    otime.get(Calendar.DAY_OF_MONTH));
            String zpointb = String.format("%s%04d%02d%02d", zpoint2_file,
                    otime.get(Calendar.YEAR), otime.get(Calendar.MONTH) + 1,
                    otime.get(Calendar.DAY_OF_MONTH));
            String tpointa = String.format("%s%04d%02d%02d", tpoint1_file,
                    otime.get(Calendar.YEAR), otime.get(Calendar.MONTH) + 1,
                    otime.get(Calendar.DAY_OF_MONTH));
            String tpointb = String.format("%s%04d%02d%02d", tpoint2_file,
                    otime.get(Calendar.YEAR), otime.get(Calendar.MONTH) + 1,
                    otime.get(Calendar.DAY_OF_MONTH));
            String tpointd = String.format("%s%04d%02d%02d", temp_bad_file,
                    otime.get(Calendar.YEAR), otime.get(Calendar.MONTH) + 1,
                    otime.get(Calendar.DAY_OF_MONTH));
            String tpointc = String.format("%s%04d%02d%02d", temp_dev_file,
                    otime.get(Calendar.YEAR), otime.get(Calendar.MONTH) + 1,
                    otime.get(Calendar.DAY_OF_MONTH));
            String databuf = String.format("%04d%02d%02d",
                    otime.get(Calendar.YEAR), otime.get(Calendar.MONTH) + 1,
                    otime.get(Calendar.DAY_OF_MONTH));

            /* Read the snow reports. */
            ReadSnowData rs = new ReadSnowData();
            rs.read_snow(prece, precip_stations, precip_stations.size(), m);

            // read level 1 precip data
            ReadPrecipA ra = new ReadPrecipA();
            int mer = ra.read_precip_a(preca, otime.getTime(), m, pdata,
                    precip_stations, precip_stations.size());

            ReadZlevelA rza = new ReadZlevelA();
            // Read the level 1 freezing level data.
            rza.read_zlevel_a(zpointa, otime.getTime(), m, zdata,
                    freezing_stations, freezing_stations.size());

            ReadTempA rta = new ReadTempA();
            // Read the level 1 temp level data.
            rta.read_t_a(tpointa, otime.getTime(), m, tdata,
                    temperature_stations, temperature_stations.size());

            // Read the list of bad precipitation values.
            BadValues bv = new BadValues();
            bv.readBadValues(precd, m);

            BadTValues bt = new BadTValues();
            bt.read_bad_tvalues(tpointd, m);
            int ier = 0;
            int zer = 0;
            int ter = 0;

            if (auto_dailyqc_flag) {
                ReadPrecipB rb = new ReadPrecipB();
                // read level 2 precip data for auto_dailyqc
                ier = rb.read_precip_b(precb, otime.getTime(), m, pdata,
                        precip_stations, precip_stations.size());

                ReadZlevelB rzb = new ReadZlevelB();
                zer = rzb.read_zlevel_b(zpointb, otime.getTime(), m, zdata,
                        freezing_stations, freezing_stations.size());

                ReadTempB rtb = new ReadTempB();
                ter = rtb.read_t_b(tpointb, otime.getTime(), m, tdata,
                        temperature_stations, temperature_stations.size());
                continue;
            } else {
                ReadPrecipB rb = new ReadPrecipB();
                // read level 2 precip data
                ier = rb.read_precip_b(precb, otime.getTime(), m, pdata,
                        precip_stations, precip_stations.size());

                ReadZlevelB rzb = new ReadZlevelB();
                zer = rzb.read_zlevel_b(zpointb, otime.getTime(), m, zdata,
                        freezing_stations, freezing_stations.size());

                ReadTempB rtb = new ReadTempB();
                ter = rtb.read_t_b(tpointb, otime.getTime(), m, tdata,
                        temperature_stations, temperature_stations.size());
            }

            // --------------------------------------------------------------
            // open and read file containing std dev of point precip data
            // --------------------------------------------------------------
            try (BufferedReader in = new BufferedReader(new FileReader(precc));
                    Scanner s = new Scanner(in)) {

                if (s.hasNextFloat()) {
                    pdata[m].stddev = s.nextFloat();

                    if ((pdata[m].stddev != 1.0) && (pdata[m].stddev != 3.0)
                            && (pdata[m].stddev != 5.0)) {

                        pdata[m].stddev = 3.0f;
                    }
                }

            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "File not found: " + precc + ".", e);
            }

            // open and read file containing std dev of point temperature
            // data
            try (BufferedReader in = new BufferedReader(
                    new FileReader(tpointc)); Scanner s = new Scanner(in)) {
                if (s.hasNextFloat()) {
                    tdata[m].stddev = s.nextFloat();

                    if ((tdata[m].stddev != 5.0) && (tdata[m].stddev != 10.0)
                            && (tdata[m].stddev != 15.0)) {
                        tdata[m].stddev = 10.0f;
                    }

                }
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "File not found: " + tpointc + ".", e);
            }
            if (ier == 1) {
                dbuf = String.format("%s - level 2 data\n", databuf);
            } else if ((ier == -1) && (mer == 1)) {
                dbuf = String.format("%s - level 1 data\n", databuf);
            } else if ((ier == -2) && (mer == 1)) {
                dbuf = String.format("%s - level 1 data overwrite\n", databuf);
            } else {
                dbuf = String.format("%s - no data\n", databuf);
            }
            statusHandler.handle(Priority.DEBUG, "dbuf=" + dbuf);

            // Estimate the daily precipitation stations.
            /*
             * do not estimate daily and partial point precipitation from each
             * other if run DQC on partial time frame and m=0
             */

            if ((m == 0) && ((curHr00_06 == 1) || (curHr06_12 == 1)
                    || (curHr18_00 == 1))) {
                EstDailyStations ed = new EstDailyStations();
                ed.estimate_daily_stations(m, precip_stations,
                        precip_stations.size());

                EstPartStations ep = new EstPartStations();
                ep.estimate_partial_stations(m, precip_stations,
                        precip_stations.size());
            }

            // Quality control the stations.
            QCStations qcs = new QCStations();
            qcs.quality_control_stations(m, precip_stations,
                    precip_stations.size());
            CheckConsistency cc = new CheckConsistency();
            cc.check_consistency(m, precip_stations, precip_stations.size());
            bv.restore_bad_values(m, precip_stations, precip_stations.size());

            // Estimate and QC the daily temperature stations.
            EstDailyTStations et = new EstDailyTStations();
            et.estimate_daily_tstations(m, temperature_stations,
                    temperature_stations.size());
            QCTStations qct = new QCTStations();
            qct.quality_control_tstations(m, temperature_stations,
                    temperature_stations.size());
            bt.restore_bad_tvalues(m, temperature_stations,
                    temperature_stations.size());

            if (ier == 1) {
                for (int k = 0; k < 5; k++) {
                    if (k < 2) {
                        stime.setTime(otime.getTime());
                        stime.add(Calendar.SECOND, -84600);
                    } else {
                        stime.setTime(otime.getTime());
                    }

                    kk = 3 - k;

                    if (ngrid_file.length() > 0) {
                        dbuf = String.format("%s%s_%04d%02d%02d", ngrid_file,
                                TIME_FILE[2][k], otime.get(Calendar.YEAR),
                                otime.get(Calendar.MONTH) + 1,
                                otime.get(Calendar.DAY_OF_MONTH));

                        if (k < 4) {
                            num = 50 + (m * 4) + kk;
                        } else {
                            num = 90 + m;
                        }

                        mer = rqg.read_qpf_grids(num, dbuf);
                    }

                    dbuf = String.format("%s%s_%04d%02d%02d", grid_file,
                            TIME_FILE[2][k], otime.get(Calendar.YEAR),
                            otime.get(Calendar.MONTH) + 1,
                            otime.get(Calendar.DAY_OF_MONTH));

                    if (k < 4) {
                        num = (m * 4) + kk;
                    } else {
                        num = 40 + m;
                    }

                    mer = rqg.read_qpf_grids(num, dbuf);

                    if (mer == -1) {
                        if (k == 4) {
                            mk = 1;
                        } else {
                            mk = 0;
                        }

                        if (pdata[m].used[k] != 0) {
                            RenderPcp rpc = new RenderPcp();
                            rpc.render_pcp(m, k, mk, precip_stations.size(),
                                    precip_stations, hrap_grid, pdata,
                                    pcp_in_use);
                            wqg.write_qpf_grids(dbuf);

                            // TODO: output grid to file in netCDF format
                        }
                    }
                    CreateMap cm = new CreateMap();
                    cm.create_map(num);
                }
            }

            if (zer == 1) {
                for (int k = 0; k < 4; k++) {
                    if (zdata[m].level[k] != 2) {
                        continue;
                    }
                    /*
                     * as the dqc_ending_6hour_obstime can be either be 06 or
                     * 12, this if needs to accommodate those possibilities
                     */

                    if (dqc_ending_6hour_obstime == 12) {
                        if (k < 1) {
                            stime.setTime(otime.getTime());
                            stime.add(Calendar.SECOND, -84_600);
                        } else {
                            stime.setTime(otime.getTime());
                        }
                    } else {
                        if (k < 2) {
                            stime.setTime(otime.getTime());
                            stime.add(Calendar.SECOND, -84_600);
                        } else {
                            stime.setTime(otime.getTime());
                        }
                    }
                    kk = 3 - k;

                    dbuf = String.format("%s%s_%04d%02d%02d", zgrid_file,
                            Z_TIME_FILE[dqcTimeStringIndex][k],
                            otime.get(Calendar.YEAR),
                            otime.get(Calendar.MONTH) + 1,
                            otime.get(Calendar.DAY_OF_MONTH));

                    num = 100 + (m * 4) + kk;

                    mer = rqg.read_qpf_grids(num, dbuf);

                    if (mer == -1) {
                        RenderZ rz = new RenderZ();
                        rz.render_z(m, k, 0, freezing_stations.size(),
                                freezing_stations, hrap_grid, zdata,
                                pcp_in_use);
                    }

                    MakeRsel mr = new MakeRsel();
                    mr.make_rsel(num, num - 100);

                }
            }
            if (ter == 1) {
                for (int k = 0; k < 6; k++) {

                    if (tdata[m].level[k] != 2) {
                        continue;
                    }

                    /*
                     * as the dqc_ending_6hour_obstime can be either be 06 or
                     * 12, this if needs to accommodate those possibilities
                     */

                    if (dqc_ending_6hour_obstime == 12) {
                        if (k < 1) {
                            stime.setTime(otime.getTime());
                            stime.add(Calendar.SECOND, -84_600);
                        } else {
                            stime.setTime(otime.getTime());
                        }
                    } else {
                        if (k < 2) {
                            stime.setTime(otime.getTime());
                            stime.add(Calendar.SECOND, -84_600);
                        } else {
                            stime.setTime(otime.getTime());
                        }
                    }
                    kk = 3 - k;

                    dbuf = String.format("%s%s_%04d%02d%02d", tgrid_file,
                            T_TIME_FILE[dqcTimeStringIndex][k],
                            otime.get(Calendar.YEAR),
                            otime.get(Calendar.MONTH) + 1,
                            otime.get(Calendar.DAY_OF_MONTH));

                    if (k < 4) {
                        num = 150 + (m * 4) + kk;
                        mer = rqg.read_qpf_grids(num, dbuf);

                        // hrap_tgrid = hrap_grid;
                        if (mer == -1) {
                            RenderT6 rt6 = new RenderT6();
                            rt6.render_t6(m, k, 0, temperature_stations.size(),
                                    temperature_stations, hrap_grid, tdata,
                                    pcp_in_use);
                        }

                        MakeMat mm = new MakeMat();
                        mm.make_mat(num, num - 150);

                    } else if (k == 4) {
                        num = 190 + m;
                        mer = rqg.read_qpf_grids(num, dbuf);

                        if (mer == -1) {
                            RenderT rt = new RenderT();
                            rt.render_t(m, k, 1, temperature_stations.size(),
                                    temperature_stations, hrap_grid, tdata,
                                    pcp_in_use);
                        }

                    } else if (k == 5) {
                        num = 200 + m;
                        mer = rqg.read_qpf_grids(num, dbuf);

                        if (mer == -1) {
                            RenderT rt = new RenderT();
                            rt.render_t(m, k, 2, temperature_stations.size(),
                                    temperature_stations, hrap_grid, tdata,
                                    pcp_in_use);
                        }
                    }

                }
            }
        }

        if (!auto_dailyqc_flag) {
            GetBadSnotel gbs = new GetBadSnotel();
            gbs.get_bad_snotel(bad_snow_file, precip_stations);
            CalculateZLevel cz = new CalculateZLevel();
            cz.calculate_zlevel(freezing_stations.size(), zdata);

            GetZLevel gl = new GetZLevel();
            for (int m = 0; m < qcDays; ++m) {
                gl.get_zlevel(m, precip_stations, freezing_stations,
                        precip_stations.size(), freezing_stations.size());
            }
        }
        dbuf = "Done!!\n";
        firstTok = 0;

        /*
         * Store this information for when the DQC data structures need to be
         * deallocated.
         */
        prev_smonth = smonth;
        prev_emonth = emonth;
        gageqc_performed = true;

        /*-------------------------------------------*/
        /*
         * if PostAnalysis calculations are on, then generate 24hr accumulated
         * grids
         */
        /*-------------------------------------------*/
        if (post_analysis_calc) {
            GenerateAccumGrids gam = new GenerateAccumGrids();
            gam.generate_accum_grids(qcDays);
        }

        return 2;
    }

    public Coordinate getHraptoLatLon(Coordinate hrap_coord) {
        /* calculate HRAP coordinates to lat,lon */
        Coordinate latlon_point = new Coordinate(0, 0);

        try {
            latlon_point = com.raytheon.uf.common.xmrg.hrap.HRAP.getInstance()
                    .gridCoordinateToLatLon(hrap_coord,
                            PixelOrientation.LOWER_LEFT);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Error computing hrap coordinate", e);
        }
        return latlon_point;
    }

    public Coordinate getLatLontoHrap(Coordinate latlon_coord) {
        /* calculate HRAP coordinates to lat,lon */
        Coordinate hrap_point = new Coordinate(0, 0);
        try {
            hrap_point = com.raytheon.uf.common.xmrg.hrap.HRAP.getInstance()
                    .latLonToGridCoordinate(latlon_coord,
                            PixelOrientation.LOWER_LEFT);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Error computing hrap coordinate", e);
        }
        return hrap_point;
    }

    public String getStationListPath(String qcArea) {
        String dir = "";

        if (qcArea != null) {
            if (mpe_station_list_dir.length() > 0) {
                dir = mpe_station_list_dir + "/" + qcArea + "_station_list";
            } else {
                statusHandler.handle(Priority.PROBLEM,
                        "Apps_default token mpe_station_list_dir is not set.");
                return dir;
            }
        } else {
            if (mpe_station_list_dir.length() > 0) {
                dir = mpe_station_list_dir + "/" + mpe_rfc_name
                        + "_station_list";
            } else {
                statusHandler.handle(Priority.PROBLEM,
                        "Apps_default token mpe_station_list_dir is not set.");
                return dir;
            }
        }
        return dir;
    }

    public int is_good(int k, int smonth, int emonth) {
        if ((smonth <= emonth) && (k >= smonth) && (k <= emonth)) {
            return (1);
        }
        if (smonth > emonth) {
            if (k <= emonth) {
                return (1);
            }

            if (k >= smonth) {
                return (1);
            }
        }
        return (-1);
    }

    public void clearData() {
        if (td_fpwr != null) {
            try {
                td_fpwr.close();
                IOPermissionsHelper.applyFilePermissions(td_fp.toPath(),
                        FilePermissionConstants.POSIX_FILE_SET);
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Problem to close file" + td_fpwr, e);
            }
        }

        bad_values = null;
        bad_tvalues = null;
        pdata = null;
        tdata = null;
        zdata = null;
        mean_areal_precip_global = null;
        ts = null;
    }

    /**
     * @return the hrap_grid
     */
    public static Hrap_Grid getHrap_grid() {
        return hrap_grid;
    }

    /**
     * @param hrap_grid
     *            the hrap_grid to set
     */
    public void setHrap_grid(Hrap_Grid hrap_grid) {
        DailyQcUtils.hrap_grid = hrap_grid;
    }

    /**
     * @return the auto_dailyqc_flag
     */
    public boolean isAuto_dailyqc_flag() {
        return auto_dailyqc_flag;
    }

    /**
     * @return the max_basins
     */
    public int getMax_basins() {
        return max_basins;
    }

    /**
     * @param max_basins
     *            the max_basins to set
     */
    public void setMax_basins(int max_basins) {
        this.max_basins = max_basins;
    }

    /**
     * Clean up all data and remove the instance
     *
     */
    public void destroy() {
        clearData();
        if (instance != null) {
            instance = null;
        }
    }

    public void setPrecipFilterValue(float precipFilterVal) {
        this.precipFilterVal = precipFilterVal;
    }

    public float getPrecipFilterValue() {
        return precipFilterVal;
    }

    public float getPrecipReverseFilterValue() {
        return precipReverseFilterVal;
    }

    public void setPrecipReverseFilterValue(float precipReverseFilterVal) {
        this.precipReverseFilterVal = precipReverseFilterVal;
    }

    public int getPointElevationFilterValue() {
        return pointElevationFilterValue;
    }

    public void setPointElevationFilterValue(int pointElevationFilterValue) {
        this.pointElevationFilterValue = pointElevationFilterValue;
    }

    public float getPxTempFilterValue() {
        return this.pxTempFilterValue;
    }

    public void setPxTempFilterValue(float pxTempFilterValue) {
        this.pxTempFilterValue = pxTempFilterValue;
    }
}
