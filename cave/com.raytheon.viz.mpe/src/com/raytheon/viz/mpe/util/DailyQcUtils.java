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

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.util.MapPrecipGagesGrid.Topo;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 16, 2009            snaples     Initial creation
 * Dec 04, 2012 15544      wkwock      fix missing 12z-18z after 12
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class DailyQcUtils {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DailyQcUtils.class);

    // private static final int MAX_QC_DAYS = 10;

    public static double[][][] QPEaccum24hr;

    public static short[][] QPEgrid1hr;

    public static final double MOSAIC_DEFAULT = -9.0;

    private static String lastQcArea = "";

    public static String currentQcArea;

    private static Date currDate;

    private static Date selDate;

    public static int qcDays;

    public static int firstTok = 1;

    public static int isohyets_used = -1;

    public static int maxmin_used = -1;

    public static int init_maxmin = -1;

    public static int pcp_in_use[] = new int[500];

    boolean newarea = false;

    private static AppsDefaults appsDefaults = AppsDefaults.getInstance();

    private static String loc_area;

    private static String station_climo_file;

    private static String hrap_gage_file;

    private static String hrap_zgage_file;

    private static String hrap_tgage_file;

    private static String hrap_grid_mask_file;

    private static String basin_file;

    private static String hrap_file;

    private static String pcpn_file;

    public static String proc_pcpn_file;

    public static String pcpn_qc_file;

    public static String pcpn_dev_file;

    public static String pcpn_bad_file;

    private static String snow_file;

    private static String zpoint1_file;

    public static String zpoint2_file;

    private static String tpoint1_file;

    public static String tpoint2_file;

    public static String temp_bad_file;

    public static String temp_dev_file;

    private static String ngrid_file;

    public static String grid_file;

    public static String zgrid_file;

    public static String tgrid_file;

    private static String bad_snow_file;

    public static String rsel_file;

    public static String mat_file;

    public static String map_file;

    public static int prev_smonth;

    public static int prev_emonth;

    public static boolean gageqc_performed = false;

    public static int method = 2;

    public static boolean wfo_all = false;

    public static boolean render_all = true;

    public static int wfo_orig;

    public static int[] wfo_in_use = new int[20];

    public static String[] tag = new String[20];

    public static String mpe_basin_file;

    private static String mpe_dqc_warningpopup_val = "";

    public static String mpe_gridmasks;

    public static String mpe_rfc_name;

    public static String mpe_archive_dir;

    public static String mpe_gif_dir;

    public static String mpe_gif_location;

    public static int mpe_td_new_algorithm_set;

    public static int mpe_td_details_set;

    public static String mpe_point_precip_dir;

    public static String mpe_grid_precip_dir;

    public static String mpe_map_precip_dir;

    public static String mpe_dev_precip_dir;

    public static String mpe_bad_precip_dir;

    public static String mpe_point_freezing_dir;

    public static String mpe_grid_freezing_dir;

    public static String mpe_maz_freezing_dir;

    public static String mpe_point_temperature_dir;

    public static String mpe_grid_temperature_dir;

    public static String mpe_mat_temperature_dir;

    public static String mpe_bad_temperature_dir;

    public static String mpe_dev_temperature_dir;

    public static String mpe_station_list_dir;

    public static String mpe_climo_list_dir;

    public static String mpe_prism_dir;

    public static int mpe_dqc_max_precip_neighbors;

    public static int mpe_dqc_max_temp_neighbors;

    public static float mpe_dqc_precip_deviation;

    public static float mpe_dqc_temperature_deviation;

    public static int mpe_dqc_min_good_stations;

    public static boolean mpe_dqc_warningpopup_flag;

    public static int mpe_dqc_grid_max_dist;

    public static boolean mpe_copy_level2_dqc_to_ihfs_shef;

    public static boolean mpe_copy_level2_dqc_to_archive_shef;

    public static boolean mpe_dqc_save_netcdf;

    public static boolean mpe_dqc_save_grib;

    public static int dqc_ending_6hour_obstime = -1;

    public static int dqcTimeStringIndex;

    public static int load_gage_data_once = 0;

    public static String mpe_show_missing_gage;

    public static int isom = -1;

    public static int old_isom = -1;

    public static ArrayList<Station> precip_stations = new ArrayList<Station>();

    public static ArrayList<Station> temperature_stations = new ArrayList<Station>();

    public static ArrayList<Station> freezing_stations = new ArrayList<Station>();

    public static Pdata pdata[];

    public static Tdata tdata[];

    public static Zdata zdata[];

    private static Hrap_Grid hrap_grid = new Hrap_Grid();

    // public static Hrap_Grid hrap_tgrid = new Hrap_Grid();

    public static String type = "QME  ";

    public static Ts[] ts;

    public static int tsmax = 0;

    public static Maps mean_areal_precip_global[];

    public static Pcp pcp = new Pcp();

    public static Pcp spf = new Pcp();

    public static Pcp tpf = new Pcp();

    public static Bad_Daily_Values bad_values[];

    public static Bad_Daily_Values bad_tvalues[];

    public static int pid = newPid();

    public static int emonth;

    public static int smonth;

    private static int max_basins;

    public static Calendar btime = Calendar.getInstance(TimeZone
            .getTimeZone("GMT"));

    /* Define timefile file extensions. */
    public static String timefile[][] = {
            { "00_06", "06_12", "12_18", "18_00", "00_00" },
            { "06_12", "12_18", "18_00", "00_06", "06_06" },
            { "12_18", "18_00", "00_06", "06_12", "12_12" },
            { "18_00", "00_06", "06_12", "12_18", "18_18" } };

    public static String ttimefile[][] = {
            { "00", "06", "12", "18", "max", "min" },
            { "06", "12", "18", "00", "max", "min" },
            { "12", "18", "00", "06", "max", "min" },
            { "18", "00", "06", "12", "max", "min" } };

    public static String ztimefile[][] = { { "00", "06", "12", "18" },
            { "06", "12", "18", "00" }, { "12", "18", "00", "06" },
            { "18", "00", "06", "12" } };

    // Gage QC Constants

    // added by zhan for DR # 8973
    public static int MAX_TYPE_SOURCES = 8; // maximum number of type sources
                                            // allowed

    public static int DEFAULT_ENDING_6HOUR_OBS_TIME = 6; /*
                                                          * The default ending
                                                          * obs time for
                                                          * temperature and
                                                          * freezing level data.
                                                          */

    public static int MAX_GAGEQC_BASINS = 2000; /*
                                                 * The maximum number of basins
                                                 * that may be defined for an
                                                 * office.
                                                 */

    public static int MAX_GAGEQC_DAYS = 10; /*
                                             * The maximum number of days
                                             * DailyQC may be run for.
                                             */

    public static int MAX_GAGEQC_WFOS = 20; /*
                                             * The maximum number of WFOs in the
                                             * RFC area.
                                             */

    public static int NUM_COLORMAP_LEVELS = 16; /*
                                                 * The number of colors in the
                                                 * color map.
                                                 */

    public static int NUM_CONSISTENCY_LEVELS = 3; /*
                                                   * The number of levels in the
                                                   * spatial consistency check.
                                                   */

    public static int GAGEQC_AREANAME_LEN = 100; /*
                                                  * The max length of the DQC
                                                  * area name.
                                                  */

    public static int GAGEQC_FILENAME_LEN = 150; /*
                                                  * The length of a gageqc file
                                                  * path and name.
                                                  */

    public static int GAGEQC_MESSAGE_LEN = 256; /*
                                                 * The length of a gageqc
                                                 * message.
                                                 */

    public static int GAGEQC_TOPO_BUF = 100;

    public static int MAX_FREEZING_STATIONS = 500; /*
                                                    * The maximum number of
                                                    * freezing stations.
                                                    */

    public static int LOG_MESSAGE_LEN = 150; /* The max length of a log message. */

    public static int MAX_STATION_RECORD_LEN = 200; /*
                                                     * The max length of a
                                                     * record in the station
                                                     * file list record.
                                                     */

    public static int QPEMAPPER_SITE_NAME_LEN = 20; /*
                                                     * The max length of a site
                                                     * name in QPEmapper.
                                                     */

    public static int MESSAGE_LEN = 512;

    public static int HHMMSS_LEN = 8;

    public static int YYYYMMDDHH_LEN = 10;

    public static int FNAME_LEN = 128;

    public static int TOKEN_VALUE_LEN = 512;

    public static int MAX_TOKEN_SIZE = 250;

    public static int PEDTSEP_LEN = 8;

    public static int MAX_GAGEQC_TYPE = 9;

    public static int DEFAULT_MAX_PRECIP_NEIGHBORS = 30;

    public static int DEFAULT_MAX_TEMP_NEIGHBORS = 20;

    public static double DEFAULT_PRECIP_DEVIATION = 3.0;

    public static double DEFAULT_TEMPERATURE_DEVIATION = 10.0;

    public static int DEFAULT_MIN_GOOD_STATIONS = 5;

    public static int DEFAULT_DQC_COPY_TO_IHFS = 0;

    public static int DEFAULT_DQC_COPY_TO_ARCHIVE = 0;

    public static int DAILYQC_FAILED = 0;

    public static int DAILYQC_OK = 0;

    public static boolean master_file = false;

    public static int new_area_flag = 0;

    public static int func[] = { 8, 0, 3, 1, 2 };

    public static int hrgt12z = -1;

    public static int[] dflag = new int[10];

    public static int[] qflag = new int[10];

    public static float pxtemp = 1.0f;

    public static int dmvalue = (int) (1.0 * 100 * 3.28 / .55);

    public static int elevation_filter_value = 0;

    public static int temperature_filter_value = -50;

    public static int temperature_reverse_filter_value = 110;

    public static float freezing_filter_value = 0.00f;

    public static float freezing_reverse_filter_value = 20.00f;

    public static final String dqc_ending_6hour_obstime_tok = "dqc_ending_6hour_obstime";

    public static int curHr12_18 = -1;

    public static int curHr18_00 = -1;

    public static int curHr00_06 = -1;

    public static int curHr06_12 = -1;

    /* Function which associates the Gage QC edit levels with a value. */
    public static int funct[] = { 8, 0, 6, 2, 3, 4, 5, 1, 7, 9 };

    public static int gage_char[] = new int[2];

    public static int plot_view = 0;

    public static boolean frzlvl_flag = true;

    public static int find_station_flag = -1;

    public static int pcpn_time = 0;

    public static int pcp_flag = -1;

    public static int pcpn_day = 0;

    public static int contour_flag = -1;

    public static int points_flag = 1;

    public static int grids_flag = -1;

    public static int map_flag = -1;

    static int curHrMinSec = -1;

    int sixHrInSec = 6 * 3600;

    int twelveHrInSec = 12 * 3600;

    int eighteenHrInSec = 18 * 3600;

    static int hydro_curDay;

    private int begin_day;

    private boolean post_analysis_calc;

    private boolean auto_dailyqc_flag;

    public static String mpe_td_details_file;

    public static String mpe_editor_logs_dir;

    public static File td_fp;

    public static BufferedWriter td_fpwr;

    public static Topo topo;

    // public static BufferedWriter td_fpwr;

    public DailyQcUtils() {
        // empty constructor
    }

    public static class Dval {

        double a;

        double xo;

        double yo;

        double lo;

    }

    public static class Ts {
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

        float gzc[];

        float uzc[];

        float mzc[];

        float lzc[];

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
        float x, y;
    }

    public static class Hrap_Grid {

        public int maxi;

        public int maxj;

        public int hrap_minx;

        public int hrap_miny;

        Coord coord[][];

        Gagem gage[][];

        public int owner[][];

        int isoh[][][];

        int max[][][];

        int min[][][];

        public int elev[][];

    }

    public static class Coord {

        int x;

        int y;

        float lat;

        float lon;

        public Coord() {

        }

        /**
         * 
         * @param ix
         * @param iy
         * @param ilat
         * @param ilon
         */
        public Coord(int ix, int iy, float ilat, float ilon) {
            x = ix;
            y = iy;
            lat = ilat;
            lon = ilon;
        }
    }

    public static class Gagem {

        int index[] = new int[30];

        int zindex[] = new int[30];

        int tindex[] = new int[30];
    }

    public class Tdata {

        public Date data_time;

        String ztime;

        public int used[] = new int[6];

        public float stddev;

        public int level[] = new int[6];

        public Ttn tstn[];
    }

    public class Pdata {

        public Date data_time;

        String ztime;

        public int used[] = new int[5];

        public float stddev;

        public int level;

        public Stn[] stn;
    }

    public class Zdata {

        public Date data_time;

        String ztime;

        public int used[] = new int[5];

        float stddev;

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

        float stddev;

        float a;
    }

    public class Zlevel {

        public float data;

        public short qual;

        float estimate;

        float stddev;
    }

    public class Rain {

        public float data;

        public short qual;

        public float estimate;

        public float stddev;
    }

    public class Bad_Daily_Values {

        int used;

        String hb5;

        String parm;

        int day;

        int quart;

        float fvalue;
    }

    public class Station {
        public float isoh[];

        public float max[];

        public float min[];

        public String hb5;

        public String name;

        public String parm;

        public String cparm;

        char rawlevel[];

        short index[];

        short zindex[];

        public float lat;

        public float lon;

        public int elev;

        public int tip;

        int qpf;

        int x;

        int y;

        public int xadd;

        public int yadd;

        public float hrap_x;

        public float hrap_y;
    }

    public static int newPid() {
        Random r = new Random();

        // Let's create a hand-made pid for default
        pid = r.nextInt() & 0x7fffffff;
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
        Cursor prev = shell.getCursor();
        Cursor wait = new Cursor(Display.getDefault(), SWT.CURSOR_WAIT);
        shell.setCursor(wait);
        new_area_flag = 1;
        int retval = loadDataSet();
        lastQcArea = currentQcArea;
        shell.setCursor(prev);
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
        selDate = prevDate;
        auto_dailyqc_flag = autoqc;
        qcDays = MPEDataManager.getInstance().getDQCDays();
        if (lastQcArea == "") {
            newarea = true;
            lastQcArea = currentQcArea;
        }
        curHrMinSec = getCurrentHrMinSec(currentDate);
        if (curHrMinSec != -1) {
            if (curHrMinSec >= 0 && curHrMinSec < sixHrInSec) {
                curHr00_06 = 1;
            } else if (curHrMinSec >= sixHrInSec && curHrMinSec < twelveHrInSec) {
                curHr06_12 = 1;
            } else if (curHrMinSec >= twelveHrInSec
                    && curHrMinSec < eighteenHrInSec) {
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
                || newarea == true) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            Cursor prev = shell.getCursor();
            Cursor wait = new Cursor(Display.getDefault(), SWT.CURSOR_WAIT);
            shell.setCursor(wait);
            int retval = loadDataSet();
            if (retval == 2) {
                load_gage_data_once = 1;
            }
            lastQcArea = currentQcArea;
            shell.setCursor(prev);
            return retval;
        } else {
            new_area_flag = 1;
            lastQcArea = currentQcArea;
            return new_area_flag;
        }
    }

    public static int getCurrentHrMinSec(Date currentDate) {

        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTime(currentDate);
        int curYear = cal.get(Calendar.YEAR);
        int curMonth = 1 + cal.get(Calendar.MONTH);
        int curDay = cal.get(Calendar.DAY_OF_MONTH);
        int curHour = cal.get(Calendar.HOUR_OF_DAY);
        int curMin = cal.get(Calendar.MINUTE);
        int curSec = cal.get(Calendar.SECOND);

        /* decide the current time's hydrologic day */
        cal.add(Calendar.HOUR_OF_DAY, 6);

        int hydro_curYear = cal.get(Calendar.YEAR);
        int hydro_curMonth = 1 + cal.get(Calendar.MONTH);
        hydro_curDay = cal.get(Calendar.DAY_OF_MONTH);

        if (hydro_curYear == curYear && hydro_curMonth == curMonth
                && hydro_curDay == curDay) {
            curHrMinSec = curHour * 3600 + curMin * 60 + curSec;
        }

        return curHrMinSec;
    }

    @SuppressWarnings("unused")
    private int loadDataSet() {

        hrgt12z = -1;
        int num;
        int kk, mk;
        // boolean newarea = false;
        bad_values = new Bad_Daily_Values[6000];
        bad_tvalues = new Bad_Daily_Values[6000];
        for (int kd = 0; kd < 6000; kd++) {
            bad_values[kd] = new Bad_Daily_Values();
            bad_tvalues[kd] = new Bad_Daily_Values();
        }
        String dbuf = "";
        ReadQPFGrids rqg = new ReadQPFGrids();
        WriteQPFGrids wqg = new WriteQPFGrids();

        if (pdata == null || pdata.length < 1) {
            new_area_flag = 1;
        }
        // newarea = (!currentQcArea.equals(lastQcArea));
        if (newarea == true || new_area_flag == 1) {
            pdata = new Pdata[10];
            tdata = new Tdata[10];
            zdata = new Zdata[10];
            ts = new Ts[20];
            mean_areal_precip_global = new Maps[2000];
            init_maxmin = -1;
        }

        btime.setTime(selDate);
        begin_day = btime.get(Calendar.DAY_OF_MONTH);

        if (begin_day == hydro_curDay && curHr12_18 == 1) {
            btime.set(Calendar.HOUR_OF_DAY, 12);
            btime.set(Calendar.MINUTE, 0);
            btime.set(Calendar.SECOND, 0);
        } else if (begin_day == hydro_curDay
                && (curHr00_06 == 1 || curHr06_12 == 1 || curHr18_00 == 1)) {
            btime.set(Calendar.HOUR_OF_DAY, 12);
            btime.set(Calendar.MINUTE, 0);
            btime.set(Calendar.SECOND, 0);
            hrgt12z = 1;
        } else {
            btime.set(Calendar.HOUR_OF_DAY, 12);
            btime.set(Calendar.MINUTE, 0);
            btime.set(Calendar.SECOND, 0);
            hrgt12z = 0;
        }
        /* In order to allow user access the 12~18Z point data for the most recent day,
        advance one more day from current day if the later than 18Z */
        Calendar currentTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        if (currentTime.get(Calendar.HOUR_OF_DAY)>=18) {
        	btime.add(Calendar.DAY_OF_MONTH, 1);
        }

        emonth = btime.get(Calendar.MONTH);
        Calendar otime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        otime.setTime(btime.getTime());
        Calendar stime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        long dayoffset = (24L * 3600L * qcDays);
        otime.add(Calendar.SECOND, -(int) dayoffset);
        smonth = otime.get(Calendar.MONTH);
        isom = emonth;
        old_isom = emonth;
        System.out.println("Current qc time is " + btime.getTime());
        if (newarea == false && new_area_flag == 1) {
            newarea = true;
        }

        final String mpe_basin_file_tok = "mpe_basin_file";
        final String mpe_gridmasks_tok = "mpe_gridmask_dir";
        final String mpe_rfc_name_tok = "mpe_site_id";
        final String mpe_archive_dir_tok = "mpe_archive_dir";
        final String mpe_gif_dir_tok = "mpe_gif_dir";
        final String mpe_gif_location_tok = "mpe_gif_location";

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
        final String mpe_copy_level2_dqc_to_ihfs_shef_tok = "mpe_copy_level2_dqc_to_ihfs_shef";
        final String mpe_copy_level2_dqc_to_archive_shef_tok = "mpe_copy_level2_dqc_to_archive_shef";
        final String mpe_show_missing_gage_tok = "mpe_show_missing_gage";
        final String mpe_dqc_warningpopup_tok = "mpe_dqc_warningpopup";
        final String mpe_dqc_save_netcdf_tok = "mpe_dqc_save_netcdf";
        final String mpe_dqc_save_grib_tok = "mpe_dqc_save_grib";

        if (firstTok == 1) {
            // read tokens
            mpe_basin_file = appsDefaults.getToken(mpe_basin_file_tok);
            mpe_gridmasks = appsDefaults.getToken(mpe_gridmasks_tok);
            mpe_rfc_name = appsDefaults.getToken(mpe_rfc_name_tok);
            mpe_archive_dir = appsDefaults.getToken(mpe_archive_dir_tok);
            mpe_gif_dir = appsDefaults.getToken(mpe_gif_dir_tok);
            mpe_gif_location = appsDefaults.getToken(mpe_gif_location_tok);
            mpe_td_new_algorithm_set = 0;
            String mpe_td_details_set_tok_val = appsDefaults
                    .getToken("mpe_td_details_set");
            if (mpe_td_details_set_tok_val != null
                    && mpe_td_details_set_tok_val.length() > 0) {
                if (mpe_td_details_set_tok_val.equalsIgnoreCase("on")) {
                    mpe_td_details_set = 1;
                } else {
                    mpe_td_details_set = 0;
                }
            } else {
                mpe_td_details_set = 0;
            }
            String mpe_td_new_algorithm_tok_val = appsDefaults
                    .getToken("mpe_td_new_algorithm");
            if (mpe_td_new_algorithm_tok_val != null
                    && mpe_td_new_algorithm_tok_val.length() > 0) {
                if (mpe_td_new_algorithm_tok_val.equalsIgnoreCase("on")) {
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
            mpe_dqc_max_precip_neighbors = Integer.parseInt(appsDefaults
                    .getToken(mpe_dqc_max_precip_neighbors_tok));
            mpe_dqc_max_temp_neighbors = Integer.parseInt(appsDefaults
                    .getToken(mpe_dqc_max_temp_neighbors_tok));
            mpe_dqc_precip_deviation = Float.parseFloat(appsDefaults
                    .getToken(mpe_dqc_precip_deviation_tok));
            mpe_dqc_temperature_deviation = Float.parseFloat(appsDefaults
                    .getToken(mpe_dqc_temperature_deviation_tok));
            mpe_dqc_min_good_stations = Integer.parseInt(appsDefaults
                    .getToken(mpe_dqc_min_good_stations_tok));
            mpe_copy_level2_dqc_to_ihfs_shef = ((appsDefaults
                    .getToken(mpe_copy_level2_dqc_to_ihfs_shef_tok)
                    .equalsIgnoreCase("ON"))) ? true : false;
            mpe_copy_level2_dqc_to_archive_shef = ((appsDefaults
                    .getToken(mpe_copy_level2_dqc_to_archive_shef_tok)
                    .equalsIgnoreCase("ON"))) ? true : false;
            String save_netcdf = appsDefaults.getToken(mpe_dqc_save_netcdf_tok);
            if (save_netcdf != null) {
                mpe_dqc_save_netcdf = save_netcdf.equalsIgnoreCase("ON") ? true
                        : false;
            } else {
                mpe_dqc_save_netcdf = false;
            }
            String save_grib = appsDefaults.getToken(mpe_dqc_save_grib_tok);
            if (save_grib != null) {
                mpe_dqc_save_grib = save_grib.equalsIgnoreCase("ON") ? true
                        : false;
            } else {
                mpe_dqc_save_grib = false;
            }
            dqc_ending_6hour_obstime = getEnding6HourObsTime();
            String gridMax = appsDefaults.getToken("mpe_dqc_grid_max_dist");
            if (gridMax != null && gridMax.length() > 0) {
                mpe_dqc_grid_max_dist = Integer.parseInt(gridMax);
            }
            String mpe_post_analysis_tok_val = appsDefaults
                    .getToken("mpe_post_analysis");
            if (mpe_post_analysis_tok_val != null
                    && mpe_post_analysis_tok_val.length() > 0) {
                if (mpe_post_analysis_tok_val.equalsIgnoreCase("on")) {
                    post_analysis_calc = true;
                } else {
                    post_analysis_calc = false;
                }
            } else {
                post_analysis_calc = false;
            }
            dqcTimeStringIndex = (dqc_ending_6hour_obstime / 6) + 1;
            mpe_dqc_warningpopup_val = ((appsDefaults
                    .getToken(mpe_dqc_warningpopup_tok)));
            if (mpe_dqc_warningpopup_val != null
                    && mpe_dqc_warningpopup_val.length() > 0) {
                mpe_dqc_warningpopup_flag = ((mpe_dqc_warningpopup_val)
                        .equalsIgnoreCase("on")) ? true : false;
            } else {
                mpe_dqc_warningpopup_flag = true;
            }
            // set Token flag to not first time
            // firstTok = 0;
        }

        if (firstTok == 1) {
            station_climo_file = mpe_climo_list_dir + "/" + mpe_rfc_name
                    + "_station_climo_list";
            String station_list_file = mpe_station_list_dir + "/"
                    + currentQcArea + "_station_list";
            hrap_gage_file = mpe_gridmasks + "/precip_neighbor_list_"
                    + currentQcArea;
            hrap_zgage_file = mpe_gridmasks + "/freezing_neighbor_list_"
                    + currentQcArea;
            hrap_tgage_file = mpe_gridmasks + "/temperature_neighbor_list_"
                    + currentQcArea;
            basin_file = mpe_basin_file;
            hrap_file = mpe_gridmasks + "/basin_to_grid_" + mpe_rfc_name;
            hrap_grid_mask_file = mpe_gridmasks + "/hsa_to_grid_mask_"
                    + mpe_rfc_name;
            String gif_file = mpe_gif_dir + "/gifs";

            // Create the filenames for the precipitation, freezing level and
            // and temperature data.
            pcpn_file = mpe_point_precip_dir + "/precip_1_" + currentQcArea
                    + "_point_";
            proc_pcpn_file = mpe_point_precip_dir + "/precip_2_"
                    + currentQcArea + "_point_";
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
            loc_area = appsDefaults.getToken("mpe_site_id").trim();
            mpe_editor_logs_dir = appsDefaults.getToken("mpe_editor_logs_dir");
            /*
             * create mpe_td_details_file to store the details of Time
             * Distributed estimates if the token mpe_td_details_set is set as
             * ON or on
             */

            if (mpe_td_details_set == 1) {
                mpe_td_details_file = String.format("%s/mpe_td_details_file",
                        mpe_editor_logs_dir);

                td_fp = new File(mpe_td_details_file);
                td_fp.setReadable(true, false);
                td_fp.setWritable(true, false);
                if (td_fp == null) {
                    statusHandler
                            .handle(Priority.PROBLEM,
									"Could not open mpe_td_details_file in load_gage_data().");
                    return 0;
                }
                try {
                    td_fpwr = new BufferedWriter(new FileWriter(td_fp, true));
                } catch (IOException e) {
                    statusHandler
                            .handle(Priority.PROBLEM,
									"Could not open mpe_td_details_file for writing, in load_gage_data().");
                }
            }

        }
        System.out.println("DQC: DailyQcUtils finished loading tokens. ");
        if (newarea == true || new_area_flag == 1) {
            String type_sources = appsDefaults.getToken(mpe_type_source_tok);
            if (type_sources.length() <= 0) {
                System.out.println("mpe_type_source_token token not defined.");
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
                    System.out.println(String.format(
                            "Could not parse any typesources from %s token.",
                            mpe_type_source_tok));
                }
                // added by zhan for DR # 8973
                else if (tsmax > MAX_TYPE_SOURCES) {
                    System.out
                            .println("Warning !!! Number of typesources read from mpe_type_source token exceeds MAX_TYPE_SOURCES !!!");

                    tsmax = MAX_TYPE_SOURCES;

                    // Exception e = new Exception();
                    // throw (e);
                }
            }

        }

        if (currentQcArea.equals(loc_area)) {
            master_file = true;
        }

        ReadFreezingStationList zl = new ReadFreezingStationList();
        ReadPrecipStationList pl = new ReadPrecipStationList();
        ReadTemperatureStationList tl = new ReadTemperatureStationList();

        if (newarea == true) {
            System.out.println("DQC: Reading Freezing Stations List. ");
            freezing_stations = zl.read_freezing_station_list(currentQcArea,
                    master_file);
            if (freezing_stations == null) {
                statusHandler
                        .handle(Priority.PROBLEM,
								"ERROR, Could not read freezing level station list file. DailyQC stopped.");
                return DAILYQC_FAILED;
            }
            System.out.println("DQC: Reading Temperature Stations List. ");
            temperature_stations = tl.read_temperature_station_list(
                    currentQcArea, master_file);
            if (temperature_stations == null) {
                statusHandler
                        .handle(Priority.PROBLEM,
								"ERROR, Could not read temperature station list file. DailyQC stopped.");
                return DAILYQC_FAILED;
            }
            System.out.println("DQC: Reading Precip Stations List. ");
            precip_stations = pl.read_precip_station_list(currentQcArea,
                    master_file);
            if (precip_stations == null) {
                statusHandler
                        .handle(Priority.PROBLEM,
								"ERROR, Could not read precip station list file. DailyQC stopped.");
                return DAILYQC_FAILED;
            }

            InitPrecipClimo ipc = new InitPrecipClimo();
            boolean status = ipc.initPrecip_climo(station_climo_file,
                    precip_stations, pl.getNumPstations());
            if (status == false) {
                statusHandler
                        .handle(Priority.PROBLEM,
								"Could not read precipitation Climo file. DailyQC stopped.");
                return DAILYQC_FAILED;
            }

            InitTempClimo itc = new InitTempClimo();
            status = itc.initTemp_climo(station_climo_file,
                    temperature_stations, tl.getNumTstations());
            if (status == false) {
                statusHandler
                        .handle(Priority.PROBLEM,
								"Could not read temperature Climo file. DailyQC stopped.");
                return DAILYQC_FAILED;
            }
        }

        boolean status = false;
        MeanMonthlyPrecip mmp = new MeanMonthlyPrecip();
        status = mmp.read_mean_monthly_precip(mpe_prism_dir, mpe_rfc_name,
                smonth, emonth);
        if (status == false) {
            statusHandler
                    .handle(Priority.PROBLEM,
							"Could not read precipitation PRISM file. DailyQC stopped.");
            return DAILYQC_FAILED;
        }

        MeanMonthlyTemp mmt = new MeanMonthlyTemp();
        status = mmt.read_mean_monthly_temp(mpe_prism_dir, mpe_rfc_name,
                smonth, emonth);
        if (status == false) {
            statusHandler.handle(Priority.PROBLEM,
					"Could not read temperature PRISM file. DailyQC stopped.");
            return DAILYQC_FAILED;
        }
        System.out.println("DQC: Finished loading Climo data. ");

        // Create the predefined gage, freezing level and temperature
        // HRAP grids which will later be used when rendering grids.
        MapPrecipGagesGrid mpg = new MapPrecipGagesGrid();
        System.out.println("DQC: Mapping precip gages to grid. ");
        long start = System.currentTimeMillis();
        System.out.println("Starting to map gages to grid.");
        hrap_grid = mpg.map_precip_gages_to_grid(smonth, emonth,
                hrap_gage_file, currentQcArea, precip_stations,
                pl.getNumPstations());
        if (hrap_grid == null) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Could not map precipitation gages to grid. DailyQC stopped.\n");
            return DAILYQC_FAILED;
        }
        System.out.println("DQC: Mapping freezing gages to grid. ");
        MapFreezingGagesGrid mfg = new MapFreezingGagesGrid();
        status = mfg.map_freeze_gages_to_grid(smonth, emonth, hrap_grid,
                hrap_zgage_file, currentQcArea, freezing_stations,
                precip_stations, zl.getNumZstations(), pl.getNumPstations());
        if (status == false) {
            statusHandler
                    .handle(Priority.PROBLEM,
							"Could not map freezing level points to the HRAP grid. DailyQC stopped.");
            return DAILYQC_FAILED;
        }
        System.out.println("DQC: Mapping temperature gages to grid. ");
        MapTempGagesGrid mtg = new MapTempGagesGrid();
        status = mtg.map_temp_gages_to_grid(smonth, emonth, hrap_grid,
                hrap_tgage_file, currentQcArea, temperature_stations,
                tl.getNumTstations());
        if (status == false) {
            statusHandler
                    .handle(Priority.PROBLEM,
							"Could not map temp level points to the HRAP grid. DailyQC stopped.");
            return DAILYQC_FAILED;
        }
        long elapsed = System.currentTimeMillis() - start;
        System.out.println("Finished mapping gages to grid, elapsed time: "
                + elapsed + " ms");
        if (newarea == true) {
            // Create the HSA mask. This maps each HRAP grid bin to an HSA.
            // If no file is supplied, then the default grid_mask is set to all
            // 1's.
            GetHrapMask ghm = new GetHrapMask();
            start = System.currentTimeMillis();
            System.out.println("Starting HSA to Grid mask");
            ghm.get_hsa_to_grid_mask(hrap_grid, tag, wfo_all,
                    hrap_grid_mask_file);
            System.out.println("Finished HSA to Grid mask, elapsed time: "
                    + (System.currentTimeMillis() - start) + " ms");
            /* Was the precipitation climatology available? */
            if (isohyets_used == 0) {
                method = 1;
            }

            GetBasinData gbd = new GetBasinData();
            status = gbd.get_basin_data(basin_file, hrap_file,
                    mean_areal_precip_global, tag);
            if (status == false) {
                statusHandler.handle(Priority.PROBLEM,
						"Error retrieving basin data.  DailyQC Stopped.");
                return DAILYQC_FAILED;
            }
        }
        // Allocate memory for the precip, freezing level and
        // temperature data in the pdata array.
        for (int zz = 0; zz < pdata.length; zz++) {
            pdata[zz] = new Pdata();
        }
        for (int l = 0; l < MAX_GAGEQC_DAYS; ++l) {
            pdata[l].stn = new Stn[pl.getNumPstations()];

            for (int m = 0; m < pl.getNumPstations(); ++m) {
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
        for (int l = 0; l < MAX_GAGEQC_DAYS; ++l) {
            tdata[l].tstn = new Ttn[tl.getNumTstations()];

            for (int m = 0; m < tl.getNumTstations(); ++m) {
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
        for (int l = 0; l < MAX_GAGEQC_DAYS; l++) {
            zdata[l].zstn = new Ztn[zl.getNumZstations()];

            for (int m = 0; m < zl.getNumZstations(); m++) {
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
            rs.read_snow(prece, precip_stations, pl.getNumPstations(), m);

            // read level 1 precip data
            ReadPrecipA ra = new ReadPrecipA();
            int mer = ra.read_precip_a(preca, otime.getTime(), m, pdata,
                    precip_stations, pl.getNumPstations());

            ReadZlevelA rza = new ReadZlevelA();
            // Read the level 1 freezing level data.
            rza.read_zlevel_a(zpointa, otime.getTime(), m, zdata,
                    freezing_stations, zl.getNumZstations());

            ReadTempA rta = new ReadTempA();
            // Read the level 1 temp level data.
            rta.read_t_a(tpointa, otime.getTime(), m, tdata,
                    temperature_stations, tl.getNumTstations());

            // Read the list of bad precipitation values.
            BadValues bv = new BadValues();
            bv.read_bad_values(precd, m);

            BadTValues bt = new BadTValues();
            bt.read_bad_tvalues(tpointd, m);
            int ier = 0;
            int zer = 0;
            int ter = 0;

            if (auto_dailyqc_flag == true) {
                ReadPrecipB rb = new ReadPrecipB();
                // read level 2 precip data for auto_dailyqc
                ier = rb.read_precip_b(precb, otime.getTime(), m, pdata,
                        precip_stations, pl.getNumPstations());

                ReadZlevelB rzb = new ReadZlevelB();
                zer = rzb.read_zlevel_b(zpointb, otime.getTime(), m, zdata,
                        freezing_stations, zl.getNumZstations());

                ReadTempB rtb = new ReadTempB();
                ter = rtb.read_t_b(tpointb, otime.getTime(), m, tdata,
                        temperature_stations, tl.getNumTstations());
                continue;
            } else {
                ReadPrecipB rb = new ReadPrecipB();
                // read level 2 precip data
                ier = rb.read_precip_b(precb, otime.getTime(), m, pdata,
                        precip_stations, pl.getNumPstations());

                ReadZlevelB rzb = new ReadZlevelB();
                zer = rzb.read_zlevel_b(zpointb, otime.getTime(), m, zdata,
                        freezing_stations, zl.getNumZstations());

                ReadTempB rtb = new ReadTempB();
                ter = rtb.read_t_b(tpointb, otime.getTime(), m, tdata,
                        temperature_stations, tl.getNumTstations());
            }

            // --------------------------------------------------------------
            // open and read file containing std dev of point precip data
            // --------------------------------------------------------------
            BufferedReader in = null;

            try {
                in = new BufferedReader(new FileReader(precc));
                String ibuf = "";
                ibuf = in.readLine();
                Scanner s = new Scanner(ibuf);

                pdata[m].stddev = s.nextFloat();

                if (pdata[m].stddev != 1.0 && pdata[m].stddev != 3.0
                        && pdata[m].stddev != 5.0) {

                    pdata[m].stddev = 3.0f;
                }
                in.close();
            } catch (FileNotFoundException e) {
                System.out.println("File not found " + precc);
            } catch (IOException e) {
                e.printStackTrace();
            } finally {
                try {
                    if (in != null) {
                        in.close();
                    }
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

            // open and read file containing std dev of point temperature
            // data
            try {
                in = new BufferedReader(new FileReader(tpointc));
                String ibuf = "";
                ibuf = in.readLine();
                Scanner s = new Scanner(ibuf);

                tdata[m].stddev = s.nextFloat();

                if (tdata[m].stddev != 5.0 && tdata[m].stddev != 10.0
                        && tdata[m].stddev != 15.0) {
                    tdata[m].stddev = 10.0f;
                }

                in.close();
            } catch (FileNotFoundException e) {
                System.out.println("File not found " + tpointc);
            } catch (IOException e) {
                e.printStackTrace();
            } finally {
                try {
                    if (in != null) {
                        in.close();
                    }
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
            if (ier == 1) {
                dbuf = String.format("%s - level 2 data\n", databuf);
            } else if (ier == -1 && mer == 1) {
                dbuf = String.format("%s - level 1 data\n", databuf);
            } else if (ier == -2 && mer == 1) {
                dbuf = String.format("%s - level 1 data overwrite\n", databuf);
            } else {
                dbuf = String.format("%s - no data\n", databuf);
            }
            System.out.println(dbuf);

            // Estimate the daily precipitation stations.
            /*
             * do not estimate daily and partial point precipitation from each
             * other if run DQC on partial time frame and m=0
             */

            if (m == 0
                    && (curHr00_06 == 1 || curHr06_12 == 1 || curHr18_00 == 1)) {
            } else {
                EstDailyStations ed = new EstDailyStations();
                ed.estimate_daily_stations(m, precip_stations,
                        pl.getNumPstations());

                EstPartStations ep = new EstPartStations();
                ep.estimate_partial_stations(m, precip_stations,
                        pl.getNumPstations());
            }

            // Quality control the stations.
            QCStations qcs = new QCStations();
            qcs.quality_control_stations(m, precip_stations,
                    pl.getNumPstations());
            CheckConsistency cc = new CheckConsistency();
            cc.check_consistency(m, precip_stations, pl.getNumPstations());
            bv.restore_bad_values(m, precip_stations, pl.getNumPstations());

            // Estimate and QC the daily temperature stations.
            EstDailyTStations et = new EstDailyTStations();
            et.estimate_daily_tstations(m, temperature_stations,
                    tl.getNumTstations());
            QCTStations qct = new QCTStations();
            qct.quality_control_tstations(m, temperature_stations,
                    tl.getNumTstations());
            bt.restore_bad_tvalues(m, temperature_stations,
                    tl.getNumTstations());

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
                                timefile[2][k], otime.get(Calendar.YEAR),
                                otime.get(Calendar.MONTH) + 1,
                                otime.get(Calendar.DAY_OF_MONTH));

                        if (k < 4) {
                            num = 50 + m * 4 + kk;
                        } else {
                            num = 90 + m;
                        }

                        mer = rqg.read_qpf_grids(num, dbuf);
                    }

                    dbuf = String.format("%s%s_%04d%02d%02d", grid_file,
                            timefile[2][k], otime.get(Calendar.YEAR),
                            otime.get(Calendar.MONTH) + 1,
                            otime.get(Calendar.DAY_OF_MONTH));

                    if (k < 4) {
                        num = m * 4 + kk;
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
                            rpc.render_pcp(m, k, mk, pl.getNumPstations(),
                                    precip_stations, hrap_grid, pdata,
                                    pcp_in_use);
                            wqg.write_qpf_grids(dbuf);

                            // TODO
                            /* output grid to file in netCDF format */
                            // if( mpe_dqc_save_netcdf == 1)
                            // {
                            // if (k < 4)
                            // begin_time = pdata[m].data_time - (4 - k) *
                            // 21600L;
                            // else
                            // begin_time = pdata[m].data_time - 86400L;
                            //
                            // sprintf(dbufnc,"%s.nc",dbuf);
                            // write_dqc_netcdf_grids(dbufnc,
                            // pdata[m].data_time, &begin_time, 1);
                            // }
                            // /* output grid to file in GRIB1 format */
                            // if( mpe_dqc_save_grib == 1)
                            // {
                            // sprintf(dbufgb,"%s.grb",dbuf);
                            // status=write_dqc_grib_grids(dbufgb, k, 1, m);
                            // if(status!=0)
                            // logMessage("\n load_gage_data: problem with writing GRIB file in write_dqc_grib_grids. status=%d\n",status);
                            //
                            // }
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
                            stime.add(Calendar.SECOND, -84600);
                        } else {
                            stime.setTime(otime.getTime());
                        }
                    } else {
                        if (k < 2) {
                            stime.setTime(otime.getTime());
                            stime.add(Calendar.SECOND, -84600);
                        } else {
                            stime.setTime(otime.getTime());
                        }
                    }
                    kk = 3 - k;

                    dbuf = String.format("%s%s_%04d%02d%02d", zgrid_file,
                            ztimefile[dqcTimeStringIndex][k],
                            otime.get(Calendar.YEAR),
                            otime.get(Calendar.MONTH + 1),
                            otime.get(Calendar.DAY_OF_MONTH));

                    num = 100 + m * 4 + kk;

                    mer = rqg.read_qpf_grids(num, dbuf);

                    if (mer == -1) {
                        RenderZ rz = new RenderZ();
                        rz.render_z(m, k, 0, zl.getNumZstations(),
                                freezing_stations, hrap_grid, zdata, pcp_in_use);
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
                            stime.add(Calendar.SECOND, -84600);
                        } else {
                            stime.setTime(otime.getTime());
                        }
                    } else {
                        if (k < 2) {
                            stime.setTime(otime.getTime());
                            stime.add(Calendar.SECOND, -84600);
                        } else {
                            stime.setTime(otime.getTime());
                        }
                    }
                    kk = 3 - k;

                    dbuf = String.format("%s%s_%04d%02d%02d", tgrid_file,
                            ttimefile[dqcTimeStringIndex][k],
                            otime.get(Calendar.YEAR),
                            otime.get(Calendar.MONTH + 1),
                            otime.get(Calendar.DAY_OF_MONTH));

                    if (k < 4) {
                        num = 150 + m * 4 + kk;
                        mer = rqg.read_qpf_grids(num, dbuf);

                        // hrap_tgrid = hrap_grid;
                        if (mer == -1) {
                            RenderT6 rt6 = new RenderT6();
                            rt6.render_t6(m, k, 0, tl.getNumTstations(),
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
                            rt.render_t(m, k, 1, tl.getNumTstations(),
                                    temperature_stations, hrap_grid, tdata,
                                    pcp_in_use);
                        }

                    } else if (k == 5) {
                        num = 200 + m;
                        mer = rqg.read_qpf_grids(num, dbuf);

                        if (mer == -1) {
                            RenderT rt = new RenderT();
                            rt.render_t(m, k, 2, tl.getNumTstations(),
                                    temperature_stations, hrap_grid, tdata,
                                    pcp_in_use);
                        }
                    }

                }
            }
        }// end for loop.
        if (auto_dailyqc_flag != true) {
            GetBadSnotel gbs = new GetBadSnotel();
            gbs.get_bad_snotel(bad_snow_file, precip_stations);
            CalculateZLevel cz = new CalculateZLevel();
            cz.calculate_zlevel(zl.getNumZstations(), zdata);

            GetZLevel gl = new GetZLevel();
            for (int m = 0; m < qcDays; ++m) {
                gl.get_zlevel(m, precip_stations, freezing_stations,
                        pl.getNumPstations(), zl.getNumZstations());
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
        if (post_analysis_calc == true) {
            GenerateAccumGrids gam = new GenerateAccumGrids();
            gam.generate_accum_grids(qcDays);
        }

        return 2;
    }

    public static Coordinate getHraptoLatLon(Coordinate hrap_coord) {
        /* calculate HRAP coordinates to lat,lon */
        Coordinate latlon_point = new Coordinate(0, 0);

        try {
            latlon_point = com.raytheon.uf.common.hydro.spatial.HRAP
                    .getInstance().gridCoordinateToLatLon(hrap_coord,
                            PixelOrientation.LOWER_LEFT);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "error computing hrap coordinate", e);
        }
        return latlon_point;
    }

    public static Coordinate getLatLontoHrap(Coordinate latlon_coord) {
        /* calculate HRAP coordinates to lat,lon */
        Coordinate hrap_point = new Coordinate(0, 0);
        try {
            hrap_point = com.raytheon.uf.common.hydro.spatial.HRAP
                    .getInstance().latLonToGridCoordinate(latlon_coord,
                            PixelOrientation.LOWER_LEFT);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "error computing hrap coordinate", e);
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
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

        bad_values = null;
        bad_tvalues = null;
        // hrap_grid = null;
        // hrap_tgrid = null;
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
    public static void setHrap_grid(Hrap_Grid hrap_grid) {
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
    public static int getMax_basins() {
        return max_basins;
    }

    /**
     * @param max_basins
     *            the max_basins to set
     */
    public static void setMax_basins(int max_basins) {
        DailyQcUtils.max_basins = max_basins;
    }
}
