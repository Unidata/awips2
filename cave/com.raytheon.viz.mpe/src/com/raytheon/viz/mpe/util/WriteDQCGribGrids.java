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

import java.awt.Rectangle;
import java.io.File;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.hydro.spatial.HRAP;
import com.raytheon.uf.common.hydro.spatial.HRAPSubGrid;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pcp;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pdata;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Creates GRIB 1 file from netCDF QPE files used for 6/24 hour MPE/DQC
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 9, 2011            snaples     Initial creation
 * Mar 5, 2013  15884      wkwock      gridPointLL and gridPointUR should be integer
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class WriteDQCGribGrids {

    AppsDefaults apps_defaults = AppsDefaults.getInstance();

    CommonGridAttributes ga = GridAttributes.commonGridAttributes;

    Pdata[] pdata = DailyQcUtils.pdata;

    private double dxdy;

    static final double SECINHR = 3600.;

    static final int PATH_LEN = 256;

    static final int FILE_LEN = 128;

    static final int BUFFSIZE = 1024;

    static final int CMDSIZE = 1000;

    static final int COPYSIZE = 4200000;

    static final int MALERR = -2;

    static final int UNERR = -3;

    static final int INPUTERR = -4;

    static final int FILEERR = -5;

    static final int SUBERR = -6;

    static final int OPENERR = -7;

    static final int ZEROCHECK = -8;

    static final int FILEOPERR = -9; /* this is a file operations error */

    static final double RADPDEG = (Math.PI / 180.);

    static final double REARTH = 6367.47; /*
                                           * NCEP uses this value and so does
                                           * MDL degribber
                                           */

    static final double NEARTH = 6371.2; /* radius of Earth used by NCEP */

    static int mxx;

    int x = 0;

    int y = 0;

    private String pds_ext;

    private int iplen;

    private int debugflag = 0;

    static int mxy;

    AppsDefaults appsDefaults = AppsDefaults.getInstance();

    /**
     * Takes an input netCDF file and creates a GRIB 1 file from it, used for
     * MPE/DailyQC.
     * 
     * @param fname_nc
     *            Full pathname of netCDF file.
     * @param fname_grib
     *            Full pathname of output grib file.
     * @param type
     *            Data type to store. 1 = QPE, 2 = QTE, 3 = QZE, etc....must
     *            match defined data type in gfe2grib.txt file
     * 
     * @return error code
     */
    public int write_dqc_grib_grids(String fname_nc, String fname_grib, int type) {
        String sep = File.separator;
        String dataType = "QPE";
        int index = fname_nc.lastIndexOf(sep.charAt(0));
        String ncfile_path = fname_nc.substring(0, index);
        String ncfile_name = fname_nc.substring(index + 1);
        index = fname_grib.lastIndexOf(sep.charAt(0));
        String gfile_path = fname_grib.substring(0, index);
        String gfile_name = fname_grib.substring(index + 1);

        switch (type) {
        case 1:
            dataType = "QPE";
            break;
        case 2:
            dataType = "QTE";
            break;
        case 3:
            dataType = "QZE";
            break;
        }

        String runArgs = "-n " + ncfile_path + " -i " + ncfile_name + " -t "
                + gfile_path + " -o " + gfile_name + " -p " + dataType + " -N";
        int exitValue = ProcessRunner.runProgram(
                appsDefaults.getToken("pproc_bin") + "/run_nc2grib", runArgs);

        // Output result
        if (exitValue == 0) {
            System.out.println("nc2grib was run. ");
        } else {
            System.out
                    .println("nc2grib did not run successfully and terminated with exit code: "
                            + exitValue + "\n");
            return 1;
        }
        return 0;
    }

    /**
     * Takes a String with args to pass to the run_nc2grib program
     * 
     * @param args
     *            String of command line style arguments to pass to run_nc2grib
     * @return error code
     */
    public int write_dqc_grib_grids(String args) {
        String runArgs = args;
        int exitValue = ProcessRunner.runProgram(
                appsDefaults.getToken("pproc_bin") + "/run_nc2grib", runArgs);

        // Output result
        if (exitValue == 0) {
            System.out.println("nc2grib was run. ");
        } else {
            System.out
                    .println("nc2grib did not run successfully and terminated with exit code: "
                            + exitValue + "\n");
            return 1;
        }
        return 0;
    }

    /**
     * 
     * Writes Dqc grib grid files
     * 
     * @param fname
     *            GRIB1 filename
     * @param pnum
     *            period number (0-5)
     * @param dtype
     *            data type code
     * @param hyday
     *            hydrologic day of qc
     * @return GRIB1 file
     */
    public int old_write_dqc_grib_grids(String fname_nc, String fname_grib,
            int pnum, int dtype, int hyday) {

        float[] grid_data;
        Calendar hydroday = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        float xmissing = -9999.f;
        int hourspast = 0;
        Hrap_Grid hrap_grid = DailyQcUtils.getHrap_grid();
        Pcp pcp = DailyQcUtils.pcp;
        int[] grib_lbl = new int[43];
        String data_level;
        int length = 0;
        int odim = COPYSIZE;
        Byte[] output_buffer = new Byte[odim];
        Maparams map_params = null;
        int m, firstLon, status, yr = 0, mon = 0, day = 0, hrmin = 0, sec = 0, esth = 0;
        double lonOrigin = 0;
        Rectangle extent = new Rectangle((int) ga.domainOrigin.x,
                (int) ga.domainOrigin.y, (int) ga.domainExtent.x,
                (int) ga.domainExtent.y);
        HRAPSubGrid subGrid = null;
        HRAP hrap = null;
        try {
            subGrid = new HRAPSubGrid(extent);
            hrap = HRAP.getInstance();
            // Rectangle hcoord = HRAPCoordinates.getHRAPCoordinates();
        } catch (Exception e1) {
            System.out
                    .println("ERROR: problem getting HRAP coordinates writeDQCGribGrids ");
        }
        /* need WMO header for precip file to go to NPVU */
        String wmohdr1 = ""; /*
                              * first part of WMO header 6 chars
                              */
        String wmohdr2 = ""; /*
                              * second part of WMO header 4 chars
                              */
        String crcrlf = "'\r', '\r', '\n'"; /*
                                             * needed to separate WMO header
                                             * from first part of GRIB message
                                             */
        String aspace = " "; /* contains a space character for the header */
        String header = ""; /* full WMO header string 17 chars */
        // struct tm *curgmtime; /* for containing the current time GMT, used in
        // WMO header */
        Calendar curgmtime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        curgmtime.setTime(new Date());
        Calendar reftime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));

        String adayhrmin = ""; /*
                                * day, hour, minute info attached to WMO header
                                * - 6 chars
                                */
        String printhdr = ""; /* for log purposes 17 char */

        /*---------------------------------------------*/
        /* first time in this routine for new Hyd Day */
        /*---------------------------------------------*/

        mxx = hrap_grid.maxi;
        mxy = hrap_grid.maxj;
        int idim = Integer.SIZE * mxx;

        grid_data = new float[mxy * mxx];
        String dqc_lonorigin = apps_defaults.getToken("mpe_dqc_lonorigin");
        if (dqc_lonorigin.length() != 0) {
            try {
                lonOrigin = Float.parseFloat(dqc_lonorigin);
            } catch (NumberFormatException e) {
                System.out
                        .println("Error parsing token for mpe_dqc_lonorigin. "
                                + e);
            }
        } else {
            lonOrigin = 0;
        }

        /* GRIB Edition number */
        grib_lbl[0] = 1;

        /* grid ID */
        grib_lbl[1] = 255; // non standard grid

        /* parameter table version */
        grib_lbl[2] = 2;

        /* center id */
        grib_lbl[3] = 9;

        /* 30 for forecaster generated */
        grib_lbl[4] = 30;

        /* ignored */
        grib_lbl[5] = 0;

        /* include the GDS? Yes = 1 */
        grib_lbl[6] = 1;

        /* for averaged grids, but not used here */
        grib_lbl[19] = 0;
        grib_lbl[20] = 0;

        /* level */
        grib_lbl[9] = 0;
        grib_lbl[10] = 0;

        /* sub-center id */

        if (ga.siteID.contains("TUA")) {
            grib_lbl[21] = 150;
            wmohdr2 = "KTUA";
        } else if (ga.siteID.contains("ACR")) {
            grib_lbl[21] = 151;
            wmohdr2 = "PACR";
        } else if (ga.siteID.contains("STR")) {
            grib_lbl[21] = 152;
            wmohdr2 = "KSTR";
        } else if (ga.siteID.contains("RSA")) {
            grib_lbl[21] = 153;
            wmohdr2 = "KRSA";
        } else if (ga.siteID.contains("ORN")) {
            grib_lbl[21] = 154;
            wmohdr2 = "KORN";
        } else if (ga.siteID.contains("RHA")) {
            grib_lbl[21] = 155;
            wmohdr2 = "KRHA";
        } else if (ga.siteID.contains("KRF")) {
            grib_lbl[21] = 156;
            wmohdr2 = "KKRF";
        } else if (ga.siteID.contains("MSR")) {
            grib_lbl[21] = 157;
            wmohdr2 = "KMSR";
        } else if (ga.siteID.contains("TAR")) {
            grib_lbl[21] = 158;
            wmohdr2 = "KTAR";
        } else if (ga.siteID.contains("PTR")) {
            grib_lbl[21] = 159;
            wmohdr2 = "KPTR";
        } else if (ga.siteID.contains("TIR")) {
            grib_lbl[21] = 160;
            wmohdr2 = "KTIR";
        } else if (ga.siteID.contains("ALR")) {
            grib_lbl[21] = 161;
            wmohdr2 = "KALR";
        } else if (ga.siteID.contains("FWR")) {
            grib_lbl[21] = 162;
            wmohdr2 = "KFWR";
        } else {
            System.out
                    .println(String
                            .format(" site ID %s is not an RFC, sor for this application setting WMO ID to KXXX\n",
                                    ga.siteID));
            grib_lbl[21] = 0;
            wmohdr2 = "KXXX";
        }

        /* binary data section flag */
        grib_lbl[23] = 0;

        /* packing width of data points */
        grib_lbl[24] = 16; // original was 16 in the example, 4 in gribit

        /* initialized but ignored in grib message */
        grib_lbl[26] = 0;
        grib_lbl[27] = 0;

        /* length of GDS */
        if (ga.projectionType.contains("POLAR")) {
            grib_lbl[25] = 32; // polar stereographic and lan/lon, 42 for
                               // Lambert

            /* grid (data representation) type, polar stereographic */
            grib_lbl[28] = 5;
            grib_lbl[29] = mxx;
            grib_lbl[30] = mxy;

            /* next for initialized but not used */
            grib_lbl[39] = 0;
            grib_lbl[40] = 0;
            grib_lbl[41] = 0;
            grib_lbl[42] = 0;

            grib_lbl[34] = (int) (lonOrigin * 1000.); /*
                                                       * longitude of grid point
                                                       * orientation
                                                       */
        } else if (ga.projectionType.contains("LAMBERT")) {
            /*
             * This isn't supported by MPE/DailyQC just yet, so we will just
             * exit
             */
            System.out
                    .println("\n Lambert Conformal isn't supported at the present time by MPE/DailyQC....Exiting ");
            return -4;
        } else {
            System.out.println(" Unknown map projection specified...Exiting ");
            return -4;
        }

        /* resolution component flags */
        grib_lbl[33] = 8;

        /* must find the grid map parameters and then the dx, dy resolution */
        /*
         * normally, these are the same for polar stereographic and even lambert
         * conformal, but not necessarily
         */

        double x1, y1, x2, y2, lat1, lon1, lat2, lon2 = 0.; /* initialize */

        /* Lower left corner of the main projected grid */

        x1 = ga.gridPointLL[0];
        y1 = ga.gridPointLL[1];
        lon1 = ga.latLonLL.x;
        lat1 = ga.latLonLL.y;

        /* upper right corner of the main projected grid */

        x2 = ga.gridPointUR[0];
        y2 = ga.gridPointUR[1];
        lon2 = ga.latLonUR.x;
        lat2 = ga.latLonUR.y;
        /*
         * check if polar stereographic or lambert conformal to set map
         * parameters correctly
         */
        if (grib_lbl[25] == 32) {
            map_params = getMapParameters(90., lonOrigin);
        }
        /* set Earth radius */
        cstrad(map_params, NEARTH);

        stcm2p(map_params, x1, y1, lat1, lon1, x2, y2, lat2, lon2); /*
                                                                     * find map
                                                                     * parameters
                                                                     * based on
                                                                     * known
                                                                     * lat/lons
                                                                     */
        /*
         * find DX DY values, should be identical for the projections for this
         * app
         */

        if (grib_lbl[25] == 32) {
            dxdy = cgszll(map_params, 60., lonOrigin);
        }

        /*************************************************************************/
        if (debugflag > 0) {

            /* debug only */

            System.out.println(String.format(" DEBUG: dxdy is %9.3f\n", dxdy));

            System.out
                    .println(" DEBUG: Crosscheck grid lower left and upper right info\n");

            System.out
                    .println(String
                            .format(" DEBUG: LL X=%6.0f, LL Y=%6.0f, UR X=%6.0f, UR Y=%6.0f\n   DEBUG: LL Lat=%f, LL Lon=%f, UR Lat=%f, UR Lon=%f\n",
                                    x1, y1, x2, y2, lat1, lon1, lat2, lon2));

            System.out.println(String.format(
                    " DEBUG: longitude at origin = %d\n", grib_lbl[34] / 1000));

        }
        /*************************************************************************/

        dxdy = Math.ceil(dxdy * 1000);
        int dx = (int) dxdy;
        int dy = (int) dxdy;

        /*
         * in GFE, the gridsize should equal the extents if using the standard
         * grid resolutions. If not, the site has changed resolutions and this
         * must be determined
         */

        if (mxy != (int) ga.domainExtent.y || mxx != (int) ga.domainExtent.x) {
            /* first calculate x */

            /*
             * this formula is in the GFE online help - Adjusting the Grid
             * Resolution in localConfig.py
             */

            dx = (int) (dxdy * ga.domainExtent.x / (mxx - 1));

            dy = (int) (dxdy * ga.domainExtent.y / (mxy - 1));

        }

        /*
         * note that this may cause problems for places where dx != dy but they
         * are still using polar stereographic and it usually assumes these are
         * the same
         */

        grib_lbl[35] = dx;
        grib_lbl[36] = dy;

        /*
         * now for the local grid (i.e grid 255 in GRIB), will need to get the
         * lower left lat, lon and will use the cxy2ll command here for the
         * domain with origin values of x and y
         */

        x = (int) ga.domainOrigin.x;
        y = (int) ga.domainOrigin.y;
        Coordinate ll1 = new Coordinate();
        try {
            ll1 = hrap.gridCoordinateToLatLon(ga.domainOrigin,
                    PixelOrientation.CENTER);
        } catch (Exception e) {
            System.out
                    .println("ERROR: writeDQCgribGrids getting hrap to lat lon. ");
        }
        // cxy2ll(map_params, x, y, lat1, lon1); /* Find lat lon */

        grib_lbl[31] = (int) (ll1.y * 1000);
        grib_lbl[32] = (int) (ll1.x * 1000);
        firstLon = grib_lbl[32];

        /***************** debug *********************/

        if (debugflag > 0) {
            System.out
                    .println(String
                            .format(" DEBUG: dx = %d dy = %d x = %d extent x = %f y = %d extent y = %f \n",
                                    dx, dy, x, ga.domainExtent.x, y,
                                    ga.domainExtent.y));
            System.out
                    .println(String
                            .format(" DEBUG: for local domain x = %d and y = %d, the corresponding lat = %f lon = %f\n",
                                    x, y, ll1.y, ll1.x));
        }
        /******************************************/

        grib_lbl[37] = 0;

        /* scanning mode flag */
        grib_lbl[38] = 64;
        /*
         * in the original packgrib_.c documentation, it was thought that this
         * pds_ext could be anything the user wanted. However, this area of the
         * GRIB message actually is used by NCEP to include ensemble forecast
         * information for GRIB1 messages. Therefore this should be set to the
         * NULL string unless one really means to include ensemble information
         * here.
         */

        pds_ext = "";
        iplen = pds_ext.length();

        /*************************************************************************/
        if (debugflag > 0) {

            /* debug only */

            System.out.println(String.format(" DEBUG: dxdy is %6.0f\n", dxdy));

            System.out.println(String.format(
                    " DEBUG: LL local domain lat=%f lon=%f\n", lat1, lon1));

        }
        /************************************************************************/
        if (dtype > 1) {
            reftime.setTimeInMillis(ga.validTimes[pnum][0] * 1000);
        } else {
            hydroday.setTime(pdata[hyday].data_time);
            hydroday.add(Calendar.SECOND, -86400); /* start of hydro day */
            reftime.setTimeInMillis(hydroday.getTimeInMillis());
        }

        grib_lbl[11] = reftime.get(Calendar.YEAR);
        grib_lbl[12] = reftime.get(Calendar.MONTH + 1);
        grib_lbl[13] = reftime.get(Calendar.DAY_OF_MONTH);
        int hr = reftime.get(Calendar.HOUR_OF_DAY);
        int min = reftime.get(Calendar.MINUTE);
        String hour = hr + "";
        String minute = min + "";
        if (hr < 10) {
            hour = "0" + hr;
        }
        String hmin = String.format("%d%d", hour, minute);
        grib_lbl[14] = Integer.parseInt(hmin);
        esth = (int) ((ga.validTimes[pnum][1] - ga.validTimes[pnum][0]) / SECINHR);

        /*************************************************************/
        if (debugflag > 0) {
            System.out
                    .println(String
                            .format(" DEBUG: esth = %d valid time = %ld initial time = %ld reftime=%s\n",
                                    esth, ga.validTimes[pnum][1],
                                    ga.validTimes[pnum][0], reftime.getTime()));
        }
        /*************************************************************/

        if (esth > 240 || esth < 0) {
            System.out
                    .println(String
                            .format(" The estimated/observed time period is either less than 0 or greater than 10 days (240 hours).\n"
                                    + " Therefore, valid times within the input filename may not have been generated \n"
                                    + " correctly.  This should be reported for debugging or you could try to generate the file again.\n"
                                    + " For debug fcsth = %d\n", esth));
            return FILEERR;
        }
        /*
         * see the GRIB table on this for determining reference and valid times
         * for different types of products
         */
        /*
         * 03/24/10 DTM - changing to make these both 0 as the temperatures and
         * freezing levels are instaneous grids in MPE DailyQC. This is
         * different than nc2grib where the temp grids are a 6-hour average
         */

        /* grib_lbl[16]=esth; *//* P1 */

        grib_lbl[16] = 0; /* P1 */
        grib_lbl[17] = 0; /* P2 */

        System.out.println(String.format(
                "\n\n NetCDF record %d is an estimate/observed product\n",
                pnum + 1));

        /*************************************************************/
        if (debugflag > 0) {
            System.out.println(String.format(
                    " DEBUG: valid time = %d %d %d %d reftime=%s\n"
                            + " DEBUG: validTimes = %ld\n", yr, mon, day,
                    hrmin, reftime, ga.validTimes[pnum][0]));
            /*************************************************************/
        }

        /*------------------------------*/
        /* create GRIB file */
        /*------------------------------*/

        int kk = 0;

        /* define grid */
        if (dtype == 1) /* precipitation */
        {
            for (int j = 0; j < mxy; j++) {
                for (int i = 0; i < mxx; i++) {
                    if (pcp.value[i][j] > xmissing) {
                        grid_data[kk] = (float) (pcp.value[i][j] / 100. * 25.4); /*
                                                                                  * convert
                                                                                  * to
                                                                                  * mm
                                                                                  * for
                                                                                  * GRIB1
                                                                                  */

                    } else {
                        grid_data[kk] = xmissing;
                    }
                    kk++;
                }
            }
            grib_lbl[7] = 61; /* precipitation GRIB id */

            grib_lbl[15] = 1; /* hour time unit */

            grib_lbl[18] = 4; /* time range; accumulation for precip */

            grib_lbl[22] = 3; /* decimal scale */

            /* level type */
            grib_lbl[8] = 1; /* surface */

            grib_lbl[4] = 172; /* QPE process GRIB number */

            /* dtm 081009: discovered this won't work for NPVU QPE GRIB files */
            /*
             * must use the hydro day basis time as reference time and add
             * number of hours past like a forecast grid
             */

            /* grib_lbl[16]=0; /* P1 */

            /* grib_lbl[17]=esth; /* P2 */

            hourspast = (int) ((ga.validTimes[pnum][1] - (hydroday
                    .getTimeInMillis() / 1000)) / SECINHR);

            grib_lbl[16] = hourspast - esth; /* P1 */
            grib_lbl[17] = hourspast; /* P2 */
            /*
             * logMessage("\n hourspast = %d pdata data time = %ld \n",hourspast,
             * pdata[hyday].data_time );
             * logMessage("\n valid time 1 = %ld \n",commonGridAttributes
             * .validTimes[period_num][1]);
             */

        } else if (dtype == 2) /* temperature */
        {
            for (int j = 0; j < mxy; j++) {
                for (int i = 0; i < mxx; i++) {
                    if (pcp.value[i][j] > xmissing) {

                        grid_data[kk] = (float) ((((pcp.value[i][j] / 100.) - 32) * 5 / 9) + 273.16); /*
                                                                                                       * convert
                                                                                                       * F
                                                                                                       * to
                                                                                                       * K
                                                                                                       * for
                                                                                                       * GRIB1
                                                                                                       */

                    } else {
                        grid_data[kk] = xmissing;
                    }

                    kk++;
                }
            }

            if (esth > 6)

            /*
             * DTM 033010 for temperature, there are instantaneous values where
             * P1 = 0; however, Daily QC also has min and max temperature grids
             * and these should be value for a 24 hour period or over the hydro
             * day. So the values every 6 hours will be valid for the time the
             * grid is valid for. But the max/min will be 24 hours after
             * basetime.
             */

            {
                grib_lbl[16] = esth; /* P1 */

                grib_lbl[17] = 0; /* P2 */
            }

            grib_lbl[7] = 11; /* temperature GRIB id */

            grib_lbl[15] = 1; /* hour time unit */

            grib_lbl[18] = 0; /*
                               * time range; this may need to be changed to 3
                               * for QTE and grib valid time will have to be
                               * reworked
                               */

            grib_lbl[22] = 2; /* decimal scale */

            /* level type */
            grib_lbl[8] = 1; /* surface */

        } else /* freezing level */
        {
            for (int j = 0; j < mxy; j++) {
                for (int i = 0; i < mxx; i++) {
                    if (pcp.value[i][j] > xmissing) {

                        grid_data[kk] = (float) (pcp.value[i][j] / 100. * 304.8); /*
                                                                                   * convert
                                                                                   * kilofeet
                                                                                   * into
                                                                                   * meters
                                                                                   * for
                                                                                   * GRIB1
                                                                                   */
                    } else {
                        grid_data[kk] = xmissing;
                    }

                    kk++;
                }
            }
            grib_lbl[8] = 4; /* freezing level surface */
            grib_lbl[7] = 7; /* height GRIB id */

            grib_lbl[15] = 1; /* hour time unit */

            grib_lbl[18] = 0; /* time range */

            grib_lbl[22] = 2; /* decimal scale */

        }
        PackGrib pg = new PackGrib();
        status = pg.packgrib(grib_lbl, pds_ext, iplen, grid_data, idim,
                xmissing, output_buffer, odim, length);
        return 0;
    }

    /**
     * @param map_params
     * @param d
     * @param lonOrigin
     * @return
     */
    private double cgszll(Maparams mparams, double lat, double longit) {
        Vector3D map = basegtom(mparams, ll_geog(lat, longit));

        double ymerc;

        if (map.v[2] >= 1.) {
            return mparams.gridszeq * (mparams.gamma >= 1. ? 2. : 0.);
        }
        if (map.v[2] <= -1.) {
            return mparams.gridszeq * (mparams.gamma <= -1. ? 2. : 0.);
        }
        if (Math.abs(mparams.gamma) >= 1.) {
            return mparams.gridszeq
                    * (1. + (mparams.gamma > 0. ? map.v[2] : -map.v[2]));
        }
        ymerc = -.5 * Math.log((1. - map.v[2]) / (1. + map.v[2]));
        return mparams.gridszeq * Math.exp(-(1. - mparams.gamma) * ymerc)
                * (1. + map.v[2]);
    }

    /**
     * @param map_params
     * @param x1
     * @param y1
     * @param lat1
     * @param lon1
     * @param x2
     * @param y2
     * @param lat2
     * @param lon2
     */
    private void stcm2p(Maparams mparams, double x1, double y1, double xlat1,
            double xlong1, double x2, double y2, double xlat2, double xlong2) {

        double den, dena;
        double[] xy1 = new double[2];
        double[] xy2 = new double[2];
        mparams.x0 = mparams.y0 = mparams.srotate = 0;
        mparams.crotate = mparams.gridszeq = 1.;
        cll2xy(mparams, xlat1, xlong1, xy1);
        cll2xy(mparams, xlat2, xlong2, xy2);
        den = Math.sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2));
        dena = Math.sqrt((xy1[0] - xy2[0]) * (xy1[0] - xy2[0])
                + (xy1[1] - xy2[1]) * (xy1[1] - xy2[1]));
        mparams.crotate = ((xy1[0] - xy2[0]) * (x1 - x2) + (xy1[1] - xy2[1])
                * (y1 - y2))
                / den / dena;
        mparams.srotate = ((xy1[1] - xy2[1]) * (x1 - x2) - (xy1[0] - xy2[0])
                * (y1 - y2))
                / den / dena;
        mparams.gridszeq *= dena / den;
        cll2xy(mparams, xlat1, xlong1, xy1);
        mparams.x0 += x1 - xy1[0];
        mparams.y0 += y1 - xy1[1];

    }

    /**
     * @param mparams
     * @param xlat1
     * @param xlong1
     * @param xy1
     */
    private void cll2xy(Maparams mparams, double xlat1, double xlong1,
            double[] xy1) {
        Vector3D geog;
        geog = ll_geog(xlat1, xlong1);
        map_xy(mparams, basegtom(mparams, geog), xy1[0], xy1[1]);

    }

    /**
     * @param mparams
     * @param basegtom
     * @param x
     * @param y
     */
    private void map_xy(Maparams mparams, Vector3D map, double x, double y) {
        X_Y[] xi_eta = new X_Y[1];
        map_xe(mparams, map, xi_eta, 0);
        xe_xy(mparams, xi_eta[0].x, xi_eta[0].y, x, y);

    }

    /**
     * @param mparams
     * @param map
     * @param xi_eta
     * @param i
     */
    private void map_xe(Maparams mparams, Vector3D map, X_Y[] xi_eta, int mode) {
        double ymerc, rhog, theta, thetalo, thetahi;
        if (Math.abs(map.v[2]) >= 1.) {
            /* Projection Pole or its antipodes ("Northing" or Southing" Pole) */
            if (mparams.gamma * map.v[2] > 0.) {
                /*
                 * This pole is in a finite part of the map, and all returned
                 * poles are the same
                 */
                xi_eta[0].x = 0.;
                xi_eta[0].y = 1. / mparams.gamma;
                if (mode > 0) {
                    xi_eta[1].x = xi_eta[2].x = xi_eta[0].x;
                    xi_eta[1].y = xi_eta[2].y = xi_eta[0].y;
                }
                return;
            } else {
                /*
                 * These poles are infinitely far away. Substitute ymerc=+-20.
                 * for "infinity"
                 */
                ymerc = (map.v[2] > 0. ? 20. : -20.);
                theta = thetahi = Math.PI;
                thetalo = -Math.PI;
            }
        } else {
            /* Away from projection poles */
            ymerc = .5 * Math.log((1. + map.v[2]) / (1. - map.v[2]));
            theta = Math.atan2(map.v[1], map.v[0]);
            if (theta <= -Math.PI) {
                theta += (Math.PI + Math.PI);
            }
            if (theta > 0) {
                thetahi = theta;
                thetalo = theta - (Math.PI + Math.PI);
            } else {
                thetalo = theta;
                thetahi = theta + (Math.PI + Math.PI);
            }
        }
        rhog = xpabova(mparams.gamma, -ymerc);

        xi_eta[0].x = (1. + mparams.gamma * rhog)
                * snabova(mparams.gamma, theta);

        xi_eta[0].y = mparams.gamma * (1. + mparams.gamma * rhog)
                * csabova(mparams.gamma, theta) - rhog;
        if (mode > 0) {
            xi_eta[1].x = (1. + mparams.gamma * rhog)
                    * snabova(mparams.gamma, thetalo);
            xi_eta[1].y = mparams.gamma * (1. + mparams.gamma * rhog)
                    * csabova(mparams.gamma, thetalo) - rhog;
            xi_eta[2].x = (1. + mparams.gamma * rhog)
                    * snabova(mparams.gamma, thetahi);
            xi_eta[2].y = mparams.gamma * (1. + mparams.gamma * rhog)
                    * csabova(mparams.gamma, thetahi) - rhog;
        }
    }

    /**
     * @param gamma
     * @param theta
     * @return
     */
    private double csabova(double a, double b) {
        /* returns (1. - cos(a * b) ) / a / a, or its limit as a -> 0 */
        double term = snabova(.5 * a, b);
        return .5 * term * term;
    }

    /**
     * @param gamma
     * @param theta
     * @return
     */
    private double snabova(double a, double b) {
        double c = a * b, csq;
        /* returns sin(a * b) / a, or its limit as a -> 0. */
        if ((csq = c * c) > .001) {
            return (Math.sin(c)) / a;
        } else {
            return b * (1. - csq / 6. * (1. - csq / 20. * (1. - csq / 42.)));
        }
    }

    /**
     * @param gamma
     * @param d
     * @return
     */
    private double xpabova(double a, double b) {
        double c = a * b, csq;
        /* returns (exp (a * b) - 1. ) / a, or its limit as a -> 0. */
        if ((csq = .25 * c * c) > .001) {
            return (Math.exp(c) - 1.) / a;
        } else {
            /* exp(.5*a*b) * sinh(.5*a*b)*b/(.5*a*b) */
            return Math.exp(.5 * c) * b
                    * (1. + csq / 6. * (1. + csq / 20. * (1. + csq / 42.)));
        }
    }

    /**
     * @param mparams
     * @param x
     * @param y
     * @param x2
     * @param y2
     */
    private void xe_xy(Maparams mparams, double xi, double eta, double x,
            double y) {
        x = mparams.x0 + mparams.EarthRad / mparams.gridszeq
                * (mparams.crotate * xi + mparams.srotate * eta);
        y = mparams.y0 + mparams.EarthRad / mparams.gridszeq
                * (mparams.crotate * eta - mparams.srotate * xi);

    }

    private class X_Y {
        double x;

        double y;
    }

    /**
     * @param mparams
     * @param geog
     * @return
     */
    private Vector3D basegtom(Maparams mparams, Vector3D geog) {
        Vector3D v3 = new Vector3D();
        /*
         * converts vectors from the "geographic" coordinate system (based on
         * the North pole, equator and Greenwich meridian) to vectors in the
         * "map" coordinate system, from which the x-y coordinates of the map
         * are obtained. Uses stcprm->rotate to perform the translation.
         */
        int k, l;
        for (k = 0; k < 3; k++) {
            for (l = 0; l < 3; l++) {
                v3.v[l] += geog.v[k] * mparams.rotate[l][k];
            }
        }
        return v3;
    }

    /**
     * @param map_params
     * @param nearth2
     */
    private void cstrad(Maparams map_params, double radius) {
        double factor = radius / map_params.EarthRad;
        map_params.EarthRad = radius;
        map_params.gridszeq *= factor;
    }

    /**
     * @param d
     * @param lonOrigin
     * @return
     */
    private Maparams getMapParameters(double reflat, double lonOrigin) {
        Maparams mp = mapf_start(reflat, 90., lonOrigin, 0., lonOrigin);
        return mp;
    }

    /**
     * @param d
     * @param e
     * @param lonOrigin
     * @param f
     * @param lonOrigin2
     * @return
     */
    private Maparams mapf_start(double coneang, double plat, double plon,
            double rlat, double rlon) {
        double cone_ang = coneang;
        int k = 0;
        double norm = 0;
        double p_lat = plat;
        double p_lon = plon;
        double r_lat = rlat;
        double r_lon = rlon;
        Maparams mparam = new Maparams();
        Vector3D temp = ll_geog(p_lat, p_lon);

        for (k = 0; k < 3; k++) {
            mparam.rotate[2][k] = temp.v[k];
        }

        temp = ll_geog(r_lat, r_lon);

        for (k = 0; k < 3; k++) {
            mparam.rotate[0][k] = temp.v[k];
        }

        norm = xProduct(mparam.rotate[2], mparam.rotate[0], mparam.rotate[1]);

        for (k = 0; k < 3; k++) {
            mparam.rotate[1][k] /= norm;
        }

        xProduct(mparam.rotate[1], mparam.rotate[2], mparam.rotate[0]);

        mparam.crotate = 1.;
        mparam.gridszeq = mparam.EarthRad = REARTH;
        mparam.gamma = Math.sin(RADPDEG * cone_ang);
        mparam.x0 = mparam.y0 = mparam.srotate = 0;
        /*
         * geographic triple : i = equator @ std merid, j = equator @ 90E, k =
         * North Pole
         */
        /*
         * map base triple : i' = M-prime meridian @ M-equator, j' = M-East at
         * location i', k' = M-Pole
         */
        return mparam;
    }

    /**
     * @param ds
     * @param ds2
     * @param ds3
     */
    private double xProduct(double[] ds, double[] ds2, double[] ds3) {
        double[] a = ds;
        double[] b = ds2;
        double[] c = ds3;
        int xp_nd[] = { 1, 2, 0, 1 };
        double norm = 0;
        int k, l;
        for (k = 0, l = 1; k < 3; k++, l++) {
            c[k] = a[xp_nd[k]] * b[xp_nd[l]] - a[xp_nd[l]] * b[xp_nd[k]];
            norm += c[k] * c[k];
        }
        return Math.sqrt(norm);

    }

    /**
     * @param p_lat
     * @param p_lon
     * @return
     */
    private Vector3D ll_geog(double lat, double lon) {
        double clat;
        Vector3D geog = new Vector3D();
        geog.v[2] = Math.sin(RADPDEG * lat);
        geog.v[0] = Math.cos(RADPDEG * lon) * (clat = Math.cos(RADPDEG * lat));
        geog.v[1] = Math.sin(RADPDEG * lon) * clat;

        return geog;
    }

    private class Maparams {
        double gamma;

        double[][] rotate = new double[3][3];

        double x0, y0, crotate, srotate, gridszeq;

        double EarthRad;

    }

    private class Vector3D {
        double[] v = new double[3];
    }

}
