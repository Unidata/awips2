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

package com.raytheon.uf.edex.plugin.qpf;

import java.util.HashMap;

import com.raytheon.uf.common.dataplugin.qpf.QPFRecord;
import com.raytheon.uf.common.dataplugin.qpf.QPFUtils;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;
import com.raytheon.uf.common.monitor.config.SCANRunSiteConfigurationManager;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.xml.SCANModelParameterXML;
import com.raytheon.uf.common.monitor.xml.SCANSiteXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.dat.utils.ScanDataCache;
import com.raytheon.uf.edex.plugin.qpf.common.QPFConfig;
import com.vividsolutions.jts.geom.Coordinate;

/* 
 *            Description: Given a sequence of VIL and 0.5 deg reflectivity
 *            images and environmental upper-air wind data, this routine and
 *            its functions prepare 0-1 hour probabilistic rainfall
 *            forecasts for the radar Umbrella. An extrapolative-statistical
 *            technique is used.
 * 
 *            Variables: which = Input short variable indicating which radar
 *            is having data evaluated rain_cat[] = Rainfall amount forecast
 *            derived from probabilities, in categories. Dimensioned
 *            IWE,INS. rainval[] = Breakpoints of rainfall categories, in
 *            .01 inch, dimensioned NCATR irain_0_30[], irain_0_60[] =
 *            Rainfall amount forecasts for 0-30 and 0-60 minutes, used as
 *            rainfall amount statistical predictors, dimensioned IWE,INS.
 *            (input) vil_0_60[] = Mean VIL forecasted by extrapolation,
 *            0-60 minutes, dimensioned IWE,INS (input) vilmax_time[] =
 *            Maximum VIL over each grid box during 0-60 min period,
 *            forecasted by extrapolation (input) maxvil_7x7() = Maximum
 *            initial time VIL, within 7x7 box region centered on each grid
 *            box (input) n24dbz[] = Over 3x3-box region, number of grid
 *            boxes and time periods in which reflectivity forecast exceeds
 *            24 dBZ during 0-60 minute period (input) nvil10_3x3() = Number
 *            of grid boxes in 3x3-region with initial-time VIL >= 10 kg/m^2
 *            (input) pb = Maximum binary correlation grid1, grid2 =
 *            Short-type working grids rns_velocity, we_velocity =
 *            North-south and east-west radar field velocities; can be
 *            dervied from STI average speed, environmental data, or pattern
 *            matching. rns_displ, we_displ = North-south and east-west
 *            radar field gridpoint displacement. earlyVIL, curVIL =
 *            Pointers to the two VIL arrays chosen for the pattern match.
 *            earlyZTR, curZTR = Pointers to the two VIL arrays chosen for
 *            the pattern match. earlyTime = Time (AbsTime class) of early
 *            pattern-match volume scan outputFile[] = Array of
 *            TextString-class objects used to hold the names of the output
 *            files. path[] = Array of TextString-class objects used to hold
 *            the path names for the output files. ptemp[] = Array of float
 *            pointers that point to the float output grids (to facilitate
 *            call to netCDF writer) secdiff = Time difference (in seconds)
 *            between two volume scans. use_env_winds = Boolean, true when
 *            environmental data is chosen to input the NS and EW field
 *            velocities. ztr_fcst[][] = Array of radar forecast fields;
 *            Inner array holds the temporal dimension, and the outer array
 *            holds the radar field. irow, irow0 = Row index counters. icol,
 *            icol0 = column index counters. index = Array counter izmnt[] =
 *            Array holding number of minutes into the next hour to forecast
 *            for. nhit = Number of 'hits' at max. binary correlation. non1,
 *            non2 = Number of 'on' points in grid1 and grid2, within bounds
 *            of (JMIN,JMAX), (IMIN,IMAX) (output) Output fields:
 * 
 *            p10[],p25[],p50[], p75[] = Probability of rainfall exceeding
 *            the 4 amount thresholds described above %. Dimensioned
 *            IWE,INS. precip_amt[] = Rainfall amount forecast, in .01 inch
 * 
 *            History: November 1998 Dave Kitzmiller(TDL) - created November
 *            1999 Mike Churma(RSIS/TDL) - re-written in C/C++ February 2000
 *            Inserted into PCMS (version control). March 2000 Tom
 *            Filiaggi(TDL) - changed how qpf() gets called in order to
 *            handle some memory problems. September 2000 Mike Churma
 *            (RSIS/TDL) - added notifications and netCDF error checking
 *            (AWIPS 5.1.1) June 2001 Changed to use Standard Library
 *            vectors rather than SeqOf's as containers of StormCellSTI
 *            objects. December 2007 Lingyan Xin (SAIC/MDL) - changed the
 *            interface of writeNetcdf() to output more attributes in netCDF
 *            file for standard file reading.
 *
 * QPF algorithm
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/17/2009   1981       dhladky    Initial Creation.
 * 02/26/2014   2836       dhladky      Added diagnostic information.
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class QPF {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(QPF.class);

    /** QPFConfig object */
    private QPFConfig qpf_config = null;

    // public constructor
    public QPF(QPFConfig qpf_config) {
        this.qpf_config = qpf_config;
    }

    /** The payload floats **/
    private HashMap<String, float[]> floats = null;

    /** EW velocity **/
    float we_velocity = 0.0f;

    /** NS velocity **/
    float rns_velocity = 0.0f;

    /** Average Speed **/
    float avg_spd = 0.0f;

    /** Average Direction **/
    float avg_dir = 0.0f;

    /**
     * QPF grid file type class
     * 
     * @param which
     * @return QPFRecord
     */
    public void genQPF() {
        float pb = 0.0f;
        int[] grid1 = new int[ScanUtils.SCAN_GRID_DIM_SQ];
        int[] grid2 = new int[ScanUtils.SCAN_GRID_DIM_SQ];
        float[] vilmax_time = new float[ScanUtils.SCAN_GRID_DIM_SQ];
        float we_displ = 0.0f;
        float rns_displ = 0.0f;
        float r_minutes = 0.0f;
        float[] vil_0_60 = new float[ScanUtils.SCAN_GRID_DIM_SQ];
        float[] itime_avg_vil = new float[ScanUtils.SCAN_GRID_DIM_SQ];
        int irow = 0;
        int icol = 0;
        int irow0 = 0;
        int icol0 = 0;
        int index = 0;
        long[] irain_0_30 = new long[ScanUtils.SCAN_GRID_DIM_SQ];
        long[] irain_0_60 = new long[ScanUtils.SCAN_GRID_DIM_SQ];

        int[] n24dbz = new int[ScanUtils.SCAN_GRID_DIM_SQ];
        int n24dbz_0;
        int i_minutes;
        int[] nvil10_3x3 = new int[ScanUtils.SCAN_GRID_DIM_SQ];
        int[] maxvil_7x7 = new int[ScanUtils.SCAN_GRID_DIM_SQ];

        int i = 0;
        int k = 0;
        int iwe_move = 0;
        int ins_move = 0;
        int nvil10 = 0;
        int maxvil7 = 0;

        // get a hold of the HDF5 and grab arrays, need 4KM grid
        short[] earlyZTR = ScanUtils.convertToGrid(qpf_config.getEarlyCZ(),
                ScanUtils.SCAN_GRID_DIM_SQ, true);
        short[] earlyVIL = ScanUtils.convertToGrid(qpf_config.getEarlyVil(),
                ScanUtils.SCAN_GRID_DIM_SQ);
        short[] curZTR = ScanUtils.convertToGrid(qpf_config.getCurrentCZ(),
                ScanUtils.SCAN_GRID_DIM_SQ, true);
        short[] curVIL = ScanUtils.convertToGrid(qpf_config.getCurrentVil(),
                ScanUtils.SCAN_GRID_DIM_SQ);
        int[][] ztr_fcst = new int[QPFUtils.N_FCST][ScanUtils.A_LEN];

        // Clear out clutter and AP adjoining the clutter region
        // by comparing any echoes > 15 dBZ to VIL; real precip
        // should have VIL >= 1.

        for (irow = QPFUtils.LOWPOINT; irow < QPFUtils.HIGHPOINT; irow++) {
            for (icol = QPFUtils.LOWPOINT; icol < QPFUtils.HIGHPOINT; icol++) {
                index = (QPFUtils.IWE * irow) + icol;
                if ((curZTR[index] >= 15) && (curVIL[index] < 1))
                    curZTR[index] = 0;
                if ((earlyZTR[index] >= 15) && (earlyVIL[index] < 1))
                    earlyZTR[index] = 0;
            }
        }

        // Set izmnt array.
        int[] izmnt = new int[QPFUtils.N_FCST];
        for (i = 0; i < izmnt.length; i++) {
            izmnt[i] = i * 10;
        }

        // Pre-clean Rainfall predictor arrays
        for (index = 0; index < ScanUtils.SCAN_GRID_DIM_SQ; index++) {
            nvil10_3x3[index] = 0;
            maxvil_7x7[index] = 0;
            grid1[index] = earlyZTR[index];
            grid2[index] = curZTR[index];
            ztr_fcst[0][index] = curZTR[index];
        }

        boolean use_env_winds = false;
        
        boolean have_model_wind = false;
        float model_uwind = 0;
        float model_vwind = 0;
        float model_wspeed = 0;

        // use STI to get the speed and direction data for cells
        if (qpf_config.isSTI()) {
            statusHandler.info(qpf_config.getIcao() + ": Processing STI: "
                    + qpf_config.getSTI().getDataURI());
            // extract the avg speed and direction
            avg_spd = new Float(qpf_config.getSTI().getRecordVals(
                    RadarConstants.MapValues.STI_TYPE,
                    RadarConstants.MapValues.STI_AVG_SPEED));
            avg_dir = new Float(qpf_config.getSTI().getRecordVals(
                    RadarConstants.MapValues.STI_TYPE,
                    RadarConstants.MapValues.STI_AVG_DIRECTION));
            // calculate true
            avg_spd = (float) ((avg_spd / 1.94) * (60.0 / 4000.0));
            // this is what QPF uses
            we_velocity = (float) (avg_spd * Math
                    .cos((avg_dir + 90.0) * 0.01745));
            rns_velocity = (float) (avg_spd * Math
                    .sin((avg_dir + 90.0) * 0.01745));
            use_env_winds = true;

        }
        // use the model data
        else if (qpf_config.isModel()) {
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler.debug(qpf_config.getIcao() + ": Processing Model Data");
            }

            try {
                ScanDataCache sdc = ScanDataCache.getInstance();

                SCANRunSiteConfigurationManager scanManager = SCANRunSiteConfigurationManager
                        .getInstance();
                scanManager.readConfigXml();

                if (scanManager.isPopulated()) {

                    SCANSiteXML siteXML = scanManager.getSiteConfig(qpf_config
                            .getIcao());
                    SCANModelParameterXML paramXMLU = siteXML
                            .getModelParameter("U700");
                    SCANModelParameterXML paramXMLV = siteXML
                            .getModelParameter("V700");

                    float uwind = (float) sdc.getModelData().getValue(
                            paramXMLU.getModelName(),
                            paramXMLU.getParameterName(),
                            new Coordinate(qpf_config.getCurrentCZ()
                                    .getLongitude(), qpf_config.getCurrentCZ()
                                    .getLatitude()));
                    float vwind = (float) sdc.getModelData().getValue(
                            paramXMLV.getModelName(),
                            paramXMLV.getParameterName(),
                            new Coordinate(qpf_config.getCurrentCZ()
                                    .getLongitude(), qpf_config.getCurrentCZ()
                                    .getLatitude()));

                    if (uwind != -99999.0 && vwind != -99999.0) {
                        have_model_wind = true;
                        model_uwind = uwind;
                        model_vwind = vwind;
                        model_wspeed = (float) Math.sqrt(uwind * uwind + vwind * vwind);
                        /*
                         * If the environmental data is recent and
                         * meteorologically valid, then check the strength of
                         * the environmental winds. If the 700mb wind is >= 15
                         * m/sec, then use that vector instead of the pattern
                         * match. Convert m/sec to boxes/minute.
                         */
                        if (model_wspeed >= 25) {
                            // this is what QPF uses

                            we_velocity = (float) (uwind * (60.0 / 4000.0));
                            rns_velocity = - (float) (vwind * (60.0 / 4000.0));
                            use_env_winds = true;
    
                            statusHandler.info(qpf_config.getIcao()
                                    + ": Environmental data to be used to determine upper level winds.");
                        }
                    }
                }
            } catch (Exception e) {
                statusHandler.error(qpf_config.getIcao()
                        + ": Failed to extract U & V wind data. ", e);
            }
        }

        /*
         * last resort, try binary correlation constant iwe_move, ins_move,
         * nhit, non1, non2 all returned here
         */
        if (!use_env_winds) {

            BincorDirectResult bdr = bincor_direct(grid1, grid2);
            iwe_move = bdr.jmove;
            ins_move = bdr.imove;
            pb = bdr.pb;

            if (pb < 0.6 && have_model_wind) {
                we_velocity = (float) (model_uwind * (60.0 / 4000.0));
                rns_velocity = - (float) (model_vwind * (60.0 / 4000.0));
                use_env_winds = true;
            } else {
                statusHandler.info(qpf_config.getIcao()
                        + ": Processing BINCOR Direct for UV wind.");
                int currTime = (int) QPFURIFilter.getTime(
                        qpf_config.getCurrentCZ().getDataURI(),
                        qpf_config.getSdf()).getTime() / 1000;
                int earlyTime = (int) QPFURIFilter.getTime(
                        qpf_config.getEarlyCZ().getDataURI(),
                        qpf_config.getSdf()).getTime() / 1000;
                i_minutes = (short) ((currTime - earlyTime) / 60);
                r_minutes = (float) i_minutes;
                we_velocity = iwe_move / r_minutes;
                rns_velocity = ins_move / r_minutes;

            }
        }

        // System.out.println("we_velocity: " + we_velocity);
        // System.out.println("rns_velocity: " + rns_velocity);

        // Make an extrapolation forecast, at 10-minute intervals, of the //
        // ZTR and VIL fields. First field (grid2) is initial time of fore- //
        // cast; grid1 holds instantaneous ZTR image at the projection time.

        for (k = 1; k < QPFUtils.N_FCST; k++) {
            we_displ = (float) (k * 10.0 * we_velocity);
            rns_displ = (float) (k * 10.0 * rns_velocity);
            grid1 = QPFUtils.extrap(grid2, we_displ, rns_displ);

            for (index = 0; index < ScanUtils.SCAN_GRID_DIM_SQ; index++) {
                ztr_fcst[k][index] = grid1[index];
            }
        }

        // Determine local number of grid boxes with ZTR > 24 dbZ during the
        // forecast period, in 3X3 region surrounding the central box.

        for (irow0 = 1; irow0 < QPFUtils.INS - 1; irow0++) {
            for (icol0 = 1; icol0 < QPFUtils.IWE - 1; icol0++) {
                n24dbz_0 = 0;
                for (k = 0; k < QPFUtils.N_FCST; k++) {
                    // Note that in the nested loops below, we use the "<="
                    // loop condition, instead of the usual "<", because we
                    // want to count to 3; the array bounds won'be be
                    // violated due to the bounds of irow0 and icol0.

                    for (irow = irow0 - 1; irow <= irow0 + 1; irow++) {
                        for (icol = icol0 - 1; icol <= icol0 + 1; icol++) {
                            if (ztr_fcst[k][(QPFUtils.INS * irow) + icol] >= 24)
                                n24dbz_0++;
                        }
                    }
                }
                n24dbz[(QPFUtils.INS * irow0) + icol0] = n24dbz_0;
            }
        }

        // Get rainfall amount forecast from ZTR forecast.
        irain_0_30 = QPFUtils.zrrain(ztr_fcst, grid1, izmnt, QPFUtils.MINUTE0,
                QPFUtils.MINUTE30);
        irain_0_60 = QPFUtils.zrrain(ztr_fcst, grid1, izmnt, QPFUtils.MINUTE0,
                QPFUtils.N_FCST - 1);

        // Get VIL forecasts, Now, grid1 holds initial-time VIL,
        // grid2 the instantaneous VIL forecast.

        for (index = 0; index < ScanUtils.SCAN_GRID_DIM_SQ; index++) {
            grid1[index] = curVIL[index];
            vil_0_60[index] = (float) grid1[index];
            vilmax_time[index] = (float) grid1[index];
        }

        // Determine initial-time local VIL maxima in 3X3 box regions.

        for (irow0 = 1; irow0 < QPFUtils.INS - 1; irow0++) {
            for (icol0 = 1; icol0 < QPFUtils.IWE - 1; icol0++) {
                nvil10 = 0;

                // Note that in the nested loops below, we use the "<="
                // loop condition, instead of the usual "<", because we
                // want to count to 3; the array bounds won'be be
                // violated due to the bounds of irow0 and icol0.

                for (irow = irow0 - 1; irow <= irow0 + 1; irow++) {
                    for (icol = icol0 - 1; icol <= icol0 + 1; icol++) {
                        if (grid1[(QPFUtils.INS * irow) + icol] >= 10)
                            nvil10++;
                    }
                }
                nvil10_3x3[(QPFUtils.INS * irow0) + icol0] = nvil10;
            }
        }

        // Determine initial-time local VIL maxima in 7X7 box regions.

        for (irow0 = 3; irow0 < QPFUtils.INS - 3; irow0++) {
            for (icol0 = 3; icol0 < QPFUtils.IWE - 3; icol0++) {
                maxvil7 = 0;

                // Note that in the nested loops below, we use the "<="
                // loop condition, instead of the usual "<", because we
                // want to count to 3; the array bounds won'be be
                // violated due to the bounds of irow0 and icol0.

                for (irow = irow0 - 3; irow <= irow0 + 3; irow++) {
                    for (icol = icol0 - 3; icol <= icol0 + 3; icol++) {
                        index = (QPFUtils.IWE * irow) + icol;
                        if (grid1[index] >= maxvil7)
                            maxvil7 = grid1[index];
                    }
                }
                maxvil_7x7[(QPFUtils.INS * irow0) + icol0] = maxvil7;
            }
        }

        for (k = 1; k < QPFUtils.N_FCST; k++) {
            we_displ = (float) (k * 10.0 * we_velocity);
            rns_displ = (float) (k * 10.0 * rns_velocity);

            grid2 = QPFUtils.extrap(grid1, we_displ, rns_displ);

            for (index = 0; index < ScanUtils.SCAN_GRID_DIM_SQ; index++) {
                vil_0_60[index] += grid2[index];
                // save this to QPF for use on front end
                if (grid2[index] > vilmax_time[index]) {
                    vilmax_time[index] = grid2[index];
                }
            }
        }

        // New code added 21 Jan 1998 - Kitzmiller
        // Dumps out time-average VIL forecast field for later use
        // to determine probability of 1" rainfall from a given
        // storm cell.

        for (index = 0; index < ScanUtils.SCAN_GRID_DIM_SQ; index++) {
            vil_0_60[index] *= QPFUtils.AVG_FCST_PARAM;

            if (vil_0_60[index] < 0) {
                vil_0_60[index] = 0;
            }

            if (vil_0_60[index] > QPFUtils.UPPER_LIMIT) {
                vil_0_60[index] = QPFUtils.UPPER_LIMIT;
            }
            // Copy vil_0_60 array to a short array to use // in SCAN's cell
            // heavy rain probability algorithm.
            itime_avg_vil[index] = (int) (vil_0_60[index] + 0.5);
        }

        // Evaluate probability equations and return forecasts.
        floats = QPFUtils.prob_fcst(irain_0_30, irain_0_60, vil_0_60,
                maxvil_7x7, nvil10_3x3, n24dbz);
        floats.put(QPFRecord.DATA_TYPE.AV_VIL.name(), itime_avg_vil);
        statusHandler.debug("QPF: " + qpf_config.getIcao() + " Generation success...");

    }

    /**
     * Gets the relative EW wind (U)
     * 
     * @return
     */
    public float getWEVelocity() {
        return we_velocity;
    }

    /**
     * Gets the relative NS wind (V)
     * 
     * @return
     */
    public float getRNSVelocity() {
        return rns_velocity;
    }

    /**
     * Gets QPF derived avg direction
     * 
     * @return
     */
    public float getAVGDir() {
        return avg_dir;
    }

    /**
     * Gets QPF derived avg speed
     * 
     * @return
     */
    public float getAVGSpd() {
        return avg_spd;
    }

    /**
     * Gets the main float array section for QPF
     * 
     * @return
     */
    public HashMap<String, float[]> getFloatArrays() {
        return floats;
    }

    /**
     * 
     * Author: Dave Kizmiller (NWS-TDL), Sept. 1995 Translated from Fortran to
     * C++ by M. Churma (RSIS @NWS-TDL)
     * 
     * Description: Determines the best binary pattern match between two images,
     * in terms of the binary correlation coefficient. Assumes that one pattern
     * may be displaced as much as JDISP1 TO JDISP2 in the 1st coordinate, and
     * IDISP1 TO IDISP2 in the 2nd coordinate. This version, started Sep 95,
     * uses directed search along possible trial vectors to find the one
     * yielding maximum correlation.
     * 
     * Variables: grid1, grid2 = Input patterns (dimensioned IWE * INS in
     * calling routine). nhit = Number of 'hits' at max. binary correlation
     * (output). non1, non2 = Number of 'on' points in grid1 and grid2, within
     * bounds of (JMIN,JMAX), (IMIN,IMAX) (output) pb = Maximum binary
     * correlation (output)
     * 
     * jd, id = Current colummn and row displacement being used in attempt to
     * maximize binary correlation. isf = Number of 'on' points in grid1, in
     * region bounded by (JMIN/JMAX,IMIN/IMAX). isfp = Number of 'on' points in
     * grid2, in region bounded by (JMIN+jd/JMAX+jd,IMIN+id/IMAX+id) isffp =
     * Number of simultaneous 'on' points in grid1 and grid2.
     * 
     * Translated to Java 16Feb2009, D Hladky Raytheon
     */

    private BincorDirectResult bincor_direct(int grid1[], int grid2[]) {

        int jmove = 0; 
        int imove = 0;
        int nhit = 0; 
        int non1 = 0; 
        int non2 = 0;        
        // pb, that's our binary correlation value
        float pb = QPFUtils.BINCOR_INITIAL;
        int i = 0;
        int j = 0;
        int id = 0;
        int jd = 0;
        int imove_last = 0;
        int jmove_last = 0;
        int isf = 0;
        int isfp = 0;
        int isffp = 0;
        float pbtemp = 0.0f;
        boolean doSearch = true;
        float binc[][] = new float[QPFUtils.IDISP2 - QPFUtils.IDISP1 + 1][QPFUtils.JDISP2
                - QPFUtils.JDISP1 + 1];
        int icount;

        // Initialize the binary correlation array.

        for (id = QPFUtils.IDISP1; id <= QPFUtils.IDISP2; id++) {
            for (jd = QPFUtils.JDISP1; jd <= QPFUtils.JDISP2; jd++) {
                binc[id + 10][jd + 10] = QPFUtils.BINCOR_INITIAL;
            }
        }

        // Search all 'adjoining' vectors to the last best guess.
        // Do not bother checking vectors already tried before.

        while (doSearch) {
            for (id = imove_last - 2; id <= imove_last + 2; id++) {
                for (jd = jmove_last - 2; jd <= jmove_last + 2; jd++) {

                    try {
                        if (binc[id + 10][jd + 10] >= 0.0) {
                            continue;
                        }
                    } catch (ArrayIndexOutOfBoundsException aioe) {
                        statusHandler.error("jd = "+jd+" id = "+id +"\n"
                        +"jd + 10 = "+(jd+10)+" id + 10 = "+(id + 10) +"\n"
                        +"imove_last = "+imove_last+" jmove_last = "+jmove_last +"\n"
                        +"binc[][] size = "+binc.length, aioe);
                    }

                    // Displace the portion of grid1 that lies between JMIN to
                    // JMAX
                    // and IMIN to IMAX by jd in the j direction and by id in
                    // the
                    // i direction, and determine the binary correlation
                    // obtained
                    // with the corresponding portion of grid2.

                    isf = 0;
                    isfp = 0;
                    isffp = 0;
                    icount = 0;
                    for (i = QPFUtils.IMIN; i <= QPFUtils.IMAX; i++) {
                        for (j = QPFUtils.JMIN; j <= QPFUtils.JMAX; j++) {
                            if ((grid1[QPFUtils.INS * i + j] >= QPFUtils.GRDMIN)
                                    && (grid1[(QPFUtils.INS * i) + j] <= QPFUtils.GRDMAX))
                                isf++;
                            if ((grid2[(QPFUtils.INS * (i + id)) + j + jd] >= QPFUtils.GRDMIN)
                                    && (grid2[(QPFUtils.INS * (i + id)) + j
                                            + jd] <= QPFUtils.GRDMAX)) {
                                isfp++;
                                if ((grid1[(QPFUtils.INS * i) + j] >= QPFUtils.GRDMIN)
                                        && (grid1[(QPFUtils.INS * i) + j] <= QPFUtils.GRDMAX))
                                    isffp++;
                            }
                            icount++;
                        }
                    }
                    binc[id + 10][jd + 10] = 0.0f;

                    if (isffp >= QPFUtils.IMNHIT) {
                        pbtemp = (float) (isffp / Math
                                .sqrt(((float) isf * isfp)));
                        binc[id + 10][jd + 10] = pbtemp;

                        if (pbtemp > (pb)) {
                            // The current direction of displacement (i.e., jd
                            // in the j
                            // direction, id in the i direction) has produced
                            // the best
                            // binary correlation so far.

                            pb = pbtemp;
                            jmove = jd;
                            imove = id;
                            jmove = jd;
                            imove = id;
                            nhit = isffp;
                            non1 = isf;
                            non2 = isfp;
                        }
                    }
                } // end of jd loop
            } // end of id loop

            // If this search has located a new maximum correlation,
            // continue. Otherwise, stop the search and return.

            if ((jmove == jmove_last) && (imove == imove_last)) {
                doSearch = false;
            } else {
                if ((imove > QPFUtils.IDISP1) && (imove < QPFUtils.IDISP2)
                        && (jmove > QPFUtils.JDISP1)
                        && (jmove < QPFUtils.JDISP2)) {
                    jmove_last = jmove;
                    imove_last = imove;
                } else {
                    doSearch = false;
                }
            }
        } // end of while loop
        
        BincorDirectResult r = new BincorDirectResult();
        r.jmove = jmove;
        r.imove = imove;
        r.nhit = nhit;
        r.non1 = non1;
        r.non2 = non2;
        r.pb = pb;
        return r;
    }

    private class BincorDirectResult {
        public int jmove;
        public int imove;
        public int nhit;
        public int non1;
        public int non2;
        public float pb;
    }

}
