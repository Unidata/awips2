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
package com.raytheon.uf.common.dataplugin.qpf;

import java.util.HashMap;

import com.raytheon.uf.common.dataplugin.qpf.QPFRecord.DATA_TYPE;
import com.raytheon.uf.common.monitor.scan.ScanUtils;

/**
 * 
 * QPF Utilities, mostly static methods.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/11/2009   1981       dhladky    Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class QPFUtils {

    public static int INS = 116; // Number of north-south grid points

    public static int IWE = 116; // Number of east-west grid points

    public static float GRDMAX = 80.0f;

    public static float GRDMIN = 15.0f; // GRDMIN,GRDMAX: Min and max values of

    // reflectivity taken to represent valid
    // precip echoes (15 & 80 dBZ)
    public static int JMIN = 11; // Minimum north-south coordinate to be used.

    public static int JMAX = 103; // Maximum north-south coordinate to be used.

    public static int IMIN = 11; // Minimum east-west coordinate to be used.

    public static int IMAX = 103; // Maximum east-west coordinate to be used.

    public static int JDISP1 = -10; // J-axis initial displacement

    public static int JDISP2 = 10; // J-axis final displacement

    public static int IDISP1 = -10; // I-axis initial displacement

    public static int IDISP2 = 10; // I-axis final displacement

    public static int JDINC = 1;

    public static int IDINC = 1; // JDINC, IDINC: Grid-box increments used in

    // searching

    // for a displacement vector that yields the maximum
    // lag-correlation patern match .
    public static int IMNHIT = 25; // Minimum number of binary "hits" required

    // to

    // achieve a satisfactory pattern match.

    public static float RMISS = 999.0f; // Missing value indicator for ztr data

    public static int NTIMES = 5; // Number of radar images to be ingested for

    // pattern matching (generally 5-6)
    public static int N_FCST = 7; // Number of images in extrapolation forecast

    // sequence, starting at 0, 10-minute intervals
    // (generally 7 to cover 1 hour)

    public static int MIN_STI_CELLS = 5; // Minimum number of STI cells required

    // to

    // used average cell speed for vector motion.
    public static int SECONDS_PER_HOUR = 3600;

    public static int SECONDS_PER_MINUTE = 60;

    public static int MINUTES_PER_HOUR = 60;

    public static float BAD_Z = 100.0f; // Maximum "good" value in reflectivity

    // field (float)

    public static int IBAD_Z = 100; // Maximum "good" value in reflectivity

    // field (int)

    public static float BAD_VIL = 120.0f; // Maximum "good" value in VIL field

    // (int)

    // needed for passing into extrap??
    public static int IBAD_VIL = 120; // Maximum "good" value in VIL field (int)

    // needed for passing into extrap??
    public static int MINUTE0 = 0; // Radar image number corresponding to

    // initial time

    // forecast.
    public static int MINUTE30 = 3; // Radar image number corresponding to

    // 30-minute

    // forecast
    public static int MISSING = -9999;

    public static int MISS = 999;

    public static int NCATR = 5; // Number of rainfall category levels.

    public static float BINCOR_INITIAL = -99.0f; // Initial gridpoint values

    // before binary correlation.

    public static int NUMBER_OF_PRODUCTS = 5; // Number of QPF output products.

    public static int FCST_TIME_INTERVAL = 10; // Minutes between forecast

    // extrapolations.

    public static float AVG_FCST_PARAM = (float) (1.0 / ((MINUTES_PER_HOUR / FCST_TIME_INTERVAL) + 1));

    // Constant used to multiply summed TAV
    // values by inverse the number of forecasts.
    // (more efficient than dividing).
    public static int LOWPOINT = 52; // Low grid point for ground clutter filter

    public static int HIGHPOINT = 63; // High grid point for ground clutter

    // filter

    public static int UPPER_LIMIT = 99;

    /*
     * Written originally in Fortran (HP-UX) by David Kitzmiller, NWS-TDL
     * Translated into C++ by Mike Churma, RSIS @ TDL, Nov 1999
     * 
     * Description: Calculates surface rain accumultion (1/100's inches) from a
     * time sequence of reflectivity images. Utilizes WSR-88D Z=300R1.4
     * relationship
     * 
     * Variables ztr = Input forecast reflectivity grids in dBZ. irain = Output
     * Rainfall accumulation grid, in 1/100 inches iwork = Input work array
     * izmnt = Array of reference times (in minutes) corresponding to the ztr
     * reflectivity images. The first value may be 0, the second the elapsed
     * time between the 1st and 2nd images, etc. (input) itim1, itim2 = Rain
     * accumulation will be calculated between ztr images with temporal
     * dimension itim1 and itim2 inclusive. (input) index = Array index counter.
     * dbz = Holds an individual reflectivity value (for code clarity). timediff
     * = difference between izmnt times of two images. rrate = Rain rate. rrate1
     * = Rain rate at the beginning of the sub-period. rrate2 = Rain rate at the
     * end of the sub-period. IF ABS(ZTR()) .GE. RMISS, NO RAINFALL CALCULATED
     * FOR THAT GRID POINT
     * 
     * Translated to Java 16Feb2009, D Hladky Raytheon
     */

    public static long[] zrrain(int[][] ztr, int[] iwork, int[] izmnt,
            int itim1, int itim2) {

        long[] irain = new long[ScanUtils.SCAN_GRID_DIM_SQ];
        int index;
        int nt;
        float rain1;
        float rrate1;
        float rrate2;
        float dbz = 0.0f;
        float rrate = 0.0f;
        float z = 0.0f;
        float timediff;

        // Calculate instantaneous rainrate at initial time.

        for (index = 0; index < ScanUtils.SCAN_GRID_DIM_SQ; index++) {
            dbz = ztr[itim1][index];

            // if the abs value of dbz is equal to or greater than the
            // missing indicator, then flag the rain value in that
            // grid box as missing and don't perform Z to R calculations.

            if (Math.abs(dbz) >= RMISS) {
                // Put missing marker into accumulation array.
                irain[index] = MISSING;
            } else {
                rrate = 0.0f;
                if (dbz > 0) {
                    z = (float) Math.pow(10.0, (0.1 * dbz));
                    rrate = (float) Math.pow((z / 300.0), 0.71429);

                    // Convert rainrate from mm/hr to inches/minute * 10,000
                    rrate = (float) ((rrate / 25.4) / 60.0);
                    rrate *= 10000.0;
                }
            }
            iwork[index] = (int) (rrate + 0.5);
        }

        // Calculate rain accumulations

        for (nt = itim1 + 1; nt <= itim2; nt++) {
            timediff = izmnt[nt] - izmnt[nt - 1];

            for (index = 0; index < ScanUtils.SCAN_GRID_DIM_SQ; index++) {
                if (irain[index] < -1)
                    continue;

                dbz = ztr[nt][index];

                if (Math.abs(dbz) >= RMISS) {
                    irain[index] = MISSING;
                    continue;
                }

                rrate = 0.0f;
                if (dbz > 0) {
                    z = (float) Math.pow(10.0, (0.1 * dbz));
                    rrate = (float) Math.pow((z / 300.0), 0.71429);

                    // Convert rainrate from mm/hr to inches/minute * 10,000
                    rrate = (float) ((rrate / 25.4) / 60.0);
                    rrate *= 10000.0;
                }

                // Rainrate over the point is assumed to be mean of rates
                // at beginning and end of the subperiod.

                rrate1 = iwork[index];
                rrate2 = rrate;
                iwork[index] = (int) (rrate + 0.5);
                rrate = (float) (0.5 * (rrate1 + rrate2));
                rain1 = timediff * rrate;
                irain[index] += (long) (rain1 + 0.5);
            }
        }

        for (index = 0; index < ScanUtils.SCAN_GRID_DIM_SQ; index++) {
            if (irain[index] > -1)
                irain[index] = irain[index] / 100;
            if (irain[index] < -1)
                irain[index] = 32000;
        }

        return irain;
    }

    /*
     * Author: Dave Kitzmiller & Ator TDL Aug 1990 HPUX Update: Translated from
     * FORTRAN to C++ by M. Churma (RSIS @ TDL) Nov 1999
     * 
     * Description:
     * 
     * Extrapolates elements in a 2D field (gridIn) using a constant
     * displacement vector (we_displ, rns_displ) and puts the result in and
     * output grid (gridOut).
     * 
     * Variables: gridIn, gridOut = Input and output grids, dimensioned in the
     * calling routine. we_displ = Number of grids points in the West-East
     * direction to displace the gridIn field in order to get gridOut. (float).
     * rns_displ = Number of grids points in the North-South direction to
     * displace the gridIn field in order to get gridOut. (float).
     * 
     * BADDT = MAXIMUM 'GOOD' VALUE IN PIN(), VALUES .GT. BADDT ARE ASSUMED TO
     * BE 'MISSING' (INPUT, REAL4) IBADDT = VALUE ASSIGNED TO 'MISSING' POINTS
     * IN POUT() (INPUT, INTEGER4) irow, icol = Grid row and column indices
     * (short). newrow, newcol = Grid row and column indices after displacement
     * from original coordinates (iow, icol). index = 1-D array counter for the
     * whole 116x116 field. newindex = 1-D array counter for displaced 116x116
     * field. rounderWE, rounderNS = Variables to allow rounding to the nearest
     * integer value, depending on the +/- sign of the floating point value to
     * be rounded.
     * 
     * Translated to Java 16feb2009 D Hladky Raytheon
     */

    public static int[] extrap(int[] gridIn, float we_displ, float rns_displ) {
        int irow, icol, index;
        int newrow, newcol, newindex;
        float rounderWE, rounderNS;
        int[] gridOut = new int[ScanUtils.SCAN_GRID_DIM_SQ];

        // First initialize the entire gridOut OUTPUT ARRAY TO ZEROES.

        for (index = 0; index < gridOut.length; index++) {
            gridOut[index] = 0;
        }

        if (rns_displ >= 0)
            rounderNS = (float) 0.5;
        else
            rounderNS = (float) -0.5;

        if (we_displ >= 0)
            rounderWE = (float) 0.5;
        else
            rounderWE = (float) -0.5;

        for (irow = 0; irow < INS; irow++) {
            for (icol = 0; icol < IWE; icol++) {
                index = IWE * irow + icol;

                if (gridIn[index] != 0) {
                    if (gridIn[index] <= BAD_Z) {
                        // The "gridIn" array element contains a valid radar
                        // echo
                        // value to be displaced in the gridOut array.

                        newrow = irow + (int) (rns_displ + rounderNS);
                        newcol = icol + (int) (we_displ + rounderWE);

                        if (((newrow >= 0) && (newrow < INS))
                                && ((newcol >= 0) && (newcol < IWE))) {
                            newindex = (IWE * newrow) + newcol;

                            // The echo can be displaced within the array size
                            // limits.
                            // Make sure that an already existing bad radar
                            // return value
                            // will not be overwritten before shifting the echo
                            // value.

                            if (gridOut[newindex] < IBAD_Z)
                                gridOut[newindex] = gridIn[index];
                        }
                    } else {
                        // The "gridIn" array element contains bad radar data,
                        // so write
                        // a bad data value in the corresponding pixel of
                        // "gridOut".

                        gridOut[index] = IBAD_Z;

                    }
                }
            }
        }

        return gridOut;
    }

    /*
     * Author: Dave Kitzmiller, TDL
     * 
     * Description: Analyzes 0-1 hr rainfall amt probabilities using OK-KS-TX
     * warm season equations.
     * 
     * Updates: Nov 28 1995 - Also have code for initial stage3-based equations.
     * - Kitzmiller
     * 
     * Feb 27 1998 - Kitzmiller Added a routine 'cat_to_amt' to convert the
     * forecast rainfall category field (in which all grid boxes have values
     * 0-5) to a rainfall amount field, with values 0-75, IN .01 inches.
     * 
     * Nov 1999 - Churma (RSIS @NWS-TDL) Translated from Fortran to C-C++
     * 
     * Variables: vil60_max, vil_smth = Local 0-60 minute VIL maximum and
     * average. rain30_max, rain_smth = Local 0-30 minute rainfall maximum and
     * average. rain60_max, rain60_smth = Local 0-60 minute rainfall maximum and
     * average. irow, icol = Row and column loop counters irow0, icol0 =
     * Secondary row and column loop counters index = 1-D array counter. index0
     * = Secondary 1-D array counter.
     * 
     * Statistical rainfall predictor fields:
     * 
     * irain_0_30[], irain_0_60[] = Rainfall amount forecasts for 0-30 and 0-60
     * minutes, used as rainfall amount statistical predictors, dimensioned
     * IWE,INS. (input) vil_0_60[] = Mean VIL forecasted by extrapolation, 0-60
     * minutes, dimensioned IWE,INS (input) vilmax_time[] = Maximum VIL over
     * each grid box during 0-60 min period, forecasted by extrapolation (input)
     * maxvil_7x7() = Maximum initial time VIL, within 7x7 box region centered
     * on each grid box (input) n24dbz[] = Over 3x3-box region, number of grid
     * boxes and time periods in which reflectivity forecast exceeds 24 dBZ
     * during 0-60 minute period (input) nvil10_3x3() = Number of grid boxes in
     * 3x3-region with initial-time VIL >= 10 kg/m^2 (input)
     * 
     * Output fields:
     * 
     * p10[],p25[],p50[], p75[] = Probability of rainfall exceeding the 4 amount
     * thresholds described above %. Dimensioned IWE,INS. rain_cat[] = Rainfall
     * amount forecast derived from probabilities, in categories. Dimensioned
     * IWE,INS. precip_amt[] = Rainfall amount forecast, in .01 inch rainval[] =
     * Breakpoints of rainfall categories, in .01 inch, dimensioned NCATR
     * 
     * Translated to JAVA 16Feb2009 D Hladky Raytheon
     */
    public static HashMap<String, float[]> prob_fcst(long[] irain_0_30,
            long[] irain_0_60, float[] vil_0_60, int[] maxvil_7x7,
            int[] nvil10_3x3, int[] n24dbz) {

        float rain60_smth;
        float rain60_max;
        float vil60_max;
        float vil_smth;
        float rain30_smth;
        float[] pwork = new float[ScanUtils.SCAN_GRID_DIM_SQ];
        double p10x;
        double p25x;
        double p50x;
        double p75x;
        double rain_catx;
        float[] p10 = new float[ScanUtils.SCAN_GRID_DIM_SQ];
        float[] p25 = new float[ScanUtils.SCAN_GRID_DIM_SQ];
        float[] p50 = new float[ScanUtils.SCAN_GRID_DIM_SQ];
        float[] p75 = new float[ScanUtils.SCAN_GRID_DIM_SQ];
        float[] rain_cat = new float[ScanUtils.SCAN_GRID_DIM_SQ];
        int irow;
        int icol;
        int irow0;
        int icol0;
        int index;
        int index0;

        // Evaluate 4 probability equations at each grid box. Note
        // that the outermost rows and columns are not looped
        // through, because adjoining grid data is needed for the
        // calculations.

        for (irow = 1; irow < INS - 1; irow++) {
            for (icol = 1; icol < IWE - 1; icol++) {
                index = IWE * irow + icol;

                p10[index] = RMISS;
                p25[index] = RMISS;
                p50[index] = RMISS;
                p75[index] = RMISS;
                rain_cat[index] = MISS;

                // Look to see if 900 can be replaced by a definition (it's
                // looking for a missing value)
                if (irain_0_60[index] <= 900) {
                    // Form local averages and local maxima for predictors.

                    rain60_smth = 0;
                    rain60_max = 0;
                    vil60_max = 0;
                    vil_smth = 0;
                    rain30_smth = 0;

                    for (irow0 = irow - 1; irow0 <= irow + 1; irow0++) {
                        for (icol0 = icol - 1; icol0 <= icol + 1; icol0++) {
                            index0 = (IWE * irow0) + icol0;

                            rain60_smth += irain_0_60[index0];
                            rain30_smth += irain_0_30[index0];
                            vil_smth += vil_0_60[index0];

                            if (irain_0_60[index0] > rain60_max)
                                rain60_max = irain_0_60[index0];
                            if (vil_0_60[index0] > vil60_max)
                                vil60_max = vil_0_60[index0];
                        }
                    }

                    rain60_smth /= 9.0;
                    vil_smth /= 9.0;
                    rain30_smth /= 9.0;

                    // Final Stage3-based equations - 22Mar+16Apr 1996 -
                    // Kitzmiller

                    p10x = 0.412 + 3.517 * vil_smth + 0.980 * n24dbz[index]
                            + 0.704 * maxvil_7x7[index];

                    p25x = -0.331 + 2.701 * vil_smth + 0.356 * rain30_smth
                            + 0.469 * maxvil_7x7[index] + 0.230 * n24dbz[index];

                    p50x = 0.144 + 2.074 * vil_smth + 0.249 * rain30_smth;
                    p75x = 0.112 + 1.125 * vil_smth + 1.905 * nvil10_3x3[index];

                    if (p10x < 0.0)
                        p10x = 0.0;
                    else if (p10x > 99.0)
                        p10x = 99.0;

                    if (p25x < 0.0)
                        p25x = 0.0;
                    else if (p25x > 99.0)
                        p25x = 99.0;

                    if (p50x < 0.0)
                        p50x = 0.0;
                    else if (p50x > 99.0)
                        p50x = 99.0;

                    if (p75x < 0.0)
                        p75x = 0.0;
                    else if (p75x > 99.0)
                        p75x = 99.0;

                    p10[index] = (float) p10x;
                    p25[index] = (float) p25x;
                    p50[index] = (float) p50x;
                    p75[index] = (float) p75x;

                    // Probability-to-categorical conversions developed Feb
                    // 1998.
                    // Added threshold for .01 inch precip categorical fcst, Oct
                    // 1996
                    // (Kitzmiller)

                    rain_catx = 0.0;

                    if (p10x >= 7.0)
                        rain_catx = 1.0;
                    if (p10x >= 33.0)
                        rain_catx = 2.0;
                    if ((rain_catx >= 2.0) && (p25x >= 30.0))
                        rain_catx = 3.0;
                    if ((rain_catx >= 3.0) && (p50x >= 22.0))
                        rain_catx = 4.0;
                    if ((rain_catx >= 4.0) && (p75x >= 14.0))
                        rain_catx = 5.0;

                    // Assign float cat value to the short array.

                    rain_cat[index] = (int) rain_catx;

                } // End of if statement.
            } // End of icol loop.

        } // End of irow loop.

        HashMap<String, float[]> floats = new HashMap<String, float[]>();
        floats.put(DATA_TYPE.PRECIP_AMT.name(), cat_to_amt(rain_cat, pwork));
        floats.put("RAIN_CAT", rain_cat);
        floats.put(DATA_TYPE.P10.name(), p10);
        floats.put(DATA_TYPE.P25.name(), p25);
        floats.put(DATA_TYPE.P50.name(), p50);
        floats.put(DATA_TYPE.P75.name(), p75);

        return floats;
    }

    /*
     * Author: Dave Kitzmiller TDL Feb 1998 HPUX Update: Translated from FORTRAN
     * to C++ by M. Churma (RSIS @ TDL) Nov 1999
     * 
     * Additional changes: Removed boundary initialization andsecondary
     * smoothing; changed smoothing loop to fix "holes" in precip data. - Churma
     * & Kitzmiller, MDL, July 2001.
     * 
     * Description: Creates a precipitation amount field from a gridded precip
     * category field, by assuming that boundaries between category amounts
     * correspond to isohyets. Precip values greater than zero between the
     * isohyets are derived by interpolation through a Cressman-type scheme.
     * 
     * Variables: rain_cat = Input category field (values 0 to NCATR)
     * dimensioned IWE INS in calling routine. precip_amt = Output precip amount
     * field, dimensioned IWE INS in calling routine pwork = Input float array,
     * dimensioned IWE INS.
     * 
     * rainval = Float vector containing the lowest rainfall amount in within
     * each of the NCAT categories, in hundredths of an inch. irow, icol, irow0,
     * icol0 = Row & column counters (short) index = array counter for the whole
     * 116x116 field. dist = Gridpoint displacement distance (short) wgt =
     * Weighted distance sum = Sum of weighted precip values. wgtsum = Sum of
     * weighting values. irad = Calculation counter npt = Calculation counter
     */
    public static float[] cat_to_amt(float rain_cat[], float pwork[]) {
        int irow;
        int icol;
        int irow0;
        int icol0;
        int npt;
        int irad;
        int i1;
        int j1;
        int i2;
        int j2;
        int index;
        float[] rainval = new float[NCATR];
        float dist;
        float wgt;
        float sum;
        float wgtsum;
        float[] precip_amt = new float[ScanUtils.SCAN_GRID_DIM_SQ];

        // Initialize rainfall value array. Values are in hundredths of an inch.
        rainval[0] = 5.0f;
        rainval[1] = 18.0f;
        rainval[2] = 38.0f;
        rainval[3] = 63.0f;
        rainval[4] = 75.0f;

        // Initialize the output array.

        for (index = 0; index < ScanUtils.SCAN_GRID_DIM_SQ; index++) {
            precip_amt[index] = 0.0f;
            int icat = (int) rain_cat[index];

            // Check boundaries
            if (icat < 1) {
                continue;
            }
            if (icat > NCATR) {
                icat = NCATR;
            }
            // icat - 1 used because C array starts at 0.
            precip_amt[index] = rainval[icat - 1];
        }

        // Get smoothed field in pwork[], leaving 0-category values at
        // 0 precip amount. Smoothing radius is set a 2 grid boxes,
        // which has worked well.

        for (irow = 0; irow < INS; irow++) {
            for (icol = 0; icol < IWE; icol++) {
                index = (INS * irow) + icol;
                if (precip_amt[index] <= 0) {
                    pwork[index] = precip_amt[index];
                } else {
                    irad = 2;
                    do {
                        j1 = irow - irad;
                        j2 = irow + irad;

                        // Check boundaries

                        if (j1 < 0) {
                            j1 = 0;
                        }
                        if (j2 > INS - 1) {
                            j2 = INS - 1;
                        }

                        i1 = icol - irad;
                        i2 = icol + irad;

                        // Check boundaries

                        if (i1 < 0) {
                            i1 = 0;
                        }
                        if (i2 > IWE - 1) {
                            i2 = IWE - 1;
                        }

                        sum = 0;
                        wgtsum = 0;
                        npt = 0;

                        for (irow0 = j1; irow0 <= j2; irow0++) {
                            for (icol0 = i1; icol0 <= i2; icol0++) {
                                dist = ((icol0 - icol) * (icol0 - icol))
                                        + ((irow0 - irow) * (irow0 - irow));
                                if (dist > 0.0) {
                                    wgt = (float) (1.0 / dist);
                                } else {
                                    wgt = 1.0f;
                                }

                                sum = sum + wgt
                                        * precip_amt[(INS * irow0) + icol0];
                                wgtsum += wgt;
                                npt++;
                            }
                        }
                        if (npt < 2) {
                            ++irad;
                        }
                    } while (npt < 2 && irad <= 50);

                    // wgtsum should never be able to equal 0, but check just in
                    // case.

                    if (wgtsum != 0) {
                        pwork[index] = sum / wgtsum;
                    } else {
                        pwork[index] = 0;
                    }
                }
            }
        }

        // Assign smoothed pwork values to output array.
        for (index = 0; index < ScanUtils.SCAN_GRID_DIM_SQ; index++) {
            if (precip_amt[index] != 0.0f) {
                precip_amt[index] = (float) (pwork[index] * 0.01);
            }
        }

        return precip_amt;
    }
}
