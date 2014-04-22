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
package com.raytheon.viz.core.contours.util;

import java.text.DecimalFormat;
import java.text.ParseException;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

import com.raytheon.uf.common.numeric.array.FloatArray2DWrapper;
import com.raytheon.uf.common.numeric.filter.InvalidRangeFilter;
import com.raytheon.uf.common.numeric.source.AxisSwapDataSource;
import com.raytheon.uf.common.numeric.source.DataSource;

/**
 * Port of the fortCon.f routine. Minimal changes made to make it perform better
 * in java. Not object orientated and not thread safe!
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 22, 2010  4583     rjpeter     Initial creation
 * Feb 27, 2014  2791     bsteffen    Use DataSource for generic data access.
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class FortConBuf {
    private static final byte b02 = 0x02;

    private static final byte b12 = 0x12;

    private static final byte b40 = 0x40;

    private static final byte b52 = 0x52;

    private static final byte bC0 = (byte) 0xc0;

    private static final byte bD2 = (byte) 0xd2;

    private static final int maxContours = 500;

    private static final byte[] CMask = new byte[maxContours];

    static {
        byte mask = 0x1;
        for (int i = 0; i < CMask.length; i++) {
            CMask[i] = mask;
            mask <<= 1;
            if (mask == 0) {
                mask = 1;
            }
        }
    }

    private PointBuffer ijPntBuffer = new PointBuffer(2000, 1000);

    private DataSource dataToContour;

    private int nxMaxIndex;

    private int nyMaxIndex;

    private byte[][] work1;

    private byte[][] work2;

    private byte[][] work3;

    private byte[][] work4;

    private float[] ConVal = new float[maxContours];

    private boolean[] ConNo = new boolean[maxContours];

    private float[] MnAvg = new float[maxContours];

    private float[] MxAvg = new float[maxContours];

    private int c;

    private int labsep2;

    private ContourContainer rval;

    private FortConBuf() {

    }

    private static Queue<FortConBuf> instancePool = new ConcurrentLinkedQueue<FortConBuf>();

    public static ContourContainer contour(float[][] data, FortConConfig config) {
        int ny = data.length;
        int nx = data[0].length;
        DataSource source = new FloatArray2DWrapper(data, nx, ny);
        source = new AxisSwapDataSource(source);
        if (config.badlo < config.badhi) {
            source = InvalidRangeFilter.apply(source, config.badlo,
                    config.badhi);
        }
        return contour(source, ny, nx, config);
    }

    /**
     * Perform contouring. This method ignores config.badlo and config.badhi and
     * only treats NaN as bad data.
     * 
     * @param data
     *            the source for data to contouring.
     * @param nx
     *            The number of points to contour in the x direction
     * @param ny
     *            The number of points to contour in the y direction
     * @param config
     *            configuration options
     * @return a ContourContainer
     */
    public static ContourContainer contour(DataSource data, int nx, int ny,
            FortConConfig config) {

        ContourContainer rval = null;
        FortConBuf instance = null;
        try {
            instance = instancePool.poll();
            if (instance == null) {
                instance = new FortConBuf();
            }

            rval = instance.contourInternal(data, nx, ny, config);
        } finally {
            if (instance != null) {
                instancePool.add(instance);
            }
        }

        return rval;
    }

    private ContourContainer contourInternal(DataSource data, int nx, int ny,
            FortConConfig config) {
        dataToContour = data;

        nxMaxIndex = nx - 1;
        nyMaxIndex = ny - 1;
        ijPntBuffer.setSize(0);
        ijPntBuffer.setXOffset(config.xOffset);
        ijPntBuffer.setYOffset(config.yOffset);

        double rawmax = -Double.MAX_VALUE;
        double rawmin = Double.MAX_VALUE;
        work1 = new byte[nx][ny];
        work2 = new byte[nx][ny];
        work3 = new byte[nx][ny];
        work4 = new byte[nx][ny];

        // contourCount is number of contour values
        int contourCount, mmm;
        int i, j, xmode, turn1, turn2, turn3, turn4, ii, jj;

        double val, val1, val2, dval, minval, maxval, D2;
        Byte bbb;

        // map out missing values
        // Figure out which vertical sides can accept contours.
        for (int cIdx = 0; cIdx < nx; cIdx++) {
            val2 = dataToContour.getDataValue(cIdx, nyMaxIndex);
            for (int rIdx = nyMaxIndex - 1; rIdx >= 0; rIdx--) {
                val1 = dataToContour.getDataValue(cIdx, rIdx);
                if (val1 < val2) {
                    if (val1 < rawmin) {
                        rawmin = val1;
                    }
                    work4[cIdx][rIdx] = b40;
                } else if (Double.isNaN(val1) || Double.isNaN(val2)) {
                    work4[cIdx][rIdx] = 1;
                } else {
                    if (val2 < rawmin) {
                        rawmin = val2;
                    }
                    if (val2 < val1) {
                        work4[cIdx][rIdx] = bC0;
                    }
                }
                val2 = val1;
            }
        }

        // Figure out which horizontal sides can accept contours.
        for (int rIdx = 0; rIdx < ny; rIdx++) {
            val2 = dataToContour.getDataValue(nxMaxIndex, rIdx);
            for (int cIdx = nxMaxIndex - 1; cIdx >= 0; cIdx--) {
                val1 = dataToContour.getDataValue(cIdx, rIdx);
                if (val1 < val2) {
                    if (val2 > rawmax) {
                        rawmax = val2;
                    }
                    work3[cIdx][rIdx] = b40;
                } else if (Double.isNaN(val1) || Double.isNaN(val2)) {
                    work3[cIdx][rIdx] = 1;
                } else {
                    if (val1 > rawmax) {
                        rawmax = val1;
                    }
                    if (val2 < val1) {
                        work3[cIdx][rIdx] = bC0;
                    }
                }
                val2 = val1;
            }
        }

        // Handle case of no good points.
        if (rawmin >= rawmax) {
            return new ContourContainer(0);
        }

        // Scale absolute extrema from grid as specified.
        minval = rawmin;
        maxval = rawmax;

        // Limit number of contours by size of work arrays, check for alternate
        // modes.
        if (config.mode > 0) {
            xmode = config.mode / 1000;
            mmm = Math.min(maxContours, config.mode - xmode * 1000);
        } else {
            xmode = (-config.mode) / 1000;
            mmm = Math.max(-maxContours, config.mode + xmode * 1000);
        }

        if (xmode == 1) {
            if (mmm <= 0) {
                minval = Math.max(minval, config.seed[1]);
                maxval = Math.min(maxval, config.seed[2]);
            }
            xmode = 0;
        } else if (xmode == 2) {
            if (mmm <= 0) {
                xmode = 0;
            } else if (config.seed[0] < 0.0
                    || (mmm < config.seed.length && config.seed[mmm] < config.seed[1])) {
                // added mmm to seed.length check, perhaps always enter if mmm <
                // seed.length also?
                xmode = 0;
            } else {
                mmm = Math.min(maxContours / 2, mmm);
            }
        } else {
            xmode = 0;
        }

        // not enumerated contours
        if (mmm <= 0) {
            // Make sure contouring increment has some finite positive value and
            // that no more contours are drawn than we have room for in work
            // arrays
            val = (maxval - minval) / maxContours;
            dval = config.seed[0];
            if (dval < val) {
                dval = val;
                if (mmm == 0) {
                    mmm = -25;
                }
            }

            // Handle case of routine picks contouring interval
            if (mmm < 0) {
                dval = (float) Math.pow(10.0,
                        ((int) (Math.log10(dval) + 100.01)) - 100);
                float t10 = -5 * mmm;
                float t5 = -2 * mmm;
                float t2 = -1 * mmm;
                val = (maxval - minval) / dval;

                while (val > t10) {
                    dval *= 10;
                    val = (maxval - minval) / dval;
                }

                if (val > t5) {
                    dval *= 5;
                }
                if (val > t2) {
                    dval *= 2;
                }
            }

            // force offset to be in same format as outputted to screen
            // e.g. dont calculate contours with an offset of 1.5 when
            // contours are printed as integers, will cause contours
            // to go missing in that case
            if (config.labelFormat != null) {
                DecimalFormat df = new DecimalFormat(config.labelFormat);
                float temp = Float.valueOf(df.format(dval));
                if (temp != 0) {
                    dval = temp;
                }
            }
            // See to it that contour values are multiples of dval
            val1 = dval * Math.round(minval / dval);
            if (val1 < minval) {
                val1 += dval;
            }
            val2 = dval * Math.round(maxval / dval);
            if (val2 > maxval) {
                val2 -= dval;
            }

            if (val1 > val2) {
                return new ContourContainer(0);
            }

            int numSteps = (int) ((val2 - val1) / dval + 1);
            val = val1;
            for (contourCount = 0; contourCount < numSteps; contourCount++) {
                ConVal[contourCount] = (float) val;
                val = val + dval;
            }
            // remove ncon-- as now ncon is the number of contour values
        } else {
            // user enumerated contours
            contourCount = -1;
            dval = Float.MAX_VALUE;
            val1 = Float.MAX_VALUE;
            val2 = -Float.MAX_VALUE;
            boolean mirror = xmode == 2;
            boolean ascend = ((config.seed[0] > config.seed[mmm - 1]) != mirror);
            int seedsgn = 1;
            if (mirror) {
                seedsgn = -1;
            }

            int iq, jq, kq;
            boolean continueWork = true;
            // mirroring loop
            while (continueWork) {
                continueWork = false;
                // Handle a negative scale and/or descending order in seed
                // values.
                if (ascend) {
                    iq = mmm - 1;
                    jq = 0;
                    kq = -1;
                } else {
                    iq = 0;
                    jq = mmm;
                    kq = 1;
                }

                float seedwrk = 0;
                // Load values for enumerated contours into internal work array,
                // record max, min and approximation of delta value.
                for (c = iq; (kq > 0 && c < jq) || (kq < 0 && c >= jq); c += kq) {
                    seedwrk = seedsgn * config.seed[c];
                    if ((mirror && seedwrk == 0)
                            || (seedwrk < minval || seedwrk > maxval)) {
                        continue;
                    }
                    if (contourCount > 0) {
                        val = Math.abs(seedwrk - ConVal[contourCount]);
                        if (val != 0) {
                            dval = Math.min(dval, val);
                        }
                    }
                    if (seedwrk != 0) {
                        dval = Math.min(dval, Math.abs(seedwrk));
                    }
                    val1 = Math.min(val1, seedwrk);
                    val2 = Math.max(val2, seedwrk);
                    contourCount++;
                    ConVal[contourCount] = seedwrk;
                    if (contourCount >= maxContours - 1) {
                        break;
                    }
                    // see if we loop back for xmode == 2, negative version of
                    // input list
                    if (mirror) {
                        mirror = false;
                        ascend = !ascend;
                        seedsgn *= -1;
                        if (contourCount < maxContours - 1) {
                            continueWork = true;
                        }
                    }
                }

                // ncon no longer index and is now the count of contour vals
                contourCount++;
            }
        } // end loading contour value array16

        contourCount = formatContourValues(config, contourCount);

        // Get plotting coordinates ranges ... unused RJP
        // 1939 Continue
        // nxm=nx-1
        // nym=ny-1

        if (config.generateMins || config.generateMaxes) {
            labsep2 = config.labelSpacingOverall + 1;
            // set mmfmt and mmlen

            // mmtol processing always amount to 0
            float dmm = 0;
            double mmlim1 = 0;
            double mmlim2 = 0;

            // Figure out limits of values which may be marked for.
            if (mmm > 1 && val2 < 0) {
                mmlim1 = minval;
                mmlim2 = val2;
            } else if (mmm > 1 && val1 > 0) {
                mmlim1 = val1;
                mmlim2 = maxval;
            } else {
                mmlim1 = minval;
                mmlim2 = maxval;
            }
            // Loop for searching for Maxes and Mins
            int jm = 0;
            j = 1;
            for (int jp = 2; jp < ny; jm = j, j = jp, jp++) {
                int im = 0;
                i = 1;
                MIN_MAX_SEARCH: for (int ip = 2; ip < nx; im = i, i = ip, ip++) {
                    // check that this is not already too close to
                    // something marked
                    bbb = work4[i][j];
                    if ((bbb & b02) == b02) {
                        continue MIN_MAX_SEARCH;
                    }
                    // check that this falls within markable limits
                    val = dataToContour.getDataValue(i, j);
                    if (val < mmlim1 || val > mmlim2) {
                        continue MIN_MAX_SEARCH;
                    }
                    // decide type of extrema that this could be
                    if (config.generateMins && work3[i][j] >= 2) {
                        // check if this point is a valid local min
                        if (work4[i][j] < 2 || work3[im][j] > -2
                                || work4[i][jm] > -2) {
                            continue MIN_MAX_SEARCH;
                        }
                        // check corner points
                        if ((work3[im][jm] >= 2 && work4[im][jm] >= 2 && val >= dataToContour
                                .getDataValue(im, jm))
                                || (work3[im][jp] >= 2 && work4[im][j] <= -2 && val >= dataToContour
                                        .getDataValue(jp, im))
                                || (work3[i][jm] <= -2 && work4[ip][jm] >= 2 && val >= dataToContour
                                        .getDataValue(ip, jm))
                                || (work3[i][jp] <= -2 && work4[ip][j] <= -2 && val >= dataToContour
                                        .getDataValue(ip, jp))) {
                            continue MIN_MAX_SEARCH;
                        }
                        // sharp min check
                        if (dmm > 0) {
                            val = val + dmm;
                            if (val > dataToContour.getDataValue(im, j)
                                    || val > dataToContour.getDataValue(i, jm)
                                    || val > dataToContour.getDataValue(ip, j)
                                    || val > dataToContour.getDataValue(i, jp)) {
                                continue MIN_MAX_SEARCH;
                            }
                        }
                        // broad min check
                        if (config.minMaxRadius > 1) {
                            int tmp = Math.min(im + config.minMaxRadius,
                                    nxMaxIndex);
                            for (ii = ip; ii <= tmp; ii++) {
                                if (work3[ii][j] <= -2) {
                                    continue MIN_MAX_SEARCH;
                                }
                            }
                            tmp = Math
                                    .min(jm + config.minMaxRadius, nyMaxIndex);
                            for (jj = jp; jj <= tmp; jj++) {
                                if (work4[i][jj] <= -2) {
                                    continue MIN_MAX_SEARCH;
                                }
                            }
                            for (ii = Math.max(i - config.minMaxRadius, 0); ii < im; ii++) {
                                if (work3[ii][j] >= 2) {
                                    continue MIN_MAX_SEARCH;
                                }
                            }
                            for (jj = Math.max(j - config.minMaxRadius, 0); jj < jm; jj++) {
                                if (work4[i][jj] >= 2) {
                                    continue MIN_MAX_SEARCH;
                                }
                            }
                        }
                    } else if (config.generateMaxes && work3[i][j] <= -2) {
                        // check if this point is a valid local max
                        if (work4[i][j] > -2 || work3[im][j] < 2
                                || work4[i][jm] < 2) {
                            continue MIN_MAX_SEARCH;
                        }
                        // check corner points
                        if ((work3[im][jm] <= -2 && work4[im][jm] <= -2 && val <= dataToContour
                                .getDataValue(im, jm))
                                || (work3[im][jp] <= -2 && work4[im][j] >= 2 && val <= dataToContour
                                        .getDataValue(ip, jp))
                                || (work3[i][jm] >= 2 && work4[ip][jm] <= -2 && val <= dataToContour
                                        .getDataValue(ip, jm))
                                || (work3[i][jp] >= 2 && work4[ip][j] >= 2 && val <= dataToContour
                                        .getDataValue(ip, jp))) {
                            continue MIN_MAX_SEARCH;
                        }
                        // sharp max check
                        if (dmm > 0) {
                            val = val - dmm;
                            if (val < dataToContour.getDataValue(im, j)
                                    || val < dataToContour.getDataValue(i, jm)
                                    || val < dataToContour.getDataValue(ip, j)
                                    || val < dataToContour.getDataValue(i, jp)) {
                                continue MIN_MAX_SEARCH;
                            }
                        }
                        // broad max check
                        if (config.minMaxRadius > 1) {
                            int tmp = Math.min(im + config.minMaxRadius,
                                    nxMaxIndex);
                            for (ii = ip; ii <= tmp; ii++) {
                                if (work3[ii][j] >= 2) {
                                    continue MIN_MAX_SEARCH;
                                }
                            }
                            tmp = Math
                                    .min(jm + config.minMaxRadius, nyMaxIndex);
                            for (jj = jp; jj <= tmp; jj++) {
                                if (work4[i][jj] >= 2) {
                                    continue MIN_MAX_SEARCH;
                                }
                            }
                            for (ii = Math.max(i - config.minMaxRadius, 0); ii < im; ii++) {
                                if (work3[ii][j] <= -2) {
                                    continue MIN_MAX_SEARCH;
                                }
                            }
                            for (jj = Math.max(j - config.minMaxRadius, 0); jj < jm; jj++) {
                                if (work4[i][jj] <= -2) {
                                    continue MIN_MAX_SEARCH;
                                }
                            }
                        }
                    } else {
                        continue MIN_MAX_SEARCH;
                    }
                    // Okay, set this as a valid extremum
                    work3[i][j] |= b12;
                    work3[im][j] |= b02;
                    work4[i][j] |= b02;
                    work4[i][jm] |= b02;
                    work3[i][jm] |= b02;
                    work3[im][jm] |= b02;

                    markVrt(work3, work4, im, j);
                }

            }
        }

        // Escape point for bypassing locating maxes and mins...Skip drawing
        // contours if there are none
        if (contourCount <= 0) {
            return new ContourContainer(0);
        }

        rval = new ContourContainer(maxContours);
        int nconMaxIndex = contourCount - 1;

        // Initialize counters for total cells, labels, and label separations on
        // lines.
        labsep2 = config.labelSpacingOverall;

        // Fill line pattern array, scale contour value array, and set use count
        // mask.
        for (c = 0; c < contourCount; c++) {
            // No line pattern or scaling of contour value
            ConNo[c] = true;
        }

        // Set bracketing values which define whether cell side is best suited
        // for
        // labeling a particular contour.
        int c1;
        MnAvg[0] = -1e37f;
        for (c1 = 0, c = 1; c < contourCount; c++, c1++) {
            MxAvg[c1] = ConVal[c] + ConVal[c1];
            MnAvg[c] = MxAvg[c1];
        }
        MxAvg[nconMaxIndex] = 1e37f;

        // Initialize spiral search for places to start contours.
        turn1 = nxMaxIndex;
        turn2 = nyMaxIndex;
        turn3 = 0;
        turn4 = 1;
        // initialize c to the middle of the possible contours
        c = (contourCount) / 2;

        // start at UL
        i = 0;
        ii = 0;
        j = 0;
        jj = 0;

        // simulate fortran goto
        int jumpLabel = 10011;
        JUMP_LOOP: while (jumpLabel > 0) {
            switch (jumpLabel) {
            case 10011: {
                // Move along side 1 after missing values
                i = ii;
                if (i >= turn1) {
                    if (j >= turn2) {
                        jumpLabel = -1;
                        continue JUMP_LOOP;
                    }
                    turn1--;
                    jumpLabel = 20011;
                    continue JUMP_LOOP;
                }
                ii = i + 1;
                bbb = work3[i][j];
                if (((bbb & 0x01) == 1) || (bbb == 0)) {
                    jumpLabel = 10011;
                    continue JUMP_LOOP;
                }

                // Cell side does not have missing values, reinitialize search.
                D2 = dataToContour.getDataValue(i, j);
                if (bbb > 0) {
                    for (c = 0; c < contourCount; c++) {
                        if (ConVal[c] > D2) {
                            jumpLabel = 10022;
                            continue JUMP_LOOP;
                        }
                    }
                    jumpLabel = 10021;
                    continue JUMP_LOOP;
                } else {
                    for (c = nconMaxIndex; c >= 0; c--) {
                        if (ConVal[c] < D2) {
                            jumpLabel = 10032;
                            continue JUMP_LOOP;
                        }
                    }
                    jumpLabel = 10031;
                    continue JUMP_LOOP;
                }
            }
            case 10021: {
                // Move along side 1, values increasing as we go
                i = ii;
                if (i >= turn1) {
                    if (j >= turn2) {
                        jumpLabel = -1;
                        continue JUMP_LOOP;
                    }
                    turn1--;
                    jumpLabel = 20021;
                    continue JUMP_LOOP;
                }
                ii = i + 1;
                bbb = work3[i][j];
                if ((bbb & 0x01) == 1) {
                    jumpLabel = 10011;
                    continue JUMP_LOOP;
                }
                if (bbb < 0) {
                    c--;
                    if (c < 0) {
                        jumpLabel = 10031;
                        continue JUMP_LOOP;
                    }
                    jumpLabel = 10032;
                    continue JUMP_LOOP;
                }
                if (c == contourCount || bbb == 0) {
                    jumpLabel = 10021;
                    continue JUMP_LOOP;
                }
            }
            case 10022: {
                // Step through possible contours we can start
                D2 = dataToContour.getDataValue(ii, jj);
                boolean keepProcessing = true;
                while (keepProcessing) {
                    if (ConVal[c] < D2) {
                        if (ConNo[c] || (work1[i][j] & CMask[c]) == 0) {
                            Contr1_E(i, j, 1);
                        }
                        c++;
                        if (c == contourCount) {
                            jumpLabel = 10021;
                            continue JUMP_LOOP;
                        }
                    } else {
                        keepProcessing = false;
                    }
                }
                jumpLabel = 10021;
                continue JUMP_LOOP;
            }
            case 10031: {
                // Move along side 1, values decreasing as we go
                i = ii;
                if (i >= turn1) {
                    if (j >= turn2) {
                        jumpLabel = -1;
                        continue JUMP_LOOP;
                    }
                    turn1--;
                    jumpLabel = 20031;
                    continue JUMP_LOOP;
                }
                ii = i + 1;
                bbb = work3[i][j];
                if ((bbb & 0x01) == 1) {
                    jumpLabel = 10011;
                    continue JUMP_LOOP;
                }
                if (bbb > 0) {
                    c++;
                    if (c == contourCount) {
                        jumpLabel = 10021;
                        continue JUMP_LOOP;
                    }
                    jumpLabel = 10022;
                    continue JUMP_LOOP;
                }
                if (c < 0 || bbb == 0) {
                    jumpLabel = 10031;
                    continue JUMP_LOOP;
                }
            }
            case 10032: {
                // Step through possible contours we can start
                D2 = dataToContour.getDataValue(ii, jj);
                boolean keepProcessing = true;
                while (keepProcessing) {
                    if (ConVal[c] > D2) {
                        if (ConNo[c] || (work1[i][j] & CMask[c]) == 0) {
                            Contr1_E(i, j, 1);
                        }
                        c--;
                        if (c < 0) {
                            jumpLabel = 10031;
                            continue JUMP_LOOP;
                        }
                    } else {
                        keepProcessing = false;
                    }
                }
                jumpLabel = 10031;
                continue JUMP_LOOP;
            }
            case 20011: {
                // Move up side 2 after missing values
                j = jj;
                if (j >= turn2) {
                    if (i <= turn3) {
                        jumpLabel = -1;
                        continue JUMP_LOOP;
                    }
                    turn2--;
                    jumpLabel = 30011;
                    continue JUMP_LOOP;
                }
                jj = j + 1;
                bbb = work4[i][j];
                if (((bbb & 0x01) == 1) || (bbb == 0)) {
                    jumpLabel = 20011;
                    continue JUMP_LOOP;
                }

                // Cell side does not have missing values, reinitialize search.
                D2 = dataToContour.getDataValue(i, j);
                if (bbb > 0) {
                    for (c = 0; c < contourCount; c++) {
                        if (ConVal[c] > D2) {
                            jumpLabel = 20022;
                            continue JUMP_LOOP;
                        }
                    }
                    jumpLabel = 20021;
                    continue JUMP_LOOP;
                } else {
                    for (c = nconMaxIndex; c >= 0; c--) {
                        if (ConVal[c] < D2) {
                            jumpLabel = 20032;
                            continue JUMP_LOOP;
                        }
                    }
                    jumpLabel = 20031;
                    continue JUMP_LOOP;
                }
            }
            case 20021: {
                // Move up side 2, values increasing as we go
                j = jj;
                if (j >= turn2) {
                    if (i <= turn3) {
                        jumpLabel = -1;
                        continue JUMP_LOOP;
                    }
                    turn2--;
                    jumpLabel = 30021;
                    continue JUMP_LOOP;
                }
                jj = j + 1;
                bbb = work4[i][j];
                if ((bbb & 0x01) == 1) {
                    jumpLabel = 20011;
                    continue JUMP_LOOP;
                }
                if (bbb < 0) {
                    c--;
                    if (c < 0) {
                        jumpLabel = 20031;
                        continue JUMP_LOOP;
                    }
                    jumpLabel = 20032;
                    continue JUMP_LOOP;
                }
                if (c == contourCount || bbb == 0) {
                    jumpLabel = 20021;
                    continue JUMP_LOOP;
                }
            }
            case 20022: {
                // Step through possible contours we can start
                D2 = dataToContour.getDataValue(ii, jj);
                boolean keepProcessing = true;
                while (keepProcessing) {
                    if (ConVal[c] < D2) {
                        if (ConNo[c] || (work2[i][j] & CMask[c]) == 0) {
                            Contr1_E(i, j, 2);
                        }
                        c++;
                        if (c == contourCount) {
                            jumpLabel = 20021;
                            continue JUMP_LOOP;
                        }
                    } else {
                        keepProcessing = false;
                    }
                }
                jumpLabel = 20021;
                continue JUMP_LOOP;
            }
            case 20031: {
                // Move up side 2, values decreasing as we go
                j = jj;
                if (j >= turn2) {
                    if (i <= turn3) {
                        jumpLabel = -1;
                        continue JUMP_LOOP;
                    }
                    turn2--;
                    jumpLabel = 30031;
                    continue JUMP_LOOP;
                }
                jj = j + 1;
                bbb = work4[i][j];
                if ((bbb & 0x01) == 1) {
                    jumpLabel = 20011;
                    continue JUMP_LOOP;
                }
                if (bbb > 0) {
                    c++;
                    if (c == contourCount) {
                        jumpLabel = 20021;
                        continue JUMP_LOOP;
                    }
                    jumpLabel = 20022;
                    continue JUMP_LOOP;
                }
                if (c < 0 || bbb == 0) {
                    jumpLabel = 20031;
                    continue JUMP_LOOP;
                }
            }
            case 20032: {
                // Find group of contours which are bracketed
                D2 = dataToContour.getDataValue(ii, jj);
                boolean keepProcessing = true;
                while (keepProcessing) {
                    if (ConVal[c] > D2) {
                        if (ConNo[c] || (work2[i][j] & CMask[c]) == 0) {
                            Contr1_E(i, j, 2);
                        }
                        c--;
                        if (c < 0) {
                            jumpLabel = 20031;
                            continue JUMP_LOOP;
                        }
                    } else {
                        keepProcessing = false;
                    }
                }
                jumpLabel = 20031;
                continue JUMP_LOOP;
            }
            case 30011: {
                // Move back along side 3 after missing values
                i = ii;
                if (i <= turn3) {
                    if (j <= turn4) {
                        jumpLabel = -1;
                        continue JUMP_LOOP;
                    }
                    turn3++;
                    jumpLabel = 40011;
                    continue JUMP_LOOP;
                }
                ii = i - 1;
                bbb = work3[ii][jj];
                if (((bbb & 0x01) == 1) || (bbb == 0)) {
                    jumpLabel = 30011;
                    continue JUMP_LOOP;
                }

                // Cell side does not have missing values, reinitialize search.
                D2 = dataToContour.getDataValue(i, j);
                if (bbb < 0) {
                    for (c = 0; c < contourCount; c++) {
                        if (ConVal[c] > D2) {
                            jumpLabel = 30022;
                            continue JUMP_LOOP;
                        }
                    }
                    jumpLabel = 30021;
                    continue JUMP_LOOP;
                } else {
                    for (c = nconMaxIndex; c >= 0; c--) {
                        if (ConVal[c] < D2) {
                            jumpLabel = 30032;
                            continue JUMP_LOOP;
                        }
                    }
                    jumpLabel = 30031;
                    continue JUMP_LOOP;
                }
            }
            case 30021: {
                // Move back along side 3, values increasing as we go
                i = ii;
                if (i <= turn3) {
                    if (j <= turn4) {
                        jumpLabel = -1;
                        continue JUMP_LOOP;
                    }
                    turn3++;
                    jumpLabel = 40021;
                    continue JUMP_LOOP;
                }
                ii = i - 1;
                bbb = work3[ii][jj];
                if ((bbb & 0x01) == 1) {
                    jumpLabel = 30011;
                    continue JUMP_LOOP;
                }
                if (bbb > 0) {
                    c--;
                    if (c < 0) {
                        jumpLabel = 30031;
                        continue JUMP_LOOP;
                    }
                    jumpLabel = 30032;
                    continue JUMP_LOOP;
                }
                if (c == contourCount || bbb == 0) {
                    jumpLabel = 30021;
                    continue JUMP_LOOP;
                }
            }
            case 30022: {
                // Step through possible contours we can start
                D2 = dataToContour.getDataValue(ii, jj);
                boolean keepProcessing = true;
                while (keepProcessing) {
                    if (ConVal[c] < D2) {
                        if (ConNo[c] || (work1[ii][jj] & CMask[c]) == 0) {
                            Contr1_E(ii, jj, 3);
                        }
                        c++;
                        if (c == contourCount) {
                            jumpLabel = 30021;
                            continue JUMP_LOOP;
                        }
                    } else {
                        keepProcessing = false;
                    }
                }
                jumpLabel = 30021;
                continue JUMP_LOOP;
            }
            case 30031: {
                // Move back along side 3, values decreasing as we go
                i = ii;
                if (i <= turn3) {
                    if (j <= turn4) {
                        jumpLabel = -1;
                        continue JUMP_LOOP;
                    }
                    turn3++;
                    jumpLabel = 40031;
                    continue JUMP_LOOP;
                }
                ii = i - 1;
                bbb = work3[ii][jj];
                if ((bbb & 0x01) == 1) {
                    jumpLabel = 30011;
                    continue JUMP_LOOP;
                }
                if (bbb < 0) {
                    c++;
                    if (c == contourCount) {
                        jumpLabel = 30021;
                        continue JUMP_LOOP;
                    }
                    jumpLabel = 30022;
                    continue JUMP_LOOP;
                }
                if (c < 0 || bbb == 0) {
                    jumpLabel = 30031;
                    continue JUMP_LOOP;
                }
            }
            case 30032: {
                // Step through possible contours we can start
                D2 = dataToContour.getDataValue(ii, jj);
                boolean keepProcessing = true;
                while (keepProcessing) {
                    if (ConVal[c] > D2) {
                        if (ConNo[c] || (work1[ii][jj] & CMask[c]) == 0) {
                            Contr1_E(ii, jj, 3);
                        }
                        c--;
                        if (c < 0) {
                            jumpLabel = 30031;
                            continue JUMP_LOOP;
                        }
                    } else {
                        keepProcessing = false;
                    }
                }
                jumpLabel = 30031;
                continue JUMP_LOOP;
            }
            case 40011: {
                // Move down side 4 after missing values
                j = jj;
                if (j <= turn4) {
                    if (i >= turn1) {
                        jumpLabel = -1;
                        continue JUMP_LOOP;
                    }
                    turn4++;
                    jumpLabel = 10011;
                    continue JUMP_LOOP;
                }
                jj = j - 1;
                bbb = work4[ii][jj];
                if (((bbb & 0x01) == 1) || (bbb == 0)) {
                    jumpLabel = 40011;
                    continue JUMP_LOOP;
                }

                // Cell side does not have missing values, reinitialize search.
                D2 = dataToContour.getDataValue(i, j);
                if (bbb < 0) {
                    for (c = 0; c < contourCount; c++) {
                        if (ConVal[c] > D2) {
                            jumpLabel = 40022;
                            continue JUMP_LOOP;
                        }
                    }
                    jumpLabel = 40021;
                    continue JUMP_LOOP;
                } else {
                    for (c = nconMaxIndex; c >= 0; c--) {
                        if (ConVal[c] < D2) {
                            jumpLabel = 40032;
                            continue JUMP_LOOP;
                        }
                    }
                    jumpLabel = 40031;
                    continue JUMP_LOOP;
                }
            }
            case 40021: {
                // Move down side 4, values increasing as we go
                j = jj;
                if (j <= turn4) {
                    if (i >= turn1) {
                        jumpLabel = -1;
                        continue JUMP_LOOP;
                    }
                    turn4++;
                    jumpLabel = 10021;
                    continue JUMP_LOOP;
                }
                jj = j - 1;
                bbb = work4[ii][jj];
                if ((bbb & 0x01) == 1) {
                    jumpLabel = 40011;
                    continue JUMP_LOOP;
                }
                if (bbb > 0) {
                    c--;
                    if (c < 0) {
                        jumpLabel = 40031;
                        continue JUMP_LOOP;
                    }
                    jumpLabel = 40032;
                    continue JUMP_LOOP;
                }
                if (c == contourCount || bbb == 0) {
                    jumpLabel = 40021;
                    continue JUMP_LOOP;
                }
            }
            case 40022: {
                // Step through possible contours we can start
                D2 = dataToContour.getDataValue(ii, jj);
                boolean keepProcessing = true;
                while (keepProcessing) {
                    if (ConVal[c] < D2) {
                        if (ConNo[c] || (work2[ii][jj] & CMask[c]) == 0) {
                            Contr1_E(ii, jj, 4);
                        }
                        c++;
                        if (c == contourCount) {
                            jumpLabel = 40021;
                            continue JUMP_LOOP;
                        }
                    } else {
                        keepProcessing = false;
                    }
                }
                jumpLabel = 40021;
                continue JUMP_LOOP;
            }
            case 40031: {
                // Move down side 4, values decreasing as we go
                j = jj;
                if (j <= turn4) {
                    if (i >= turn1) {
                        jumpLabel = -1;
                        continue JUMP_LOOP;
                    }
                    turn4++;
                    jumpLabel = 10031;
                    continue JUMP_LOOP;
                }
                jj = j - 1;
                bbb = work4[ii][jj];
                if ((bbb & 0x01) == 1) {
                    jumpLabel = 40011;
                    continue JUMP_LOOP;
                }
                if (bbb < 0) {
                    c++;
                    if (c == contourCount) {
                        jumpLabel = 40021;
                        continue JUMP_LOOP;
                    }
                    jumpLabel = 40022;
                    continue JUMP_LOOP;
                }
                if (c < 0 || bbb == 0) {
                    jumpLabel = 40031;
                    continue JUMP_LOOP;
                }
            }
            case 40032: {
                // Step through possible contours we can start
                D2 = dataToContour.getDataValue(ii, jj);
                boolean keepProcessing = true;
                while (keepProcessing) {
                    if (ConVal[c] > D2) {
                        if (ConNo[c] || (work2[ii][jj] & CMask[c]) == 0) {
                            Contr1_E(ii, jj, 4);
                        }
                        c--;
                        if (c < 0) {
                            jumpLabel = 40031;
                            continue JUMP_LOOP;
                        }
                    } else {
                        keepProcessing = false;
                    }
                }
                jumpLabel = 40031;
                continue JUMP_LOOP;
            }
            }

            // exit loop
            jumpLabel = -1;
        } // jump loop

        // Skip generating maxes and mins if we don't need to
        if (config.generateMins || config.generateMaxes) {
            // loop for drawing Maxes and Mins
            for (j = 1; j < nyMaxIndex; j++) {
                for (i = 1; i < nxMaxIndex; i++) {
                    // See what kind, if any, this extremum is.
                    bbb = work3[i][j];
                    if (bbb == b52) {
                        // minima
                        rval.minVals.add((float) dataToContour.getDataValue(i,
                                j));
                        rval.minLabelPoints.add(new float[] {
                                i + config.xOffset, j + config.yOffset });
                    } else if (bbb == bD2) {
                        // maxima
                        rval.maxVals.add((float) dataToContour.getDataValue(i,
                                j));
                        rval.maxLabelPoints.add(new float[] {
                                i + config.xOffset, j + config.yOffset });
                    }
                }
            }
        }

        // clean up references to release memory
        dataToContour = null;
        work1 = null;
        work2 = null;
        work3 = null;
        work4 = null;
        ContourContainer tmp = rval;
        rval = null;
        return tmp;
    }

    /**
     * @param config
     *            the FortConConfig
     * @param contourCount
     *            The contour count
     * @return If changed, the new contourCount.
     */
    private int formatContourValues(FortConConfig config, int contourCount) {
        if (config.labelFormat != null) {
            DecimalFormat decimalFormat = new DecimalFormat(config.labelFormat);
            int curIndex = -1;
            for (int i = 0; i < contourCount; i++) {
                try {
                    Number number = decimalFormat.parse(decimalFormat
                            .format(ConVal[i]));
                    // previous formated value is not the same as cur value
                    if (curIndex < 0 || ConVal[curIndex] != number.floatValue()) {
                        curIndex++;
                        ConVal[curIndex] = number.floatValue();
                    }
                } catch (ParseException e) {
                    // Unable to parse, just leave the array alone;
                }
            }
            contourCount = curIndex + 1;
        }
        return contourCount;
    }

    private void Contr1_E(int istart, int jstart, int sstart) {
        // Nomenclature for cell (i,j)
        // point 4 (i,j+1)-----side 3------(i+1,j+1) point 3
        // | |
        // | |
        // | |
        // side 4 side 2
        // | |
        // | |
        // | |
        // point 1 (i,j)-------side 1------(i+1,j) point 2
        float val = ConVal[c];
        byte cmw = CMask[c];
        boolean GGG1 = false;
        boolean GGG2 = false;
        boolean GGG3 = false;
        boolean GGG4 = false;
        boolean GGGD = false;
        float val4 = val * 4;
        boolean clos1 = false;
        boolean clos2 = false;
        boolean clos3 = false;
        boolean clos4 = false;
        boolean backok = false;
        int icell = 0;
        int jcell = 0;
        int iplus = 0;
        int jplus = 0;
        float D1 = 0;
        float D2 = 0;
        float D3 = 0;
        float D4 = 0;

        // reset point buffer
        ijPntBuffer.setSize(0);
        ijPntBuffer.setDirection(true);
        ijPntBuffer.setNextWriteIndex(ijPntBuffer.capacity() / 2);

        int jumpLabel = 0;
        switch (sstart) {
        case 1:
            jumpLabel = 51;
            break;
        case 2:
            jumpLabel = 52;
            break;
        case 3:
            jumpLabel = 53;
            break;
        case 4:
            jumpLabel = 54;
        }

        JUMP_LOOP: while (jumpLabel > 0) {
            switch (jumpLabel) {
            case 51: {
                // start contour on side one
                if (jstart > 0) {
                    backok = true;
                }
                // fall through
            }
            case 61: {
                jcell = jstart;
                jplus = jstart + 1;
                icell = istart;
                iplus = istart + 1;
                clos1 = true;
                D3 = (float) dataToContour.getDataValue(iplus, jcell);
                D4 = (float) dataToContour.getDataValue(icell, jcell);
                GGG3 = val >= D3;
                GGG4 = val >= D4;
                if (GGG3 == GGG4) {
                    jumpLabel = 9000;
                    continue JUMP_LOOP;
                }
                work1[icell][jcell] = (byte) (work1[icell][jcell] | cmw);
                ConNo[c] = false;
                ijPntBuffer.put(icell + (val - D4) / (D3 - D4), jcell);
                jumpLabel = 101;
                continue JUMP_LOOP;
            }
            case 52: {
                // start contour on side two
                if (istart < nxMaxIndex) {
                    backok = true;
                }
                // fall through
            }
            case 62: {
                icell = istart - 1;
                iplus = istart;
                jcell = jstart;
                jplus = jstart + 1;
                clos2 = true;
                D1 = (float) dataToContour.getDataValue(iplus, jcell);
                D4 = (float) dataToContour.getDataValue(iplus, jplus);
                GGG1 = val >= D1;
                GGG4 = val >= D4;
                if (GGG1 == GGG4) {
                    jumpLabel = 9000;
                    continue JUMP_LOOP;
                }
                work2[iplus][jcell] = (byte) (work2[iplus][jcell] | cmw);
                ConNo[c] = false;
                ijPntBuffer.put(iplus, jcell + (val - D1) / (D4 - D1));
                jumpLabel = 102;
                continue JUMP_LOOP;
            }
            case 53: {
                // Start contour on side three
                if (jstart < nyMaxIndex) {
                    backok = true;
                }
                // fall through
            }
            case 63: {
                jcell = jstart - 1;
                jplus = jstart;
                icell = istart;
                iplus = istart + 1;
                clos3 = true;
                D1 = (float) dataToContour.getDataValue(icell, jplus);
                D2 = (float) dataToContour.getDataValue(iplus, jplus);
                GGG1 = val >= D1;
                GGG2 = val >= D2;
                if (GGG1 == GGG2) {
                    jumpLabel = 9000;
                    continue JUMP_LOOP;
                }
                work1[icell][jplus] = (byte) (work1[icell][jplus] | cmw);
                ConNo[c] = false;
                ijPntBuffer.put(icell + (val - D1) / (D2 - D1), jplus);
                jumpLabel = 103;
                continue JUMP_LOOP;
            }
            case 54: {
                // Start contour on side four
                if (istart > 0) {
                    backok = true;
                }
                // fall through
            }
            case 64: {
                icell = istart;
                iplus = istart + 1;
                jcell = jstart;
                jplus = jstart + 1;
                clos4 = true;
                D3 = (float) dataToContour.getDataValue(icell, jplus);
                D2 = (float) dataToContour.getDataValue(icell, jcell);
                GGG3 = val >= D3;
                GGG2 = val >= D2;
                if (GGG3 == GGG2) {
                    jumpLabel = 9000;
                    continue JUMP_LOOP;
                }
                work2[icell][jcell] = (byte) (work2[icell][jcell] | cmw);
                ConNo[c] = false;
                ijPntBuffer.put(icell, jcell + (val - D2) / (D3 - D2));
                jumpLabel = 104;
                continue JUMP_LOOP;
            }
            case 101: {
                // Entering side one, establish cell boundary information
                GGG1 = GGG4;
                D1 = D4;
                GGG2 = GGG3;
                D2 = D3;
                D3 = (float) dataToContour.getDataValue(iplus, jplus);
                D4 = (float) dataToContour.getDataValue(icell, jplus);
                GGG3 = val >= D3;
                GGG4 = val >= D4;

                // handle case of missing data
                if ((work3[icell][jplus] & 1) == 1) {
                    if ((work4[icell][jcell] & 1) == 0) {
                        if (GGG4 != GGG2) {
                            jumpLabel = 9000;
                            continue JUMP_LOOP;
                        }
                        jumpLabel = 4422;
                        continue JUMP_LOOP;
                    } else if ((work4[iplus][jcell] & 1) == 0) {
                        if (GGG3 != GGG1) {
                            jumpLabel = 9000;
                            continue JUMP_LOOP;
                        }
                        jumpLabel = 2244;
                        continue JUMP_LOOP;
                    }
                    jumpLabel = 9000;
                    continue JUMP_LOOP;
                }

                // Determine proper path through cell from side one
                if (GGG3 == GGG4) {
                    if (GGG2 == GGG3) {
                        jumpLabel = 4422;
                        continue JUMP_LOOP;
                    }
                    jumpLabel = 2244;
                    continue JUMP_LOOP;
                } else {
                    if (GGG1 == GGG4) {
                        jumpLabel = 3311;
                        continue JUMP_LOOP;
                    }
                    GGGD = val4 >= (D1 + D2 + D3 + D4);
                    if (GGG1 != GGGD) {
                        jumpLabel = 4422;
                        continue JUMP_LOOP;
                    }
                    jumpLabel = 2244;
                    continue JUMP_LOOP;
                }
            }
            case 102: {
                // Entering side two, establish cell boundary information
                GGG2 = GGG1;
                D2 = D1;
                GGG3 = GGG4;
                D3 = D4;
                D1 = (float) dataToContour.getDataValue(icell, jcell);
                D4 = (float) dataToContour.getDataValue(icell, jplus);
                GGG1 = val >= D1;
                GGG4 = val >= D4;

                // handle case of missing data
                if ((work4[icell][jcell] & 1) == 1) {
                    if ((work3[icell][jplus] & 1) == 0) {
                        if (GGG4 != GGG2) {
                            jumpLabel = 9000;
                            continue JUMP_LOOP;
                        }
                        jumpLabel = 3311;
                        continue JUMP_LOOP;
                    } else if ((work3[icell][jcell] & 1) == 0) {
                        if (GGG3 != GGG1) {
                            jumpLabel = 9000;
                            continue JUMP_LOOP;
                        }
                        jumpLabel = 1133;
                        continue JUMP_LOOP;
                    }
                    jumpLabel = 9000;
                    continue JUMP_LOOP;
                }

                // Determine proper path through cell from side two.
                if (GGG1 == GGG4) {
                    if (GGG3 == GGG4) {
                        jumpLabel = 1133;
                        continue JUMP_LOOP;
                    }
                    jumpLabel = 3311;
                    continue JUMP_LOOP;
                } else {
                    if (GGG1 == GGG2) {
                        jumpLabel = 4422;
                        continue JUMP_LOOP;
                    }
                    GGGD = val4 >= (D1 + D2 + D3 + D4);
                    if (GGG2 != GGGD) {
                        jumpLabel = 1133;
                        continue JUMP_LOOP;
                    }
                    jumpLabel = 3311;
                    continue JUMP_LOOP;
                }
            }
            case 103: {
                // Entering side three, establish cell boundary information
                GGG3 = GGG2;
                D3 = D2;
                GGG4 = GGG1;
                D4 = D1;
                D1 = (float) dataToContour.getDataValue(icell, jcell);
                D2 = (float) dataToContour.getDataValue(iplus, jcell);
                GGG1 = val >= D1;
                GGG2 = val >= D2;

                // handle case of missing data
                if ((work3[icell][jcell] & 1) == 1) {
                    if ((work4[iplus][jcell] & 1) == 0) {
                        if (GGG4 != GGG2) {
                            jumpLabel = 9000;
                            continue JUMP_LOOP;
                        }
                        jumpLabel = 2244;
                        continue JUMP_LOOP;
                    } else if ((work4[icell][jcell] & 1) == 0) {
                        if (GGG3 != GGG1) {
                            jumpLabel = 9000;
                            continue JUMP_LOOP;
                        }
                        jumpLabel = 4422;
                        continue JUMP_LOOP;
                    }
                    jumpLabel = 9000;
                    continue JUMP_LOOP;
                }

                // Determine proper path through cell from side three
                if (GGG1 == GGG2) {
                    if (GGG1 == GGG4) {
                        jumpLabel = 2244;
                        continue JUMP_LOOP;
                    }
                    jumpLabel = 4422;
                    continue JUMP_LOOP;
                } else {
                    if (GGG2 == GGG3) {
                        jumpLabel = 1133;
                        continue JUMP_LOOP;
                    }
                    GGGD = val4 >= (D1 + D2 + D3 + D4);
                    if (GGG3 != GGGD) {
                        jumpLabel = 2244;
                        continue JUMP_LOOP;
                    }
                    jumpLabel = 4422;
                    continue JUMP_LOOP;
                }
            }
            case 104: {
                // Entering side four, establish cell boundary information
                GGG4 = GGG3;
                D4 = D3;
                GGG1 = GGG2;
                D1 = D2;
                D3 = (float) dataToContour.getDataValue(iplus, jplus);
                D2 = (float) dataToContour.getDataValue(iplus, jcell);
                GGG3 = val >= D3;
                GGG2 = val >= D2;

                // handle case of missing data
                if ((work4[iplus][jcell] & 1) == 1) {
                    if ((work3[icell][jcell] & 1) == 0) {
                        if (GGG4 != GGG2) {
                            jumpLabel = 9000;
                            continue JUMP_LOOP;
                        }
                        jumpLabel = 1133;
                        continue JUMP_LOOP;
                    } else if ((work3[icell][jplus] & 1) == 0) {
                        if (GGG3 != GGG1) {
                            jumpLabel = 9000;
                            continue JUMP_LOOP;
                        }
                        jumpLabel = 3311;
                        continue JUMP_LOOP;
                    }
                    jumpLabel = 9000;
                    continue JUMP_LOOP;
                }

                // Determine proper path through cell from side four.
                if (GGG2 == GGG3) {
                    if (GGG1 == GGG2) {
                        jumpLabel = 3311;
                        continue JUMP_LOOP;
                    }
                    jumpLabel = 1133;
                    continue JUMP_LOOP;
                } else {
                    if (GGG3 == GGG4) {
                        jumpLabel = 2244;
                        continue JUMP_LOOP;
                    }
                    GGGD = val4 >= (D1 + D2 + D3 + D4);
                    if (GGG4 != GGGD) {
                        jumpLabel = 3311;
                        continue JUMP_LOOP;
                    }
                    jumpLabel = 1133;
                    continue JUMP_LOOP;
                }
            }
            case 1133: {
                // Cross to side 1 and enter new cell from side 3.
                ijPntBuffer.put(icell + (val - D1) / (D2 - D1), jcell);
                work1[icell][jcell] = (byte) (work1[icell][jcell] | cmw);
                if (jcell == 0) {
                    jumpLabel = 9000;
                    continue JUMP_LOOP;
                }

                // check if contour has closed off, advance values for cell
                // bounds indices
                if (clos3) {
                    if (icell == istart && jcell == jstart) {
                        jumpLabel = 8999;
                        continue JUMP_LOOP;
                    }
                }
                jplus = jcell;
                jcell--;
                jumpLabel = 103;
                continue JUMP_LOOP;
            }
            case 2244: {
                // Cross to side 2 and enter new cell from side 4
                ijPntBuffer.put(iplus, jcell + (val - D2) / (D3 - D2));
                work2[iplus][jcell] = (byte) (work2[iplus][jcell] | cmw);
                if (iplus == nxMaxIndex) {
                    jumpLabel = 9000;
                    continue JUMP_LOOP;
                }

                // check if contour has closed off, advance values for cell
                // bounds indices
                if (clos4) {
                    if (iplus == istart && jcell == jstart) {
                        jumpLabel = 8999;
                        continue JUMP_LOOP;
                    }
                }
                icell = iplus;
                iplus++;
                jumpLabel = 104;
                continue JUMP_LOOP;
            }
            case 3311: {
                // Cross to side 3 and enter new cell from side 1.
                ijPntBuffer.put(icell + (val - D4) / (D3 - D4), jplus);
                work1[icell][jplus] = (byte) (work1[icell][jplus] | cmw);
                if (jplus == nyMaxIndex) {
                    jumpLabel = 9000;
                    continue JUMP_LOOP;
                }

                // check if contour has closed off, advance values for cell
                // bounds indices
                if (clos1) {
                    if (icell == istart && jplus == jstart) {
                        jumpLabel = 8999;
                        continue JUMP_LOOP;
                    }
                }
                jcell = jplus;
                jplus++;
                jumpLabel = 101;
                continue JUMP_LOOP;
            }
            case 4422: {
                // Cross to side 4 and enter new cell from side 2
                ijPntBuffer.put(icell, jcell + (val - D1) / (D4 - D1));
                work2[icell][jcell] = (byte) (work2[icell][jcell] | cmw);
                if (icell == 0) {
                    jumpLabel = 9000;
                    continue JUMP_LOOP;
                }

                // check if contour has closed off, advance values for cell
                // bounds indices
                if (clos2) {
                    if (icell == istart && jcell == jstart) {
                        jumpLabel = 8999;
                        continue JUMP_LOOP;
                    }
                }
                iplus = icell;
                icell--;
                jumpLabel = 102;
                continue JUMP_LOOP;
            }
            case 8999: {
                // Finished
                backok = false;
                // fall through
            }
            case 9000: {
                if (backok) {
                    backok = false;
                    clos1 = false;
                    clos2 = false;
                    clos3 = false;
                    clos4 = false;
                    ijPntBuffer.setDirection(false);
                    ijPntBuffer.setNextWriteIndex(ijPntBuffer.getMinIndex());

                    switch (sstart) {
                    case 1:
                        jumpLabel = 63;
                        break;
                    case 2:
                        jumpLabel = 64;
                        break;
                    case 3:
                        jumpLabel = 61;
                        break;
                    case 4:
                        jumpLabel = 62;
                    }
                    continue JUMP_LOOP;
                }

                // check if we have anything to draw
                if (ijPntBuffer.size() == 0) {
                    return;
                }

                // System.out.println("i/j PntBuffer size " +
                // iPntBuffer.size());
                // put points into global buffer
                rval.contourVals.add(val);
                rval.xyContourPoints.add(ijPntBuffer.getPoints());
                return;
            }
            }
        }
    }

    private void markVrt(byte[][] workH, byte[][] workV, int i, int j) {
        markCheck(workV, i, j);
        if (labsep2 <= 0)
            return;

        markCheck(workH, i - 1, j);
        markCheck(workH, i - 1, j + 1);
        markCheck(workH, i, j);
        markCheck(workH, i, j + 1);

        markCheck(workV, i, j - 1);
        markCheck(workV, i, j + 1);
        if (labsep2 <= 1)
            return;

        markCheck(workH, i - 2, j);
        markCheck(workH, i - 2, j + 1);
        markCheck(workH, i - 1, j - 1);
        markCheck(workH, i - 1, j + 2);
        markCheck(workH, i, j - 1);
        markCheck(workH, i, j + 2);
        markCheck(workH, i + 1, j);
        markCheck(workH, i + 1, j + 1);

        markCheck(workV, i - 1, j - 1);
        markCheck(workV, i - 1, j);
        markCheck(workV, i - 1, j + 1);
        markCheck(workV, i, j - 2);
        markCheck(workV, i, j + 2);
        markCheck(workV, i + 1, j - 1);
        markCheck(workV, i + 1, j);
        markCheck(workV, i + 1, j + 1);
        if (labsep2 <= 2)
            return;

        markCheck(workH, i - 3, j);
        markCheck(workH, i - 3, j + 1);
        markCheck(workH, i - 2, j - 1);
        markCheck(workH, i - 2, j + 2);
        markCheck(workH, i - 1, j - 2);
        markCheck(workH, i - 1, j + 3);
        markCheck(workH, i, j - 2);
        markCheck(workH, i, j + 3);
        markCheck(workH, i + 1, j - 1);
        markCheck(workH, i + 1, j + 2);
        markCheck(workH, i + 2, j);
        markCheck(workH, i + 2, j + 1);

        markCheck(workV, i - 2, j - 1);
        markCheck(workV, i - 2, j);
        markCheck(workV, i - 2, j + 1);
        markCheck(workV, i - 1, j - 2);
        markCheck(workV, i - 1, j + 2);
        markCheck(workV, i, j - 3);
        markCheck(workV, i, j + 3);
        markCheck(workV, i + 1, j - 2);
        markCheck(workV, i + 1, j + 2);
        markCheck(workV, i + 2, j - 1);
        markCheck(workV, i + 2, j);
        markCheck(workV, i + 2, j + 1);
        if (labsep2 <= 3)
            return;

        markCheck(workH, i - 4, j);
        markCheck(workH, i - 4, j + 1);
        markCheck(workH, i - 3, j - 1);
        markCheck(workH, i - 3, j + 2);
        markCheck(workH, i - 2, j - 2);
        markCheck(workH, i - 2, j + 3);
        markCheck(workH, i - 1, j - 3);
        markCheck(workH, i - 1, j + 4);
        markCheck(workH, i, j - 3);
        markCheck(workH, i, j + 4);
        markCheck(workH, i + 1, j - 2);
        markCheck(workH, i + 1, j + 3);
        markCheck(workH, i + 2, j - 1);
        markCheck(workH, i + 2, j + 2);
        markCheck(workH, i + 3, j);
        markCheck(workH, i + 3, j + 1);

        markCheck(workV, i - 3, j - 1);
        markCheck(workV, i - 3, j);
        markCheck(workV, i - 3, j + 1);
        markCheck(workV, i - 2, j - 2);
        markCheck(workV, i - 2, j + 2);
        markCheck(workV, i - 1, j - 3);
        markCheck(workV, i - 1, j + 3);
        markCheck(workV, i, j - 4);
        markCheck(workV, i, j + 4);
        markCheck(workV, i + 1, j - 3);
        markCheck(workV, i + 1, j + 3);
        markCheck(workV, i + 2, j - 2);
        markCheck(workV, i + 2, j + 2);
        markCheck(workV, i + 3, j - 1);
        markCheck(workV, i + 3, j);
        markCheck(workV, i + 3, j + 1);
        if (labsep2 <= 4)
            return;

        markCheck(workH, i - 5, j);
        markCheck(workH, i - 5, j + 1);
        markCheck(workH, i - 4, j - 1);
        markCheck(workH, i - 4, j + 2);
        markCheck(workH, i - 3, j - 2);
        markCheck(workH, i - 3, j + 3);
        markCheck(workH, i - 2, j - 3);
        markCheck(workH, i - 2, j + 4);
        markCheck(workH, i - 1, j - 4);
        markCheck(workH, i - 1, j + 5);
        markCheck(workH, i, j - 4);
        markCheck(workH, i, j + 5);
        markCheck(workH, i + 1, j - 3);
        markCheck(workH, i + 1, j + 4);
        markCheck(workH, i + 2, j - 2);
        markCheck(workH, i + 2, j + 3);
        markCheck(workH, i + 3, j - 1);
        markCheck(workH, i + 3, j + 2);
        markCheck(workH, i + 4, j);
        markCheck(workH, i + 4, j + 1);

        markCheck(workV, i - 4, j - 1);
        markCheck(workV, i - 4, j);
        markCheck(workV, i - 4, j + 1);
        markCheck(workV, i - 3, j - 2);
        markCheck(workV, i - 3, j + 2);
        markCheck(workV, i - 2, j - 3);
        markCheck(workV, i - 2, j + 3);
        markCheck(workV, i - 1, j - 4);
        markCheck(workV, i - 1, j + 4);
        markCheck(workV, i, j - 5);
        markCheck(workV, i, j + 5);
        markCheck(workV, i + 1, j - 4);
        markCheck(workV, i + 1, j + 4);
        markCheck(workV, i + 2, j - 3);
        markCheck(workV, i + 2, j + 3);
        markCheck(workV, i + 3, j - 2);
        markCheck(workV, i + 3, j + 2);
        markCheck(workV, i + 4, j - 1);
        markCheck(workV, i + 4, j);
        markCheck(workV, i + 4, j + 1);
    }

    private static void markCheck(byte[][] work, int i, int j) {
        if (i >= 0 && i < work.length && j >= 0 && j < work[0].length) {
            work[i][j] |= b02;
        }
    }
}
