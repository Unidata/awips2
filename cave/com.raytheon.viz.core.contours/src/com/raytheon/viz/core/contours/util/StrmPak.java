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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.numeric.array.FloatArray2DWrapper;
import com.raytheon.uf.common.numeric.filter.DataFilter;
import com.raytheon.uf.common.numeric.source.AxisSwapDataSource;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.numeric.source.FilteredDataSource;
import com.raytheon.viz.core.contours.util.StreamLineContainer.StreamLinePoint;

/**
 * Port of strmpak.f.
 * <p>
 * This routine draws a set of streamlines. Works in area defined by current.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 10, 2013  1999     dgilling    Initial creation
 * Feb 27, 2014  2791     bsteffen    Use DataSource for generic data access.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class StrmPak {

    private final class PointValueBuffer<V> {

        private final Map<Integer, V> buffer;

        private final V defaultValue;

        public PointValueBuffer(int initialSize, V defaultValue) {
            buffer = new HashMap<Integer, V>(initialSize, 1f);
            this.defaultValue = defaultValue;
        }

        public V get(int key) {
            // TODO: introduce a bounds check so that this class throws an
            // equivalent to an ArrayIndexOutOfBoundsException when data is
            // requested that would have been outside of the original array's
            // bounds.
            // For now, neglecting for performance reasons
            V retVal = buffer.get(key);
            if (retVal == null) {
                retVal = defaultValue;
            }
            return retVal;
        }

        public V put(int key, V value) {
            // TODO: introduce a bounds check so that this class throws an
            // equivalent to an ArrayIndexOutOfBoundsException when data is
            // requested that would have been outside of the original array's
            // bounds.
            // For now, neglecting for performance reasons
            return buffer.put(key, value);
        }

        public void clear() {
            buffer.clear();
        }
    }

    private static final float minmag = 0.0f;

    private static final float maxmag = 1e36f;

    @SuppressWarnings("unused")
    private static final float checkval = 1e36f;

    // private static Queue<StrmPak> instancePool = new
    // ConcurrentLinkedQueue<StrmPak>();

    private int ill;

    private int iur;

    private int jll;

    private int jur;

    private int ium;

    private int jum;

    private float asiz;

    // in fortran itrack/jtrack were size 2000 arrays, indexed 1 ->
    // 2000
    private final PointValueBuffer<Integer> itrack;

    private final PointValueBuffer<Integer> jtrack;

    // A2 porting note: LCPnt was a byte array in the original
    // source, but the only values ever assigned to the array were 0 or 1,
    // so a boolean array will work just as well.
    // in fortran LCPnt/IPnt/JPnt were size 8001 arrays, indexed -4000 ->
    // 4000
    private final PointValueBuffer<Boolean> LCPnt;

    private final PointValueBuffer<Float> IPnt;

    private final PointValueBuffer<Float> JPnt;

    /**
     * Generate streamlines using column major float data. The u component is
     * assumed to moving in the opposite direction of the x axis.
     * 
     * @param uComp
     *            data values for the u component of the vector
     * @param vComp
     *            data values for the v component of the vector
     * @param xSize
     *            unused
     * @param nx
     *            number of x coordinates
     * @param ny
     *            number of y coordinates
     * @param config
     *            configuration options
     * @return
     */
    public static StreamLineContainer strmpak(float[][] uComp, float[][] vComp,
            int xSize, int nx, int ny, StrmPakConfig config) {
        DataSource uSource = new FloatArray2DWrapper(uComp, ny, nx);
        DataSource vSource = new FloatArray2DWrapper(vComp, ny, nx);
        uSource = new AxisSwapDataSource(uSource);
        vSource = new AxisSwapDataSource(vSource);
        uSource = FilteredDataSource.addFilters(uSource, new DataFilter() {

            @Override
            public double filter(double value) {
                return -value;
            }
        });
        return strmpak(uSource, vSource, nx, ny, config);
    }

    /**
     * Generate streamlines using any data source.
     * 
     * @param uComp
     *            data values for the u component of the vector
     * @param vComp
     *            data values for the v component of the vector
     * @param nx
     *            number of x coordinates
     * @param ny
     *            number of y coordinates
     * @param config
     *            configuration options
     * @return
     */
    public static StreamLineContainer strmpak(DataSource uComp,
            DataSource vComp, int nx, int ny, StrmPakConfig config) {
        StrmPak instance = new StrmPak();
        StreamLineContainer rval = instance.strmpakInternal(uComp, vComp, nx,
                ny, config);
        return rval;
    }

    private StrmPak() {
        itrack = new PointValueBuffer<Integer>(512, 0);
        jtrack = new PointValueBuffer<Integer>(512, 0);
        LCPnt = new PointValueBuffer<Boolean>(64, Boolean.TRUE);
        IPnt = new PointValueBuffer<Float>(512, 0.0f);
        JPnt = new PointValueBuffer<Float>(512, 0.0f);
    }

    /**
     * This routine draws a set of streamlines. Works in area defined by
     * current.
     * 
     * @param U
     *            Array of U components
     * @param V
     *            Array of V components
     * @param nx
     *            Inner dimension of grid.
     * @param ny
     *            Outer dimension of grid.
     * @param config
     *            Parameters for drawing the stream lines.
     * 
     * @return A <code>StreamLineContainer</code> containing the coordinates for
     *         the stream lines. Line segments will be divided by the sentinel
     *         value (-99999, -99999).
     */
    private StreamLineContainer strmpakInternal(DataSource U, DataSource V,
            int nx, int ny, StrmPakConfig config) {
        StreamLineContainer rVal = new StreamLineContainer();

        // Initialize environment of streamline output.
        float minmag2 = minmag * minmag;
        float maxmag2 = maxmag * maxmag;
        ill = 0;
        jll = 0;
        iur = nx - 1;
        jur = ny - 1;
        ium = iur - 1;
        jum = jur - 1;
        asiz = config.asize;

        // Initialize work arrays.
        // A2 porting note: In the fortran, Work was an int/short array, but I'm
        // using byte because it seems the only values ever used in this
        // algorithm are -1, 0, and 1.
        byte[][][] Work = new byte[2][nx][ny];
        for (int i = ill; i <= ium; i++) {
            int ii = i + 1;

            for (int j = jll; j <= jum; j++) {
                int jj = j + 1;

                if ((!((U.getDataValue(i, j) < config.badlo) || U.getDataValue(
                        i, j) > config.badhi))
                        || (!((U.getDataValue(i, jj) < config.badlo) || U
                                .getDataValue(i, jj) > config.badhi))
                        || (!((U.getDataValue(ii, j) < config.badlo) || U
                                .getDataValue(ii, j) > config.badhi))
                        || (!((U.getDataValue(ii, jj) < config.badlo) || U
                                .getDataValue(ii, jj) > config.badhi))
                        || (!((V.getDataValue(i, j) < config.badlo) || V
                                .getDataValue(i, j) > config.badhi))
                        || (!((V.getDataValue(i, jj) < config.badlo) || V
                                .getDataValue(i, jj) > config.badhi))
                        || (!((V.getDataValue(ii, j) < config.badlo) || V
                                .getDataValue(ii, j) > config.badhi))
                        || (!((V.getDataValue(ii, jj) < config.badlo) || V
                                .getDataValue(ii, jj) > config.badhi))) {
                    Work[0][i][j] = -1;
                    Work[1][i][j] = -1;
                    continue;
                }

                if ((minmag <= 0.0f) && (maxmag >= config.badlo)) {
                    continue;
                }
                double mag2 = U.getDataValue(i, j) * U.getDataValue(i, j)
                        + V.getDataValue(i, j) * V.getDataValue(i, j);
                if ((mag2 >= minmag2) && (mag2 <= maxmag2)) {
                    continue;
                }
                mag2 = U.getDataValue(ii, j) * U.getDataValue(ii, j)
                        + V.getDataValue(ii, j) * V.getDataValue(ii, j);
                if ((mag2 >= minmag2) && (mag2 <= maxmag2)) {
                    continue;
                }
                mag2 = U.getDataValue(i, jj) * U.getDataValue(i, jj)
                        + V.getDataValue(i, jj) * V.getDataValue(i, jj);
                if ((mag2 >= minmag2) && (mag2 <= maxmag2)) {
                    continue;
                }
                mag2 = U.getDataValue(ii, jj) * U.getDataValue(ii, jj)
                        + V.getDataValue(ii, jj) * V.getDataValue(ii, jj);
                if ((mag2 >= minmag2) && (mag2 <= maxmag2)) {
                    continue;
                }
                Work[0][i][j] = -1;
                Work[1][i][j] = -1;
            }
        }

        float maxspc4 = (config.maxspc * 4);
        int k = (int) ((maxspc4 >= 0) ? (maxspc4 + 0.5f) : (maxspc4 - 0.5f));
        if (k < 1) {
            k = 1;
        }

        do {
            int i1 = (ill + iur) / 2;
            int i2 = i1 + 1;
            int j1 = (jll + jur) / 2;
            int j2 = j1 + 1;

            boolean again;
            do {
                again = false;
                float mymax = config.maxspc;
                if (k > mymax) {
                    mymax = k;
                }

                if (j1 >= jll) {
                    float rj0 = j1;
                    for (int i = i1; i <= i2 - 1; i += k) {
                        float ri0 = i + 0.5f;
                        StrmLin(U, V, Work, ri0, rj0, config.minspc, mymax,
                                rVal);
                    }
                    j1 -= k;
                    again = true;
                }

                if (i1 >= ill) {
                    float ri0 = i1;
                    for (int j = j1; j <= j2 - 1; j += k) {
                        float rj0 = j + 0.5f;
                        StrmLin(U, V, Work, ri0, rj0, config.minspc, mymax,
                                rVal);

                    }
                    i1 -= k;
                    again = true;
                }

                if (j2 < jur) {
                    float rj0 = j2;
                    for (int i = i1; i <= i2 - 1; i += k) {
                        float ri0 = i + 0.5f;
                        StrmLin(U, V, Work, ri0, rj0, config.minspc, mymax,
                                rVal);
                    }
                    j2 += k;
                    again = true;
                }

                if (i2 <= iur) {
                    float ri0 = i2;
                    for (int j = j1; j <= j2 - 1; j += k) {
                        float rj0 = j + 0.5f;
                        StrmLin(U, V, Work, ri0, rj0, config.minspc, mymax,
                                rVal);
                    }
                    i2 += k;
                    again = true;
                }
            } while (again);

            k /= 2;
        } while (k >= 1);

        return rVal;
    }

    /**
     * This routine draws a single streamline through the point (ri0, rj0). ri0
     * are rj0 real numbers in array index space.
     * 
     * @param U
     *            Array of U components
     * @param V
     *            Array of V components
     * @param Work
     *            Workspace which keeps track of how many streamlines have been
     *            drawn in each cell. A value of -1 designates a cell as having
     *            bad or missing data. 1 is for previously drawn streamlines, 2
     *            includes the streamline currently being drawn.
     * @param ri0
     *            X-coordinate to draw the streamline through. Coordinates are
     *            in array index space.
     * @param rj0
     *            Y-coordinate to draw the streamline through. Coordinates are
     *            in array index space.
     * @param minspc
     *            If greater than one, no two streamlines will approach any
     *            closer than this number of cells. If less than zero, a
     *            streamline will terminate if it runs through 1/minspc
     *            consecutive already occupied cells.
     * @param maxspc
     *            No streamline will be started any closer than this number of
     *            cells to an existing streamline.
     * @param container
     *            <code>StreamLineContainer</code> object accumulating all line
     *            segments necessary to draw stream lines for U and V.
     */
    private void StrmLin(DataSource U, DataSource V, byte[][][] Work,
            float ri0, float rj0, float minspc, float maxspc,
            StreamLineContainer container) {
        if ((ri0 < ill) || (ri0 > iur) || (rj0 < jll) || (rj0 > jur)) {
            return;
        }

        // declare formal arguments
        int side0;
        int i;
        int ii;
        int iii;
        int j;
        int jj;
        int jjj;
        int k;
        int kk;
        int kkk;
        int ovrlap = 0;
        float x;
        float y;
        // FIXME? in fortran SgSide was a size 8 array, indexed 1 -> 8
        int[] SgSide = new int[9];
        float[] SgLoc = new float[9];
        float[] SgSF = new float[9];
        // FIXME? in fortran SgCont was a size 8 array, indexed 1 -> 8
        float[] SgCont = new float[9];

        itrack.clear();
        jtrack.clear();
        LCPnt.clear();
        IPnt.clear();
        JPnt.clear();

        // Initialize some variables.
        int ntb = 1 + Math.min(iur - ill, jur - jll) / 4;
        int track = 0;
        int ntrack = (int) ((maxspc >= 0) ? (maxspc + 0.5f) : (maxspc - 0.5f));
        if (ntrack < ntb) {
            ntrack = ntb;
        }
        int qi0 = (int) ((ri0 >= 0) ? (ri0 + 0.5f) : (ri0 - 0.5f));
        int qj0 = (int) ((rj0 >= 0) ? (rj0 + 0.5f) : (rj0 - 0.5f));
        int kpnt1 = 0;
        int kpnt2 = 0;

        // Loop for trying streamlines in both directions from this point.
        for (int kstrm = 1; kstrm <= 2; kstrm++) {
            int kpnt = 0;
            int dkpnt = 0;
            int btrack = track + 1;

            // NOTE on A2 port: we use named blocks to substitute for common
            // goto points within the original fortran code.
            LABEL_7777: {
                LABEL_7775: {
                    // Determine which side and which cell we are starting with
                    if (Math.abs(ri0 - qi0) < Math.abs(rj0 - qj0)) {
                        if (kstrm == 1) {
                            side0 = 4;
                            i = (int) ((ri0 >= 0) ? (ri0 + 0.5f) : (ri0 - 0.5f));
                            j = (int) rj0;
                            k = Math.max(
                                    ((int) ((maxspc >= 0) ? (maxspc + 0.5f)
                                            : (maxspc - 0.5f)) - 1), 0);
                            for (jjj = Math.max(jll, (j - k)); jjj <= Math.min(
                                    jum, j + k); jjj++) {
                                for (iii = Math.max(ill, (i - k - 1)); iii <= Math
                                        .min(ium, i + k); iii++) {
                                    if (Work[0][iii][jjj] > 0) {
                                        return;
                                    }
                                }
                            }
                            if (i > ium) {
                                break LABEL_7775;
                            }
                        } else {
                            side0 = 2;
                            i = (int) ((ri0 >= 0) ? (ri0 + 0.5f) : (ri0 - 0.5f)) - 1;
                            j = (int) rj0;
                            if (i < 1) {
                                break LABEL_7775;
                            }
                        }

                        x = qi0 - (float) i;
                        y = rj0 - j;
                    } else {
                        if (kstrm == 1) {
                            side0 = 1;
                            i = (int) ri0;
                            j = (int) ((rj0 >= 0) ? (rj0 + 0.5f) : (rj0 - 0.5f));
                            k = Math.max(
                                    ((int) ((maxspc >= 0) ? (maxspc + 0.5f)
                                            : (maxspc - 0.5f)) - 1), 0);
                            for (jjj = Math.max(jll, (j - k)); jjj <= Math.min(
                                    jum, (j + k)); jjj++) {
                                for (iii = Math.max(ill, (i - k - 1)); iii <= Math
                                        .min(ium, (i + k)); iii++) {
                                    if (Work[0][iii][jjj] > 0) {
                                        return;
                                    }
                                }
                            }
                            if (j > jum) {
                                break LABEL_7775;
                            }
                        } else {
                            side0 = 3;
                            i = (int) ri0;
                            j = (int) ((rj0 >= 0) ? (rj0 + 0.5f) : (rj0 - 0.5f)) - 1;
                            if (j < 1) {
                                break LABEL_7775;
                            }
                        }

                        x = ri0 - i;
                        y = qj0 - (float) j;
                    }

                    ii = i + 1;
                    jj = j + 1;

                    // Check if cell has missing values.
                    if (Work[0][i][j] == -1) {
                        break LABEL_7777;
                    }

                    // Determine whether we are working with or against the
                    // flow.
                    float dirflg;
                    double influx;
                    if (side0 == 1) {
                        influx = V.getDataValue(i, j) * (1.0f - x)
                                + V.getDataValue(ii, j) * x;
                    } else if (side0 == 2) {
                        influx = U.getDataValue(ii, j) * (1.0f - y)
                                + U.getDataValue(ii, jj) * y;
                    } else if (side0 == 3) {
                        influx = -(V.getDataValue(i, jj) * (1.0f - x) + V
                                .getDataValue(ii, jj) * x);
                    } else {
                        influx = -(U.getDataValue(i, j) * (1.0f - y) + U
                                .getDataValue(i, jj) * y);
                    }
                    if (influx < 0.0f) {
                        dirflg = -1.0f;
                        dkpnt = -1;
                    } else if (influx > 0.0f) {
                        dirflg = 1.0f;
                        dkpnt = 1;
                    } else {
                        break LABEL_7777;
                    }

                    // Set some initialize values at streamline start point.
                    float rpxi = i;
                    float rpyj = j;
                    int narrow = (ntb + 3) * 3 / 4;
                    boolean done = false;
                    int ntot = (narrow + 1) / 2;
                    int loopct = 0;
                    IPnt.put(kpnt, rpxi + x);
                    JPnt.put(kpnt, rpyj + y);

                    int i1 = 0;
                    int j1 = 0;
                    int i0 = 0;
                    int j0 = 0;
                    int icheck = 0;

                    float xx;
                    float yy;

                    // Start process of crossing this cell, check if we have
                    // missing data.
                    while (true) {
                        if (Work[0][i][j] == -1) {
                            break LABEL_7777;
                        }

                        // Determine if there are already too many streamlines
                        // around.
                        if (track > btrack) {
                            if (minspc < 1.5f) {
                                if (Work[icheck][i][j] == 0) {
                                    ovrlap = 0;
                                } else {
                                    ovrlap += Work[icheck][i][j];
                                    float minspcInv = 1.0f / minspc;
                                    if (ovrlap >= (int) ((minspcInv >= 0) ? (minspcInv + 0.5f)
                                            : (minspcInv - 0.5f))) {
                                        break LABEL_7777;
                                    }
                                }

                                icheck = 1;
                            } else {
                                if (Work[icheck][i][j] > 0) {
                                    break LABEL_7777;
                                }
                                for (int djj = -1; djj <= 0; djj++) {
                                    LABEL_20: for (int dii = -1; dii <= 0; dii++) {
                                        if ((dii == 0) && (djj == 0)) {
                                            continue;
                                        }

                                        if ((dii != 0) && (djj != 0)) {
                                            float minspc707 = minspc * .707f;
                                            k = (int) ((minspc707 >= 0) ? (minspc707 + 0.5f)
                                                    : (minspc707 - 0.5f)) - 1;
                                        } else {
                                            k = (int) ((minspc >= 0) ? (minspc + 0.5f)
                                                    : (minspc - 0.5f)) - 1;
                                        }
                                        if (k > (track - btrack)) {
                                            k = track - btrack;
                                        }
                                        if (k < 1) {
                                            continue;
                                        }

                                        iii = i;
                                        jjj = j;
                                        LABEL_18: for (int l = 1; l <= k; l++) {
                                            iii += dii;
                                            if ((iii < ill) || (iii > ium)) {
                                                continue LABEL_20;
                                            }
                                            jjj += djj;
                                            if ((jjj < jll) || (jjj > jum)) {
                                                continue LABEL_20;
                                            }

                                            if (Work[0][iii][jjj] > 0) {
                                                break LABEL_7777;
                                            }
                                            if (Work[1][iii][jjj] <= 0) {
                                                continue;
                                            }

                                            for (int kkkk = (1 + track - k); kkkk <= track; kkkk++) {
                                                if ((itrack.get(kkkk) == iii)
                                                        && (jtrack.get(kkkk) == jjj)) {
                                                    continue LABEL_18;
                                                }
                                            }
                                            break LABEL_7777;
                                        }
                                    }
                                }
                            }

                            icheck = 1;
                        }

                        // Determine flux contributions from each component.
                        // FIXME? in fortran Flux was a size 8 array, indexed 1
                        // -> 8
                        float[] Flux = { Float.NaN,
                                (-dirflg) * (float) V.getDataValue(i, j),
                                (-dirflg) * (float) V.getDataValue(ii, j),
                                (-dirflg) * (float) U.getDataValue(ii, j),
                                (-dirflg) * (float) U.getDataValue(ii, jj),
                                dirflg * (float) V.getDataValue(ii, jj),
                                dirflg * (float) V.getDataValue(i, jj),
                                dirflg * (float) U.getDataValue(i, jj),
                                dirflg * (float) U.getDataValue(i, j) };

                        // Count total number of in, out, and zero contributions
                        // to net flux.
                        int nin = 0;
                        int nout = 0;
                        for (k = 1; k <= 8; k++) {
                            if (Flux[k] < 0.0f) {
                                nin++;
                            } else if (Flux[k] > 0.0f) {
                                nout++;
                            }
                        }
                        if (nin == 0) {
                            break LABEL_7777;
                        }

                        // Check if there are no exit points in this cell.
                        LABEL_77: {
                            if (nout == 0) {
                                // Determine termination point within this cell.
                                float x1 = -Flux[8] - Flux[7];
                                float x2 = -Flux[4] - Flux[3];
                                if ((x1 + x2) <= 0.0f) {
                                    xx = 0.5f;
                                } else {
                                    xx = x1 / (x1 + x2);
                                }

                                float y1 = -Flux[5] - Flux[6];
                                float y2 = -Flux[1] - Flux[2];
                                if ((y1 + y2) <= 0.0f) {
                                    yy = 0.5f;
                                } else {
                                    yy = y1 / (y1 + y2);
                                }
                                done = true;

                                // go to drawing portion
                                break LABEL_77;
                            }

                            // Make a list of segments on cell border with like
                            // contribution to flux. Record location, side, and
                            // flux contribution.
                            influx = 0.0f;
                            float outflux = 0.0f;
                            int nsg = 0;
                            SgLoc[0] = 0.0f;
                            for (k = 1; k <= 4; k++) {
                                kkk = k + k;
                                kk = kkk - 1;
                                boolean flxflg;
                                if (Flux[kk] < 0.0f) {
                                    flxflg = (Flux[kkk] > 0.0f);
                                } else if (Flux[kk] > 0.0f) {
                                    flxflg = (Flux[kkk] < 0.0f);
                                } else {
                                    flxflg = false;
                                }

                                if (flxflg) {
                                    float xy = Flux[kk]
                                            / (Flux[kk] - Flux[kkk]);
                                    if (xy > 0.0f) {
                                        nsg++;
                                        SgSide[nsg] = k;
                                        SgLoc[nsg] = (k - 1f) + xy;
                                        SgCont[nsg] = xy * Flux[kk] / 2f;
                                        if (SgCont[nsg] < 0.0f) {
                                            influx += SgCont[nsg];
                                        } else {
                                            outflux += SgCont[nsg];
                                        }
                                    }

                                    if (xy < 1.0f) {
                                        nsg++;
                                        SgSide[nsg] = k;
                                        SgCont[nsg] = (1.0f - xy) * Flux[kkk]
                                                / 2f;
                                    }

                                    SgLoc[nsg] = k;
                                } else {
                                    nsg++;
                                    SgLoc[nsg] = k;
                                    SgSide[nsg] = k;
                                    SgCont[nsg] = Flux[kk] + Flux[kkk];
                                }

                                if (SgCont[nsg] < 0.0f) {
                                    influx += SgCont[nsg];
                                } else {
                                    outflux += SgCont[nsg];
                                }
                            }

                            // Adjust the magnitude of the flux segments to make
                            // total flux integrated around the cell zero.
                            // Integrate to get stream function values.
                            float mult = (float) Math.sqrt((-outflux) / influx);
                            SgSF[0] = 0.0f;
                            for (k = 1; k < nsg; k++) {
                                if (SgCont[k] > 0.0f) {
                                    SgSF[k] = SgSF[k - 1] + SgCont[k] / mult;
                                } else {
                                    SgSF[k] = SgSF[k - 1] + SgCont[k] * mult;
                                }
                            }
                            k = nsg + 1;
                            SgSF[nsg] = 0.0f;

                            // Based on side of entry, determine circular
                            // location of endpoint and direction to search for
                            // exit point.
                            float curloc;
                            boolean forward;
                            if (side0 == 1) {
                                curloc = x;
                                forward = ((dirflg * (U.getDataValue(i, j)
                                        * (1.0f - x) + U.getDataValue(ii, j)
                                        * x)) > 0.0f);
                            } else if (side0 == 2) {
                                curloc = 1.0f + y;
                                forward = ((dirflg * (V.getDataValue(ii, j)
                                        * (1.0f - y) + V.getDataValue(ii, jj)
                                        * y)) > 0.0f);
                            } else if (side0 == 3) {
                                curloc = 3.0f - x;
                                forward = ((dirflg * (U.getDataValue(i, jj) * x + U
                                        .getDataValue(ii, jj) * (1.0f - x))) < 0.0f);
                            } else {
                                curloc = 4.0f - y;
                                forward = ((dirflg * (V.getDataValue(i, j) * y + V
                                        .getDataValue(i, jj) * (1.0f - y))) < 0.0f);
                            }

                            // Determine stream function value of entry
                            // location.
                            float curSF = 0.0f;
                            for (kk = 1; kk <= nsg; kk++) {
                                if ((SgLoc[kk] > curloc) || (kk == nsg)) {
                                    k = kk - 1;
                                    if (SgCont[kk] >= 0.0f) {
                                        break LABEL_7777;
                                    }
                                    curSF = SgSF[k] + (SgSF[kk] - SgSF[k])
                                            * (curloc - SgLoc[k])
                                            / (SgLoc[kk] - SgLoc[k]);
                                    if (k < 1) {
                                        k = nsg;
                                    }
                                    break;
                                }
                            }

                            // Search for next occurrence of this value of the
                            // stream function.
                            kkk = k;
                            if (forward) {
                                do {
                                    k = kk;
                                    kk++;
                                    if (kk > nsg) {
                                        kk = 1;
                                    }
                                    if (k == kkk) {
                                        break LABEL_7777;
                                    }
                                } while ((SgCont[kk] <= 0.0f)
                                        || ((SgSF[k] <= curSF) != (curSF < SgSF[kk])));
                            } else {
                                do {
                                    kk = k;
                                    k--;
                                    if (k < 1) {
                                        k = nsg;
                                    }
                                    if (k == kkk) {
                                        break LABEL_7777;
                                    }
                                } while ((SgCont[kk] <= 0.0f)
                                        || ((SgSF[k] <= curSF) != (curSF < SgSF[kk])));
                            }
                            if (k == nsg) {
                                k = 0;
                            }
                            float outloc = SgLoc[k] + (SgLoc[kk] - SgLoc[k])
                                    * (curSF - SgSF[k]) / (SgSF[kk] - SgSF[k]);
                            side0 = SgSide[kk];

                            // Based upon exit side, figure out location in x/y
                            // space.
                            if (side0 == 1) {
                                xx = outloc;
                                yy = 0.0f;
                            } else if (side0 == 2) {
                                xx = 1.0f;
                                yy = outloc - 1.0f;
                            } else if (side0 == 3) {
                                xx = 3.0f - outloc;
                                yy = 1.0f;
                            } else {
                                xx = 0.0f;
                                yy = 4.0f - outloc;
                            }
                        }

                        // Record plotting location for this cell.
                        ntot++;
                        kpnt += dkpnt;
                        if ((ntot > narrow) && (Work[1][i][j] == 0)) {
                            float dx = xx - x;
                            float dy = yy - y;
                            float mag = (float) Math.sqrt(dx * dx + dy * dy);
                            if (mag >= 0.2f) {
                                LCPnt.put(kpnt, Boolean.FALSE);
                                ntot = 0;
                            }
                        }
                        IPnt.put(kpnt, rpxi + xx);
                        JPnt.put(kpnt, rpyj + yy);

                        // Patch to prevent infinite loop.
                        i1 = i0;
                        j1 = j0;
                        i0 = i;
                        j0 = j;

                        // Keep track of cells used so far.
                        Work[1][i][j]++;
                        track++;
                        itrack.put(track, i);
                        jtrack.put(track, j);

                        // Based on exit side, figure out stuff for next cell.
                        if (done) {
                            break LABEL_7775;
                        }
                        if (side0 == 1) {
                            side0 = 3;
                            jj = j;
                            j--;
                            if (j < 1) {
                                break LABEL_7775;
                            }
                            rpyj -= 1.0f;
                            x = xx;
                            y = 1.0f;
                        } else if (side0 == 2) {
                            side0 = 4;
                            i = ii;
                            ii++;
                            if (i > ium) {
                                break LABEL_7775;
                            }
                            rpxi += 1.0f;
                            x = 0.0f;
                            y = yy;
                        } else if (side0 == 3) {
                            side0 = 1;
                            j = jj;
                            jj++;
                            if (j > jum) {
                                break LABEL_7775;
                            }
                            rpyj += 1.0f;
                            x = xx;
                            y = 0.0f;
                        } else if (side0 == 4) {
                            side0 = 2;
                            ii = i;
                            i--;
                            if (i < 1) {
                                break LABEL_7775;
                            }
                            rpxi -= 1.0f;
                            x = 1.0f;
                            y = yy;
                        }

                        // Patch to prevent infinite loop.
                        if ((i == i1) && (j == j1)) {
                            loopct++;
                            if (loopct >= 3) {
                                break LABEL_7777;
                            }
                        } else {
                            loopct = 0;
                        }
                    }
                }

                // Escape point for streamline hitting grid border, source, or
                // sink.
                // allow shorter streamlines in this case.
                ntrack = (ntrack + 1) / 2;
            }

            // Escape point for all others...record kpnt value.
            if (dkpnt == -1) {
                kpnt1 = kpnt;
            } else if (dkpnt == 1) {
                kpnt2 = kpnt;
            }
        }

        // Don't draw this streamline if it is too short.
        if (track < ntrack) {
            for (k = 1; k <= track; k++) {
                Work[1][itrack.get(k)][jtrack.get(k)]--;
            }
            // A2 porting note: this code appeared in the original algorithm but
            // is unnecessary since the values stored to LCPnt don't persist
            // between calls to StrmLin().
            // for (k = kpnt1; k <= kpnt2; k++) {
            // LCPnt.put(k, Boolean.TRUE);
            // }
            return;
        } else {
            for (k = 1; k <= track; k++) {
                Work[0][itrack.get(k)][jtrack.get(k)]++;
            }
        }

        int npass = 5;
        int wgt1 = 6;
        int wgt2 = 88;

        // Do smoothing.
        if (npass > 0) {
            for (int kkkk = 1; kkkk <= npass; kkkk++) {
                k = kpnt1 + 1;
                float im = IPnt.get(kpnt1);
                float xx = IPnt.get(k);
                float jm = JPnt.get(kpnt1);
                float yy = JPnt.get(k);

                for (int kp = kpnt1 + 2; kp <= kpnt2; kp++) {
                    float ip = IPnt.get(kp);
                    float jp = JPnt.get(kp);
                    IPnt.put(k, (wgt1 * (ip + im) + wgt2 * xx) / 100f);
                    JPnt.put(k, (wgt1 * (jp + jm) + wgt2 * yy) / 100f);
                    im = xx;
                    jm = yy;
                    xx = ip;
                    yy = jp;
                    k = kp;
                }
            }
        }

        // Draw streamline.
        List<StreamLinePoint> lineSegment = new ArrayList<StreamLinePoint>(
                kpnt2 - kpnt1 + 1);
        for (i = kpnt1; i <= kpnt2; i++) {
            lineSegment.add(new StreamLinePoint(IPnt.get(i), JPnt.get(i)));
        }
        container.addLine(lineSegment);

        // Draw arrows.
        if (kpnt1 < 0) {
            kkk = kpnt1 + 2;
            LCPnt.put(kkk,
                    LCPnt.get(kpnt1).booleanValue()
                            && LCPnt.get(kpnt1 + 1).booleanValue());
        } else {
            kkk = kpnt1;
        }

        for (k = kkk; k <= kpnt2; k++) {
            if (!LCPnt.get(k)) {
                LCPnt.put(k, Boolean.TRUE);

                int km = ((k < 0) ? k : k - 1);
                int kp = ((k < 0) ? k + 1 : k);

                float dx = IPnt.get(kp) - IPnt.get(km);
                float dy = JPnt.get(kp) - JPnt.get(km);
                float mag = (float) Math.sqrt(dx * dx + dy * dy);
                if (mag != 0.0f) {
                    mag /= asiz;
                    dx /= mag;
                    dy /= mag;
                    x = (IPnt.get(km) + IPnt.get(kp)) / 2f;
                    y = (JPnt.get(km) + JPnt.get(kp)) / 2f;

                    // A2 porting note: these variables are not required in our
                    // port of this code--they were used specifically by the A1
                    // D2D drawing routines
                    // float[] px = { (dy - dx), 0, (0 - dx - dx) };
                    // float[] py = { px[2], 0, (0 - px[0]) };

                    List<StreamLinePoint> arrowHead = new ArrayList<StreamLinePoint>(
                            3);
                    arrowHead.add(new StreamLinePoint((x - (dy - dx)),
                            (y - (-dy - dx))));
                    arrowHead.add(new StreamLinePoint(x, y));
                    arrowHead.add(new StreamLinePoint((x - (-dy - dx)),
                            (y + (dy - dx))));
                    container.addLine(arrowHead);
                }
            }
        }
    }
}
