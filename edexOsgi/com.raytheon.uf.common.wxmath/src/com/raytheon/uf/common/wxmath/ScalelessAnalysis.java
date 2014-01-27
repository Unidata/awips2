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
package com.raytheon.uf.common.wxmath;

import java.lang.ref.SoftReference;
import java.util.Arrays;

/**
 * Port of scaleless_analysis.c.
 * 
 * Code has been cleaned up and altered for Java cleanliness and multi-threading
 * safety, but original syserr printfs remain.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 13, 2013 2262       njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ScalelessAnalysis {

    private static final int EIGHT = 8;

    /**
     * Flag for turning on some of the prints for debugging.
     */
    private static final boolean DEBUG = false;

    private static final int right = 1;

    private static final int left = 2;

    private static final int edgeno = 8;

    private static final int edgeyes = 16;

    private static final int full = 32;

    private static final int allcon = -1;

    // don't need the first 8 values because the original code never used them
    private static final int[] octwrap = { // 0, 1, 2, 3, 4, 5, 6, 7,
    0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7 };

    private static final int octmagd[] = { 0, 1, 2, 3, 4, 3, 2, 1, 0, 1, 2, 3,
            4, 3, 2, 1, 0, 1, 2, 3, 4, 3, 2, 1 };

    private static final double rat1 = Math.tan(Math.atan(1.0) / 2);

    private static final double rat2 = Math.tan((Math.atan(1.0) / 2) * 3);

    private static final double sqrt2 = Math.sqrt(2.0);

    /**
     * Node struct
     */
    private static class Node {
        Node prev;

        Node next;

        int base;

        int loc;
    }

    /**
     * Container for the results of the init_distance_tables method.
     */
    private static class DistanceTables {

        int nx;

        int ny;

        int m_swath = 0;

        /** i coord list for octants **/
        short[][] ioct = new short[ScalelessAnalysis.EIGHT][];

        /** j coord list for octants **/
        short[][] joct = new short[ScalelessAnalysis.EIGHT][];

        /** distance for each coord in octant **/
        float[][] doct = new float[ScalelessAnalysis.EIGHT][];

        /** start point of each octant **/
        int[][] oct0 = new int[ScalelessAnalysis.EIGHT][];

        /** last right side point of each octant **/
        int[][] roct = new int[ScalelessAnalysis.EIGHT][];

        /** mid point of each octant **/
        int[][] moct = new int[ScalelessAnalysis.EIGHT][];

        /** first left point of each octant **/
        int[][] loct = new int[ScalelessAnalysis.EIGHT][];

        /** flag for whether edge points exist **/
        byte[][] eoct = new byte[ScalelessAnalysis.EIGHT][];
    }

    private static SoftReference<DistanceTables> previousDistTable;

    /**
     * Initializes the distance tables
     * 
     * @param nx
     * @param ny
     * @return the distance tables
     */
    private static DistanceTables init_distance_tables(int nx, int ny) {
        DistanceTables dt = new DistanceTables();
        dt.nx = nx;
        dt.ny = ny;

        if (ny > nx) {
            nx = ny;
        }
        nx = (nx * 3 + 1) / 2;
        if (nx < 10) {
            nx = 10;
        }

        /* fprintf(stderr,"rat1,rat2 %f %f\n",rat1,rat2); */

        int sxy = nx * 3 / 2;
        /* fprintf(stderr,"sxy %d\n",sxy); */
        int sxym = sxy - 1;
        int n_swath = sxy * 2;
        int n_pts = sxy * sxy;

        float[] dlookup = new float[n_pts];
        short[] binidx = new short[n_pts];
        byte[] pttype = new byte[n_pts];

        /* Handle the special case of the nearest two rows. */
        pttype[sxy] = pttype[1] = -edgeyes;
        pttype[1 + sxy] = edgeyes;
        // original code had if/else statement here, but there's
        // only one possible path ever
        pttype[2 * sxy] = -edgeyes;
        pttype[1 + 2 * sxy] = edgeyes;
        pttype[2 + 2 * sxy] = edgeno;
        pttype[2 + sxy] = edgeyes;
        pttype[2] = -edgeyes;

        /* Calculate the distances and bin indices. */
        for (int jj = 0, kk = 0; jj < sxy; jj++) {
            for (int ii = 0; ii < sxy; ii++, kk++) {
                if (kk == 0) {
                    dlookup[kk] = 0;
                    binidx[kk] = -1;
                    continue;
                }
                double dist;
                int bb;
                if (ii == jj) {
                    bb = ii;
                    dist = bb * sqrt2;
                } else {
                    dist = Math.sqrt(ii * ii + jj * jj);
                    bb = (int) (0.99999 + dist / sqrt2);
                }
                dlookup[kk] = (float) dist;
                binidx[kk] = (short) (bb * 2 - 2);
                double rat;
                if (ii == 0) {
                    rat = (float) (jj) / 0.1;
                } else if (jj == 0) {
                    rat = 0.1 / (float) (ii);
                } else {
                    rat = (float) (jj) / (float) (ii);
                }
                if (ii <= 2 && jj <= 2) {
                    continue;
                }
                if (rat > rat1 && rat < rat2) {
                    pttype[kk] = edgeno;
                } else {
                    pttype[kk] = -edgeno;
                }
            }
        }

        /* Move points not at the very edge of their bin to the previous bin, */
        /* identify points adjacent to the other octant. */
        for (int jj = 0, kk = 0; jj < sxy; jj++) {
            for (int ii = 0; ii < sxy; ii++, kk++) {
                if (ii <= 2 && jj <= 2) {
                    continue;
                }
                if (pttype[kk] > 0) {
                    if (ii == sxym || jj == sxym) {
                        continue;
                    }
                    if (binidx[kk] >= binidx[kk + sxy + 1]) {
                        binidx[kk]--;
                    }
                    if (pttype[kk + sxym] < 0 || pttype[kk - sxym] < 0) {
                        pttype[kk] = edgeyes;
                    }
                } else if (jj > ii) {
                    if (jj < sxym && binidx[kk] >= binidx[kk + sxy]) {
                        binidx[kk]--;
                    }
                    if (pttype[kk + 1] > 0) {
                        pttype[kk] = -edgeyes;
                    }
                } else {
                    if (ii < sxym && binidx[kk] >= binidx[kk + 1]) {
                        binidx[kk]--;
                    }
                    if (pttype[kk + sxy] > 0) {
                        pttype[kk] = -edgeyes;
                    }
                }
            }
        }

        /* set up the pointers to the info we will cache about each octant */
        dt.m_swath = n_swath - 1;
        int n_oct = 2 * n_pts / 3;

        int[] oct0_1 = new int[2 * n_swath];
        int[] loct_1 = new int[2 * n_swath];
        int[] roct_1 = new int[2 * n_swath];
        int[] moct_1 = new int[2 * n_swath];
        byte[] eoct_1 = new byte[2 * n_swath];
        float[] doct_1 = new float[2 * n_oct];
        Arrays.fill(oct0_1, -1);
        Arrays.fill(loct_1, -1);
        Arrays.fill(roct_1, -1);
        Arrays.fill(moct_1, -1);

        int[] oct0_2 = new int[2 * n_swath];
        int[] loct_2 = new int[2 * n_swath];
        int[] roct_2 = new int[2 * n_swath];
        int[] moct_2 = new int[2 * n_swath];
        byte[] eoct_2 = new byte[2 * n_swath];
        float[] doct_2 = new float[2 * n_oct];
        Arrays.fill(oct0_2, -1);
        Arrays.fill(loct_2, -1);
        Arrays.fill(roct_2, -1);
        Arrays.fill(moct_2, -1);

        // The original code was just organizing the pointers in
        // memory to point at the right spot in the arrays. We are
        // doing the same here.
        for (int oo = 0; oo < EIGHT; oo++) {
            dt.ioct[oo] = new short[EIGHT * n_oct];
            dt.joct[oo] = new short[EIGHT * n_oct];
            if ((oo % 2) == 0) {
                dt.doct[oo] = doct_1;
                dt.eoct[oo] = eoct_1;
                dt.oct0[oo] = oct0_1;
                dt.loct[oo] = loct_1;
                dt.roct[oo] = roct_1;
                dt.moct[oo] = moct_1;
            } else {
                dt.doct[oo] = doct_2;
                dt.eoct[oo] = eoct_2;
                dt.oct0[oo] = oct0_2;
                dt.loct[oo] = loct_2;
                dt.roct[oo] = roct_2;
                dt.moct[oo] = moct_2;
            }
        }

        /* For each swath, create a list if indices in that references */
        /* the square arrays for quadrants 0 and 1. */
        int nb = sxy * n_swath;
        int[] kswath0 = new int[nb];
        int[] kswath1 = new int[nb];
        int[] eswath0 = new int[n_swath];
        int[] eswath1 = new int[n_swath];
        int mswath0 = 0;
        int mswath1 = 0;
        Arrays.fill(kswath0, -1);
        Arrays.fill(kswath1, -1);
        Arrays.fill(eswath0, -1);
        Arrays.fill(eswath1, -1);
        for (int jj = 0, kk = 0; jj < sxy; jj++) {
            for (int ii = 0; ii < sxy; ii++, kk++) {
                int bb = binidx[kk];
                if (bb < 0) {
                    continue;
                }
                if (pttype[kk] > 0) {
                    if (jj < ii) {
                        continue;
                    }
                    int oo = bb * sxy + jj - ii;
                    kswath1[oo] = kk;
                    if (bb > mswath1) {
                        mswath1 = bb;
                    }
                    if (oo > eswath1[bb]) {
                        eswath1[bb] = oo;
                    }
                } else if (ii > jj) {
                    int oo = bb * sxy + jj;
                    kswath0[oo] = kk;
                    if (bb > mswath0) {
                        mswath0 = bb;
                    }
                    if (oo > eswath0[bb]) {
                        eswath0[bb] = oo;
                    }
                }
            }
        }

        if (DEBUG) {
            System.err.printf("mswath0 %d\n", mswath0);
            for (int bb = 0; bb <= mswath0; bb++) {
                System.err.printf("%d ", eswath0[bb]);
            }
            System.err.printf("\n");
            for (int oo = 0, bb = 0; bb < n_swath; bb++) {
                for (int ii = 0; ii < sxy; ii++, oo++) {
                    System.err.printf("%d ", kswath0[oo]);
                }
                System.err.printf("\n");
            }
            System.err.printf("mswath1 %d\n", mswath1);
            for (int bb = 0; bb <= mswath1; bb++) {
                System.err.printf("%d ", eswath1[bb]);
            }
            System.err.printf("\n");
            // System.err.printf("kswath1\n");
            for (int oo = 0, bb = 0; bb < n_swath; bb++) {
                for (int ii = 0; ii < sxy; ii++, oo++) {
                    System.err.printf("%d ", kswath1[oo]);
                }
                System.err.printf("\n");
            }
        }

        /* Set up all the info for octant 0 and like octants. */
        int ss = 0;
        int bb = 0;
        int jj = 0;
        for (ss = bb = 0; bb <= mswath0; bb++) {
            dt.oct0[0][bb] = ss;
            if (eswath0[bb] < 0) {
                continue;
            }
            int oo0 = bb * sxy;

            // System.err.printf("bb,ss,oo0,eswath0 %d %d %d %d\n", bb, ss, oo0,
            // eswath0[bb]);

            for (int oo = eswath0[bb]; oo >= oo0; oo--) {
                int kk = kswath0[oo];
                if (kk < 0) {
                    continue;
                }
                jj = -kk / sxy;
                int ii = kk + jj * sxy;

                // System.err.printf("oo,kk, ii,jj, ss  %d %d  %d %d  %d\n", oo,
                // kk, ii, jj, ss);

                dt.doct[0][ss] = dlookup[kk];
                dt.ioct[0][ss] = (short) ii;
                dt.joct[0][ss] = (short) jj;
                dt.ioct[2][ss] = (short) -jj;
                dt.joct[2][ss] = (short) ii;
                dt.ioct[4][ss] = (short) -ii;
                dt.joct[4][ss] = (short) -jj;
                dt.ioct[6][ss] = (short) jj;
                dt.joct[6][ss] = (short) -ii;
                if (jj == 0) {
                    dt.moct[0][bb] = dt.loct[0][bb] = ss;
                }
                dt.roct[0][bb] = ss;
                if (pttype[kk] == -edgeyes) {
                    dt.eoct[0][bb] = edgeyes;
                }
                ss++;
            }

            for (int oo = oo0 + 1; oo <= eswath0[bb]; oo++) {
                int kk = kswath0[oo];
                if (kk < 0) {
                    continue;
                }
                jj = kk / sxy;
                int ii = kk - jj * sxy;

                // System.err.printf("oo,kk, ii,jj, ss  %d %d  %d %d  %d\n", oo,
                // kk, ii, jj, ss);

                if (dt.loct[0][bb] < 0) {
                    dt.loct[0][bb] = ss;
                }
                dt.doct[0][ss] = dlookup[kk];
                dt.ioct[0][ss] = (short) ii;
                dt.joct[0][ss] = (short) jj;
                dt.ioct[2][ss] = (short) -jj;
                dt.joct[2][ss] = (short) ii;
                dt.ioct[4][ss] = (short) -ii;
                dt.joct[4][ss] = (short) -jj;
                dt.ioct[6][ss] = (short) jj;
                dt.joct[6][ss] = (short) -ii;
                ss++;
            }
            dt.eoct[0][bb] |= 1;
            if ((bb % 2) == 1) {
                if (dt.moct[0][bb] < 0 || (dt.eoct[0][bb] & edgeyes) == 0) {
                    continue;
                }
                if ((ss - dt.oct0[0][bb]) * 3 > jj * 2) {
                    dt.eoct[0][bb] |= full;
                }
            } else {
                dt.eoct[0][bb] |= full;
            }
        }
        dt.oct0[0][bb] = ss;

        /* Set up all the info for octant 1 and like octants. */
        for (int ii = ss = bb = 0; bb <= mswath1; bb++) {
            dt.oct0[1][bb] = ss;
            if (eswath1[bb] < 0) {
                continue;
            }
            int oo0 = bb * sxy;
            for (int oo = eswath1[bb]; oo >= oo0; oo--) {
                int kk = kswath1[oo];
                if (kk < 0) {
                    continue;
                }
                ii = kk / sxy;
                jj = kk - ii * sxy;
                dt.doct[1][ss] = dlookup[kk];
                dt.ioct[1][ss] = (short) ii;
                dt.joct[1][ss] = (short) jj;
                dt.ioct[3][ss] = (short) -jj;
                dt.joct[3][ss] = (short) ii;
                dt.ioct[5][ss] = (short) -ii;
                dt.joct[5][ss] = (short) -jj;
                dt.ioct[7][ss] = (short) jj;
                dt.joct[7][ss] = (short) -ii;
                if (ii == jj) {
                    dt.moct[1][bb] = dt.loct[1][bb] = ss;
                }
                dt.roct[1][bb] = ss;
                if (pttype[kk] == edgeyes) {
                    dt.eoct[1][bb] = edgeyes;
                }
                ss++;
            }
            for (int oo = oo0 + 1; oo <= eswath1[bb]; oo++) {
                int kk = kswath1[oo];
                if (kk < 0) {
                    continue;
                }
                jj = kk / sxy;
                ii = kk - jj * sxy;
                if (dt.loct[1][bb] < 0) {
                    dt.loct[1][bb] = ss;
                }
                dt.doct[1][ss] = dlookup[kk];
                dt.ioct[1][ss] = (short) ii;
                dt.joct[1][ss] = (short) jj;
                dt.ioct[3][ss] = (short) -jj;
                dt.joct[3][ss] = (short) ii;
                dt.ioct[5][ss] = (short) -ii;
                dt.joct[5][ss] = (short) -jj;
                dt.ioct[7][ss] = (short) jj;
                dt.joct[7][ss] = (short) -ii;
                ss++;
            }
            dt.eoct[1][bb] |= 1;
            if ((bb % 2) == 1) {
                if (dt.moct[1][bb] < 0 || (dt.eoct[1][bb] & edgeyes) == 0) {
                    continue;
                }
                if ((ss - dt.oct0[1][bb]) * 3 > (jj - ii) * 2) {
                    dt.eoct[1][bb] |= full;
                }
            } else {
                dt.eoct[1][bb] |= full;
            }
        }
        dt.oct0[1][bb] = ss;

        if (DEBUG) {
            kswath0 = new int[n_pts];
            for (int oo = 0; oo < EIGHT; oo++) {
                System.err.printf("dump octant %d", oo);
                for (int kk = bb = 0; kk < n_swath;) {
                    System.err.printf("\n%d %d   %d   %d %d %d>> ", kk,
                            dt.eoct[oo][kk], dt.oct0[oo][kk], dt.roct[oo][kk],
                            dt.moct[oo][kk], dt.loct[oo][kk]);
                    kk++;
                    for (; bb < dt.oct0[oo][kk]; bb++) {
                        int ii = dt.ioct[oo][bb];
                        jj = dt.joct[oo][bb];
                        System.err.printf("%d:%d ", ii, jj);
                        if (ii >= 0 && jj >= 0) {
                            kswath0[ii + sxy * jj] = 1;
                        }
                    }
                }
                System.err.printf("\n");
            }
            System.err.println("kswath0");
            for (jj = sxy - 1; jj >= 0; jj--) {
                int ii = 0;
                for (int kk = jj * sxy; ii < sxy; ii++, kk++) {
                    System.err.printf("%d ", (int) (kswath0[kk]));
                }
                System.err.printf("\n");
            }
        }

        return dt;
    }

    /**
     * Scalelessly analyzes data into a grid.
     * 
     * @param xind
     *            the x indices of the values
     * @param yind
     *            the y indices of the values
     * @param values
     *            the actual data values
     * @param nv
     *            the total number of points
     * @param nx
     *            the number of x points of the output grid
     * @param ny
     *            the number of y points of the output grid
     * @return float[nx * ny] of the scalelessly analyzed grid
     */
    public static float[] scaleless_analysis(float[] xind, float[] yind,
            float[] values, int nv, int nx, int ny) {
        // System.err.printf("in scaleless_analysis\n");

        /* Verify input counts. */
        int nvv = nv;
        int nxx = nx;
        int nyy = ny;
        if (nvv <= 0 || nxx <= 0 || nyy <= 0 || nxx >= 1000 || nyy >= 1000) {
            return null;
            // System.err.printf("nvv %d   nxx %d nyy %d\n", nvv, nxx, nyy);
        }

        int nx1 = nxx - 1;
        int ny1 = nyy - 1;
        int nnn = nxx * nyy;
        int imin = -nxx / 2;
        int imax = nx1 - imin;
        int jmin = -nyy / 2;
        int jmax = ny1 - jmin;
        // System.err.printf("imin,imax,jmin,jmax %d %d %d %d\n", imin, imax,
        // jmin, jmax);

        /* Compute range of grid indicies over which we will scan for data. */
        float xmin = 999999;
        float ymin = 999999;
        float xmax = -999999;
        float ymax = -999999;
        // original code had different pointers for each array, since we
        // know the length and just use an index to access it, we can reuse
        // the same variables
        for (int rp0 = 0; rp0 < nvv; rp0++) {
            if (xind[rp0] < xmin) {
                xmin = xind[rp0];
            } else if (xind[rp0] > xmax) {
                xmax = xind[rp0];
            }
        }
        if (xmax < 0 || xmin > nx1) {
            return null;
        }
        for (int rp0 = 0; rp0 < nvv; rp0++) {
            if (yind[rp0] < ymin) {
                ymin = yind[rp0];
            } else if (yind[rp0] > ymax) {
                ymax = yind[rp0];
            }
        }
        if (ymax < 0 || ymin > ny1) {
            return null;
        }

        // System.err.printf("xmin,xmax,ymin,ymax %f %f %f %f\n", xmin, xmax,
        // ymin, ymax);

        if (xmin > -1) {
            imin = -1;
        } else if (xmin > imin) {
            imin = -(int) (0.5 - xmin);
        }
        if (xmax < nxx) {
            imax = nxx;
        } else if (xmax < imax) {
            imax = (int) (0.5 + xmax);
        }
        if (ymin > -1) {
            jmin = -1;
        } else if (ymin > jmin) {
            jmin = -(int) (0.5 - ymin);
        }
        if (ymax < nyy) {
            jmax = nyy;
        } else if (ymax < jmax) {
            jmax = (int) (0.5 + ymax);
        }

        // System.err.printf("imin,imax,jmin,jmax %d %d %d %d\n", imin, imax,
        // jmin, jmax);

        int nxd = 1 + imax - imin;
        int nyd = 1 + jmax - jmin;
        int nnd = nxd * nyd;
        int ddd = jmin * nxd + imin;
        int ddx = nxd - nxx;

        // System.err.printf("nxd %d nyd %d %d %d %d %d \n", nxd, nyd, imin,
        // imax,
        // jmin, jmax);

        DistanceTables dt = null;
        if (previousDistTable != null) {
            DistanceTables cached = previousDistTable.get();
            if (cached != null) {
                if (cached.nx == nxx && cached.ny == nyy) {
                    dt = cached;
                }
            }
        }
        if (dt == null) {
            dt = init_distance_tables(nxx, nyy);
            if (dt != null) {
                previousDistTable = new SoftReference<DistanceTables>(dt);
            }
        }
        if (dt == null) {
            return null;
        }

        // System.err.printf("have distance tables\n");

        /* Allocate and initialize all of our work grids. */
        float[] grid = new float[nx * ny];
        float[] raw = new float[nnd];
        int[] counts = new int[nnd];
        Arrays.fill(raw, 1e37f);
        Arrays.fill(grid, 1e37f);
        int raw0 = -ddd;
        int counts0 = -ddd;

        /* Assign the values to appropriate grid point in our data work grid. */
        int nvs = 0;
        int ii;
        // original code used multiple variables here to keep track of pointers,
        // we instead use a single variable for the index
        for (int rp = 0; rp < nvv; rp++) {
            if (values[rp] > 1e36) {
                continue;
            }
            int jj;
            if (xind[rp] < 0) {
                ii = -(int) (0.5 - xind[rp]);
            } else {
                ii = (int) (0.5 + xind[rp]);
            }
            if (yind[rp] < 0) {
                jj = -(int) (0.5 - yind[rp]);
            } else {
                jj = (int) (0.5 + yind[rp]);
            }
            int bb = 0;
            if (ii < imin) {
                bb -= (imin - ii);
                ii = imin;
            } else if (ii > imax) {
                bb -= (ii - imax);
                ii = imax;
            } else {
                ii = (int) (xind[rp] + 0.5);
            }
            if (jj < jmin) {
                bb -= (jmin - jj);
                jj = jmin;
            } else if (jj > jmax) {
                bb -= (jj - jmax);
                jj = jmax;
            } else {
                jj = (int) (yind[rp] + 0.5);
            }
            int k0 = ii + nxd * jj;
            if (counts[counts0 + k0] == 0) {
                nvs++;
            }
            if (bb < 0) {
                if (counts[counts0 + k0] != 0 && bb < counts[counts0 + k0]) {
                    continue;
                }
                raw[raw0 + k0] = values[rp];
                counts[counts0 + k0] = bb;
            } else if (counts[counts0 + k0] <= 0) {
                raw[raw0 + k0] = values[rp];
                counts[counts0 + k0] = 1;
            } else {
                raw[raw0 + k0] += values[rp];
                counts[counts0 + k0]++;
            }
        }

        // System.err.printf("raw, counts set from obs\n");

        /* work arrays with info about each grid resolved observation */
        float[] dists = new float[nnd];
        Arrays.fill(dists, 1e37f);
        int dists0 = -ddd;
        byte[] octant = new byte[nnd];
        int[] nearest = new int[nnd];
        byte fillByte = -1;
        Arrays.fill(octant, fillByte);
        Arrays.fill(nearest, -1);
        int octant0 = -ddd;
        int nearest0 = -ddd;
        short[] obsi = new short[nvs];
        short[] obsj = new short[nvs];
        int[] srchlist = new int[nvs];
        // original code had pointers to the start of the memory, we
        // thankfully don't need that, index 0 is the start
        short[][] obsrch = new short[EIGHT][];
        byte[][] conflict = new byte[EIGHT][];
        short[][] leftcon = new short[EIGHT][];
        short[][] rightcon = new short[EIGHT][];
        for (int oo = 0; oo < EIGHT; oo++) {
            obsrch[oo] = new short[nvs];
            conflict[oo] = new byte[nvs];
            short[] tempL = new short[nvs];
            short fillShort = -1;
            Arrays.fill(tempL, fillShort);
            leftcon[oo] = tempL;
            short[] tempR = new short[nvs];
            Arrays.fill(tempR, fillShort);
            rightcon[oo] = tempR;
        }
        // System.err.printf("have our memory initialized\n");

        /* finalize superobing of multiple points that resolve to one grid point */
        int nsrch = 0;
        // original code had variable nund here, it was removed since
        // it doesn't do anything
        for (int rip = 0, jj = jmin; jj <= jmax; jj++) {
            for (ii = imin; ii <= imax; ii++, rip++) {
                if (counts[rip] == 0) {
                    continue;
                }
                if (counts[rip] > 1) {
                    raw[rip] /= counts[rip];
                }
                srchlist[nsrch] = nsrch;
                int kk = rip;
                dists[kk] = 0;
                nearest[kk] = nsrch;
                obsi[nsrch] = (short) ii;
                obsj[nsrch] = (short) jj;

                // System.err.printf("%d  %d %d %d  %d %f\n", nsrch, ii, jj, kk,
                // counts[ip], raw[rp0]);

                if (ii < 0) {
                    kk = obsrch[3][nsrch] = obsrch[4][nsrch] = obsrch[5][nsrch] = -2;
                } else if (ii >= nxx) {
                    kk = obsrch[0][nsrch] = obsrch[1][nsrch] = obsrch[7][nsrch] = -2;
                }
                if (jj < 0) {
                    kk = obsrch[5][nsrch] = obsrch[6][nsrch] = obsrch[7][nsrch] = -2;
                } else if (jj >= nyy) {
                    kk = obsrch[1][nsrch] = obsrch[2][nsrch] = obsrch[3][nsrch] = -2;
                }
                if (kk < 0) {
                    for (int oo = 0; oo < EIGHT; oo++) {
                        conflict[oo][nsrch] = allcon;
                    }
                    nsrch++;
                    continue;
                }
                if (ii <= 0) {
                    conflict[3][nsrch] = conflict[4][nsrch] = conflict[5][nsrch] = allcon;
                } else if (ii >= nx1) {
                    conflict[0][nsrch] = conflict[1][nsrch] = conflict[7][nsrch] = allcon;
                }
                if (jj <= 0) {
                    conflict[5][nsrch] = conflict[6][nsrch] = conflict[7][nsrch] = allcon;
                } else if (jj >= ny1) {
                    conflict[1][nsrch] = conflict[2][nsrch] = conflict[3][nsrch] = allcon;
                }
                int kkg = ii + jj * nxx;
                grid[kkg] = raw[rip];
                nsrch++;
            }
        }

        if (DEBUG) {
            System.err.printf("nsrch,nvs %d %d\n", nsrch, nvs);
            for (int ss = 0; ss < nvs; ss++) {
                ii = obsi[ss];
                int jj = obsj[ss];
                System.err.printf("%d %d %d   ", ii, jj, srchlist[ss]);
                for (int oo = 0; oo < EIGHT; oo++) {
                    System.err.printf(" %d", (int) (obsrch[oo][ss]));
                }
                System.err.printf("  ");
                for (int oo = 0; oo < EIGHT; oo++) {
                    System.err.printf(" %d", (int) (conflict[oo][ss]));
                }
                System.err.printf("\n");
            }
        }

        /* Make tables that will let us quickly decide whether we are */
        /* in our range of i and j values for raw data points */
        ii = imin - nxd;
        int[] iout = new int[3 * nxd];
        int iout0 = 0 - ii;
        for (int i = ii; i < 3 * nxd + ii; i++) {
            if (i >= 0 && i < nxx) {
                iout[iout0 + i] = 0;
            } else if (i >= imin && i <= imax) {
                iout[iout0 + i] = -1;
            } else {
                iout[iout0 + i] = 1;
            }
        }
        int jj = jmin - nyd;
        int[] jout = new int[3 * nyd];
        int jout0 = 0 - jj;
        for (int j = jj; j < 3 * nyd + jj; j++) {
            if (j >= 0 && j < nyy) {
                jout[jout0 + j] = 0;
            } else if (j >= jmin && j <= jmax) {
                jout[jout0 + j] = -2;
            } else {
                jout[jout0 + j] = 2;
            }
        }

        if (DEBUG) {
            System.err.printf("raw\n");
            for (jj = nyd - 1; jj >= 0; jj--) {
                int kk = jj * nxd;
                for (int i = 0; i < nxd; i++, kk++) {
                    if (raw[kk] > 1e36) {
                        System.err.printf("*** ");
                    } else {
                        System.err.printf("%.1f ", raw[kk]);
                    }
                }
                System.err.printf("\n");
            }
            System.err.printf("\n");

            System.err.printf("dists\n");
            for (jj = nyd - 1; jj >= 0; jj--) {
                int kk = jj * nxd;
                for (int i = 0; i < nxd; i++, kk++) {
                    if (dists[kk] > 1e36) {
                        System.err.printf("*** ");
                    } else {
                        System.err.printf("%.1f ", dists[kk]);
                    }
                }
                System.err.printf("\n");
            }
            System.err.printf("\n");
        }

        /* main loop that establishes initially how far from the nearest ob */
        /* each grid point is */
        int bb = 0;
        while (nsrch > 0) {
            int nsw = 0;
            // System.err.printf("nsrch %d\n", nsrch);
            for (int ss = 0; ss < nsrch; ss++) {
                // System.err.printf("ss %d\n", ss);
                int sss = srchlist[ss];
                // System.err.printf("sss %d\n", sss);
                ii = obsi[sss];
                jj = obsj[sss];
                // System.err.printf("ii,jj %d %d\n", ii, jj);
                boolean any = false;
                for (int oo = 0; oo < EIGHT; oo++) {
                    int osv = obsrch[oo][sss];
                    // System.err.printf("oo,bb,osv %d %d %d\n", oo, bb, osv);
                    if (osv < 0) {
                        continue;
                    }
                    if (dt.eoct[oo][bb] < full) {
                        any = true;
                        if (dt.eoct[oo][bb] == 0) {
                            continue;
                        }
                    }
                    int b, nb;
                    if (osv == left) {
                        b = dt.loct[oo][bb];
                    } else {
                        b = dt.oct0[oo][bb];
                    }
                    // original code had multiple pointers here to iterate over
                    // the values in the arrays, we can use just one variable
                    // since it's the same index for each
                    int iijjddp = b;
                    if (osv == right) {
                        nb = dt.roct[oo][bb];
                    } else {
                        nb = dt.oct0[oo][bb + 1] - 1;
                    }
                    int nin = 0;
                    int nset = 0;
                    int lset = 0;
                    int rset = 0;
                    for (; b <= nb; b++, iijjddp++) {
                        int i = ii + dt.ioct[oo][iijjddp];
                        int j = jj + dt.joct[oo][iijjddp];
                        if (iout[iout0 + i] > 0 || jout[jout0 + j] > 0) {
                            continue;
                        }
                        int islft = b >= dt.loct[oo][bb] ? 1 : 0;
                        int isrgt = b <= dt.roct[oo][bb] ? 1 : 0;
                        int k0 = j * nxd + i;
                        int cc = nearest[nearest0 + k0];
                        int cco = octant[octant0 + k0];
                        if (dists[dists0 + k0] == 0) {
                            conflict[oo][sss] = allcon;
                            conflict[octwrap[oo + 3]][cc] = allcon;
                            conflict[octwrap[oo + 4]][cc] = allcon;
                            conflict[octwrap[oo + 5]][cc] = allcon;
                        } else if (cc < 0) {
                            nearest[nearest0 + k0] = sss;
                            octant[octant0 + k0] = (byte) oo;
                        } else if (conflict[oo][sss] == allcon
                                && conflict[cco][cc] == allcon) {
                            ;
                        } else if (octmagd[oo + 8 - cco] >= 2) {
                            conflict[oo][sss] = allcon;
                            conflict[cco][cc] = allcon;
                        } else if (leftcon[oo][sss] != cc
                                && rightcon[oo][sss] != cc) {
                            int ivec = obsi[cc] - ii;
                            int jvec = obsj[cc] - jj;
                            int cross = dt.ioct[oo][iijjddp] * jvec
                                    - dt.joct[oo][iijjddp] * ivec;
                            if (dists[dists0 + k0] < dt.doct[oo][iijjddp]) {
                                conflict[oo][sss] = allcon;
                                if (cross > 0) {
                                    conflict[cco][cc] |= right;
                                    rightcon[cco][cc] = (short) sss;
                                } else {
                                    conflict[cco][cc] |= left;
                                    leftcon[cco][cc] = (short) sss;
                                }
                            } else if (dists[dists0 + k0] > dt.doct[oo][iijjddp]) {
                                conflict[cco][cc] = allcon;
                                if (cross > 0) {
                                    conflict[oo][sss] |= left;
                                    leftcon[oo][sss] = (short) cc;
                                } else {
                                    conflict[oo][sss] |= right;
                                    rightcon[oo][sss] = (short) cc;
                                }
                            } else if (cross > 0) {
                                conflict[cco][cc] |= right;
                                rightcon[cco][cc] = (short) sss;
                                conflict[oo][sss] |= left;
                                leftcon[oo][sss] = (short) cc;
                            } else {
                                conflict[cco][cc] |= left;
                                leftcon[cco][cc] = (short) sss;
                                conflict[oo][sss] |= right;
                                rightcon[oo][sss] = (short) cc;
                            }
                        }
                        if (iout[iout0 + i] == jout[jout0 + j]) {
                            nin++;
                        }
                        if (dists[dists0 + k0] < dt.doct[oo][iijjddp]) {
                            continue;
                        }
                        nset++;
                        lset += islft;
                        rset += isrgt;
                        dists[dists0 + k0] = dt.doct[oo][iijjddp];
                    }
                    if (nset == 0) {
                        if (dt.eoct[oo][bb] >= full) {
                            obsrch[oo][sss] = -1;
                        }
                        continue;
                    }
                    if (dt.eoct[oo][bb] < full) {
                        ;
                    } else if (lset == 0) {
                        obsrch[oo][sss] = right;
                    } else if (rset == 0) {
                        obsrch[oo][sss] = left;
                    }

                    /* we want to be sure that if we are starting outside, we */
                    /* get a chance to get inside before we end because we had */
                    /* all points outside */
                    int ooo = octwrap[oo + 4];
                    if (nin == 0) {
                        if (obsrch[ooo][sss] == -2) {
                            any = true;
                        } else if (dt.eoct[oo][bb] >= full) {
                            obsrch[oo][sss] = -1;
                        }
                    } else {
                        any = true;
                        if (obsrch[ooo][sss] == -2) {
                            obsrch[ooo][sss] = -1;
                        }
                    }
                }

                if (any) {
                    srchlist[nsw++] = sss;
                }

            }
            nsrch = nsw;
            bb++;
        }

        if (DEBUG) {
            System.err.printf("nsrch,nvs %d %d\n", nsrch, nvs);
            for (int ss = 0; ss < nvs; ss++) {
                ii = obsi[ss];
                jj = obsj[ss];
                System.err.printf("%d %d    ", ii, jj);
                for (int oo = 0; oo < EIGHT; oo++) {
                    System.err.printf(" %d", conflict[oo][ss]);
                }
                System.err.printf("\n");
            }

            System.err.printf("dists\n");
            for (jj = nyd - 1; jj >= 0; jj--) {
                int kk = jj * nxd;
                for (int i = 0; i < nxd; i++, kk++) {
                    if (dists[kk] > 1e36) {
                        System.err.printf("*** ");
                    } else {
                        System.err.printf("%.1f ", dists[kk]);
                    }
                }
                System.err.printf("\n");
            }
            System.err.printf("\n");
        }

        /* Here is where we copy the original observation out to the boundary */
        /* where that makes sense. */
        int oo = 0;
        for (int ss = 0; ss < nvs; ss++) {
            /* Find the first octant that we will fill some of */
            for (oo = 7; oo >= 0; oo--) {
                if (conflict[oo][ss] != allcon) {
                    break;
                }
            }
            if (oo < 0) {
                continue;
            }
            while (oo > 0 && conflict[oo - 1][ss] != allcon) {
                oo--;
            }

            /* Find the last octant that we will fill some of */
            int ooo = oo;
            do {
                int nxto = octwrap[ooo + 1];
                if (conflict[nxto][ss] == allcon) {
                    break;
                }
                ooo = nxto;
            } while (ooo != oo);

            /* get some info about the obs point. */
            ii = obsi[ss];
            jj = obsj[ss];
            int kk0 = ii + jj * nxd;
            float val = raw[raw0 + kk0];

            // System.err.printf("fill %d  %d %d    %d %d %f\n", ss, oo, ooo,
            // ii,
            // jj, val);

            /* The first one, we fill and worry about adjacent distances */
            /* on the right and left, as appropriate. */
            boolean any = true;
            // original code had these variables as pointers initially to
            // the front of array ioct[oo] and joct[oo], so we use them as
            // an index associated with those arrays
            int iip = 0;
            int jjp = 0;
            int nxto = octwrap[oo + 2];
            int di = dt.ioct[nxto][0];
            int dj = dt.joct[nxto][0];
            double ddis = ((oo % 2) == 1) ? Math.sqrt(2.0) : 1;

            // System.err.printf("filling first %d %d\n", oo,
            // (int) (conflict[oo)[ss]));

            /* swath points range, which should be defined immed */
            int b1 = -1;
            int b2 = -1;

            /* edge points, which can be undefined for part swath */
            int eb1 = -1;
            int eb2 = -1;
            eb1 = eb2 = -1;
            int kk = 0;
            int b = 0;
            for (bb = 0; !any; b++, iip++, jjp++) {
                if (b == dt.oct0[oo][bb]) {
                    while ((kk = dt.eoct[oo][bb]) == 0) {
                        bb++;
                    }
                    if ((conflict[oo][ss] | right) == 1) {
                        b1 = dt.loct[oo][bb];
                    } else {
                        b1 = b;
                    }
                    if ((conflict[oo][ss] | left) == 1) {
                        b2 = dt.roct[oo][bb++];
                    } else {
                        b2 = dt.oct0[oo][++bb] - 1;
                    }
                    if ((kk & edgeyes) == 1) {
                        eb1 = b1;
                        if (oo != ooo) {
                            eb2 = b2;
                        }
                    }
                    if ((kk & full) > 0) {
                        any = true;
                    }
                }
                if (b < b1 || b > b2) {
                    continue;
                }
                int i = ii + iip;
                int j = jj + jjp;

                // System.err.printf("%d %d  %d %d  %d %d\n", ii, jj,
                // (int) ioct[oo)[iip], (int) joct[oo)[jjp], i, j);

                if (iout[iout0 + i] != jout[jout0 + j]) {
                    continue;
                }
                any = true;
                kk0 = i + j * nxd;
                int kkg = i + j * nxx;
                raw[raw0 + kk0] = grid[kkg] = val;
                dists[dists0 + kk0] = 0;

                /* check for right distance adjust. */
                if (b == eb1) {
                    double curdis = 0;
                    for (int iii = i, jjj = j;;) {
                        iii -= di;
                        jjj -= dj;
                        if (iout[iout0 + iii] != jout[jout0 + jjj]) {
                            break;
                        }
                        curdis += ddis;
                        kk0 = iii + jjj * nxd;
                        if (curdis >= dists[dists0 + kk0]) {
                            break;
                        }
                        dists[dists0 + kk0] = (float) curdis;
                    }
                } else if (b == eb2) {
                    double curdis = 0;
                    for (int iii = i, jjj = j;;) {
                        iii += di;
                        jjj += dj;
                        if (iout[iout0 + iii] != jout[jout0 + jjj]) {
                            break;
                        }
                        curdis += ddis;
                        kk0 = iii + jjj * nxd;
                        if (curdis >= dists[dists0 + kk0]) {
                            break;
                        }
                        dists[dists0 + kk0] = (float) curdis;
                    }
                }

            }
            if (oo == ooo) {
                continue;
            }

            /* The intermediate ones, we just fill the whole octant. */
            oo = octwrap[oo + 1];
            while (oo != ooo) {
                any = true;
                // original code had these variables as pointers initially to
                // the front of array ioct[oo] and joct[oo], so we use them as
                // an index associated with those arrays
                iip = 0;
                jjp = 0;
                // System.err.printf("filling mid %d\n", oo);
                for (b = bb = 0; any; b++, iip++, jjp++) {
                    if (b == dt.oct0[oo][bb]) {
                        bb++;
                        while (dt.eoct[oo][bb] == 0) {
                            bb++;
                        }
                        if ((kk & full) == 1) {
                            any = false;
                        }
                    }
                    int i = ii + iip;
                    int j = jj + jjp;

                    // System.err.printf("%d %d  %d %d  %d %d\n",ii,jj,(int)ioct[oo)[iip],(int
                    // )joct[oo)[jjp],i,j);

                    if (iout[iout0 + i] != jout[jout0 + j]) {
                        continue;
                    }
                    any = true;
                    kk0 = i + j * nxd;
                    int kkg = i + j * nxx;
                    raw[raw0 + kk0] = grid[kkg] = val;
                    dists[dists0 + kk0] = 0;
                }
                oo = octwrap[oo + 1];
            }

            /* The last one, we fill and worry about adjacent distances */
            /* on the left only. */
            any = true;
            // original code had these variables as pointers initially to
            // the front of array ioct[oo] and joct[oo], so we use them as
            // an index associated with those arrays
            iip = 0;
            jjp = 0;
            nxto = octwrap[oo + 2];
            di = dt.ioct[nxto][0];
            dj = dt.joct[nxto][0];
            ddis = ((oo % 2) == 1) ? Math.sqrt(2.0) : 1;
            /*
             * fprintf(stderr,"filling last %d %d\n",oo,(int)(conflict[oo][ss]));
             */
            eb2 = -1; /* left edge point, which can be undefined for part swath */
            for (b = bb = 0; any; b++, iip++, jjp++) {
                if (b == dt.oct0[oo][bb]) {
                    while ((kk = dt.eoct[oo][bb]) == 0) {
                        bb++;
                    }
                    if ((conflict[oo][ss] | right) == 1) {
                        b1 = dt.loct[oo][bb];
                    } else {
                        b1 = b;
                    }
                    if ((conflict[oo][ss] | left) == 1) {
                        b2 = dt.roct[oo][bb++];
                    } else {
                        b2 = dt.oct0[oo][++bb] - 1;
                    }
                    if ((kk & edgeyes) > 0) {
                        eb2 = b2;
                    }
                    if ((kk & full) > 0) {
                        any = false;
                    }
                }
                if (b < b1 || b > b2) {
                    continue;
                }
                int i = ii + iip;
                int j = jj + jjp;
                /*
                 * fprintf(stderr,"%d %d  %d %d  %d %d\n",ii,jj,(int)*iip,(int)*jjp
                 * ,i,j);
                 */
                if (iout[iout0 + i] != jout[jout0 + j]) {
                    continue;
                }
                any = true;
                kk0 = i + j * nxd;
                int kkg = i + j * nxx;
                raw[raw0 + kk0] = grid[kkg] = val;
                dists[dists0 + kk0] = 0;

                /* check for left distance adjust. */
                if (b != eb2) {
                    continue;
                }
                double curdis = 0;
                for (int iii = i, jjj = j;;) {
                    iii += di;
                    jjj += dj;
                    if (iout[iout0 + iii] != jout[jout0 + jjj]) {
                        break;
                    }
                    curdis += ddis;
                    kk0 = iii + jjj * nxd;
                    if (curdis >= dists[dists0 + kk0]) {
                        break;
                    }
                    dists[dists0 + kk0] = (float) curdis;
                }

            }

        }

        if (DEBUG) {
            System.err.printf("raw\n");
            for (jj = nyd - 1; jj >= 0; jj--) {
                int k = jj * nxd;
                for (int i = 0; i < nxd; i++, k++) {
                    if (raw[k] > 1e36) {
                        System.err.printf("*** ");
                    } else {
                        System.err.printf("%.1f ", raw[k]);
                    }
                }
                System.err.printf("\n");
            }
            System.err.printf("\n");

            System.err.printf("dists\n");
            for (jj = nyd - 1; jj >= 0; jj--) {
                int k = jj * nxd;
                for (int i = 0; i < nxd; i++, k++) {
                    if (dists[k] > 1e36) {
                        System.err.printf("*** ");
                    } else {
                        System.err.printf("%.1f ", dists[k]);
                    }
                }
                System.err.printf("\n");
            }
            System.err.printf("\n");
        }

        int nbase = nsrch = 0;
        int mbase = nxd;
        if (nyd > mbase) {
            mbase = nyd;
        }
        mbase *= 15;
        Node[] nodes = new Node[nnn];
        Node[] bases = new Node[mbase];

        for (int j = 0, kg = 0, k0 = 0; j < nyy; j++, k0 += ddx) {
            for (int i = 0; i < nxx; i++, kg++, k0++) {
                grid[kg] = raw[raw0 + k0];
                Node onenode = nodes[kg];
                if (onenode == null) {
                    onenode = new Node();
                    nodes[kg] = onenode;
                }
                int hi = (int) (dists[dists0 + k0] * 10);
                if (hi <= 0) {
                    continue;
                }
                nsrch++;
                if (hi > nbase) {
                    nbase = hi;
                }
                onenode.loc = kg;
                onenode.base = hi;
                onenode.next = bases[hi];
                if (onenode.next != null) {
                    onenode.next.prev = onenode;
                }
                bases[hi] = onenode;
            }
            // System.err.printf("nbase nsrch %d %d\n", nbase, nsrch);
            // verify_nodes(0);
            // for (i = 0; i < nnn; i++) {
            // if (bases[i] != null)
            // System.err.printf("this=%s, hi=%d, loc=%d, next=%s\n",
            // bases[i], bases[i].base, bases[i].loc, bases[i].next);
            // else
            // System.err.printf("null\n");
            // }
            // for (i = 0; i < nnn; i++) {
            // if (nodes[i] != null)
            // System.err.printf("this=%s, hi=%d, loc=%d, next=%s\n",
            // nodes[i], nodes[i].base, nodes[i].loc, nodes[i].next);
            // else
            // System.err.printf("null\n");
            // }
        }

        /* main loop that sets the value for the furthest grid point */
        float[] qdis = new float[8];
        float[] qval = new float[8];
        while (--nsrch >= 0) {

            while (bases[nbase] == null && nbase > 0) {
                nbase--;
            }
            // System.err.printf("nbase nsrch %d %d\n", nbase, nsrch);
            if (nbase == 0) {
                System.err.printf("prematurely exhausted nodes\n");
                break;
            }
            Node onenode = bases[nbase];
            if (onenode.next != null) {
                onenode.next.prev = null;
            }
            bases[nbase] = onenode.next;
            int kg = onenode.loc;
            // System.err.printf("kg=%d, nbase=%d\n", kg, nbase);
            onenode.prev = onenode.next = null;
            jj = kg / nxx;
            ii = kg - jj * nxx;
            int k0 = ii + jj * nxd;
            // System.err.printf("<%d %d> %f\n", ii, jj, dists[dists0 + k0]);
            dists[dists0 + k0] = 0;

            for (oo = 0; oo < EIGHT; oo++) {
                // System.err.printf("octant %d\n", oo);
                qdis[oo] = qval[oo] = 0;
                float wtot = 0;
                int b = 0;
                int nb = 0;
                boolean ok = true;
                // the original code had pointers that are incremented
                // off the start of the array, we just keep the index
                // into the array and start at 0. iip, jjp, and b are
                // all the same so we just use bb.
                int oop = 0;// oop = oct0[oo];
                int ddp = 0;// ddp = doct[oo];
                for (bb = 0; bb < dt.m_swath && nb == 0 && ok; bb++) {
                    oop++;
                    if (dt.eoct[oo][bb] == 0) {
                        continue;
                    }
                    if ((dt.eoct[oo][bb] & full) > 0) {
                        ok = false;
                    }
                    for (; b < dt.oct0[oo][oop]; b++) {
                        int i = ii + dt.ioct[oo][b];
                        int j = jj + dt.joct[oo][b];
                        if (iout[iout0 + i] > 0 || jout[jout0 + j] > 0) {
                            continue;
                        }
                        // if (ok == 0)
                        // System.err.printf("** ");
                        // System.err.printf("|%d %d %d %d %d %d %d %d ", ii,
                        // jj,
                        // iip, jjp, ioct[oo)[iip], joct[oo)[jjp],
                        // i, j);
                        ok = true;
                        int kk0 = j * nxd + i;
                        if (raw[raw0 + kk0] > 1e36) {
                            if (iout[iout0 + i] != jout[jout0 + j]) {
                                continue;
                            }
                            if (dt.doct[oo][ddp + b] >= dists[dists0 + kk0]) {
                                continue;
                            }
                            int kkg = j * nxx + i;
                            onenode = nodes[kkg];
                            if (onenode.loc != kkg) {
                                System.err.printf(
                                        "node location mismatch %d %d\n",
                                        onenode.loc, kkg);
                                continue;
                            }
                            int hi = (int) (dt.doct[oo][ddp + b] * 10);
                            if (hi >= onenode.base) {
                                continue;
                            }

                            // System.err.printf("  >>> %f %d  %f %d\n",
                            // dists[dists0 + kk0], onenode.base,
                            // doct[oo)[ddp + b], hi);

                            // int nc = verify_nodes(-1);
                            if (onenode.next != null) {
                                onenode.next.prev = onenode.prev;
                            }
                            if (onenode.prev != null) {
                                onenode.prev.next = onenode.next;
                            } else {
                                bases[onenode.base] = onenode.next;
                            }
                            onenode.base = hi;
                            onenode.next = bases[hi];
                            if (onenode.next != null) {
                                onenode.next.prev = onenode;
                            }
                            onenode.prev = null;
                            bases[hi] = onenode;
                            dists[dists0 + kk0] = dt.doct[oo][ddp + b];
                            // verify_nodes(nc);
                            continue;
                        }

                        // System.err.printf(
                        // "%d  %d %d  %d %d  %d %d  %d %d  %f %f\n",
                        //
                        // oo, ii, jj, iip, jjp, ioct[oo)[iip],
                        // joct[oo)[jjp], i, j, doct[oo)[ddp + b],
                        // raw[raw0 + kk0]);

                        if (nb == 0) {
                            qdis[oo] = dt.doct[oo][ddp + b];
                            qval[oo] = raw[raw0 + kk0];
                            wtot = 1 / (dt.doct[oo][ddp + b] * dt.doct[oo][ddp
                                    + b]);
                        } else {
                            float wgt = 1 / dt.doct[oo][ddp + b]
                                    * dt.doct[oo][ddp + b];
                            qval[oo] = (raw[raw0 + kk0] * wgt + qval[oo] * wtot)
                                    / (wgt + wtot);
                            wtot += wgt;
                            if (dt.doct[oo][ddp + b] < qdis[oo]) {
                                qdis[oo] = dt.doct[oo][ddp + b];
                            }
                        }
                        nb++;
                    }
                }
                // if (nb <= 1)
                // System.err.printf("\n");
                // System.err.printf("  %f %f\n", qdis[oo], qval[oo]);
            }

            // System.err.printf("finish\n");
            for (oo = 0; oo < EIGHT; oo++) {
                if (qdis[oo] == 0) {
                    continue;
                }
                if (qdis[octwrap[oo + 3]] > 0 || qdis[octwrap[oo + 5]] > 0
                        || qdis[octwrap[oo + 4]] > 0) {
                    break;
                }
            }
            boolean ss = (oo < EIGHT);
            float wtot = 0;
            float val = 0;
            for (oo = 0; oo < EIGHT; oo++) {
                if (qdis[oo] == 0) {
                    continue;
                }
                // System.err.printf("  %f %f\n", qdis[oo], qval[oo]);
                float wgt = qdis[oo];
                wgt = 1 / (wgt * wgt);
                if (ss) {
                    wgt *= wgt;
                }
                wtot += wgt;
                val += wgt * qval[oo];
            }
            raw[raw0 + k0] = grid[kg] = val / wtot;
            // System.err.printf("== %f %f %f\n", wtot, val, grid[kg]);

        }

        // System.err.printf("nbase,bases[nbase] %d %d\n\n", nbase,
        // (int) (bases[nbase].loc));

        return grid;
    }
}
