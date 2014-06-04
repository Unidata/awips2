package gov.noaa.nws.ncep.viz.common.ui;

/**
 * This subroutine locates relative minima and maxima over a grid. Up to NMAX
 * maxima and NMIN minima are found. Minima and maxima may be restricted to a
 * specified range of values. If the range bounds are equal (for example 0 and
 * 0), all extrema are found up to the maximum number requested. If the INTFLG
 * is true, interpolations are done to estimate the extrema at off-grid point
 * positions. Radius is a scaling factor with a default of 3. It defines a
 * moving search area where extrema are found.
 * 
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * Nov 3,2010    324         X. Guo       Initial Creation
 * Dec 6,2010                X. Guo       Change clean up function
 * Apr22,2014    1129        B. Hebbard   Move maxHi/maxLo from here to GridRelativeHiLoDisplay so can handle relative to current extent
 * 
 * </pre>
 * 
 * @author xguo
 * @version 1
 */

public class HILORelativeMinAndMaxLocator {

    // Grid point values array
    protected float[] grid;

    // Number of grid columns
    private int gridCol;

    // Number of grid rows
    private int gridRow;

    // Search Radius
    private int gridRadius;

    // Search Interpolation
    private boolean gridInterp;

    // Lower bound on maximum
    private float gridLowerMax;

    // Upper bound on maximum
    private float gridUpperMax;

    // Lower bound on minimum
    private float gridLowerMin;

    // Upper bound on minimum
    private float gridUpperMin;

    // located Flag
    private boolean isHiLoRelativeMinAndMaxLocated;

    // All new min/max information
    private float[] dgMaxCol;

    private float[] dgMaxRow;

    private float[] dgMaxVal;

    private float[] dgMinCol;

    private float[] dgMinRow;

    private float[] dgMinVal;

    // X, Y, V Storage
    private float[] xExtr;

    private float[] yExtr;

    private float[] vExtr;

    // Total number of relative extrema
    private int numOfExtr;

    // Number of clusters
    private int numOfClusters;

    // Missing data vale
    private final float GRID_MISSED_VALUES = -999999.0F;

    private final float GRID_MISSED_DIFF_VALUES = 0.1F;

    private final float GRID_POINT_VALUES_DIFF = 0.000001F;

    private final int GRID_MXM = 256;

    // Error message
    private String hiloRelativeMinAndMaxLocatorErrorMsg = "NO ERROR";

    private final String[] hiloRelativeMinAndMaxLocatorErrorMsgDesc = {
            "NO ERROR", "Invalid Grib point values",
            "Too many Grib point found" };

    protected enum HILORelativeMinAndMaxLocatorErrCodes {
        NO_ERROR(0), INVALID_GRID_VALUES(1), TOO_MANY_GRIB_POINTS(2);
        private final int value;

        HILORelativeMinAndMaxLocatorErrCodes(int value) {
            this.value = value;
        }
    }

    // HILO Min/Max locator default construct
    public HILORelativeMinAndMaxLocator() {
        isHiLoRelativeMinAndMaxLocated = false;
    }

    // HILO Min/Max locator construct
    public HILORelativeMinAndMaxLocator(float[] gridData, int col, int row,
            int radius, boolean interp, float maxLowerBd, float maxUpperBd,
            float minLowerBd, float minUpperBd) {

        isHiLoRelativeMinAndMaxLocated = false;
        grid = gridData;
        gridCol = col;
        gridRow = row;

        if ((isHiLoRelativeMinAndMaxLocated = checkGridValues())) {
            setGridRadius(radius);
            gridInterp = interp;
            gridLowerMax = maxLowerBd;
            gridUpperMax = maxUpperBd;
            gridLowerMin = minLowerBd;
            gridUpperMin = minUpperBd;
            findExtrema();
        }

    }

    /**
     * Set Grid radius
     */
    private void setGridRadius(int radius) {
        if ((radius > gridCol) || (radius > gridRow)) {
            gridRadius = Math.min((gridCol - 1) / 2, (gridRow - 1) / 2);
        } else {
            gridRadius = radius;
        }
    }

    /**
     * Find the extrema
     */
    private boolean findExtrema() {
        int mm;
        float rmin, rmax;
        xExtr = new float[GRID_MXM];
        yExtr = new float[GRID_MXM];
        vExtr = new float[GRID_MXM];

        for (mm = 0; mm < 2; mm++) {
            if (mm == 0) {
                rmin = gridLowerMin;
                rmax = gridUpperMin;
            } else {
                rmin = gridLowerMax;
                rmax = gridUpperMax;
            }

            chkGribPoint(mm, rmin, rmax);
            // Sort extrema
            sortExtrema(mm);
            clusterLocator();

            if (mm == 0) {
                loadMinGridValues();
            } else {
                loadMaxGridValues();
            }
        }
        return true;
    }

    /**
     * Check each point and see if it is present
     */
    private void chkGribPoint(int mm, float min, float max) {
        int i, j, cidx, cidx1, ii, jj;
        float xq, yq, vq, qxm1, qxp1, qym1, qyp1;
        boolean ok, alleq;

        numOfExtr = 0;

        for (j = gridRadius + 1; j <= gridRow - gridRadius; j++) {
            for (i = gridRadius + 1; i <= gridCol - gridRadius; i++) {
                cidx = (j - 1) * gridCol + i - 1;
                // Check the grid point is present. (No visibility check
                // possible at this stage.)
                if (!checkGridMissingValues(grid[cidx])) {
                    ok = true;
                    alleq = true;
                    qxm1 = GRID_MISSED_VALUES;
                    qxp1 = GRID_MISSED_VALUES;
                    qym1 = GRID_MISSED_VALUES;
                    qyp1 = GRID_MISSED_VALUES;

                    jj = j - gridRadius;
                    while ((jj <= (j + gridRadius)) && (ok)) {

                        ii = i - gridRadius;
                        while ((ii <= (i + gridRadius)) && (ok)) {

                            /*
                             * Test point(iq,jq) must not equal reference
                             * point(i,j) or be missing
                             */
                            cidx1 = (jj - 1) * gridCol + ii - 1;
                            if ((ii != i || jj != j)
                                    && (!checkGridMissingValues(grid[cidx1]))) {
                                // Grid points must satisfy extremum condition
                                if (mm == 0) {
                                    if (grid[cidx] > grid[cidx1])
                                        ok = false;
                                } else {
                                    if (grid[cidx] < grid[cidx1])
                                        ok = false;
                                }
                                if (!checkGridPointValuesDiff(grid[cidx],
                                        grid[cidx1]))
                                    alleq = false;

                                if (ok && gridInterp) {
                                    if ((ii == i - 1) && (jj == j))
                                        qxm1 = grid[cidx1];
                                    if ((ii == i + 1) && (jj == j))
                                        qxp1 = grid[cidx1];
                                    if ((jj == j - 1) && (ii == i))
                                        qym1 = grid[cidx1];
                                    if ((jj == j + 1) && (ii == i))
                                        qyp1 = grid[cidx1];
                                }
                            }
                            ii++;
                        }
                        jj++;
                    }

                    if (ok
                            && (!alleq)
                            && ((checkGridPointValuesDiff(min, max)) || ((grid[cidx] >= min) && (grid[cidx] <= max)))) {
                        xq = i;
                        yq = j;
                        vq = grid[cidx];
                        if (gridInterp && (!checkGridMissingValues(qxm1))
                                && (!checkGridMissingValues(qxp1))
                                && (!checkGridMissingValues(qym1))
                                && (!checkGridMissingValues(qyp1))) {
                            float dqdx = (qxp1 - qxm1) / 2;
                            float dqdy = (qyp1 - qym1) / 2;
                            float dqdx2 = qxp1 - 2 * vq + qxm1;
                            float dqdy2 = qyp1 - 2 * vq + qym1;
                            if ((!checkGridPointValuesDiff(dqdx2, 0.0F))
                                    && (!checkGridPointValuesDiff(dqdy2, 0.0F))) {
                                float dx = -dqdx / dqdx2;
                                float dy = -dqdy / dqdy2;
                                xq += dx;
                                yq += dy;
                                vq += dx * dqdx + dy * dqdy;
                            }
                        }
                        // Load each point into array for sorting
                        if (numOfExtr < GRID_MXM) {
                            xExtr[numOfExtr] = xq;
                            yExtr[numOfExtr] = yq;
                            vExtr[numOfExtr] = vq;
                            numOfExtr++;
                        } else {
                            setErrorMessage(getloactorErrorDec(HILORelativeMinAndMaxLocatorErrCodes.TOO_MANY_GRIB_POINTS.value));
                            return;
                        }
                    }
                }
            }
        }
    }

    /**
     * Load minimum grid values
     */
    private void loadMinGridValues() {
        dgMinCol = new float[numOfExtr];
        dgMinRow = new float[numOfExtr];
        dgMinVal = new float[numOfExtr];
        for (int k = 0; k < numOfExtr; k++) {
            dgMinCol[k] = xExtr[k];
            dgMinRow[k] = yExtr[k];
            dgMinVal[k] = vExtr[k];
        }
    }

    /**
     * Load maximum grid values
     */
    private void loadMaxGridValues() {
        dgMaxCol = new float[numOfExtr];
        dgMaxRow = new float[numOfExtr];
        dgMaxVal = new float[numOfExtr];
        for (int k = 0; k < numOfExtr; k++) {
            dgMaxCol[k] = xExtr[k];
            dgMaxRow[k] = yExtr[k];
            dgMaxVal[k] = vExtr[k];
        }
    }

    /**
     * Locate a cluster
     */
    private void clusterLocator() {
        boolean[] keep = new boolean[numOfExtr];
        int[] idofcl = new int[numOfExtr];

        numOfClusters = 1;
        for (int i = 0; i < numOfExtr; i++) {
            keep[i] = true;
        }
        // Loop through all extrema
        for (int icntpt = 1; icntpt <= numOfExtr; icntpt++) {
            if (keep[icntpt - 1]) {
                keep[icntpt - 1] = false;
                chkClusterExist(icntpt, keep, idofcl);
                if (numOfClusters > 1) {
                    int idextr = findCenterOfCluster(idofcl);
                    keep[idextr - 1] = true;
                } else {
                    keep[icntpt - 1] = true;
                }
            }
        }
        cleanRows();
    }

    /**
     * discards all rows of X, Y, V Storage where keep is false
     */
    private void discardRows(boolean[] keep) {
        int itotal = numOfExtr, if1 = 1, if2 = 0;

        while ((if1 + if2) <= itotal) {
            if (!keep[if1 - 1]) {
                for (int iarrix = if1 - 1; iarrix < itotal - 1; iarrix++) {
                    xExtr[iarrix] = xExtr[iarrix + 1];
                    yExtr[iarrix] = yExtr[iarrix + 1];
                    vExtr[iarrix] = vExtr[iarrix + 1];
                    keep[iarrix] = keep[iarrix + 1];
                }
                keep[itotal - 1] = false;
                itotal--;
                if2++;
            } else {
                if1++;
            }
        }
        numOfExtr = itotal;
    }

    /**
     * clean all rows of X, Y, V Storage where keep is false
     */
    private void cleanRows() {
        int itotal = numOfExtr, if1 = 1;

        while (if1 < itotal) {
            if ((Math.abs(xExtr[if1 - 1] - xExtr[if1]) <= gridRadius)
                    && (Math.abs(yExtr[if1 - 1] - yExtr[if1]) <= gridRadius)) {
                for (int iarrix = if1; iarrix < itotal - 1; iarrix++) {
                    xExtr[iarrix] = xExtr[iarrix + 1];
                    yExtr[iarrix] = yExtr[iarrix + 1];
                    vExtr[iarrix] = vExtr[iarrix + 1];
                }
                itotal--;
            } else {
                if1++;
            }
        }
        numOfExtr = itotal;
    }

    /**
     * Determine whether or not a cluster exists
     */
    private void chkClusterExist(int icntpt, boolean[] keep, int[] idofcl) {
        int iptrcl = 1;

        idofcl[numOfClusters - 1] = icntpt;
        // loop through all points in cluster
        while (iptrcl <= numOfClusters) {
            // loop through all extrema
            for (int iptcnt = 1; iptcnt <= numOfClusters; iptcnt++) {
                if (keep[iptcnt - 1]) {
                    if ((Math.abs(xExtr[idofcl[iptrcl - 1] - 1]
                            - xExtr[iptcnt - 1]) <= gridRadius)
                            && (Math.abs(yExtr[idofcl[iptrcl - 1] - 1]
                                    - yExtr[iptcnt - 1]) <= gridRadius)) {
                        numOfClusters++;
                        idofcl[numOfClusters - 1] = iptcnt;
                        keep[iptcnt - 1] = false;
                    }
                }
            }
            iptrcl++;
        }
    }

    /**
     * Find the center of cluster
     */
    private int findCenterOfCluster(int[] idofcl) {
        int iclust, cPos = 0;
        float xt = 0.0F, yt = 0.0F, xcgrav, ycgrav, rmindis, dist, tmpX, tmpY;

        for (iclust = 0; iclust < numOfClusters; iclust++) {
            xt += xExtr[idofcl[iclust] - 1];
            yt += yExtr[idofcl[iclust] - 1];
        }
        xcgrav = xt / numOfClusters;
        ycgrav = yt / numOfClusters;

        rmindis = 99999.0F;
        for (iclust = 0; iclust < numOfClusters; iclust++) {
            tmpX = xExtr[idofcl[iclust] - 1] - xcgrav;
            tmpY = yExtr[idofcl[iclust] - 1] - ycgrav;
            dist = tmpX * tmpX + tmpY * tmpY;
            if (dist < rmindis) {
                rmindis = dist;
                cPos = idofcl[iclust];
            }
        }
        return cPos;
    }

    /**
     * Bubble sort extrema
     */
    private void sortExtrema(int mm) {
        int m = numOfExtr;
        boolean sorted = false;

        while ((!sorted) && (m >= 2)) {
            sorted = true;
            float temp1, temp2, temp3;
            for (int n = 2; n <= m; n++) {
                if (((mm == 0) && (vExtr[n - 2] > vExtr[n - 1]))
                        || ((mm == 1) && (vExtr[n - 2] < vExtr[n - 1]))) {
                    temp1 = vExtr[n - 2];
                    temp2 = xExtr[n - 2];
                    temp3 = yExtr[n - 2];
                    vExtr[n - 2] = vExtr[n - 1];
                    xExtr[n - 2] = xExtr[n - 1];
                    yExtr[n - 2] = yExtr[n - 1];
                    vExtr[n - 1] = temp1;
                    xExtr[n - 1] = temp2;
                    yExtr[n - 1] = temp3;
                    sorted = false;
                }
            }
            m--;
        }
    }

    /**
     * Check missing values
     */
    private boolean checkGridMissingValues(float gData) {
        boolean missed = false;

        if (Math.abs(gData - GRID_MISSED_VALUES) < GRID_MISSED_DIFF_VALUES) {
            missed = true;
        }
        return missed;
    }

    /**
     * Check difference between two grid point values
     */
    private boolean checkGridPointValuesDiff(float data1, float data2) {
        boolean closed = false;

        if (Math.abs(data1 - data2) < GRID_POINT_VALUES_DIFF) {
            closed = true;
        }
        return closed;
    }

    /**
     * Check grid point values size
     * 
     * @return boolean
     */
    private boolean checkGridValues() {
        boolean ck = true;

        if ((grid != null) && ((gridCol * gridRow) != grid.length)) {
            ck = false;
            setErrorMessage(getloactorErrorDec(HILORelativeMinAndMaxLocatorErrCodes.INVALID_GRID_VALUES.value));
        }
        return ck;
    }

    /**
     * Get HILO Min/Max located flag
     * 
     * @return boolean
     */
    public boolean isHILOMinAndMaxLocated() {
        return isHiLoRelativeMinAndMaxLocated;
    }

    /**
     * return error message
     */
    public String getErrorMessage() {
        return this.hiloRelativeMinAndMaxLocatorErrorMsg;
    }

    /**
     * @param String
     *            error message
     */
    private void setErrorMessage(String errorMsg) {
        this.hiloRelativeMinAndMaxLocatorErrorMsg = errorMsg;
    }

    /**
     * Get loactor error description
     */
    private String getloactorErrorDec(int value) {
        return hiloRelativeMinAndMaxLocatorErrorMsgDesc[value];
    }

    /**
     * Get Column positions of maximum
     * 
     * @return []float Grid values are on interval [1, gridCol]
     */
    public float[] getMaxColPositions() {
        return dgMaxCol;
    }

    /**
     * Get Row positions of maximum
     * 
     * @return []float Grid values are on interval [1, gridRow]
     */
    public float[] getMaxRowPositions() {
        return dgMaxRow;
    }

    /**
     * Get maximum values
     * 
     * @return []float
     */
    public float[] getMaxValues() {
        return dgMaxVal;
    }

    /**
     * Get Column positions of minimum
     * 
     * @return []float Grid values are on interval [1, gridCol]
     */
    public float[] getMinColPositions() {
        return dgMinCol;
    }

    /**
     * Get Row positions of minimum
     * 
     * @return []float Grid values are on interval [1, gridRow]
     */
    public float[] getMinRowPositions() {
        return dgMinRow;
    }

    /**
     * Get minimum values
     * 
     * @return []float
     */
    public float[] getMinValues() {
        return dgMinVal;
    }

    /**
     * Display all information
     */
    public void display() {
        int i;
        if (isHiLoRelativeMinAndMaxLocated) {
            System.out.println(" =======All Information===============");
            System.out.println("    Grid:");
            for (i = 0; i < grid.length; i++) {
                System.out.print(grid[i] + "\t");
                if ((i + 1) % 20 == 0) {
                    System.out.print("\n");
                }
            }
            System.out.print("\n");
            System.out.println("    gridCol:" + gridCol + "   gridRow:"
                    + gridRow);
            System.out.println("    Radius:" + gridRadius + " interp:"
                    + gridInterp);
            System.out.println("    Lower Bound on Max:" + gridLowerMax
                    + " Upper Bound on Max:" + gridUpperMax);
            System.out.println("    Lower Bound on Min:" + gridLowerMin
                    + " Upper Bound on Min:" + gridUpperMin);
        }
    }
}
