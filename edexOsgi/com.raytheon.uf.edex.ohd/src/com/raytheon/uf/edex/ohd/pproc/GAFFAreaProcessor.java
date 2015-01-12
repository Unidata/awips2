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
package com.raytheon.uf.edex.ohd.pproc;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * Processes areal values for FFG.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 08, 2011            mpduff      Initial creation
 * Mar 28, 2014   2952     mpduff      Changed to use UFStatus for logging.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GAFFAreaProcessor {
    private static final float MISSING_VALUE_FLOAT = -99.0f;

    private static final IUFStatusHandler log = UFStatus
            .getHandler(GAFFAreaProcessor.class);

    private int xor;

    private int yor;

    private int height;

    private int width;

    private short[] data;

    private double minAreaCoverage;

    private int duration;

    private long validTimeMillis;

    private boolean goodAvgValue = true;

    private double percentValid = 0;

    private double avgVal = 0;

    private final GAFFDB db = new GAFFDB();

    /**
     * Default Constructor.
     */
    public GAFFAreaProcessor() {

    }

    public GAFFAreaProcessor(int xor, int yor, int height, int width,
            short[] data, double minAreaCoverage, int duration,
            long validTimeMillis) {
        this.xor = xor;
        this.yor = yor;
        this.height = height;
        this.width = width;
        this.data = data;
        this.minAreaCoverage = minAreaCoverage;
        this.duration = duration;
        this.validTimeMillis = validTimeMillis;
    }

    public void processAreas() {
        int[] rows = null;
        int[] begCol = null;
        int[] endCol = null;

        Object[] rs = db.getGeoAreaIds();
        /* begin loop on basins */
        if ((rs != null) && (rs.length > 0)) {
            for (int i = 0; i < rs.length; i++) {
                String areaId = (String) rs[i];

                /*
                 * read the HRAP bin coords for the area and extract the
                 * information from the blob fields.
                 */
                Object[] lineSegsRs = db.getLineSegs(areaId);
                log.debug(lineSegsRs.length + " rows in the lineSegsRs");
                int numRows = lineSegsRs.length;
                rows = new int[numRows];
                begCol = new int[numRows];
                endCol = new int[numRows];

                for (int j = 0; j < numRows; j++) {
                    Object[] oa = (Object[]) lineSegsRs[j];
                    rows[j] = (Integer) oa[0];
                    begCol[j] = (Integer) oa[1];
                    endCol[j] = (Integer) oa[2];
                }

                /* do crude qc check for corrupted linesegs */
                if ((numRows > 3000) || (numRows <= 0)) {
                    log.warn("Invalid number of HRAP rows " + numRows
                            + "  for " + areaId + ".");
                } else if ((rows[0] > 10000) || (rows[0] < 0)
                        || (begCol[0] > 10000) || (begCol[0] < 0)
                        || (endCol[0] > 10000) || (endCol[0] < 0)) {
                    log.warn("Invalid HRAP info for " + areaId
                            + ".  Check LineSegs blobspace!");

                } else {
                    /* compute average FFG value for basin and areal coverage */
                    try {
                        computeAvgFfg(areaId, lineSegsRs.length, rows, begCol,
                                endCol);
                    } catch (Exception e) {
                        log.error(
                                "Error computing average FFG value for basin and areal coverage",
                                e);
                    }
                    /*
                     * if average FFG value for basin successfully computed AND
                     * minimum areal coverage exceeded, then write to
                     * ContingencyValue table
                     */
                    if (goodAvgValue) {
                        if (percentValid > minAreaCoverage) {
                            log.info("AreaId = " + areaId + ", value = "
                                    + avgVal + " inches, area covered = "
                                    + percentValid);
                            try {
                                db.writeContingency(areaId, validTimeMillis,
                                        duration, avgVal);
                            } catch (DataAccessLayerException e) {
                                log.error(
                                        "Error inserting data into ContingencyValue table",
                                        e);
                            } catch (Exception e) {
                                log.error("Error writing contingency data", e);
                            }
                        } else {
                            log.info("AreaId = " + areaId
                                    + " -- minimum areal coverage not exceeded");
                        }
                    } else {
                        log.info("AreaId = " + areaId
                                + " -- outside of site's area");
                    }
                }
            }
        } else {
            log.info("No basins found in GeoArea table");
        }
    }

    /*
     * computes the mean areal FFG value for a single basin and returns a single
     * value. The value is computed in mm and changed to inches in this routine.
     */
    private void computeAvgFfg(String areaId, int numRows, int[] rows,
            int[] begCol, int[] endCol) {
        /* compute average FFG value for basin and areal coverage */
        int missCnt = 0;
        int totalCnt = 0;
        int valCnt = 0;
        double max = Integer.MIN_VALUE;
        double min = Integer.MAX_VALUE;
        double sum = 0.0;
        double rawVal;

        for (int i = 0; i < numRows; i++) {
            totalCnt += endCol[i] - begCol[i];

            /* loop on the number of columns in each row */
            for (int jcol = begCol[i]; jcol <= endCol[i]; jcol++) {
                /*
                 * sum the value and increment the cnts. note that the array
                 * index method must match the method by which the grid was
                 * originally loaded
                 */
                int row = rows[i] - yor;

                // Grid starts outside the window
                if (row < 0) {
                    continue;
                }
                int col = jcol - xor;

                /*
                 * check that box is within site's area. if not, return with
                 * status set to -1
                 */
                if ((row > (height - 1)) || (col > (width - 1))) {
                    goodAvgValue = false;
                }

                // if grid ends outside window
                if (row * width + col >= data.length) {
                    continue;
                }
                rawVal = data[row * width + col] / 100;

                if (rawVal != MISSING_VALUE_FLOAT) {
                    sum += rawVal;
                    if (rawVal > max) {
                        max = rawVal;
                    }
                    if (rawVal < min) {
                        min = rawVal;
                    }
                    valCnt++;
                } else {
                    missCnt++;
                }
            }
        }

        /*
         * compute the avg ffg value as the average of all the bins within the
         * area that have valid area_id data.
         */
        if (log.isPriorityEnabled(Priority.DEBUG)) {
            log.debug(areaId + " bincnts: total: " + totalCnt + "(= valid "
                    + valCnt + " + msg " + missCnt + ")");
        }

        if (totalCnt <= 0) {
            percentValid = 0;
        } else {
            percentValid = valCnt / totalCnt;
        }

        if (valCnt > 0) {
            avgVal = (sum / valCnt) / 25.4;
        } else {
            avgVal = 0;
            max = 0;
            min = 0;
        }

        if (log.isPriorityEnabled(Priority.DEBUG)) {
            log.debug(areaId + ": Sum/Cnt=unadjstd avg => " + sum + "/"
                    + valCnt + " = " + avgVal + "; max,min=" + max + "," + min);
        }

        /*
         * adjust the returned value if it is less than some minimal number;
         * this is due to the nature of the precip data, especially the radar
         * data which contains super-tiny values
         */
        if (avgVal < 0.00001) {
            avgVal = 0;
        }
        if (max < 0.00001) {
            max = 0;
        }
        if (min < 0.00001) {
            min = 0;
        }
    }

    /**
     * @return the xor
     */
    public int getXor() {
        return xor;
    }

    /**
     * @param xor
     *            the xor to set
     */
    public void setXor(int xor) {
        this.xor = xor;
    }

    /**
     * @return the yor
     */
    public int getYor() {
        return yor;
    }

    /**
     * @param yor
     *            the yor to set
     */
    public void setYor(int yor) {
        this.yor = yor;
    }

    /**
     * @return the height
     */
    public int getHeight() {
        return height;
    }

    /**
     * @param height
     *            the height to set
     */
    public void setHeight(int height) {
        this.height = height;
    }

    /**
     * @return the width
     */
    public int getWidth() {
        return width;
    }

    /**
     * @param width
     *            the width to set
     */
    public void setWidth(int width) {
        this.width = width;
    }

    /**
     * @return the data
     */
    public short[] getData() {
        return data;
    }

    /**
     * @param data
     *            the data to set
     */
    public void setData(short[] data) {
        this.data = data;
    }

    /**
     * @return the minAreaCoverage
     */
    public double getMinAreaCoverage() {
        return minAreaCoverage;
    }

    /**
     * @param minAreaCoverage
     *            the minAreaCoverage to set
     */
    public void setMinAreaCoverage(double minAreaCoverage) {
        this.minAreaCoverage = minAreaCoverage;
    }

    /**
     * @return the duration
     */
    public int getDuration() {
        return duration;
    }

    /**
     * @param duration
     *            the duration to set
     */
    public void setDuration(int duration) {
        this.duration = duration;
    }

    /**
     * @return the validTimeMillis
     */
    public long getValidTimeMillis() {
        return validTimeMillis;
    }

    /**
     * @param validTimeMillis
     *            the validTimeMillis to set
     */
    public void setValidTimeMillis(long validTimeMillis) {
        this.validTimeMillis = validTimeMillis;
    }
}
