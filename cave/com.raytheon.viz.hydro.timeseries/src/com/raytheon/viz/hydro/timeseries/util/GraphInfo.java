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
package com.raytheon.viz.hydro.timeseries.util;

import java.util.ArrayList;
import java.util.List;

/**
 * Graph metadata container
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * May 23, 2018  6748        randerso     Initial creation
 *
 * </pre>
 *
 * @author randerso
 */

public class GraphInfo {
    /** Derived PP mode */
    public static enum DERIVE_PP {
        /** no derived PP */
        NO_PC_TO_PP,

        /** derive by interpolation */
        INTERPOLATE,

        /** derive by assignment */
        ASSIGN;

        /**
         * Get enum value from string.
         *
         * @param s
         *            the string
         * @return the enum value that matches the string or NO_PC_TO_PP if no
         *         match
         */
        public static DERIVE_PP fromString(String s) {
            String prefix = s.toUpperCase();
            for (DERIVE_PP derivePP : values()) {
                if (derivePP.name().startsWith(prefix)) {
                    return derivePP;
                }
            }
            return DERIVE_PP.NO_PC_TO_PP;
        }
    }

    private int graphPos;

    private int xsize;

    private int ysize;

    private boolean yScaleToData = true;

    private String ylinear;

    private boolean showcat = false;

    private DERIVE_PP derivepp = DERIVE_PP.NO_PC_TO_PP;

    private boolean latestfcstonly = true;

    private List<TraceInfo> traceInfoList;

    /**
     * Constructor
     */
    public GraphInfo() {
        traceInfoList = new ArrayList<>();
    }

    /**
     * @return the graphPos
     */
    public int getGraphPos() {
        return graphPos;
    }

    /**
     * @param graphPos
     *            the graphPos to set
     */
    public void setGraphPos(int graphPos) {
        this.graphPos = graphPos;
    }

    /**
     * @return the xsize
     */
    public int getXsize() {
        return xsize;
    }

    /**
     * @param xsize
     *            the xsize to set
     */
    public void setXsize(int xsize) {
        this.xsize = xsize;
    }

    /**
     * @return the ysize
     */
    public int getYsize() {
        return ysize;
    }

    /**
     * @param ysize
     *            the ysize to set
     */
    public void setYsize(int ysize) {
        this.ysize = ysize;
    }

    /**
     * @return true if y scale is Data, false if Category
     */
    public boolean isYscaleToData() {
        return yScaleToData;
    }

    /**
     * @param yScaleToData
     *            true for Data, false for Category
     */
    public void setYscaleToData(boolean yScaleToData) {
        this.yScaleToData = yScaleToData;
    }

    /**
     * @return the ylinear
     */
    public String getYlinear() {
        return ylinear;
    }

    /**
     * @param ylinear
     *            the ylinear to set
     */
    public void setYlinear(String ylinear) {
        this.ylinear = ylinear;
    }

    /**
     * @return the showcat
     */
    public boolean isShowcat() {
        return showcat;
    }

    /**
     * @param showcat
     *            the showcat to set
     */
    public void setShowcat(boolean showcat) {
        this.showcat = showcat;
    }

    /**
     * @return the derivepp
     */
    public DERIVE_PP getDerivepp() {
        return derivepp;
    }

    /**
     * @param derivepp
     *            the derivepp to set
     */
    public void setDerivepp(DERIVE_PP derivepp) {
        this.derivepp = derivepp;
    }

    /**
     * @return the latestfcstonly
     */
    public boolean isLatestfcstonly() {
        return latestfcstonly;
    }

    /**
     * @param latestfcstonly
     *            the latestfcstonly to set
     */
    public void setLatestfcstonly(boolean latestfcstonly) {
        this.latestfcstonly = latestfcstonly;
    }

    /**
     * @return the traceInfoList
     */
    public List<TraceInfo> getTraceInfoList() {
        return traceInfoList;
    }

    /**
     * Add a traceInfo to the list
     *
     * @param traceInfo
     */
    public void addTraceInfo(TraceInfo traceInfo) {
        traceInfoList.add(traceInfo);
    }
}
