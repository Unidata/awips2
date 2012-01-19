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
package com.raytheon.viz.hydrobase.data;

import java.util.ArrayList;
import java.util.List;

/**
 * HRAP Bin List object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 15, 2009 2772           mpduff     Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class HrapBinList {
    /** the rows */
    private List<Long> rows = new ArrayList<Long>();

    /** The beginning columns */
    private List<Long> beginCols = new ArrayList<Long>();

    /** The ending columns */
    private List<Long> endCols = new ArrayList<Long>();

    /** The number of rows */
    private long numRows;

    /** The number of bins */
    private long numBins;

    /** Area in square miles */
    private double area;

    /**
     * @return the rows
     */
    public List<Long> getRows() {
        return rows;
    }

    /**
     * @param rows
     *            the rows to set
     */
    public void setRows(List<Long> rows) {
        this.rows = rows;
    }

    /**
     * @return the beginCols
     */
    public List<Long> getBeginCols() {
        return beginCols;
    }

    /**
     * @param beginCols
     *            the beginCols to set
     */
    public void setBeginCols(List<Long> beginCols) {
        this.beginCols = beginCols;
    }

    /**
     * @return the endCols
     */
    public List<Long> getEndCols() {
        return endCols;
    }

    /**
     * @param endCols
     *            the endCols to set
     */
    public void setEndCols(List<Long> endCols) {
        this.endCols = endCols;
    }

    /**
     * @return the numRows
     */
    public long getNumRows() {
        return numRows;
    }

    /**
     * @param numRows
     *            the numRows to set
     */
    public void setNumRows(long numRows) {
        this.numRows = numRows;
    }

    /**
     * @return the numBins
     */
    public long getNumBins() {
        return numBins;
    }

    /**
     * @param numBins
     *            the numBins to set
     */
    public void setNumBins(long numBins) {
        this.numBins = numBins;
    }

    /**
     * @return the area
     */
    public double getArea() {
        return area;
    }

    /**
     * @param area
     *            the area to set
     */
    public void setArea(double area) {
        this.area = area;
    }
}
