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
package com.raytheon.viz.mpe.ui.dialogs.gagetable;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 15, 2009 2476       mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class GageTableSortSettings {
    private int ascending1 = 1;
    private int ascending2 = -999;
    private int ascending3 = -999;
    private int ascending4 = -999;
    private int sortCol1Index = 0;
    private int sortCol2Index = -999;
    private int sortCol3Index = -999;
    private int sortCol4Index = -999;
    private int columnWidth = 100;

    public GageTableSortSettings() {
       // Empty constructor 
    }
    
    public GageTableSortSettings(boolean ascending, int sortCol1Index,
            int sortCol2Index, int sortCol3Index, int sortCol4Index) {
        this.sortCol1Index = sortCol1Index;
        this.sortCol2Index = sortCol2Index;
        this.sortCol3Index = sortCol3Index;
        this.sortCol4Index = sortCol4Index;
        if (ascending) {
            ascending1 = 1;
        } else {
            ascending1 = 0;
        }
    }

    /**
     * @return the sortCol1Index
     */
    public int getSortCol1Index() {
        return sortCol1Index;
    }

    /**
     * @param sortCol1Index the sortCol1Index to set
     */
    public void setSortCol1Index(int sortCol1Index) {
        this.sortCol1Index = sortCol1Index;
    }

    /**
     * @return the sortCol2Index
     */
    public int getSortCol2Index() {
        return sortCol2Index;
    }

    /**
     * @param sortCol2Index the sortCol2Index to set
     */
    public void setSortCol2Index(int sortCol2Index) {
        this.sortCol2Index = sortCol2Index;
    }

    /**
     * @return the sortCol3Index
     */
    public int getSortCol3Index() {
        return sortCol3Index;
    }

    /**
     * @param sortCol3Index the sortCol3Index to set
     */
    public void setSortCol3Index(int sortCol3Index) {
        this.sortCol3Index = sortCol3Index;
    }

    /**
     * @return the sortCol4Index
     */
    public int getSortCol4Index() {
        return sortCol4Index;
    }

    /**
     * @param sortCol4Index the sortCol4Index to set
     */
    public void setSortCol4Index(int sortCol4Index) {
        this.sortCol4Index = sortCol4Index;
    }

    /**
     * @return the columnWidth
     */
    public int getColumnWidth() {
        return columnWidth;
    }

    /**
     * @param columnWidth the columnWidth to set
     */
    public void setColumnWidth(int columnWidth) {
        this.columnWidth = columnWidth;
    }

    /**
     * @return the ascending1
     */
    public int getAscending1() {
        return ascending1;
    }

    /**
     * @param ascending1 the ascending1 to set
     */
    public void setAscending1(int ascending1) {
        this.ascending1 = ascending1;
    }

    /**
     * @return the ascending2
     */
    public int getAscending2() {
        return ascending2;
    }

    /**
     * @param ascending2 the ascending2 to set
     */
    public void setAscending2(int ascending2) {
        this.ascending2 = ascending2;
    }

    /**
     * @return the ascending3
     */
    public int getAscending3() {
        return ascending3;
    }

    /**
     * @param ascending3 the ascending3 to set
     */
    public void setAscending3(int ascending3) {
        this.ascending3 = ascending3;
    }

    /**
     * @return the ascending4
     */
    public int getAscending4() {
        return ascending4;
    }

    /**
     * @param ascending4 the ascending4 to set
     */
    public void setAscending4(int ascending4) {
        this.ascending4 = ascending4;
    }
}
