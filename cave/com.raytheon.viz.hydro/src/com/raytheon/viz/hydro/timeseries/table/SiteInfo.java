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
package com.raytheon.viz.hydro.timeseries.table;


/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 14, 2008 1520       mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class SiteInfo {
    /** Location Id */
    private String lid = null;
    
    /** Physical Element */
    private String pe = null;
    
    /** Duration */
    private int dur;
    
    /** Type Source */
    private String ts = null;
    
    /** Extremum */
    private String ext = null;
    
    /** Forecast Basis Time */
    private String basisTime = null;
    
    /** Selected Position */
    private boolean selected = false;
    
    
    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("[");
        sb.append("lid = " + lid + "\n");
        sb.append("pe = " + pe + "\n");
        sb.append("dur = " + dur + "\n");
        sb.append("ts = " + ts + "\n");
        sb.append("ext = " + ext + "\n");
        sb.append("basisTime = " + basisTime + "\n");
        sb.append("selected = " + selected + "]\n");

        return sb.toString();
    }

    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * @param lid
     *            the lid to set
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * @return the pe
     */
    public String getPe() {
        return pe;
    }

    /**
     * @param pe
     *            the pe to set
     */
    public void setPe(String pe) {
        this.pe = pe;
    }

    /**
     * @return the dur
     */
    public int getDur() {
        return dur;
    }

    /**
     * @param dur
     *            the dur to set
     */
    public void setDur(int dur) {
        this.dur = dur;
    }

    /**
     * @return the ts
     */
    public String getTs() {
        return ts;
    }

    /**
     * @param ts
     *            the ts to set
     */
    public void setTs(String ts) {
        this.ts = ts;
    }

    /**
     * @return the ext
     */
    public String getExt() {
        return ext;
    }

    /**
     * @param ext
     *            the ext to set
     */
    public void setExt(String ext) {
        this.ext = ext;
    }

    /**
     * @return the basisTime
     */
    public String getBasisTime() {
        return basisTime;
    }

    /**
     * @param basisTime
     *            the basisTime to set
     */
    public void setBasisTime(String basisTime) {
        this.basisTime = basisTime;
    }

    /**
     * @return the selected position
     */
    public boolean isSelected() {
        return selected;
    }

    /**
     * @param selected the selected to set
     */
    public void setSelected(boolean selected) {
        this.selected = selected;
    }
}
