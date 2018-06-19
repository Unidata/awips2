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
package com.raytheon.viz.hydro.pointdatacontrol.data;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2008            mpduff      Initial creation
 * Jul 21, 2015 4500       rjpeter     Use Number in blind cast.
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class IngestFilter {
    private String lid = null;

    private String pe = null;

    private int dur;

    private String ts = null;

    private String extremum = null;

    private int tsRank;

    public IngestFilter() {

    }

    public IngestFilter(Object[] data) {
        int i = 0;
        setLid((String) data[i++]);
        setPe((String) data[i++]);
        setDur(((Number) data[i++]).intValue());
        setTs((String) data[i++]);
        setExtremum((String) data[i++]);
        setTsRank(((Number) data[i++]).intValue());
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
     * @return the extremum
     */
    public String getExtremum() {
        return extremum;
    }

    /**
     * @param extremum
     *            the extremum to set
     */
    public void setExtremum(String extremum) {
        this.extremum = extremum;
    }

    /**
     * @return the tsRank
     */
    public int getTsRank() {
        return tsRank;
    }

    /**
     * @param tsRank
     *            the tsRank to set
     */
    public void setTsRank(int tsRank) {
        this.tsRank = tsRank;
    }

}
