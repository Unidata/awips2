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
package com.raytheon.uf.common.hydro.data;

import java.util.Date;

import com.raytheon.uf.common.hydro.CommonHydroConstants;

/**
 * Precipitation total data object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 19, 2010 4564       mpduff      Initial creation.
 * May 26, 2016 5571       skorolev    Relocated to a common plugin 
 *                                     for use in both EDEX/CAVE. Cleanup.
 * Jul 25, 2016 4623       skorolev    Combined TotalPrecip.java with PrecipTotal.
 * 
 * </pre>
 * 
 * @author mpduff
 */

public class PrecipTotal {

    /** Total precip amount */
    protected double total = CommonHydroConstants.MISSING_VALUE;

    /** Number of seconds covered */
    protected int secondsCovered = CommonHydroConstants.MISSING_VALUE;

    /** The time of the match. */
    protected Date matchTime;

    /** Was a duration match found? */
    protected boolean durationMatchFound = false;

    /** Physical element. */
    protected String pe = null;

    /** Type Source. */
    protected String ts = null;

    /** Number of hours covered in the time period. */
    protected double hoursCovered;

    /** Percent of time filled by data. */
    protected double percentFilled;

    /** Date error data object. */
    public DataErr err;

    public String lid;

    public float value;

    private boolean summedFlag;

    /** Value indicator */
    private char valueIndicator;

    /** quality code. */
    public char qc;

    private boolean reportedMissing;

    /**
     * Constructor
     */
    public PrecipTotal() {
        setLid("");
        setPe("");
        setTs("");
        setValue(CommonHydroConstants.MISSING_PRECIP);
        setSummedFlag(false);
        setHoursCovered(0);
        setPercentFilled(0);
        setValueIndicator(CommonHydroConstants.MISSING_CHAR);
        setQc('Z');
        setErr(new DataErr());
        setReportedMissing(false);

    }

    /**
     * @return the total
     */
    public double getTotal() {
        return total;
    }

    /**
     * @param total
     *            the total to set
     */
    public void setTotal(double total) {
        this.total = total;
    }

    /**
     * @return the secondsCovered
     */
    public int getSecondsCovered() {
        return secondsCovered;
    }

    /**
     * @param secondsCovered
     *            the secondsCovered to set
     */
    public void setSecondsCovered(int secondsCovered) {
        this.secondsCovered = secondsCovered;
    }

    /**
     * @return the matchTime
     */
    public Date getMatchTime() {
        return matchTime;
    }

    /**
     * @param matchTime
     *            the matchTime to set
     */
    public void setMatchTime(Date matchTime) {
        this.matchTime = matchTime;
    }

    /**
     * @return the durationMatchFound
     */
    public boolean isDurationMatchFound() {
        return durationMatchFound;
    }

    /**
     * @param durationMatchFound
     *            the durationMatchFound to set
     */
    public void setDurationMatchFound(boolean durationMatchFound) {
        this.durationMatchFound = durationMatchFound;
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
     * @return the hoursCovered
     */
    public double getHoursCovered() {
        return hoursCovered;
    }

    /**
     * @param hoursCovered
     *            the hoursCovered to set
     */
    public void setHoursCovered(double hoursCovered) {
        this.hoursCovered = hoursCovered;
    }

    /**
     * @return the percentFilled
     */
    public double getPercentFilled() {
        return percentFilled;
    }

    /**
     * @param percentFilled
     *            the percentFilled to set
     */
    public void setPercentFilled(double percentFilled) {
        this.percentFilled = percentFilled;
    }

    /**
     * @return the err
     */
    public DataErr getErr() {
        return err;
    }

    public void setErr(DataErr err) {
        this.err = err;
    }

    /**
     * @return the valueIndicator
     */
    public char getValueIndicator() {
        return valueIndicator;
    }

    /**
     * @param valueIndicator
     *            the valueIndicator to set
     */
    public void setValueIndicator(char valueIndicator) {
        this.valueIndicator = valueIndicator;
    }

    /**
     * @return the qc
     */
    public char getQc() {
        return qc;
    }

    /**
     * @param qc
     *            the qc to set
     */
    public void setQc(char qc) {
        this.qc = qc;
    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    public float getValue() {
        return value;
    }

    public void setValue(float value) {
        this.value = value;
    }

    public boolean isSummedFlag() {
        return summedFlag;
    }

    public void setSummedFlag(boolean summedFlag) {
        this.summedFlag = summedFlag;
    }

    public boolean isReportedMissing() {
        return reportedMissing;
    }

    public void setReportedMissing(boolean reportedMissing) {
        this.reportedMissing = reportedMissing;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(lid);
        sb.append(" ").append(pe);
        sb.append(" ").append(ts);
        sb.append(" ").append(percentFilled * 100);
        sb.append(" ").append(secondsCovered);
        sb.append(" ").append(total);
        sb.append(" ").append(value);

        return sb.toString();
    }

    /**
     * Data Errors
     * 
     */
    public static class DataErr {
        public boolean negval;

        public boolean negdiff;

        private boolean largediff;

        public DataErr() {
            negval = false;
            negdiff = false;
            largediff = false;
        }

        /**
         * @return the negval
         */
        public boolean isNegval() {
            return negval;
        }

        /**
         * @param negval
         *            the negval to set
         */
        public void setNegval(boolean negval) {
            this.negval = negval;
        }

        /**
         * @return the negdiff
         */
        public boolean isNegdiff() {
            return negdiff;
        }

        /**
         * @param negdiff
         *            the negdiff to set
         */
        public void setNegdiff(boolean negdiff) {
            this.negdiff = negdiff;
        }

        /**
         * @return the largediff
         */
        public boolean isLargediff() {
            return largediff;
        }

        /**
         * @param largediff
         *            the largediff to set
         */
        public void setLargediff(boolean largediff) {
            this.largediff = largediff;
        }
    }
}
