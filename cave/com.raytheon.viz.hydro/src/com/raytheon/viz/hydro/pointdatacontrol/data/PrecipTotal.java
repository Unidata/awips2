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

import java.util.Date;

import com.raytheon.viz.hydrocommon.HydroConstants;

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
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class PrecipTotal {
    public static final String OK_CHAR = " ";
    public static final String MISSING_CHAR = "m";
    public static final String REJECTED_CHAR = "r";
    
    /** Total precip amount */
    private double total = HydroConstants.MISSING_VALUE;
    
    /** Number of seconds covered */
    private int secondsCovered = HydroConstants.MISSING_VALUE;
    
    /**
     * The time of the match.
     */
    private Date matchTime = null;
    
    /**
     * Was a duration match found?
     */
    private boolean durationMatchFound = false;
    
    /** Physical element. */
    private String pe = null;
    
    /** Type Source. */
    private String ts = null;
    
    /**
     * Number of hours covered in the time period.
     */
    private double hoursCovered;
    
    /**
     * Percent of time filled by data.
     */
    private double percentFilled;
    
    /**
     * Date error data object.
     */
    private DataErr err = new DataErr();
    
    /** Value indicator */
    private String valueIndicator = null;
    
    /** quality code. */
    private String qc = "Z";

    /**
     * @return the total
     */
    public double getTotal() {
        return total;
    }

    /**
     * @param total the total to set
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
     * @param secondsCovered the secondsCovered to set
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
     * @param matchTime the matchTime to set
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
     * @param durationMatchFound the durationMatchFound to set
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
     * @param pe the pe to set
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
     * @param ts the ts to set
     */
    public void setTs(String ts) {
        this.ts = ts;
    }
    
    public static class DataErr {
        private boolean negval;

        private boolean negdiff;

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
         * @param negval the negval to set
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
         * @param negdiff the negdiff to set
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
         * @param largediff the largediff to set
         */
        public void setLargediff(boolean largediff) {
            this.largediff = largediff;
        }
    }

    /**
     * @return the hoursCovered
     */
    public double getHoursCovered() {
        return hoursCovered;
    }

    /**
     * @param hoursCovered the hoursCovered to set
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
     * @param percentFilled the percentFilled to set
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

    /**
     * @return the valueIndicator
     */
    public String getValueIndicator() {
        return valueIndicator;
    }

    /**
     * @param valueIndicator the valueIndicator to set
     */
    public void setValueIndicator(String valueIndicator) {
        this.valueIndicator = valueIndicator;
    }

    /**
     * @return the qc
     */
    public String getQc() {
        return qc;
    }

    /**
     * @param qc the qc to set
     */
    public void setQc(String qc) {
        this.qc = qc;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(pe + " " + ts + " " + percentFilled + " " + secondsCovered + " " + total);
        
        return sb.toString();
    }
}
