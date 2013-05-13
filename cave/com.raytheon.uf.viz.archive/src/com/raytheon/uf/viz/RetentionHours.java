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
package com.raytheon.uf.viz;

import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Convenience class for taking retention hours and converting to days/hours.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 2, 2013  1966       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class RetentionHours {

    private int retentionHours;

    /**
     * Constructor with default 7 day retention.
     */
    public RetentionHours() {
        this(7 * TimeUtil.HOURS_PER_DAY);
    }

    /**
     * Constructor specify retention hours.
     * 
     * @param retentionHours
     */
    public RetentionHours(int retentionHours) {
        this.retentionHours = retentionHours;
    }

    /**
     * Set retention to this number of days.
     * 
     * @param days
     */
    public void setDays(int days) {
        retentionHours = days * TimeUtil.HOURS_PER_DAY;
    }

    /**
     * Convert retention hours to days. Note values are truncated so a retention
     * of 23 hours will return 0 days.
     * 
     * @return days
     */
    public int getDays() {
        return retentionHours / TimeUtil.HOURS_PER_DAY;
    }

    /**
     * Get retention in hours.
     * 
     * @return
     */
    public int getHours() {
        return retentionHours;
    }

    /**
     * Set number hours of retention.
     * 
     * @param hours
     */
    public void setHours(int hours) {
        retentionHours = hours;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "RetentionHours [days:" + getDays() + ", hours:" + getHours()
                + "]";
    }
}
