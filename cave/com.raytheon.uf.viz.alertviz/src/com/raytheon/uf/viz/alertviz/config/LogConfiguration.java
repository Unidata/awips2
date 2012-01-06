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
package com.raytheon.uf.viz.alertviz.config;

/**
 * Provides configuration support
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 16, 2008 1433       chammack     Initial creation
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class LogConfiguration {

    private int logRetentionInMinutes;

    /**
     * @return the logRetentionInMinutes
     */
    public int getLogRetentionInMinutes() {
        return logRetentionInMinutes;
    }

    /**
     * @param logRetentionInMinutes
     *            the logRetentionInMinutes to set
     */
    public void setLogRetentionInMinutes(int logRetentionInMinutes) {
        this.logRetentionInMinutes = logRetentionInMinutes;
    }

}
