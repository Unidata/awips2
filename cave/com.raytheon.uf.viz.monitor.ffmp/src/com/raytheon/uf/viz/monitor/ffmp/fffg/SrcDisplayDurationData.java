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
package com.raytheon.uf.viz.monitor.ffmp.fffg;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 28, 2010            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class SrcDisplayDurationData {
    private String displayName;

    private double durationHrs = 0;

    public SrcDisplayDurationData(String displayName, double durationHrs) {
        this.displayName = displayName;
        this.durationHrs = durationHrs;
    }

    public String getDisplayName() {
        return displayName;
    }

    public double getDurationHrs() {
        return durationHrs;
    }

    public String getDurationHrsString() {
        return String.valueOf(durationHrs);
    }

    public boolean dataMatch(String name, int dur) {
        if ((name.compareTo(displayName) == 0) && (dur == durationHrs)) {
            return true;
        }

        return false;
    }

    public boolean dataMatch(String name, String dur) {
        if ((name.compareTo(displayName) == 0)
                && (dur.compareTo(String.valueOf(durationHrs)) == 0)) {
            return true;
        }

        return false;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return displayName + ":" + durationHrs;
    }
}