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
package com.raytheon.uf.viz.monitor.util;

import com.raytheon.uf.viz.monitor.ReportModel;

/**
 * This class contains utility methods.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 26, 2009 2047       grichard    Initial creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public final class ConfigUtil {

    // Private constructor -- all contents must be public static
    private ConfigUtil() {
    }

    /**
     * Method that fetches the visibility as a string.
     * 
     * @param vis
     *            -- whole number that represents visibility in sixteenths of a
     *            statute mile
     * @return String that represents visibility in reportable fractional units
     *         of statute miles
     */
    public static String lookupVisibility(int vis) {
        ReportModel reportModel = ReportModel.getInstance();
        return reportModel.getReportableStringVisibilityLookUp().get(vis);
    }

    /**
     * Method that fetches the visibility as a float.
     * 
     * @param vis
     *            -- string that represents visibility in reportable fractional
     *            units of statute miles
     * @return float that represents visibility in statute miles
     */
    public static float lookupVisibility(String vis) {
        ReportModel reportModel = ReportModel.getInstance();
        // System.out.println("Vis = " + vis);
        return reportModel.getReportableFloatVisibilityLookUp().get(vis.trim());
    }

    /**
     * Method that fetches the visibility as a float.
     * 
     * @param vis
     *            -- float that represents visibility in reportable fractional
     *            units of meters
     * @return float that represents visibility in statute miles
     */
    public static float lookupVisibility(float vis) {
        ReportModel reportModel = ReportModel.getInstance();
        // System.out.println("Vis = " + vis);
        return reportModel.metersToStatuteMiles(vis);
    }

}
