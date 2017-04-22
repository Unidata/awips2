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
package com.raytheon.viz.warngen;

/**
 * PreferenceUtil
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Jan 3, 2008             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class PreferenceUtil {

    public static String substitute(String str, String localSite) {
        String trimStr = str.trim();

        if (!trimStr.startsWith("$")) {
            return str;
        }

        String varName = trimStr.substring(1);

        /*
         * Over-ride the configuration for CWA filter
         */
        if (varName.equalsIgnoreCase("warngenCWAFilter")) {
            return localSite;
        }

        return Activator.getDefault().getPreferenceStore().getString(varName);

    }
}
