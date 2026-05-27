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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Utility to substitute variables in WarnGen xml files with values from WarnGen
 * preferences.
 *
 * <pre>
 *
 *    SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 03, 2008           chammack  Initial Creation.
 * Jan 17, 2018  7130     randerso  Improved variable substitution code to
 *                                  handle variables embedded within a string
 *
 * </pre>
 *
 * @author chammack
 */
public class PreferenceUtil {

    /**
     * Pattern to find variable references. Variable names must contain only
     * word characters: a-zA-Z_0-9
     */
    private static Pattern VARIABLE_PATTERN = Pattern.compile("\\$(\\w+)");

    /**
     * substitute variable references with values
     *
     * @param str
     * @param localSite
     * @return substituted string
     */
    public static String substitute(String str, String localSite) {
        String substituted = str.trim();

        Matcher m = VARIABLE_PATTERN.matcher(substituted);
        while (m.find()) {
            String varName = m.group(1);
            String repl;
            if ("warngenCWAFilter".equalsIgnoreCase(varName)) {
                // Over-ride the configuration for CWA filter
                repl = localSite;

            } else {
                // get replacement from preference store
                repl = Activator.getDefault().getPreferenceStore()
                        .getString(varName);
            }
            if (!repl.isEmpty()) {
                substituted = substituted.replaceAll("\\$" + varName, repl);
            }
        }

        return substituted;
    }
}
