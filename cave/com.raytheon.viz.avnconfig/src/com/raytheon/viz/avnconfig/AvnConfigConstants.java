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
package com.raytheon.viz.avnconfig;

/**
 * Constants for use in AVNFPS Monitoring Rules. Use to generate the display for
 * the Monitoring Rules dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 11, 2009            avarani     Initial creation
 * Oct 29, 1010 7262       rferrel     Added new Rule methods
 * 
 * </pre>
 * 
 * @author avarani
 * @version 1.0
 */

public class AvnConfigConstants {

    /**
     * Rule methods for the mtr tab.
     */
    private static final String[] mtrs_methods = { "AirportOpsThresh",
            "CigCatDelta", "CigMetarThresh", "CigTafThresh", "DDDelta",
            "FFDelta", "FltCatDelta", "FuelAlternate", "LFFMetar",
            "VsbyCatDelta", "VsbyMetarThresh", "VsbyTafThresh", "WxMetar",
            "WxMetarDelta", "WxTafDelta", "WxVsbyDelta", "XFFMetar",
            "CAC_FltCatDelta", "CAC_AirportOpsThresh", "CAC_VsbyMetarThresh",
            "CAC_VsbyTafThresh", "CAC_WxTafDelta" };

    /**
     * Rule methods for the grids tab.
     */
    private static final String[] grids_methods = { "DDDelta", "FFDelta",
            "CigCatDelta", "SkyMismatch", "VsbyCatDelta", "WxTafDelta",
            "WxGridsDelta" };

    /**
     * Rule methods for the ltg tab.
     */
    private static final String[] ltg_methods = { "TSObsDelta" };

    /**
     * Rule methods for the rltg tab.
     */
    private static final String[] rltg_methods = { "TSInTaf", "TSNotInTaf" };

    /**
     * Rule methods for the ccfp tab.
     */
    private static final String[] ccfp_methods = { "TSNotInTaf", "CBNotInTaf" };

    /**
     * Rule methods for the llws tab.
     */
    private static final String[] llws_methods = { "WSinRadar" };

    /**
     * Not sure what this is for and could not find any use in the java, python
     * or xml files.
     */
    @Deprecated
    public static final String[] MONITORING_RULE_FIELDS = { ".type", ".unique",
            ".args", ".msg", ".comment" };

    /**
     * Not sure what this is for and could not find any use in the java, python
     * or xml files.
     */
    @Deprecated
    public static final String ACTIVE_RULES = ".rules.active";

    /**
     * 
     * This enum class associates the tabs name with the methods used to
     * generate it rules and the name of the configuration file that contains
     * the monitoring rules.
     * 
     */
    public enum DataSource {
        mtrs("mtrs", mtrs_methods), ltg("ltg", ltg_methods), rltg("rltg",
                rltg_methods), ccfp("ccfp", ccfp_methods), grids("grids",
                grids_methods), llws("llws", llws_methods);
        /**
         * The methods for the monitoring rules
         */
        private String[] methods;

        /**
         * The file name without the suffix.
         */
        private String filename;

        /**
         * The constructor
         * 
         * @param fname
         *            - the file name for the desired monitoring rules.
         * @param array
         *            - Array of methods used with the monitoring rules.
         */
        private DataSource(String fname, String[] array) {
            methods = array;
            filename = fname;
        }

        /**
         * Get the array of methods used with the monitoring rules.
         * 
         * @return methods
         */
        public String[] getMethods() {
            return methods;
        }

        /**
         * The name of the configuration file that contains the monitoring
         * rules. This assumes the suffix for the file is always ".cfg".
         * 
         * @return filename
         */
        public String getFilename() {
            return filename + ".cfg";
        }
    }

    /**
     * Enumeration of types.
     */
    public enum RuleType {
        vsby, wind, sky, wx, cat;
    }

    /**
     * 
     * The various kinds of triggers. Since trigger should be going away most
     * likely this should be depreciated and eventually go away.
     * 
     */
    public enum triggerType {
        TAF, METAR, CCFP;
    }
}
