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
 * Constants for use in AVNFPS Monitoring Rules
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
    private static final String[] mtrs_methods = { "AirportOpsThresh",
            "CigCatDelta", "CigMetarThresh", "CigTafThresh", "DDDelta",
            "FFDelta", "FltCatDelta", "FuelAlternate", "LFFMetar",
            "VsbyCatDelta", "VsbyMetarThresh", "VsbyTafThresh", "WxMetar",
            "WxMetarDelta", "WxTafDelta", "WxVsbyDelta", "XFFMetar",
            "CAC_FltCatDelta", "CAC_AirportOpsThresh", "CAC_VsbyMetarThresh",
            "CAC_VsbyTafThresh", "CAC_WxTafDelta" };

    private static final String[] grids_methods = { "DDDelta", "FFDelta",
            "CigCatDelta", "SkyMismatch", "VsbyCatDelta", "WxTafDelta",
            "WxGridsDelta" };

    private static final String[] ltg_methods = { "TSObsDelta" };

    private static final String[] rltg_methods = { "TSInTaf", "TSNotInTaf" };

    private static final String[] ccfp_methods = { "TSNotInTaf", "CBNotInTaf" };

    private static final String[] llws_methods = { "WSinRadar" };

    public static final String[] MONITORING_RULE_FIELDS = { ".type", ".unique",
            ".args", ".msg", ".comment" };

    public static final String ACTIVE_RULES = ".rules.active";

    public enum DataSource {
        mtrs("mtrs", mtrs_methods), ltg("ltg", ltg_methods), rltg("rltg",
                rltg_methods), ccfp("ccfp", ccfp_methods), grids("grids",
                grids_methods), llws("llws", llws_methods);

        private String[] methods;

        private String filename;

        private DataSource(String fname, String[] array) {
            methods = array;
            filename = fname;
        }

        public String[] getMethods() {
            return methods;
        }

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

    public enum triggerType {
        TAF, METAR, CCFP;
    }
}
