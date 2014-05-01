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
package com.raytheon.viz.texteditor;

import java.util.HashMap;
import java.util.TimeZone;

/**
 * Constants used in conjunction with text processing for warning related
 * products.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 8, 2008   1737      grichard    Initial creation.
 * Mar 14, 2014  DR 17175  D. Friedman Fixed Atlantic and Samoa time zones.
 *                                     Add short name map.
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public final class TextWarningConstants {

    /**
     * The VTEC Action enumeration
     */
    public static enum vtecActionEnum {
        NEW, CON, EXT, EXA, EXB, UPG, CAN, EXP, COR, ROU;
    }

    /**
     * The VTEC Afos Product enumeration
     */
    public static enum vtecAfosProductEnum {
        WOU, WCN, SVR, TOR, SMW, SVS, MWS, FFA, FLW, FFS, FLS, WSW, NPW, FWW, RFW, CFW, TCV, CWF, NSH, OFF, GLF, TSU;
    }

    /**
     * Lockable text begin element tag
     */
    public static final String BEGIN_ELEMENT_TAG = "<L>";

    /**
     * Lockable text end element tag
     */
    public static final String END_ELEMENT_TAG = "</L>";

    public static final String TTAAII = "TTAAII";

    public static final String METADATA_DB = "metadata";

    public static final String FXA_DB = "fxa";

    public static HashMap<String, TimeZone> timeZoneAbbreviationMap = null;

    public static HashMap<String, TimeZone> timeZoneShortNameMap = null;

    static {
        // build the abbreviation map
        timeZoneAbbreviationMap = new HashMap<String, TimeZone>();
        timeZoneAbbreviationMap.put("A", TimeZone.getTimeZone("US/Alaska"));
        timeZoneAbbreviationMap.put("C", TimeZone.getTimeZone("CST6CDT"));
        timeZoneAbbreviationMap.put("E", TimeZone.getTimeZone("EST5EDT"));
        timeZoneAbbreviationMap.put("G", TimeZone.getTimeZone("Pacific/Guam"));
        timeZoneAbbreviationMap.put("H", TimeZone.getTimeZone("HST"));
        timeZoneAbbreviationMap.put("M", TimeZone.getTimeZone("MST7MDT"));
        timeZoneAbbreviationMap.put("m", TimeZone.getTimeZone("MST"));
        timeZoneAbbreviationMap.put("P", TimeZone.getTimeZone("PST8PDT"));
        timeZoneAbbreviationMap.put("S", TimeZone.getTimeZone("US/Samoa"));
        timeZoneAbbreviationMap.put("V", TimeZone.getTimeZone("America/Puerto_Rico"));

        HashMap<String, TimeZone> t = timeZoneAbbreviationMap;
        timeZoneShortNameMap = new HashMap<String, TimeZone>();
        timeZoneShortNameMap.put("AKST", t.get("A"));
        timeZoneShortNameMap.put("AKDT", t.get("A"));
        timeZoneShortNameMap.put("CST", t.get("C"));
        timeZoneShortNameMap.put("CDT", t.get("C"));
        timeZoneShortNameMap.put("EST", t.get("E"));
        timeZoneShortNameMap.put("EDT", t.get("E"));
        timeZoneShortNameMap.put("CHST", t.get("G"));
        timeZoneShortNameMap.put("ChST", t.get("G"));
        timeZoneShortNameMap.put("HST", t.get("H"));
        timeZoneShortNameMap.put("MST", t.get("m"));
        timeZoneShortNameMap.put("MDT", t.get("M"));
        timeZoneShortNameMap.put("PST", t.get("P"));
        timeZoneShortNameMap.put("PDT", t.get("P"));
        timeZoneShortNameMap.put("SST", t.get("S"));
        timeZoneShortNameMap.put("AST", t.get("V"));
    }

    /**
     * Constructor.
     */
    private TextWarningConstants() {
    }

}
