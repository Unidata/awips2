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
package com.raytheon.viz.hydro.alertalarm;

import java.text.SimpleDateFormat;

/**
 * Alert Alarm constants.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 14, 2008            Eric Babin  Initial creation.
 * 10/21/2008   1617       grichard    Support alert alarm data.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class AlertAlarmConstants {

    public static final SimpleDateFormat smallerTime = new SimpleDateFormat(
            "MM-dd HH:mm");

    public static final SimpleDateFormat alertAlarmTimeFormat = new SimpleDateFormat(
            "EEE MM/dd HH:mm");

    public static final SimpleDateFormat dbDatabaseFormat = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm:ss");// 2008-08-04 07:00:00

    public static final SimpleDateFormat db2DatabaseFormat = new SimpleDateFormat(
    "yyyy-MM-dd HH:mm:ss.SSS");// 2010-11-18 12:34:00.325
    
    public static final SimpleDateFormat monthDay = new SimpleDateFormat(
            "MM-dd");

    /**
     * Used for type selection.
     */
    public static enum TypeOption {
        ALL("All"), OBS("Observed"), FCST("Forecast");

        private String stringValue;

        private TypeOption(String value) {
            this.stringValue = value;
        }

        @Override
        public String toString() {
            return stringValue;
        }
    }

    /**
     * Used by type of alarms.
     */
    public static enum AaOption {
        BOTH("Alert/Alarms"), ALARM("Alarm"), ALERT("Alerts");

        private String stringValue;

        private AaOption(String value) {
            this.stringValue = value;
        }

        @Override
        public String toString() {
            return stringValue;
        }
    }

    /**
     * Used by Exceeding dropdown.
     */
    public static enum CheckOption {
        ANY("Any Limit"), ROC("Rate-of-Change (roc)"), UPPER("Upper Limit"), LOWER(
                "Lower Limit"), DIFF("Diff Limit");

        private String stringValue;

        private CheckOption(String value) {
            this.stringValue = value;
        }

        @Override
        public String toString() {
            return stringValue;
        }
    }

    public static final String UPPER_CHECKSTR = "'upper'";

    public static final String LOWER_CHECKSTR = "'lower'";

    public static final String DIFF_CHECKSTR = "'diff'";

    public static final String ROC_CHECKSTR = "'roc'";

    public static final String ALERT_CATEGSTR = "'alert'";

    public static final String ALARM_CATEGSTR = "'alarm'";
}
