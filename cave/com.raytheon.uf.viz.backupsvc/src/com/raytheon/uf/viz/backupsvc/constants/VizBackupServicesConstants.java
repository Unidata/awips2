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
package com.raytheon.uf.viz.backupsvc.constants;

import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * This class defines the public constants for Backup Service Jobs.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jun 14, 2021 84476       Gang Chen   Initial creation
 * Jun 22, 2021 92788       Gang Chen   Added Job File Location and
 *                                      Job File Path
 * Aug 12, 2021 84654       Robert.Blum Cleaned up constant files.
 * Sep 23, 2021 96321       Robert.Blum Added Component column back to UI.
 * Mar 24, 2023 2033487     Lisa.Singh  Fixed "Domain" database column
 *                                      reference.
 * May 01, 2023 2033487     Lisa.Singh  Swapped Domain and Sender columns for Incoming table.
 *                                      Updated the name of constants, and made constant assignments
 *                                      easier to read.
 * </pre>
 *
 * @author Gang Chen
 */

public class VizBackupServicesConstants {
    /*
     * Menu texts in backup service dialog
     */
    public static final String OUTGOING_FILTER_DIALOG_MENU_TEXT = "Filter Dialog for Backup Service Jobs - Outgoing";

    public static final String INCOMING_FILTER_DIALOG_MENU_TEXT = "Filter Dialog for Backup Service Jobs - Incoming";

    /*
     * Possible date and time combination format used for user input validation
     * for filter dialog
     */
    public static final HashMap<String, String> REGEX_TIME_DATE_MAP = new HashMap<String, String>() {
        {
            // 31-03-2021 12:01
            put("^\\d{1,2}-\\d{1,2}-\\d{4}\\s\\d{1,2}:\\d{2}$",
                    "dd-MM-yyyy HH:mm");
            // 31/03/2021 12:01
            put("^\\d{1,2}/\\d{1,2}/\\d{4}\\s\\d{1,2}:\\d{2}$",
                    "dd/MM/yyyy HH:mm");
            // 19:42Z 08-Jul-2021
            put("^\\d{1,2}:\\d{2}[a-z]{1}\\s\\d{1,2}-[a-z]{3}-\\d{4}",
                    "HH:mm'Z' dd-MMM-yyyy");
            // 19:42 08-Jul-2021
            put("^\\d{1,2}:\\d{2}\\s\\d{1,2}-[a-z]{3}-\\d{4}",
                    "HH:mm dd-MMM-yyyy");
            // 19-Jun-2021 22:45
            put("^\\d{1,2}-[a-z]{3}-\\d{4}\\s\\d{1,2}:\\d{2}$",
                    "dd-MMM-yyyy HH:mm");
            // 19/Jun/2021 22:45
            put("^\\d{1,2}/[a-z]{3}/\\d{4}\\s\\d{1,2}:\\d{2}$",
                    "dd/MMM/yyyy HH:mm");
            // Jun-19-2021 22:45
            put("^[a-z]{3}-\\d{1,2}-\\d{4}\\s\\d{1,2}:\\d{2}$",
                    "MMM-dd-yyyy HH:mm");
            // Jun/19/2021 22:45
            put("^[a-z]{3}/\\d{1,2}/\\d{4}\\s\\d{1,2}:\\d{2}$",
                    "MMM/dd/yyyy HH:mm");
            // 31/09/2021
            put("^\\d{1,2}-\\d{1,2}-\\d{4}$", "dd-MM-yyyy");
            // 30/07/2021
            put("^\\d{1,2}/\\d{1,2}/\\d{4}$", "dd/MM/yyyy");
            // 05/30/2021
            put("^\\d{1,2}/\\d{1,2}/\\d{4}$", "MM/dd/yyyy");
            // 05-30-2021
            put("^\\d{1,2}-\\d{1,2}-\\d{4}$", "MM-dd-yyyy");
            // 31/Jun/2021
            put("^\\d{1,2}/[a-z]{3}/\\d{4}$", "dd/MMM/yyyy");
            // 31-Jun-2021
            put("^\\d{1,2}-[a-z]{3}-\\d{4}$", "dd-MMM-yyyy");
            // Jan/31/2021
            put("^[a-z]{3}/\\d{1,2}/\\d{4}$", "MMM/dd/yyyy");
            // Jan-31-2021
            put("^[a-z]{3}-\\d{1,2}-\\d{4}$", "MMM-dd-yyyy");
        }
    };

    /**
     * Below are the column names for the Backup Services Incoming
     * and Outgoing tables in CAVE.
     */

    public static final String VIZ_COLUMN_ID = "ID";

    public static final String VIZ_COLUMN_COMPONENT = "Component";

    public static final String VIZ_COLUMN_JOB_FILE_PATH = "Job File Path";

    /**
     * Domain's meaning is different depending on the table.
     * In the Incoming table, Domain represents the receiving site.
     * In the Outgoing table, Domain represents the sending site.
     */
    public static final String VIZ_COLUMN_DOMAIN = "Domain";

    public static final String VIZ_COLUMN_RECIPIENT = "Recipient";

    public static final String VIZ_COLUMN_SYSTEM_VERSION = "System Version";

    public static final String VIZ_COLUMN_RECIPIENT_VERSION = "Recipient Version";

    public static final String VIZ_COLUMN_LAST_UPDATE = "Last Update";

    public static final String VIZ_COLUMN_STATUS = "Status";

    public static final String VIZ_COLUMN_JOB_FILE_LOCATION = "Job File Location";

    public static final String VIZ_COLUMN_SENDER = "Sender";

    public static final String VIZ_COLUMN_SENDER_VERSION = "Sender Version";

    /**
     * Below are the database column names for backup services.
     */

    public static final String DB_COLUMN_ID = "bksvc_id";

    public static final String DB_COLUMN_COMPONENT = "component";

    public static final String DB_COLUMN_JOB_FILE_PATH = "jobfilepath";

    public static final String DB_COLUMN_SENDER_SITE = "sendersite";

    public static final String DB_COLUMN_RECIPIENT_SITE = "recipientsite";

    public static final String DB_COLUMN_SYSTEM_VERSION = "systemversion";

    public static final String DB_COLUMN_RECIPIENT_VERSION = "recipientversion";

    public static final String DB_COLUMN_SENDER_VERSION = "senderversion";

    public static final String DB_COLUMN_UPDATE_TIME = "updatetime";

    public static final String DB_COLUMN_STATUS = "status";

    public static final String DB_COLUMN_JOB_FILE_LOCATION = "jobfilelocation";

    /*
     * A map used for matching the Outgoing tab's table columns in CAVE with
     * their corresponding sql database columns names. This map is used to
     * display the database values in the tab, as well as for filtering out
     * content in the tables using Backup Service's filter feature.
     */
    public final static Map<String, String> OUTGOING_COL_NAME_MAP;

    /*
     * A map used for matching the Incoming tab's table columns in CAVE with
     * their corresponding database columns names. This map is used to display
     * the sql database values in the tab, as well as for filtering out content
     * in the tables using Backup Service's filter feature.
     */
    public final static Map<String, String> INCOMING_COL_NAME_MAP;

    /**
     * An array used to populate the column names of the table in the Outgoing
     * tab, in order.
     */
    public final static String[] OUTGOING_TABLE_COLUMNS;

    /**
     * An array used to populate the column names of the table in the Incoming
     * tab, in order.
     */
    public final static String[] INCOMING_TABLE_COLUMNS;

    static {
        // Use LinkedHashMap to preserve the order of the keys, which are used to
        // populate the column arrays below.
        Map<String, String> outMap = new LinkedHashMap<>();
        Map<String, String> inMap = new LinkedHashMap<>();

        outMap.put(VIZ_COLUMN_ID,                DB_COLUMN_ID);
        outMap.put(VIZ_COLUMN_COMPONENT,         DB_COLUMN_COMPONENT);
        outMap.put(VIZ_COLUMN_JOB_FILE_PATH,     DB_COLUMN_JOB_FILE_PATH);
        outMap.put(VIZ_COLUMN_DOMAIN,            DB_COLUMN_SENDER_SITE);
        outMap.put(VIZ_COLUMN_RECIPIENT,         DB_COLUMN_RECIPIENT_SITE);
        outMap.put(VIZ_COLUMN_SYSTEM_VERSION,    DB_COLUMN_SYSTEM_VERSION);
        outMap.put(VIZ_COLUMN_RECIPIENT_VERSION, DB_COLUMN_RECIPIENT_VERSION);
        outMap.put(VIZ_COLUMN_LAST_UPDATE,       DB_COLUMN_UPDATE_TIME);
        outMap.put(VIZ_COLUMN_STATUS,            DB_COLUMN_STATUS);
        outMap.put(VIZ_COLUMN_JOB_FILE_LOCATION, DB_COLUMN_JOB_FILE_LOCATION);

        OUTGOING_COL_NAME_MAP = Collections.unmodifiableMap(outMap);

        inMap.put(VIZ_COLUMN_ID,                DB_COLUMN_ID);
        inMap.put(VIZ_COLUMN_COMPONENT,         DB_COLUMN_COMPONENT);
        inMap.put(VIZ_COLUMN_JOB_FILE_PATH,     DB_COLUMN_JOB_FILE_PATH);
        inMap.put(VIZ_COLUMN_DOMAIN,            DB_COLUMN_RECIPIENT_SITE);
        inMap.put(VIZ_COLUMN_SENDER,            DB_COLUMN_SENDER_SITE);
        inMap.put(VIZ_COLUMN_SYSTEM_VERSION,    DB_COLUMN_SYSTEM_VERSION);
        inMap.put(VIZ_COLUMN_SENDER_VERSION,    DB_COLUMN_SENDER_VERSION);
        inMap.put(VIZ_COLUMN_LAST_UPDATE,       DB_COLUMN_UPDATE_TIME);
        inMap.put(VIZ_COLUMN_STATUS,            DB_COLUMN_STATUS);
        inMap.put(VIZ_COLUMN_JOB_FILE_LOCATION, DB_COLUMN_JOB_FILE_LOCATION);

        INCOMING_COL_NAME_MAP = Collections.unmodifiableMap(inMap);

        OUTGOING_TABLE_COLUMNS = OUTGOING_COL_NAME_MAP.keySet()
                .toArray(new String[OUTGOING_COL_NAME_MAP.size()]);

        INCOMING_TABLE_COLUMNS = INCOMING_COL_NAME_MAP.keySet()
                .toArray(new String[INCOMING_COL_NAME_MAP.size()]);
    }
}
