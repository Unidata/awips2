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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.backupsvc.ui.BackupServicesFilterDialog;

/**
 * This class is used to store user's filter conditions for Backup Service Jobs
 * during runtime.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer       Description
 * ------------ ---------- -----------    --------------------------
 * Jun 14, 2021 84476      Gang Chen         Initial creation
 * Jul 13, 2021 93058      Amanuel Challa    Added Date/Time format validation,
 *                                           generated SQL statement to query JobFilePath/Location and
 *                                           match Database column name with Dialog column name
 * Aug 05, 2021 94872      Lisa Singh        Made filter searches case-insensitive. Trimmed filter values.
 * Aug 12, 2021 84654      Robert.Blum       Cleaned up constant files.
 * Spt 21, 2021 96163      Amanuel.Challa    Added Filter applied texts in setFilterDataArray()
 * May 01, 2023 2033487    Lisa.Singh        Renamed constants.
 *
 *
 * </pre>
 *
 * @author Gang Chen
 */

public final class BSJFilterMemo {

    private static ArrayList<BSJFilterData> incomingFilterDataArray;

    private static ArrayList<BSJFilterData> outgoingFilterDataArray;

    // Database Column names from Outgoing/Incoming Tables
    public static final String FILE_PATH_DB = "jobfilepath";

    public static final String FILE_LOCATION_DB = "jobfilelocation";

    // CAVE Filter Dialog column names
    public static final String FILE_PATH = VizBackupServicesConstants.VIZ_COLUMN_JOB_FILE_PATH;

    public static final String FILE_LOCATION = VizBackupServicesConstants.VIZ_COLUMN_JOB_FILE_LOCATION;

    private static String outgoingTabName;

    private static String incomingTabName;

    protected final static IUFStatusHandler statusLogger = UFStatus
            .getHandler(BSJFilterMemo.class);

    // Don't let anyone instantiate this class
    private BSJFilterMemo() {
    }

    public static void setFilterDataArray(
            ArrayList<BSJFilterData> theFilterDataArray, boolean isOutgoing) {
        if (isOutgoing) { // Set the filter data array for outgoing BSJ
            if (outgoingFilterDataArray == null) {
                outgoingFilterDataArray = new ArrayList<>();
            }
            outgoingTabName = "Outgoing";
            outgoingFilterDataArray.clear();
            if ((theFilterDataArray != null)
                    && (theFilterDataArray.size() > 0)) {
                for (int i = 0; i < theFilterDataArray.size(); i++) {
                    outgoingFilterDataArray.add(theFilterDataArray.get(i));
                }
                outgoingTabName = "Outgoing-Filter Applied";
            }
        } else { // Set the filter data array for incoming BSJ
            if (incomingFilterDataArray == null) {
                incomingFilterDataArray = new ArrayList<>();
            }
            incomingTabName = "Incoming";
            incomingFilterDataArray.clear();
            if ((theFilterDataArray != null)
                    && (theFilterDataArray.size() > 0)) {
                for (int i = 0; i < theFilterDataArray.size(); i++) {
                    incomingFilterDataArray.add(theFilterDataArray.get(i));
                }
                incomingTabName = "Incoming-Filter Applied";
            }
        }
    }

    public static ArrayList<BSJFilterData> getFilterDataArray(
            boolean isOutgoing) {
        return (isOutgoing) ? outgoingFilterDataArray : incomingFilterDataArray;
    }

    public static String getOutgoingTabName() {
        return outgoingTabName;
    }

    public static String getIncomingTabName() {
        return incomingTabName;
    }

    public static String combineSQLStatement(
            ArrayList<BSJFilterData> filterDataArray, boolean isOutgoing) {

        String sqlStatement = "";
        String sqlStmtHelper = "jobinfo_id in(select id from BackupSvcJobInfo where";

        // verify outgoingFilterDataArray contains
        // JobFilePath/JobFileLocation
        boolean useOutHelperStmt = false;
        for (BSJFilterData filterDataItem2 : filterDataArray) {
            if (filterDataItem2.getFieldName().equals(FILE_PATH)
                    || filterDataItem2.getFieldName().equals(FILE_LOCATION)) {
                useOutHelperStmt = true;
            }
        }

        for (BSJFilterData filterDataItem : filterDataArray) {

            if (filterDataItem.getFieldName()
                    .equals(VizBackupServicesConstants.VIZ_COLUMN_LAST_UPDATE)) {
                // If filePath/Location are selected in Filter Dialog
                // remove (and) condition and add it last
                if (sqlStmtHelper.contains(FILE_PATH_DB)
                        || sqlStmtHelper.contains(FILE_LOCATION_DB)
                        || useOutHelperStmt == true) {
                    sqlStatement = sqlStatement + " " + getSQLSubstring(
                            matchDBColumnName(filterDataItem.getFieldName(),
                                    isOutgoing),
                            filterDataItem.getOperand(),
                            dateToMs(filterDataItem.getValues()));
                    sqlStatement = sqlStatement + " and ";
                } else {
                    sqlStatement = sqlStatement + " "
                            + filterDataItem.getCondition() + " "
                            + getSQLSubstring(matchDBColumnName(
                                    filterDataItem.getFieldName(), isOutgoing),
                                    filterDataItem.getOperand(),
                                    dateToMs(filterDataItem.getValues()));
                }
                // If filePath/Location are selected build sqlStmtHelper
            } else if (filterDataItem.getFieldName().equals(FILE_PATH)
                    || filterDataItem.getFieldName().equals(FILE_LOCATION)) {
                // if sqlStmtHelper is empty remove the (and) condition
                if (!(sqlStmtHelper.contains(FILE_PATH_DB)
                        || (sqlStmtHelper.contains(FILE_LOCATION_DB)))) {

                    sqlStmtHelper = sqlStmtHelper + " " + getSQLSubstring(
                            matchDBColumnName(filterDataItem.getFieldName(),
                                    isOutgoing),
                            filterDataItem.getOperand(),
                            filterDataItem.getValues());
                } else {
                    sqlStmtHelper = sqlStmtHelper + " "
                            + filterDataItem.getCondition() + " "
                            + getSQLSubstring(matchDBColumnName(
                                    filterDataItem.getFieldName(), isOutgoing),
                                    filterDataItem.getOperand(),
                                    filterDataItem.getValues());
                }
            } else {
                // If filePath/Location are selected in Filter Dialog
                // remove (and) condition and add it last
                if (sqlStmtHelper.contains(FILE_PATH_DB)
                        || sqlStmtHelper.contains(FILE_LOCATION_DB)
                        || useOutHelperStmt == true) {
                    sqlStatement = sqlStatement + " " + getSQLSubstring(
                            matchDBColumnName(filterDataItem.getFieldName(),
                                    isOutgoing),
                            filterDataItem.getOperand(),
                            filterDataItem.getValues());
                    sqlStatement = sqlStatement + " and ";
                } else {
                    sqlStatement = sqlStatement + " "
                            + filterDataItem.getCondition() + " "
                            + getSQLSubstring(matchDBColumnName(
                                    filterDataItem.getFieldName(), isOutgoing),
                                    filterDataItem.getOperand(),
                                    filterDataItem.getValues());
                }

            }
        }
        if (sqlStmtHelper.contains(FILE_LOCATION_DB)
                || sqlStmtHelper.contains(FILE_PATH_DB)) {

            sqlStatement = sqlStatement + sqlStmtHelper + " )";
        }

        return sqlStatement;
    }

    public static String toSQLStatement(boolean isOutgoing) {

        // SQL query sample
        // from backup_svc_outgoing where status='New' and jobinfo_id
        // in(select id from backup_svc_jobinfo where
        // jobfilepath='/backupSvc.xml');
        String sqlStatement = "";

        if (isOutgoing) {
            // filter conditions for outgoing backup service jobs
            if ((outgoingFilterDataArray == null)
                    || (outgoingFilterDataArray.size() == 0)) {
                return sqlStatement;
            }
            sqlStatement = combineSQLStatement(outgoingFilterDataArray,
                    isOutgoing);

        } else {
            // filter conditions for incoming backup service jobs
            if ((incomingFilterDataArray == null)
                    || (incomingFilterDataArray.size() == 0)) {
                return sqlStatement;
            }
            sqlStatement = combineSQLStatement(incomingFilterDataArray,
                    isOutgoing);
        }
        return sqlStatement;
    }

    /**
     * Convert the given DateTime String to Milliseconds
     *
     * @param myDate
     * @return millis The string Value of Millisecond
     */
    private static String dateToMs(String myDate) {

        String timeDatePattern = determineDateFormat(myDate);
        SimpleDateFormat sdf = new SimpleDateFormat(timeDatePattern);
        sdf.setTimeZone(TimeUtil.GMT_TIME_ZONE);

        Date date = null;
        // Current Date/Time as default value
        String millis = String
                .valueOf(SimulatedTime.getSystemTime().getMillis());
        try {
            date = sdf.parse(myDate);

            millis = String.valueOf(date.getTime());

        } catch (Exception e) {
            statusLogger.error(
                    "Error converting Date-Time to Millisecond:Date-Time set to current Time. ",
                    e);

        }
        return millis;

    }

    /**
     * Find and return DateTime format from the given String param
     *
     * @param dateTimeStr
     * @return BackupServicesConstants.REGEX_TimeDateMap
     * @return null if no format found
     */
    public static String determineDateFormat(String dateTimeStr) {
        for (String key : VizBackupServicesConstants.REGEX_TIME_DATE_MAP
                .keySet()) {
            if (dateTimeStr.toLowerCase().matches(key)) {
                return VizBackupServicesConstants.REGEX_TIME_DATE_MAP.get(key);
            }
        }
        // no format found
        return null;
    }

    /**
     * Matches Dialog Table column names with Database column names
     *
     * @param key
     * @param isOutgoing
     * @return value Outgoing/Incoming Database column name
     */
    public static String matchDBColumnName(String key, boolean isOutgoing) {

        String value = "";
        if (isOutgoing) {
            for (int i = 0; i <= VizBackupServicesConstants.OUTGOING_COL_NAME_MAP
                    .size(); i++) {
                if (VizBackupServicesConstants.OUTGOING_COL_NAME_MAP
                        .containsKey(key)) {
                    value = VizBackupServicesConstants.OUTGOING_COL_NAME_MAP
                            .get(key);
                    break;
                }
            }
        } else {
            for (int i = 0; i <= VizBackupServicesConstants.INCOMING_COL_NAME_MAP
                    .size(); i++) {
                if (VizBackupServicesConstants.INCOMING_COL_NAME_MAP
                        .containsKey(key)) {
                    value = VizBackupServicesConstants.INCOMING_COL_NAME_MAP
                            .get(key);
                    break;
                }
            }
        }
        return value;
    }

    private static String getSQLSubstring(String fieldName, String operand,
            String values) {
        String sqlSubString = "";
        /*
         * Only ID field and Last Update values do not need quotes. The valid
         * values are true or false. true: need quotes; false: no quotes
         */
        boolean isQuotedVal = !fieldName.equalsIgnoreCase("bksvc_id")
                && !fieldName.equalsIgnoreCase("updatetime");

        if (isQuotedVal) {
            // convert all values to uppercase for case-insensitivity
            fieldName = "UPPER(" + fieldName + ")";
            values = values.toUpperCase();
        }

        // Convert certain operands into sql-terminology
        if (operand.equals(BackupServicesFilterDialog.NOT_EQUALS_OPERAND)) {
            operand = "<>";
        }

        if (operand.equalsIgnoreCase("in")) {
            String[] tmpValues = values.split(",");
            for (int i = 0; i < tmpValues.length; i++) {
                if (i == 0) {
                    sqlSubString = getQuotedValue(tmpValues[i].trim(),
                            isQuotedVal);
                } else {
                    sqlSubString = sqlSubString + ","
                            + getQuotedValue(tmpValues[i].trim(), isQuotedVal);
                }
            }
            sqlSubString = fieldName + " " + operand + " (" + sqlSubString
                    + ")";
        } else {
            sqlSubString = fieldName + " " + operand + " "
                    + getQuotedValue(values.trim(), isQuotedVal);
        }
        return sqlSubString;
    }

    private static String getQuotedValue(String value, boolean isQuotedVal) {
        String retValue = null;
        if (isQuotedVal) {
            retValue = "'" + value.trim() + "'";
        } else {
            retValue = value.trim();
        }
        return retValue;
    }
}
