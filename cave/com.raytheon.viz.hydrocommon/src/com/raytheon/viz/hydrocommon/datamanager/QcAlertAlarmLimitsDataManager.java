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
package com.raytheon.viz.hydrocommon.datamanager;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.text.StrSubstitutor;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.DataLimitData;
import com.raytheon.viz.hydrocommon.data.LocationDataLimitData;

/**
 * Class for managing database query calls.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 8, 2008 1697        askripsky   Initial Creation
 * Apr 18,2013 1790        rferrel     Cleanup method interfaces;
 *                                      part of non-blocking dialogs.
 * Jul 21, 2015 4500       rjpeter     Use Number in blind cast.
 * Apr 18, 2018 DCS19644   jwu         Add column 'ts' (Type-Source) in locdatalimits.
 * Jul 10, 2018 7211       dgilling    Rewrite as lib of static functions.
 * </pre>
 *
 * @author askripsky
 */

public class QcAlertAlarmLimitsDataManager {

    private static IUFStatusHandler statusHandler = UFStatus
            .getHandler(QcAlertAlarmLimitsDataManager.class);

    /**
     * Private constructor.
     */
    private QcAlertAlarmLimitsDataManager() {
        throw new AssertionError();
    }

    public static List<DataLimitData> getDataLimits(String whereOrderPhrase)
            throws VizException {
        StringBuilder query = new StringBuilder(
                "SELECT pe, dur, monthdaystart, monthdayend, gross_range_min, ")
                        .append("gross_range_max, reason_range_min, reason_range_max, roc_max, alert_upper_limit, ")
                        .append("alert_roc_limit, alarm_upper_limit, alarm_roc_limit, alert_lower_limit, ")
                        .append("alarm_lower_limit, alert_diff_limit, alarm_diff_limit FROM datalimits ");
        query.append(whereOrderPhrase);

        QueryResult result = HydroDBDataManager.getInstance()
                .runMappedQuery(query.toString());

        if (result != null) {
            List<DataLimitData> data = Arrays.stream(result.getRows())
                    .map(r -> new DataLimitData(r, result.getColumnNames()))
                    .collect(Collectors.toList());
            return data;
        }

        return Collections.emptyList();
    }

    public static List<LocationDataLimitData> getLocDataLimits(
            String whereOrderPhrase) throws VizException {
        StringBuilder query = new StringBuilder(
                "SELECT lid, pe, dur, ts, monthdaystart, monthdayend, gross_range_min, ")
                        .append("gross_range_max, reason_range_min, reason_range_max, roc_max, alert_upper_limit, ")
                        .append("alert_roc_limit, alarm_upper_limit, alarm_roc_limit, alert_lower_limit, ")
                        .append("alarm_lower_limit, alert_diff_limit, alarm_diff_limit FROM locdatalimits ");
        query.append(whereOrderPhrase);

        QueryResult result = HydroDBDataManager.getInstance()
                .runMappedQuery(query.toString());

        if (result != null) {
            List<LocationDataLimitData> data = Arrays.stream(result.getRows())
                    .map(r -> new LocationDataLimitData(r,
                            result.getColumnNames()))
                    .collect(Collectors.toList());
            return data;
        }

        return Collections.emptyList();
    }

    public static void putDataLimits(DataLimitData data) throws VizException {
        Map<String, String> valuesMap = new HashMap<>();
        if (data instanceof LocationDataLimitData) {
            valuesMap.put("lid", ((LocationDataLimitData) data).getLid());
            valuesMap.put("ts", ((LocationDataLimitData) data).getTs());
        }
        valuesMap.put("pe", data.getPe());
        valuesMap.put("dur", data.getDBString(data.getDur()));
        valuesMap.put("monthdaystart", data.getMonthDayStart());
        valuesMap.put("monthdayend", data.getMonthDayEnd());
        valuesMap.put("gross_range_min",
                data.getDBString(data.getGrossRangeMin()));
        valuesMap.put("gross_range_max",
                data.getDBString(data.getGrossRangeMax()));
        valuesMap.put("reason_range_min",
                data.getDBString(data.getReasonRangeMin()));
        valuesMap.put("reason_range_max",
                data.getDBString(data.getReasonRangeMax()));
        valuesMap.put("roc_max", data.getDBString(data.getRocMax()));
        valuesMap.put("alert_upper_limit",
                data.getDBString(data.getAlertUpperLimit()));
        valuesMap.put("alert_roc_limit",
                data.getDBString(data.getAlertRocLimit()));
        valuesMap.put("alarm_upper_limit",
                data.getDBString(data.getAlarmUpperLimit()));
        valuesMap.put("alarm_roc_limit",
                data.getDBString(data.getAlarmRocLimit()));
        valuesMap.put("alert_lower_limit",
                data.getDBString(data.getAlertLowerLimit()));
        valuesMap.put("alarm_lower_limit",
                data.getDBString(data.getAlarmLowerLimit()));
        valuesMap.put("alert_diff_limit",
                data.getDBString(data.getAlertDiffLimit()));
        valuesMap.put("alarm_diff_limit",
                data.getDBString(data.getAlarmDiffLimit()));

        String insertFormat = (data instanceof LocationDataLimitData)
                ? "INSERT INTO locdatalimits ( lid, pe, dur, ts, monthdaystart, monthdayend, gross_range_min, "
                        + "gross_range_max, reason_range_min, reason_range_max, roc_max, alert_upper_limit, "
                        + "alert_roc_limit, alarm_upper_limit, alarm_roc_limit, alert_lower_limit, "
                        + "alarm_lower_limit, alert_diff_limit, alarm_diff_limit )"
                        + " VALUES ( '${lid}', '${pe}', ${dur}, '${ts}', '${monthdaystart}', '${monthdayend}', ${gross_range_min}, "
                        + "${gross_range_max}, ${reason_range_min}, ${reason_range_max}, ${roc_max}, "
                        + "${alert_upper_limit}, ${alert_roc_limit}, ${alarm_upper_limit}, ${alarm_roc_limit}, "
                        + "${alert_lower_limit}, ${alarm_lower_limit}, ${alert_diff_limit}, ${alarm_diff_limit})"
                : "INSERT INTO Datalimits ( pe, dur, monthdaystart, monthdayend, gross_range_min, "
                        + "gross_range_max, reason_range_min, reason_range_max, roc_max, alert_upper_limit, "
                        + "alert_roc_limit, alarm_upper_limit, alarm_roc_limit, alert_lower_limit, "
                        + "alarm_lower_limit, alert_diff_limit, alarm_diff_limit )"
                        + " VALUES ( '${pe}', ${dur}, '${monthdaystart}', '${monthdayend}', ${gross_range_min}, "
                        + "${gross_range_max}, ${reason_range_min}, ${reason_range_max}, ${roc_max}, "
                        + "${alert_upper_limit}, ${alert_roc_limit}, ${alarm_upper_limit}, ${alarm_roc_limit}, "
                        + "${alert_lower_limit}, ${alarm_lower_limit}, ${alert_diff_limit}, ${alarm_diff_limit})";

        StrSubstitutor sub = new StrSubstitutor(valuesMap);
        String insertStmt = sub.replace(insertFormat);

        statusHandler.debug("QcAlertAlarmLimitsDataManager: Insert Statement: "
                + insertStmt);

        HydroDBDataManager.getInstance().runStatement(insertStmt);
    }

    public static void updateDataLimits(DataLimitData data, String wherePhrase)
            throws VizException {
        String tableName = (data instanceof LocationDataLimitData)
                ? "locdatalimits" : "Datalimits";
        StringBuilder updateStmt = new StringBuilder("UPDATE ")
                .append(tableName).append(" SET ");
        if (data instanceof LocationDataLimitData) {
            updateStmt.append(String.format("lid='%s', ",
                    ((LocationDataLimitData) data).getLid()));
            updateStmt.append(String.format("ts='%s', ",
                    ((LocationDataLimitData) data).getTs()));
        }

        String updateFormat = "pe='%s', dur=%s, monthdaystart='%s', monthdayend='%s', gross_range_min=%s, "
                + "gross_range_max=%s, reason_range_min=%s, reason_range_max=%s, roc_max=%s, alert_upper_limit=%s, "
                + "alert_roc_limit=%s, alarm_upper_limit=%s, alarm_roc_limit=%s, alert_lower_limit=%s, "
                + "alarm_lower_limit=%s, alert_diff_limit=%s, alarm_diff_limit=%s";
        updateStmt.append(String.format(updateFormat, data.getPe(),
                data.getDBString(data.getDur()), data.getMonthDayStart(),
                data.getMonthDayEnd(),
                data.getDBString(data.getGrossRangeMin()),
                data.getDBString(data.getGrossRangeMax()),
                data.getDBString(data.getReasonRangeMin()),
                data.getDBString(data.getReasonRangeMax()),
                data.getDBString(data.getRocMax()),
                data.getDBString(data.getAlertUpperLimit()),
                data.getDBString(data.getAlertRocLimit()),
                data.getDBString(data.getAlarmUpperLimit()),
                data.getDBString(data.getAlarmRocLimit()),
                data.getDBString(data.getAlertLowerLimit()),
                data.getDBString(data.getAlarmLowerLimit()),
                data.getDBString(data.getAlertDiffLimit()),
                data.getDBString(data.getAlarmDiffLimit())));
        if (StringUtils.isNotBlank(wherePhrase)) {
            updateStmt.append(wherePhrase);
        }

        statusHandler.debug("QcAlertAlarmLimitsDataManager: Update Statement: "
                + updateStmt);

        HydroDBDataManager.getInstance().runStatement(updateStmt.toString());
    }

    public static void deleteDataLimits(DataLimitData data, final String where)
            throws VizException {
        if (StringUtils.isNotBlank(where)) {
            StringBuilder deleteStmt = new StringBuilder("DELETE FROM ");
            String tableName = (data instanceof LocationDataLimitData)
                    ? "locdatalimits" : "Datalimits";
            deleteStmt.append(tableName).append(where);

            statusHandler
                    .debug("QcAlertAlarmLimitsDataManager: Delete Statement: "
                            + deleteStmt);

            HydroDBDataManager.getInstance()
                    .runStatement(deleteStmt.toString());
        }
    }
}