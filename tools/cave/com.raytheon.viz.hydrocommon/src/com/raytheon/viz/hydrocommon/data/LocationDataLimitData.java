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
package com.raytheon.viz.hydrocommon.data;

import java.util.Map;

import com.raytheon.uf.common.dataquery.db.QueryResultRow;

/**
 * This class contains data for the locdatalimits tables.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Dec 9, 2008  1744        askripsk    Initial Creation
 * 
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */
public class LocationDataLimitData extends DataLimitData {
    public LocationDataLimitData() {
    }

    public LocationDataLimitData(QueryResultRow data,
            Map<String, Integer> dataMap) {

        super(data, dataMap);
        setLid(getDBValue("lid", data, dataMap, ""));
    }

    @Override
    public String getConstrainedSelectStatement() {
        return "";
    }

    @Override
    public String getDeleteStatement() {
        String deleteQuery = "DELETE FROM locdatalimits WHERE %s";
        return String.format(deleteQuery, getPKStatement());
    }

    @Override
    public String getExistsStatement() {
        String selectQuery = "SELECT lid, pe, dur, monthdaystart, monthdayend, gross_range_min, "
                + "gross_range_max, reason_range_min, reason_range_max, roc_max, alert_upper_limit, "
                + "alert_roc_limit, alarm_upper_limit, alarm_roc_limit, alert_lower_limit, "
                + "alarm_lower_limit, alert_diff_limit, alarm_diff_limit FROM locdatalimits WHERE %s";

        return String.format(selectQuery, getPKStatement());
    }

    @Override
    public String getInsertStatement() {
        String rval = "INSERT INTO locdatalimits ( lid, pe, dur, monthdaystart, monthdayend, gross_range_min, "
                + "gross_range_max, reason_range_min, reason_range_max, roc_max, alert_upper_limit, "
                + "alert_roc_limit, alarm_upper_limit, alarm_roc_limit, alert_lower_limit, "
                + "alarm_lower_limit, alert_diff_limit, alarm_diff_limit )"
                + " VALUES ( '%s', '%s', %s, '%s', '%s', %s, %s, %s, %s, %s, "
                + "%s, %s, %s, %s, %s, %s, %s, %s)";

        rval = String.format(rval, lid, pe, getDBString(dur), monthDayStart,
                monthDayEnd, getDBString(grossRangeMin),
                getDBString(grossRangeMax), getDBString(reasonRangeMin),
                getDBString(reasonRangeMax), getDBString(rocMax),
                getDBString(alertUpperLimit), getDBString(alertRocLimit),
                getDBString(alarmUpperLimit), getDBString(alarmRocLimit),
                getDBString(alertLowerLimit), getDBString(alarmLowerLimit),
                getDBString(alertDiffLimit), getDBString(alarmDiffLimit));

        return rval;
    }

    @Override
    public String getPKStatement() {
        String pkString = "lid='%s' AND pe='%s' AND dur=%s AND monthdaystart='%s'";
        return String
                .format(pkString, lid, pe, getDBString(dur), monthDayStart);
    }

    @Override
    public String getSelectStatement() {
        String selectQuery = "SELECT lid, pe, dur, monthdaystart, monthdayend, gross_range_min, "
                + "gross_range_max, reason_range_min, reason_range_max, roc_max, alert_upper_limit, "
                + "alert_roc_limit, alarm_upper_limit, alarm_roc_limit, alert_lower_limit, "
                + "alarm_lower_limit, alert_diff_limit, alarm_diff_limit FROM locdatalimits";

        selectQuery += " ORDER BY lid, pe, dur, monthdaystart, monthdayend, gross_range_min, gross_range_max";

        return selectQuery;
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE locdatalimits SET monthdayend='%s', gross_range_min=%s, "
                + "gross_range_max=%s, reason_range_min=%s, reason_range_max=%s, roc_max=%s, alert_upper_limit=%s, "
                + "alert_roc_limit=%s, alarm_upper_limit=%s, alarm_roc_limit=%s, alert_lower_limit=%s, "
                + "alarm_lower_limit=%s, alert_diff_limit=%s, alarm_diff_limit=%s"
                + " WHERE %s";

        // Populate the values
        rval = String.format(rval, monthDayEnd, getDBString(grossRangeMin),
                getDBString(grossRangeMax), getDBString(reasonRangeMin),
                getDBString(reasonRangeMax), getDBString(rocMax),
                getDBString(alertUpperLimit), getDBString(alertRocLimit),
                getDBString(alarmUpperLimit), getDBString(alarmRocLimit),
                getDBString(alertLowerLimit), getDBString(alarmLowerLimit),
                getDBString(alertDiffLimit), getDBString(alarmDiffLimit),
                getPKStatement());

        return rval;
    }
}
