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

import java.text.SimpleDateFormat;
import java.util.Map;

import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * This class contains data for the locdatalimits and datalimits tables.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Oct 22, 2008				askripsk	Initial creation
 * Dec 8, 2008  1744        askripsk    Changed to implement IHydroDBData
 * 
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */
public class DataLimitData extends HydroDBData implements IHydroDBData {
    protected String lid;

    protected String pe;

    protected int dur;

    protected String monthDayStart;

    protected String monthDayEnd;

    protected double grossRangeMin;

    protected double grossRangeMax;

    protected double reasonRangeMin;

    protected double reasonRangeMax;

    protected double rocMax;

    protected double alertUpperLimit;

    protected double alertRocLimit;

    protected double alarmUpperLimit;

    protected double alarmRocLimit;

    protected double alertLowerLimit;

    protected double alarmLowerLimit;

    protected double alertDiffLimit;

    protected double alarmDiffLimit;

    private SimpleDateFormat monthDayFormat = new SimpleDateFormat("MM-dd");

    public DataLimitData() {
    }

    /**
     * Constructor that accepts the object array from a db call
     */
    public DataLimitData(Object[] data) {
        setPe((String) data[0]);
        // limitQuery.append("pe,");
        setDur(((Number) data[1]).shortValue());
        // limitQuery.append("dur,");
        setMonthDayStart((String) data[2]);
        // limitQuery.append("monthdaystart,");

        setMonthDayEnd((String) data[3]);
        // limitQuery.append("monthdayend,");

        setGrossRangeMin((Double) data[4]);
        // limitQuery.append("gross_range_min,");

        setGrossRangeMax((data[5] == null) ? HydroConstants.MISSING_VALUE
                : (Double) data[5]);
        // limitQuery.append("gross_range_max,");

        setReasonRangeMin((data[6] == null) ? HydroConstants.MISSING_VALUE
                : (Double) data[6]);
        // limitQuery.append("reason_range_min,");

        setReasonRangeMax((data[7] == null) ? HydroConstants.MISSING_VALUE
                : (Double) data[7]);
        // limitQuery.append("reason_range_max,");

        setRocMax((data[8] == null) ? HydroConstants.MISSING_VALUE
                : (Double) data[8]);
        // limitQuery.append("roc_max,");

        setAlarmUpperLimit((data[9] == null) ? HydroConstants.MISSING_VALUE
                : (Double) data[9]);
        // limitQuery.append("alert_upper_limit,");

        setAlarmRocLimit((data[10] == null) ? HydroConstants.MISSING_VALUE
                : (Double) data[10]);
        // limitQuery.append("alert_roc_limit,");

        setAlarmUpperLimit((data[11] == null) ? HydroConstants.MISSING_VALUE
                : (Double) data[11]);
        // limitQuery.append("alarm_upper_limit,");

        setAlarmRocLimit((data[12] == null) ? HydroConstants.MISSING_VALUE
                : (Double) data[12]);
        // limitQuery.append("alarm_roc_limit,");

        setAlertLowerLimit((data[13] == null) ? HydroConstants.MISSING_VALUE
                : (Double) data[13]);
        // limitQuery.append("alert_lower_limit,");

        setAlarmLowerLimit((data[14] == null) ? HydroConstants.MISSING_VALUE
                : (Double) data[14]);
        // limitQuery.append("alarm_lower_limit,");

        setAlertDiffLimit((data[15] == null) ? HydroConstants.MISSING_VALUE
                : (Double) data[15]);
        // limitQuery.append("alert_diff_limit,");

        setAlarmDiffLimit((data[16] == null) ? HydroConstants.MISSING_VALUE
                : (Double) data[16]);
        // limitQuery.append("alarm_diff_limit");
    }

    public DataLimitData(QueryResultRow data, Map<String, Integer> dataMap) {
        // initDateFormat();

        setPe(getDBValue("pe", data, dataMap, ""));
        setDur(getDBValue("dur", data, dataMap, HydroConstants.MISSING_VALUE));
        setMonthDayStart(getDBValue("monthdaystart", data, dataMap, ""));
        setMonthDayEnd(getDBValue("monthdayend", data, dataMap, ""));
        setGrossRangeMin(getDBValue("gross_range_min", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setGrossRangeMax(getDBValue("gross_range_max", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setReasonRangeMin(getDBValue("reason_range_min", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setReasonRangeMax(getDBValue("reason_range_max", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setRocMax(getDBValue("roc_max", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setAlertUpperLimit(getDBValue("alert_upper_limit", data, dataMap,
                Double.valueOf(HydroConstants.MISSING_VALUE)));
        setAlertRocLimit(getDBValue("alert_roc_limit", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setAlarmUpperLimit(getDBValue("alarm_upper_limit", data, dataMap,
                Double.valueOf(HydroConstants.MISSING_VALUE)));
        setAlarmRocLimit(getDBValue("alarm_roc_limit", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setAlertLowerLimit(getDBValue("alert_lower_limit", data, dataMap,
                Double.valueOf(HydroConstants.MISSING_VALUE)));
        setAlarmLowerLimit(getDBValue("alarm_lower_limit", data, dataMap,
                Double.valueOf(HydroConstants.MISSING_VALUE)));
        setAlertDiffLimit(getDBValue("alert_diff_limit", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setAlarmDiffLimit(getDBValue("alarm_diff_limit", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    public String getPe() {
        return pe;
    }

    public void setPe(String pe) {
        this.pe = pe;
    }

    public int getDur() {
        return dur;
    }

    public void setDur(int dur) {
        this.dur = dur;
    }

    public String getMonthDayStart() {
        return monthDayStart;
    }

    public void setMonthDayStart(String monthDayStart) {
        this.monthDayStart = monthDayStart;
    }

    public String getMonthDayEnd() {
        return monthDayEnd;
    }

    public void setMonthDayEnd(String monthDayEnd) {
        this.monthDayEnd = monthDayEnd;
    }

    public double getGrossRangeMin() {
        return grossRangeMin;
    }

    public void setGrossRangeMin(double grossRangeMin) {
        this.grossRangeMin = grossRangeMin;
    }

    public double getGrossRangeMax() {
        return grossRangeMax;
    }

    public void setGrossRangeMax(double grossRangeMax) {
        this.grossRangeMax = grossRangeMax;
    }

    public double getReasonRangeMin() {
        return reasonRangeMin;
    }

    public void setReasonRangeMin(double reasonRangeMin) {
        this.reasonRangeMin = reasonRangeMin;
    }

    public double getReasonRangeMax() {
        return reasonRangeMax;
    }

    public void setReasonRangeMax(double reasonRangeMax) {
        this.reasonRangeMax = reasonRangeMax;
    }

    public double getRocMax() {
        return rocMax;
    }

    public void setRocMax(double rocMax) {
        this.rocMax = rocMax;
    }

    public double getAlertUpperLimit() {
        return alertUpperLimit;
    }

    public void setAlertUpperLimit(double alertUpperLimit) {
        this.alertUpperLimit = alertUpperLimit;
    }

    public double getAlertRocLimit() {
        return alertRocLimit;
    }

    public void setAlertRocLimit(double alertRocLimit) {
        this.alertRocLimit = alertRocLimit;
    }

    public double getAlarmUpperLimit() {
        return alarmUpperLimit;
    }

    public void setAlarmUpperLimit(double alarmUpperLimit) {
        this.alarmUpperLimit = alarmUpperLimit;
    }

    public double getAlarmRocLimit() {
        return alarmRocLimit;
    }

    public void setAlarmRocLimit(double alarmRocLimit) {
        this.alarmRocLimit = alarmRocLimit;
    }

    public double getAlertLowerLimit() {
        return alertLowerLimit;
    }

    public void setAlertLowerLimit(double alertLowerLimit) {
        this.alertLowerLimit = alertLowerLimit;
    }

    public double getAlarmLowerLimit() {
        return alarmLowerLimit;
    }

    public void setAlarmLowerLimit(double alarmLowerLimit) {
        this.alarmLowerLimit = alarmLowerLimit;
    }

    public double getAlertDiffLimit() {
        return alertDiffLimit;
    }

    public void setAlertDiffLimit(double alertDiffLimit) {
        this.alertDiffLimit = alertDiffLimit;
    }

    public double getAlarmDiffLimit() {
        return alarmDiffLimit;
    }

    public void setAlarmDiffLimit(double alarmDiffLimit) {
        this.alarmDiffLimit = alarmDiffLimit;
    }

    public SimpleDateFormat getMonthDayFormat() {
        return monthDayFormat;
    }

    public void setMonthDayFormat(SimpleDateFormat monthDayFormat) {
        this.monthDayFormat = monthDayFormat;
    }

    @Override
    public String getConstrainedSelectStatement() {
        return "";
    }

    @Override
    public String getDeleteStatement() {
        String deleteQuery = "DELETE FROM datalimits WHERE %s";
        return String.format(deleteQuery, getPKStatement());
    }

    @Override
    public String getExistsStatement() {
        String selectQuery = "SELECT pe, dur, monthdaystart, monthdayend, gross_range_min, "
                + "gross_range_max, reason_range_min, reason_range_max, roc_max, alert_upper_limit, "
                + "alert_roc_limit, alarm_upper_limit, alarm_roc_limit, alert_lower_limit, "
                + "alarm_lower_limit, alert_diff_limit, alarm_diff_limit FROM DataLimits";

        return selectQuery + " WHERE " + getPKStatement();
    }

    @Override
    public String getInsertStatement() {
        String rval = "INSERT INTO Datalimits ( pe, dur, monthdaystart, monthdayend, gross_range_min, "
                + "gross_range_max, reason_range_min, reason_range_max, roc_max, alert_upper_limit, "
                + "alert_roc_limit, alarm_upper_limit, alarm_roc_limit, alert_lower_limit, "
                + "alarm_lower_limit, alert_diff_limit, alarm_diff_limit )"
                + " VALUES ( '%s', %s, '%s', '%s', %s, %s, %s, %s, %s, %s, "
                + "%s, %s, %s, %s, %s, %s, %s)";

        rval = String.format(rval, pe, getDBString(dur), monthDayStart,
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
        String pkString = "pe='%s' AND dur=%s AND monthdaystart='%s'";
        return String.format(pkString, pe, getDBString(dur), monthDayStart);
    }

    @Override
    public String getSelectStatement() {
        String selectQuery = "SELECT pe, dur, monthdaystart, monthdayend, gross_range_min, "
                + "gross_range_max, reason_range_min, reason_range_max, roc_max, alert_upper_limit, "
                + "alert_roc_limit, alarm_upper_limit, alarm_roc_limit, alert_lower_limit, "
                + "alarm_lower_limit, alert_diff_limit, alarm_diff_limit FROM DataLimits";

        selectQuery += " ORDER BY pe, dur, monthdaystart, monthdayend, gross_range_min, gross_range_max";

        return selectQuery;
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE datalimits SET monthdayend='%s', gross_range_min=%s, "
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
