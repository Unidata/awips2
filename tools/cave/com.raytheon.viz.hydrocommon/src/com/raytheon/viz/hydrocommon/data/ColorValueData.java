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
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 10, 2008            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ColorValueData extends HydroDBData implements IHydroDBData,
        Comparable {

    private static final String databaseName = "colorvalue";

    private static final String selectStmtString = "SELECT userid, application_name, color_use_name, duration, threshold_value, threshold_unit, color_name FROM "
            + databaseName;

    private static final String constSelectStmtString = selectStmtString
            + " WHERE userid='%s' and application_name='%s' and color_use_name='%s' and duration='%s'";

    private static final String deleteStmtString = "DELETE FROM "
            + databaseName + " WHERE %s";

    private static final String existsStmtString = selectStmtString
            + " WHERE %s";

    private static final String insertStmtString = "INSERT INTO "
            + databaseName
            + " ( userid, application_name, color_use_name, duration, threshold_value, threshold_unit, color_name ) VALUES ( '%s', '%s', '%s', '%s', '%s', '%s', '%s' )";

    private static final String pkStmtString = "userid='%s' AND application_name='%s' AND color_use_name='%s' AND duration='%s' AND threshold_value='%s'";

    private static final String updateStmtString = "UPDATE "
            + databaseName
            + " SET userid='%s', application_name='%s', color_use_name='%s', duration='%s', threshold_value='%s', threshold_unit='%s', color_name='%s' WHERE %s";

    private String userId;

    private String applicationName;

    private String colorUseName;

    private String duration;

    private String thresholdValue;

    private String thresholdUnit;

    private String colorName;

    /**
     * Constructor
     */
    public ColorValueData() {

    }

    /**
     * Constructor.
     * 
     * @param data
     *            Result data.
     * @param dataMap
     *            Column to Index map.
     */
    public ColorValueData(QueryResultRow data, Map<String, Integer> dataMap) {
        setUserId(getDBValue("userid", data, dataMap, ""));
        setApplicationName(getDBValue("application_name", data, dataMap, ""));
        setColorUseName(getDBValue("color_use_name", data, dataMap, ""));
        Object obj = getDBValue("duration", data, dataMap, "");
        setDuration(obj.toString());
        obj = getDBValue("threshold_value", data, dataMap, "");
        setThresholdValue(obj.toString());
        setThresholdUnit(getDBValue("threshold_unit", data, dataMap, ""));
        setColorName(getDBValue("color_name", data, dataMap, ""));
    }

    @Override
    public String getConstrainedSelectStatement() {
        return String.format(constSelectStmtString, userId, applicationName,
                colorUseName, duration);
    }

    @Override
    public String getDeleteStatement() {
        return String.format(deleteStmtString, getPKStatement());
    }

    @Override
    public String getExistsStatement() {
        return String.format(existsStmtString, getPKStatement());
    }

    @Override
    public String getInsertStatement() {
        return String.format(insertStmtString, userId, applicationName,
                colorUseName, duration, thresholdValue, thresholdUnit,
                colorName);
    }

    @Override
    public String getPKStatement() {
        return String.format(pkStmtString, userId, applicationName,
                colorUseName, duration, thresholdValue);
    }

    @Override
    public String getSelectStatement() {
        return selectStmtString;
    }

    @Override
    public String getUpdateStatement() {
        return String.format(updateStmtString, userId, applicationName,
                colorUseName, duration, thresholdValue, thresholdUnit,
                colorName, getPKStatement());
    }

    public String getUserId() {
        return userId;
    }

    public void setUserId(String userId) {
        this.userId = userId;
    }

    public String getApplicationName() {
        return applicationName;
    }

    public void setApplicationName(String applicationName) {
        this.applicationName = applicationName;
    }

    public String getColorUseName() {
        return colorUseName;
    }

    public void setColorUseName(String colorUseName) {
        this.colorUseName = colorUseName;
    }

    public String getDuration() {
        return duration;
    }

    public void setDuration(String duration) {
        this.duration = duration;
    }

    public String getThresholdValue() {
        return thresholdValue;
    }

    public void setThresholdValue(String thresholdValue) {
        this.thresholdValue = thresholdValue;
    }

    public String getThresholdUnit() {
        return thresholdUnit;
    }

    public void setThresholdUnit(String thresholdUnit) {
        this.thresholdUnit = thresholdUnit;
    }

    public String getColorName() {
        return colorName;
    }

    public void setColorName(String colorName) {
        this.colorName = colorName;
    }

    @Override
    public int compareTo(Object o) {
        if (o instanceof ColorValueData) {
            double l = Double.parseDouble(getThresholdValue());
            double r = Double.parseDouble(((ColorValueData) o)
                    .getThresholdValue());
            if (l > r) {
                return 1;
            }
        }
        return 0;
    }

}
