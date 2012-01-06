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
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * this class contains the Purge Dyn data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Dec 17, 2008	1787		askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class PurgeDynData extends HydroDBData implements IHydroDBData {

    /**
     * Table
     */
    private String tableName;

    /**
     * Time Column Name
     */
    private String timeColumnName;

    /**
     * num_hours_host
     */
    private int hostHours;

    /**
     * num_hours_backup
     */
    private int backupHours;

    /**
     * Constructor
     */
    public PurgeDynData() {
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public PurgeDynData(QueryResultRow data, Map<String, Integer> dataMap) {
        setTableName(getDBValue("table_name", data, dataMap, ""));
        setTimeColumnName(getDBValue("time_column_name", data, dataMap, ""));
        setHostHours(getDBValue("num_hours_host", data, dataMap,
                HydroConstants.MISSING_VALUE));
        setBackupHours(getDBValue("num_hours_backup", data, dataMap,
                HydroConstants.MISSING_VALUE));
    }

    /**
     * @return the tableName
     */
    public String getTableName() {
        return tableName;
    }

    /**
     * @param tableName
     *            the tableName to set
     */
    public void setTableName(String tableName) {
        this.tableName = tableName;
    }

    /**
     * @return the timeColumnName
     */
    public String getTimeColumnName() {
        return timeColumnName;
    }

    /**
     * @param timeColumnName
     *            the timeColumnName to set
     */
    public void setTimeColumnName(String timeColumnName) {
        this.timeColumnName = timeColumnName;
    }

    /**
     * @return the hostHours
     */
    public int getHostHours() {
        return hostHours;
    }

    /**
     * @param hostHours
     *            the hostHours to set
     */
    public void setHostHours(int hostHours) {
        this.hostHours = hostHours;
    }

    /**
     * @return the backupHours
     */
    public int getBackupHours() {
        return backupHours;
    }

    /**
     * @param backupHours
     *            the backupHours to set
     */
    public void setBackupHours(int backupHours) {
        this.backupHours = backupHours;
    }

    @Override
    public String getInsertStatement() {
        String rval = "INSERT INTO purgedyndata ( "
                + "table_name, time_column_name, num_hours_host, num_hours_backup"
                + " ) VALUES ( %s, %s, %s, %s )";

        rval = String.format(rval, getDBString(tableName),
                getDBString(timeColumnName), getDBString(hostHours),
                getDBString(backupHours));

        return rval;
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE purgedyndata SET "
                + "table_name=%s, time_column_name=%s, num_hours_host=%s, num_hours_backup=%s"
                + " WHERE %s";

        // Populate the values
        rval = String.format(rval, getDBString(tableName),
                getDBString(timeColumnName), getDBString(hostHours),
                getDBString(backupHours), getPKStatement());

        return rval;
    }

    @Override
    public String getSelectStatement() {
        return "SELECT table_name, time_column_name, num_hours_host, num_hours_backup FROM purgedyndata ORDER BY time_column_name, table_name";
    }

    @Override
    public String getDeleteStatement() {
        return String.format("DELETE FROM purgedyndata WHERE %s",
                getPKStatement());
    }

    @Override
    public String getPKStatement() {
        String pkString = "table_name=%s";
        return String.format(pkString, getDBString(tableName));
    }

    @Override
    public String getExistsStatement() {
        String selectQuery = "SELECT table_name FROM purgedyndata WHERE "
                + getPKStatement();

        return selectQuery;
    }

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement();
    }
}
