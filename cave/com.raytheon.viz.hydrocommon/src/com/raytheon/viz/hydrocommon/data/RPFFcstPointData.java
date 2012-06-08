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

import java.util.ArrayList;
import java.util.Map;

import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;

/**
 * this class contains the RiverPro Forecast Group data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Dec 19, 2008	1787		askripsky	Initial creation
 * Mar 08, 2012 14600       wkwock      Delete one lid instead of one group
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class RPFFcstPointData extends HydroDBData implements IHydroDBData {
    /**
     * lid - Station
     */
    private String lid;

    /**
     * Name of the Location
     */
    private String lidName;

    /**
     * group_id
     */
    private String groupID;

    /**
     * ordinal
     */
    private int ordinal;

    /**
     * chg_threshold
     */
    private double changeThreshold;

    /**
     * rec_type
     */
    private String recordType;

    /**
     * primary_back
     */
    private String primaryBackup;

    /**
     * secondary_back
     */
    private String secondaryBackup;

    /**
     * backhrs
     */
    private int backHours;

    /**
     * forwardhrs
     */
    private int forwardHours;

    /**
     * adjustendhrs
     */
    private double adjustEndHours;

    /**
     * Default Shift hours
     */
    private static double defaultShiftHours;

    /**
     * Default Obs hours
     */
    private static int defaultObsHours;

    /**
     * Default Forecast hours
     */
    private static int defaultFctHours;

    static {
        getDefaultValues();
    }

    /**
     * Constructor
     */
    public RPFFcstPointData() {
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public RPFFcstPointData(QueryResultRow data, Map<String, Integer> dataMap) {

        setLid(getDBValue("lid", data, dataMap, ""));
        setLidName(getDBValue("name", data, dataMap, ""));
        setGroupID(getDBValue("group_id", data, dataMap, ""));
        setOrdinal(getDBValue("ordinal", data, dataMap,
                HydroConstants.MISSING_VALUE));
        setChangeThreshold(getDBValue("chg_threshold", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setRecordType(getDBValue("rec_type", data, dataMap, ""));
        setPrimaryBackup(getDBValue("primary_back", data, dataMap, ""));
        setSecondaryBackup(getDBValue("secondary_back", data, dataMap, ""));
        setBackHours(getDBValue("backhrs", data, dataMap, defaultObsHours));
        setForwardHours(getDBValue("forwardhrs", data, dataMap, defaultFctHours));
        setAdjustEndHours(getDBValue("adjustendhrs", data, dataMap,
                defaultShiftHours));
    }

    /**
     * Retrieves the default hours.
     */
    private static void getDefaultValues() {
        defaultShiftHours = AppsDefaults.getInstance().getDouble(
                "rpf_endtime_shifthrs", 0.0);

        if (defaultShiftHours < 0 || defaultShiftHours > 48) {
            defaultShiftHours = 6;
        }

        try {
            ArrayList<RPFParamData> data = HydroDBDataManager.getInstance()
                    .getData(RPFParamData.class);

            if (data != null && data.size() > 0) {
                // There should only be one record
                defaultObsHours = data.get(0).getObservationHours();
                defaultFctHours = data.get(0).getForecastHours();
            }
        } catch (VizException e) {
            e.printStackTrace();
        }
    }

    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * @param lid
     *            the lid to set
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * @return the lidName
     */
    public String getLidName() {
        return lidName;
    }

    /**
     * @param lidName
     *            the lidName to set
     */
    public void setLidName(String lidName) {
        this.lidName = lidName;
    }

    /**
     * @return the groupID
     */
    public String getGroupID() {
        return groupID;
    }

    /**
     * @param groupID
     *            the groupID to set
     */
    public void setGroupID(String groupID) {
        this.groupID = groupID;
    }

    /**
     * @return the ordinal
     */
    public int getOrdinal() {
        return ordinal;
    }

    /**
     * @param ordinal
     *            the ordinal to set
     */
    public void setOrdinal(int ordinal) {
        this.ordinal = ordinal;
    }

    /**
     * @return the changeThreshold
     */
    public double getChangeThreshold() {
        return changeThreshold;
    }

    /**
     * @param changeThreshold
     *            the changeThreshold to set
     */
    public void setChangeThreshold(double changeThreshold) {
        this.changeThreshold = changeThreshold;
    }

    /**
     * @return the recordType
     */
    public String getRecordType() {
        return recordType;
    }

    /**
     * @param recordType
     *            the recordType to set
     */
    public void setRecordType(String recordType) {
        this.recordType = recordType;
    }

    /**
     * @return the primaryBackup
     */
    public String getPrimaryBackup() {
        return primaryBackup;
    }

    /**
     * @param primaryBackup
     *            the primaryBackup to set
     */
    public void setPrimaryBackup(String primaryBackup) {
        this.primaryBackup = primaryBackup;
    }

    /**
     * @return the secondaryBackup
     */
    public String getSecondaryBackup() {
        return secondaryBackup;
    }

    /**
     * @param secondaryBackup
     *            the secondaryBackup to set
     */
    public void setSecondaryBackup(String secondaryBackup) {
        this.secondaryBackup = secondaryBackup;
    }

    /**
     * @return the backHours
     */
    public int getBackHours() {
        return backHours;
    }

    /**
     * @param backHours
     *            the backHours to set
     */
    public void setBackHours(int backHours) {
        this.backHours = backHours;
    }

    /**
     * @return the forwardHours
     */
    public int getForwardHours() {
        return forwardHours;
    }

    /**
     * @param forwardHours
     *            the forwardHours to set
     */
    public void setForwardHours(int forwardHours) {
        this.forwardHours = forwardHours;
    }

    /**
     * @return the adjustEndHours
     */
    public double getAdjustEndHours() {
        return adjustEndHours;
    }

    /**
     * @param adjustEndHours
     *            the adjustEndHours to set
     */
    public void setAdjustEndHours(double adjustEndHours) {
        this.adjustEndHours = adjustEndHours;
    }

    @Override
    public String getInsertStatement() {
        String columns = "lid, group_id, ordinal, chg_threshold, rec_type, primary_back, secondary_back, backhrs, forwardhrs, adjustendhrs";
        String rval = "INSERT INTO rpffcstpoint ( " + columns
                + " ) VALUES ( %s, %s, %s, %s, %s, %s, %s, %s, %s, %s )";

        rval = String.format(rval, getDBString(lid), getDBString(groupID),
                getDBString(ordinal), getDBString(changeThreshold),
                getDBString(recordType), getDBString(primaryBackup),
                getDBString(secondaryBackup), getDBString(backHours),
                getDBString(forwardHours), getDBString(adjustEndHours));

        return rval;
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE rpffcstpoint SET lid=%s, group_id=%s, ordinal=%s, chg_threshold=%s, rec_type=%s, primary_back=%s, secondary_back=%s, backhrs=%s, forwardhrs=%s, adjustendhrs=%s WHERE %s";

        // Populate the values
        rval = String.format(rval, getDBString(lid), getDBString(groupID),
                getDBString(ordinal), getDBString(changeThreshold),
                getDBString(recordType), getDBString(primaryBackup),
                getDBString(secondaryBackup), getDBString(backHours),
                getDBString(forwardHours), getDBString(adjustEndHours),
                getPKStatement());

        return rval;
    }

    @Override
    public String getSelectStatement() {
        String columns = "l.lid, group_id, ordinal, chg_threshold, rec_type, primary_back, secondary_back, backhrs, forwardhrs, adjustendhrs, l.name";
        return "SELECT "
                + columns
                + " FROM rpffcstpoint, location l WHERE l.lid=rpffcstpoint.lid ORDER BY ordinal, lid";
    }

    @Override
    public String getDeleteStatement() {
        return "DELETE FROM rpffcstpoint WHERE lid="+getDBString(lid)+" and group_id="
                + getDBString(groupID);
    }

    @Override
    public String getPKStatement() {
        String pkString = "lid=%s";
        return String.format(pkString, getDBString(lid));
    }

    @Override
    public String getExistsStatement() {
        String selectQuery = "SELECT lid FROM rpffcstpoint WHERE "
                + getPKStatement();

        return selectQuery;
    }

    @Override
    public String getConstrainedSelectStatement() {
        String columns = "l.lid, group_id, ordinal, chg_threshold, rec_type, primary_back, secondary_back, backhrs, forwardhrs, adjustendhrs, l.name";
        return "SELECT "
                + columns
                + " FROM rpffcstpoint, location l WHERE l.lid=rpffcstpoint.lid AND rpffcstpoint.lid='"
                + lid + "' ORDER BY ordinal, lid";
    }
}
