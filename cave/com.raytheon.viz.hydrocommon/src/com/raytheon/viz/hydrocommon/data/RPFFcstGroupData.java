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
 * this class contains the RiverPro Forecast Group data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Dec 19, 2008	1787		askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class RPFFcstGroupData extends HydroDBData implements IHydroDBData {

    /**
     * group_id
     */
    private String groupID;

    /**
     * group_name.
     */
    private String groupName;

    /**
     * ordinal.
     */
    private int ordinal;

    /**
     * rec_all_included
     */
    private String recommendAll;

    /**
     * Order By Statement
     */
    private String orderByStatement = "ORDER BY ordinal, group_id";

    /**
     * Constructor
     */
    public RPFFcstGroupData() {
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public RPFFcstGroupData(QueryResultRow data, Map<String, Integer> dataMap) {
        setGroupID(getDBValue("group_id", data, dataMap, ""));
        setGroupName(getDBValue("group_name", data, dataMap, ""));
        setOrdinal(getDBValue("ordinal", data, dataMap,
                HydroConstants.MISSING_VALUE));
        setRecommendAll(getDBValue("rec_all_included", data, dataMap, ""));
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
     * @return the groupName
     */
    public String getGroupName() {
        return groupName;
    }

    /**
     * @param groupName
     *            the groupName to set
     */
    public void setGroupName(String groupName) {
        this.groupName = groupName;
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
     * @return the recommendAll
     */
    public String getRecommendAll() {
        return recommendAll;
    }

    /**
     * @param recommendAll
     *            the recommendAll to set
     */
    public void setRecommendAll(String recommendAll) {
        this.recommendAll = recommendAll;
    }

    /**
     * @return the orderByStatement
     */
    public String getOrderByStatement() {
        return orderByStatement;
    }

    /**
     * @param orderByStatement
     *            the orderByStatement to set
     */
    public void setOrderByStatement(String orderByStatement) {
        this.orderByStatement = orderByStatement;
    }

    @Override
    public String getInsertStatement() {
        String columns = "group_id, group_name, ordinal, rec_all_included";
        String rval = "INSERT INTO rpffcstgroup ( " + columns
                + " ) VALUES ( %s, %s, %s, %s )";

        rval = String.format(rval, getDBString(groupID),
                getDBString(groupName), getDBString(ordinal),
                getDBString(recommendAll));

        return rval;
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE rpffcstgroup SET group_id=%s, group_name=%s, ordinal=%s, rec_all_included=%s WHERE %s";

        // Populate the values
        rval = String.format(rval, getDBString(groupID),
                getDBString(groupName), getDBString(ordinal),
                getDBString(recommendAll), getPKStatement());

        return rval;
    }

    @Override
    public String getSelectStatement() {
        return "SELECT group_id, group_name, ordinal, rec_all_included FROM rpffcstgroup "
                + orderByStatement;
    }

    @Override
    public String getDeleteStatement() {
        return String.format("DELETE FROM rpffcstgroup WHERE %s",
                getPKStatement());
    }

    @Override
    public String getPKStatement() {
        String pkString = "group_id=%s";
        return String.format(pkString, getDBString(groupID));
    }

    @Override
    public String getExistsStatement() {
        String selectQuery = "SELECT group_id, group_name, ordinal, rec_all_included FROM rpffcstgroup WHERE "
                + getPKStatement();

        return selectQuery;
    }

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement();
    }
}
