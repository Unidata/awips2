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
 * This class contains the data for the ZoneInfo view.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jan 5, 2008	1802    	askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class ZoneInfoData extends HydroDBData implements IHydroDBData,
        Comparable<ZoneInfoData> {

    /**
     * Description
     */
    private String description;

    /**
     * State
     */
    private String state;

    /**
     * Zone Number
     */
    private String zoneNumber;

    /**
     * Location
     */
    private String lid;

    /**
     * Constructor
     */
    public ZoneInfoData() {
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public ZoneInfoData(QueryResultRow data, Map<String, Integer> dataMap) {
        setDescription(getDBValue("descr", data, dataMap, ""));
        setState(getDBValue("state", data, dataMap, ""));
        setZoneNumber(getDBValue("zonenum", data, dataMap, ""));
        setLid(getDBValue("lid", data, dataMap, ""));
    }

    /**
     * @return the description
     */
    public String getDescription() {
        return description;
    }

    /**
     * @param description
     *            the description to set
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * @return the state
     */
    public String getState() {
        return state;
    }

    /**
     * @param state
     *            the state to set
     */
    public void setState(String state) {
        this.state = state;
    }

    /**
     * @return the zoneNumber
     */
    public String getZoneNumber() {
        return zoneNumber;
    }

    /**
     * @param zoneNumber
     *            the zoneNumber to set
     */
    public void setZoneNumber(String zoneNumber) {
        this.zoneNumber = zoneNumber;
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

    @Override
    public String getConstrainedSelectStatement() {
        String query = "SELECT lid, state, zonenum, descr FROM zoneinfo WHERE lid="
                + getDBString(lid) + " ORDER BY state, zonenum";
        return query;
    }

    @Override
    public String getDeleteStatement() {
        return "DELETE FROM zonenum WHERE lid=" + getDBString(lid);
    }

    @Override
    public String getExistsStatement() {
        return "SELECT lid FROM zoneinfo WHERE " + getPKStatement();
    }

    @Override
    public String getInsertStatement() {
        String columns = "lid, state, zonenum";

        String rval = "INSERT INTO zonenum ( " + columns
                + " ) VALUES ( %s, %s, %s )";

        rval = String.format(rval, getDBString(lid), getDBString(state),
                getDBString(zoneNumber));

        return rval;
    }

    @Override
    public String getPKStatement() {
        return "lid=" + getDBString(lid) + " AND zonenum="
                + getDBString(zoneNumber) + " AND state=" + getDBString(state);
    }

    @Override
    public String getSelectStatement() {
        String query = "SELECT lid, state, zonenum, descr FROM zoneinfo";
        return query;
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE zonenum SET lid=%s, state=%s, zonenum=%s WHERE %s";

        // Populate the values
        rval = String.format(rval, getDBString(lid), getDBString(state),
                getDBString(zoneNumber), getPKStatement());

        return rval;
    }

    @Override
    public boolean equals(Object comp) {
        boolean rval = false;

        if (comp instanceof ZoneInfoData) {
            ZoneInfoData compData = (ZoneInfoData) comp;

            if (compareFields(compData.getDescription(), description)
                    && compareFields(compData.getState(), state)
                    && compareFields(compData.getZoneNumber(), zoneNumber)
                    && compareFields(compData.getLid(), lid)) {
                rval = true;
            }
        }

        return rval;
    }

    public boolean compareFields(String str1, String str2) {
        boolean areEqual = false;

        if (str1 == null && str2 == null) {
            areEqual = true;
        } else if (str1 == null || str2 == null) {
            areEqual = false;
        } else {
            areEqual = str1.equals(str2);
        }

        return areEqual;
    }

    @Override
    public int compareTo(ZoneInfoData o) {
        int rval = 0;

        // Sort: ORDER BY state, zonenum
        rval = state.compareToIgnoreCase(o.getState());

        if (0 == rval) {
            rval = zoneNumber.compareToIgnoreCase(o.getZoneNumber());
        }

        return rval;
    }
}
