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
 * This class contains the data for the EligZon.
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
public class EligZoneData extends HydroDBData implements IHydroDBData {

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
     * Constructor
     */
    public EligZoneData() {
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public EligZoneData(QueryResultRow data, Map<String, Integer> dataMap) {
        setDescription(getDBValue("descr", data, dataMap, ""));
        setState(getDBValue("state", data, dataMap, ""));
        setZoneNumber(getDBValue("zonenum", data, dataMap, ""));
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

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement();
    }

    @Override
    public String getDeleteStatement() {
        return "DELETE FROM eligzon WHERE " + getPKStatement();
    }

    @Override
    public String getExistsStatement() {
        return "SELECT state FROM eligzon WHERE " + getPKStatement();
    }

    @Override
    public String getInsertStatement() {
        String columns = "state, zonenum, descr";

        String rval = "INSERT INTO eligzon ( " + columns
                + " ) VALUES ( %s, %s, %s )";

        rval = String.format(rval, getDBString(state), getDBString(zoneNumber),
                getDBString(description));

        return rval;
    }

    @Override
    public String getPKStatement() {
        return "zonenum=" + getDBString(zoneNumber) + " AND state="
                + getDBString(state);
    }

    @Override
    public String getSelectStatement() {
        String query = "SELECT state, zonenum, descr FROM eligzon where state != 'XX' ORDER BY state, zonenum";
        return query;
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE eligzon SET state=%s, zonenum=%s, descr=%s WHERE %s";

        // Populate the values
        rval = String.format(rval, getDBString(state), getDBString(zoneNumber),
                getDBString(description), getPKStatement());

        return rval;
    }
}
