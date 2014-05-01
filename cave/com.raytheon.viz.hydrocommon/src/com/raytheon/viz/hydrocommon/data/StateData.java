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
 * this class contains the State data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jan 8, 2008	1802    	askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class StateData extends HydroDBData implements IHydroDBData {
    /**
     * State
     */
    private String state;

    /**
     * Name
     */
    private String name;

    /**
     * Constructor
     */
    public StateData() {
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public StateData(QueryResultRow data, Map<String, Integer> dataMap) {
        setState(getDBValue("state", data, dataMap, ""));
        setName(getDBValue("name", data, dataMap, ""));

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
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement();
    }

    @Override
    public String getDeleteStatement() {
        return "DELETE FROM state WHERE " + getPKStatement();
    }

    @Override
    public String getExistsStatement() {
        return "SELECT state FROM state WHERE " + getPKStatement();
    }

    @Override
    public String getInsertStatement() {
        return "INSERT INTO state ( state, name ) VALUES ( "
                + getDBString(state) + "," + getDBString(name) + " )";
    }

    @Override
    public String getPKStatement() {
        return "state='" + getState() + "'";
    }

    @Override
    public String getSelectStatement() {
        StringBuffer rval = new StringBuffer();

        rval.append("SELECT state, name");
        rval.append(" FROM state");
        rval.append(" WHERE state != 'XX' ORDER BY state");

        return rval.toString();
    }

    @Override
    public String getUpdateStatement() {
        String rval = "UPDATE state SET state=%s, name=%s WHERE %s";

        // Populate the values
        rval = String.format(rval, getDBString(state), getDBString(name),
                getPKStatement());

        return rval;
    }
}
