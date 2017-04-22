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
 * this class contains the Counties data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Dec 1, 2008	1744    	askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class CountiesData extends HydroDBData implements IHydroDBData {

    /**
     * County
     */
    private String county;

    /**
     * State
     */
    private String state;

    /**
     * County Number
     */
    private String countyNumber;

    /**
     * WFO
     */
    private String wfo;

    /**
     * primary_back
     */
    private String primaryBack;

    /**
     * secondary_back
     */
    private String secondaryBack;

    /**
     * Constructor
     */
    public CountiesData() {
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public CountiesData(QueryResultRow data, Map<String, Integer> dataMap) {
        setCounty(getDBValue("county", data, dataMap, ""));
        setState(getDBValue("state", data, dataMap, ""));
        setCountyNumber(getDBValue("countynum", data, dataMap, ""));
        setWfo(getDBValue("wfo", data, dataMap, ""));
        setPrimaryBack(getDBValue("primary_back", data, dataMap, ""));
        setSecondaryBack(getDBValue("secondary_back", data, dataMap, ""));
    }

    public String getCounty() {
        return county;
    }

    public void setCounty(String county) {
        this.county = county;
    }

    public String getState() {
        return state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public String getCountyNumber() {
        return countyNumber;
    }

    public void setCountyNumber(String countyNumber) {
        this.countyNumber = countyNumber;
    }

    public String getWfo() {
        return wfo;
    }

    public void setWfo(String wfo) {
        this.wfo = wfo;
    }

    public String getPrimaryBack() {
        return primaryBack;
    }

    public void setPrimaryBack(String primaryBack) {
        this.primaryBack = primaryBack;
    }

    public String getSecondaryBack() {
        return secondaryBack;
    }

    public void setSecondaryBack(String secondaryBack) {
        this.secondaryBack = secondaryBack;
    }

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement();
    }

    @Override
    public String getDeleteStatement() {
        return "DELETE FROM counties WHERE " + getPKStatement();
    }

    @Override
    public String getExistsStatement() {
        return "SELECT county FROM counties WHERE " + getPKStatement();
    }

    @Override
    public String getInsertStatement() {
        String columns = "county, state, countynum, wfo, primary_back, secondary_back";

        String rval = "INSERT INTO counties ( " + columns
                + " ) VALUES ( %s, %s, %s, %s, %s, %s )";

        rval = String.format(rval, getDBString(county), getDBString(state),
                getDBString(countyNumber), getDBString(wfo),
                getDBString(primaryBack), getDBString(secondaryBack));

        return rval;
    }

    @Override
    public String getPKStatement() {
        return "county='" + getCounty() + "' and state='" + getState() + "'";
    }

    @Override
    public String getSelectStatement() {
        StringBuffer rval = new StringBuffer();

        rval
                .append("SELECT county, state, countynum, wfo, primary_back, secondary_back");
        rval.append(" FROM counties");
        rval.append(" WHERE state != 'XX' ORDER BY state, county");

        return rval.toString();
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE counties SET county=%s, state=%s, countynum=%s, wfo=%s, "
                + "primary_back=%s, secondary_back=%s WHERE %s";

        // Populate the values
        rval = String.format(rval, getDBString(county), getDBString(state),
                getDBString(countyNumber), getDBString(wfo),
                getDBString(primaryBack), getDBString(secondaryBack),
                getPKStatement());

        return rval;
    }
}
