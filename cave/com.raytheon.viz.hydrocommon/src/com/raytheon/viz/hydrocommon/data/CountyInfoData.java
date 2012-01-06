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
 * This class contains the data for the CountyInfo view (countynum and counties
 * tables).
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
public class CountyInfoData extends HydroDBData implements IHydroDBData,
        Comparable<CountyInfoData> {

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
     * Location
     */
    private String lid;

    /**
     * Constructor
     */
    public CountyInfoData() {
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public CountyInfoData(QueryResultRow data, Map<String, Integer> dataMap) {
        setCounty(getDBValue("county", data, dataMap, ""));
        setState(getDBValue("state", data, dataMap, ""));
        setCountyNumber(getDBValue("countynum", data, dataMap, ""));
        setLid(getDBValue("lid", data, dataMap, ""));
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

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    @Override
    public String getConstrainedSelectStatement() {
        String query = "SELECT lid, state, county, countynum FROM countyinfo WHERE lid="
                + getDBString(lid) + " ORDER BY state, county";
        return query;
    }

    @Override
    public String getDeleteStatement() {
        return "DELETE FROM countynum WHERE lid=" + getDBString(lid);
    }

    @Override
    public String getExistsStatement() {
        return "SELECT lid FROM countyinfo WHERE " + getPKStatement();
    }

    @Override
    public String getInsertStatement() {
        String columns = "lid, state, county";

        String rval = "INSERT INTO countynum ( " + columns
                + " ) VALUES ( %s, %s, %s )";

        rval = String.format(rval, getDBString(lid), getDBString(state),
                getDBString(county));

        return rval;
    }

    @Override
    public String getPKStatement() {
        return "lid='" + getLid() + "' AND county='" + getCounty()
                + "' AND state='" + getState() + "'";
    }

    @Override
    public String getSelectStatement() {
        String query = "SELECT lid, state, county, countynum FROM countyinfo";
        return query;
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE countynum SET lid=%s, state=%s, county=%s WHERE %s";

        // Populate the values
        rval = String.format(rval, getDBString(lid), getDBString(state),
                getDBString(county), getPKStatement());

        return rval;
    }

    @Override
    public boolean equals(Object comp) {
        boolean rval = false;

        if (comp instanceof CountyInfoData) {
            CountyInfoData compData = (CountyInfoData) comp;

            if (compareFields(compData.getCounty(), county)
                    && compareFields(compData.getState(), state)
                    && compareFields(compData.getCountyNumber(), countyNumber)
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
    public int compareTo(CountyInfoData o) {
        int rval = 0;

        // Sort: ORDER BY state, county
        rval = state.compareToIgnoreCase(o.getState());

        if (0 == rval) {
            rval = county.compareToIgnoreCase(o.getCounty());
        }

        return rval;
    }
}
