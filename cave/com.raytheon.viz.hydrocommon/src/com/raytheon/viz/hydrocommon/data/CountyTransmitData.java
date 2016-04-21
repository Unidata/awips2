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
 * this class contains the NWR Transmitter County data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jan 9, 2008	1802    	askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class CountyTransmitData extends HydroDBData implements IHydroDBData {

    /**
     * call_sign character varying(6) NOT NULL,
     */
    private String callSign;

    /**
     * county character varying(20),
     */
    private String county;

    /**
     * state character varying(2),
     */
    private String state;

    /**
     * Constructor
     */
    public CountyTransmitData() {
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public CountyTransmitData(QueryResultRow data, Map<String, Integer> dataMap) {
        setCallSign(getDBValue("call_sign", data, dataMap, ""));
        setCounty(getDBValue("county", data, dataMap, ""));
        setState(getDBValue("state", data, dataMap, ""));
    }

    /**
     * @return the callSign
     */
    public String getCallSign() {
        return callSign;
    }

    /**
     * @param callSign
     *            the callSign to set
     */
    public void setCallSign(String callSign) {
        this.callSign = callSign;
    }

    /**
     * @return the county
     */
    public String getCounty() {
        return county;
    }

    /**
     * @param county
     *            the county to set
     */
    public void setCounty(String county) {
        this.county = county;
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

    @Override
    public String getConstrainedSelectStatement() {
        String columns = "call_sign, county, state";
        String query = "SELECT " + columns
                + " FROM countytransmit WHERE call_sign="
                + getDBString(callSign) + " ORDER BY state, county";
        return query;
    }

    @Override
    public String getDeleteStatement() {
        return "DELETE FROM countytransmit WHERE " + getPKStatement();
    }

    @Override
    public String getExistsStatement() {
        return "SELECT call_sign FROM countytransmit WHERE " + getPKStatement();
    }

    @Override
    public String getInsertStatement() {
        String columns = "call_sign, county, state";

        String rval = "INSERT INTO countytransmit ( " + columns
                + " ) VALUES ( %s, %s, %s )";

        rval = String.format(rval, getDBString(callSign), getDBString(county),
                getDBString(state));

        return rval;
    }

    @Override
    public String getPKStatement() {
        String pk = "call_sign=%s AND county=%s AND state=%s";
        return String.format(pk, getDBString(callSign), getDBString(county),
                getDBString(state));
    }

    @Override
    public String getSelectStatement() {
        StringBuffer rval = new StringBuffer();

        String columns = "call_sign, county, state";

        rval.append("SELECT ");
        rval.append(columns);
        rval.append(" FROM countytransmit");
        rval.append(" ORDER BY state, county");

        return rval.toString();
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE countytransmit SET call_sign=%s, county=%s, state=%s WHERE %s";

        // Populate the values
        rval = String.format(rval, getDBString(callSign), getDBString(county),
                getDBString(state), getPKStatement());

        return rval;
    }
}
