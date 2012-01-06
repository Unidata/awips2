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
 * This class contains the data for the Floodstmt table.
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
public class FloodStatementData extends HydroDBData implements IHydroDBData {
    /**
     * Location
     */
    private String lid;

    /**
     * Impact Value
     */
    private double impactValue;

    /**
     * Statement
     */
    private String statement;

    /**
     * Rise/Fall
     */
    private String riseFall;

    /**
     * Date Start
     */
    private String dateStart;

    /**
     * Date End
     */
    private String dateEnd;

    /**
     * Impact PE
     */
    private String impactPE;

    /**
     * Constructor
     */
    public FloodStatementData() {
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public FloodStatementData(QueryResultRow data, Map<String, Integer> dataMap) {
        setLid(getDBValue("lid", data, dataMap, ""));
        setImpactValue(getDBValue("impact_value", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setStatement(getDBValue("statement", data, dataMap, ""));
        setRiseFall(getDBValue("rf", data, dataMap, ""));
        setDateStart(getDBValue("datestart", data, dataMap, ""));
        setDateEnd(getDBValue("dateend", data, dataMap, ""));
        setImpactPE(getDBValue("impact_pe", data, dataMap, ""));
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
     * @return the impactValue
     */
    public double getImpactValue() {
        return impactValue;
    }

    /**
     * @param impactValue
     *            the impactValue to set
     */
    public void setImpactValue(double impactValue) {
        this.impactValue = impactValue;
    }

    /**
     * @return the statement
     */
    public String getStatement() {
        return statement;
    }

    /**
     * @param statement
     *            the statement to set
     */
    public void setStatement(String statement) {
        this.statement = statement;
    }

    /**
     * @return the riseFall
     */
    public String getRiseFall() {
        return riseFall;
    }

    /**
     * @param riseFall
     *            the riseFall to set
     */
    public void setRiseFall(String riseFall) {
        this.riseFall = riseFall;
    }

    /**
     * @return the dateStart
     */
    public String getDateStart() {
        return dateStart;
    }

    /**
     * @param dateStart
     *            the dateStart to set
     */
    public void setDateStart(String dateStart) {
        this.dateStart = dateStart;
    }

    /**
     * @return the dateEnd
     */
    public String getDateEnd() {
        return dateEnd;
    }

    /**
     * @param dateEnd
     *            the dateEnd to set
     */
    public void setDateEnd(String dateEnd) {
        this.dateEnd = dateEnd;
    }

    /**
     * @return the impactPE
     */
    public String getImpactPE() {
        return impactPE;
    }

    /**
     * @param impactPE
     *            the impactPE to set
     */
    public void setImpactPE(String impactPE) {
        this.impactPE = impactPE;
    }

    @Override
    public String getConstrainedSelectStatement() {
        String columns = "lid, impact_value, statement, rf, datestart, dateend, impact_pe";
        String query = "SELECT " + columns + " FROM floodstmt WHERE lid="
                + getDBString(lid) + " ORDER BY impact_value DESC";
        return query;
    }

    @Override
    public String getDeleteStatement() {
        return "DELETE FROM floodstmt WHERE " + getPKStatement();
    }

    @Override
    public String getExistsStatement() {
        return "SELECT lid FROM floodstmt WHERE " + getPKStatement();
    }

    @Override
    public String getInsertStatement() {
        String columns = "lid, impact_value, statement, rf, datestart, dateend, impact_pe";

        String rval = "INSERT INTO floodstmt ( " + columns
                + " ) VALUES ( %s, %s, %s, %s, %s, %s, %s )";

        rval = String.format(rval, getDBString(lid), getDBString(impactValue),
                getDBString(statement), getDBString(riseFall),
                getDBString(dateStart), getDBString(dateEnd),
                getDBString(impactPE));

        return rval;
    }

    @Override
    public String getPKStatement() {
        String pk = "lid=%s AND impact_value=%s AND rf=%s AND datestart=%s AND dateend=%s";
        return String.format(pk, getDBString(lid), getDBString(impactValue),
                getDBString(riseFall), getDBString(dateStart),
                getDBString(dateEnd));
    }

    @Override
    public String getSelectStatement() {
        String columns = "lid, impact_value, statement, rf, datestart, dateend, impact_pe";
        String query = "SELECT " + columns + " FROM floodstmt";
        return query;
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE floodstmt SET lid=%s, impact_value=%s, statement=%s, rf=%s, datestart=%s, dateend=%s, impact_pe=%s WHERE %s";

        // Populate the values
        rval = String.format(rval, getDBString(lid), getDBString(impactValue),
                getDBString(statement), getDBString(riseFall),
                getDBString(dateStart), getDBString(dateEnd),
                getDBString(impactPE), getPKStatement());

        return rval;
    }
}
