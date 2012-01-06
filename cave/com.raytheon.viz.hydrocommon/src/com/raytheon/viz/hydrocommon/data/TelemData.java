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
 * this class contains the Telemetry data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jan 10, 2008	1802    	askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class TelemData extends HydroDBData implements IHydroDBData {

    /**
     * lid character varying(8) NOT NULL,
     */
    private String lid;

    /**
     * "type" character varying(10),
     */
    private String type;

    /**
     * payor character varying(10),
     */
    private String payor;

    /**
     * "cost" double precision,
     */
    private double cost;

    /**
     * criteria character varying(50),
     */
    private String criteria;

    /**
     * "owner" character varying(10),
     */
    private String owner;

    /**
     * phone character varying(12),
     */
    private String phone;

    /**
     * sensorid character varying(10),
     */
    private String sensorId;

    /**
     * rptfreq character varying(4),
     */
    private String reportFrequency;

    /**
     * obsvfreq character varying(4)
     */
    private String observationFrequency;
    
    /**
     * Database table name
     */
    private String dbTable = "telem";

    /**
     * Constructor
     */
    public TelemData() {
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public TelemData(QueryResultRow data, Map<String, Integer> dataMap) {
        setLid(getDBValue("lid", data, dataMap, ""));
        setType(getDBValue("type", data, dataMap, ""));
        setPayor(getDBValue("payor", data, dataMap, ""));
        setCost(getDBValue("cost", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setCriteria(getDBValue("criteria", data, dataMap, ""));
        setOwner(getDBValue("owner", data, dataMap, ""));
        setPhone(getDBValue("phone", data, dataMap, ""));
        setSensorId(getDBValue("sensorid", data, dataMap, ""));
        setReportFrequency(getDBValue("rptfreq", data, dataMap, ""));
        setObservationFrequency(getDBValue("obsvfreq", data, dataMap, ""));
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
     * @return the type
     */
    public String getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * @return the payor
     */
    public String getPayor() {
        return payor;
    }

    /**
     * @param payor
     *            the payor to set
     */
    public void setPayor(String payor) {
        this.payor = payor;
    }

    /**
     * @return the cost
     */
    public double getCost() {
        return cost;
    }

    /**
     * @param cost
     *            the cost to set
     */
    public void setCost(double cost) {
        this.cost = cost;
    }

    /**
     * @return the criteria
     */
    public String getCriteria() {
        return criteria;
    }

    /**
     * @param criteria
     *            the criteria to set
     */
    public void setCriteria(String criteria) {
        this.criteria = criteria;
    }

    /**
     * @return the owner
     */
    public String getOwner() {
        return owner;
    }

    /**
     * @param owner
     *            the owner to set
     */
    public void setOwner(String owner) {
        this.owner = owner;
    }

    /**
     * @return the phone
     */
    public String getPhone() {
        return phone;
    }

    /**
     * @param phone
     *            the phone to set
     */
    public void setPhone(String phone) {
        this.phone = phone;
    }

    /**
     * @return the sensorId
     */
    public String getSensorId() {
        return sensorId;
    }

    /**
     * @param sensorId
     *            the sensorId to set
     */
    public void setSensorId(String sensorId) {
        this.sensorId = sensorId;
    }

    /**
     * @return the reportFrequency
     */
    public String getReportFrequency() {
        return reportFrequency;
    }

    /**
     * @param reportFrequency
     *            the reportFrequency to set
     */
    public void setReportFrequency(String reportFrequency) {
        this.reportFrequency = reportFrequency;
    }

    @Override
    public String getConstrainedSelectStatement() {
        StringBuffer rval = new StringBuffer();

        String columns = "lid, type, payor, cost, criteria, owner, phone, sensorid, rptfreq, obsvfreq";

        rval.append("SELECT ");
        rval.append(columns);
        rval.append(" FROM ");
        rval.append(dbTable);
        rval.append(" WHERE lid=");
        rval.append(getDBString(lid));

        return rval.toString();
    }

    @Override
    public String getDeleteStatement() {
        return "DELETE FROM " + dbTable + " WHERE " + getPKStatement();
    }

    @Override
    public String getExistsStatement() {
        return "SELECT lid FROM " + dbTable + " WHERE " + getPKStatement();
    }

    @Override
    public String getInsertStatement() {
        String columns = "lid, type, payor, cost, criteria, owner, phone, sensorid, rptfreq, obsvfreq";

        String rval = "INSERT INTO " + dbTable + " ( " + columns
                + " ) VALUES ( %s, %s, %s, %s, %s, %s, %s, %s, %s, %s )";

        rval = String.format(rval, getDBString(lid), getDBString(type),
                getDBString(payor), getDBString(cost), getDBString(criteria),
                getDBString(owner), getDBString(phone), getDBString(sensorId),
                getDBString(reportFrequency), getDBString(observationFrequency));

        return rval;
    }

    @Override
    public String getPKStatement() {
        return "lid=" + getDBString(lid);
    }

    @Override
    public String getSelectStatement() {
        StringBuffer rval = new StringBuffer();

        String columns = "lid, type, payor, cost, criteria, owner, phone, sensorid, rptfreq, obsvfreq";

        rval.append("SELECT ");
        rval.append(columns);
        rval.append(" FROM ");
        rval.append(dbTable);

        return rval.toString();
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE "
                + dbTable
                + " SET lid=%s, type=%s, payor=%s, cost=%s, criteria=%s, owner=%s, phone=%s, sensorid=%s, rptfreq=%s, obsvfreq=%s WHERE %s";

        // Populate the values
        rval = String.format(rval, getDBString(lid), getDBString(type),
                getDBString(payor), getDBString(cost), getDBString(criteria),
                getDBString(owner), getDBString(phone), getDBString(sensorId),
                getDBString(reportFrequency), getDBString(observationFrequency),
                getPKStatement());

        return rval;
    }

    /**
     * @return the observationFrequency
     */
    public String getObservationFrequency() {
        return observationFrequency;
    }

    /**
     * @param observationFrequency the observationFrequency to set
     */
    public void setObservationFrequency(String observationFrequency) {
        this.observationFrequency = observationFrequency;
    }
}
