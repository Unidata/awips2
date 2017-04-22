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
 * this class contains the DCP data.
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
public class DcpData extends HydroDBData implements IHydroDBData {

    /**
     * lid character varying(8) NOT NULL,
     */
    private String lid;

    /**
     * criteria character varying(50),
     */
    private String criteria;

    /**
     * "owner" character varying(10),
     */
    private String owner;

    /**
     * goes character varying(8),
     */
    private String goesId;

    /**
     * rptfreq character varying(4),
     */
    private String reportFrequency;

    /**
     * rptime character varying(8),
     */
    private String reportTime;

    /**
     * "notify" character varying(1),
     */
    private String notify;
    
    /**
     * randrept character varying(1)
     */
    private String randomReport;
    
    /**
     * obsvfreq character varying(4)
     */
    private String observationFrequency;

    /**
     * Database table name
     */
    private String dbTable = "dcp";

    /**
     * Constructor
     */
    public DcpData() {
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public DcpData(QueryResultRow data, Map<String, Integer> dataMap) {
        setLid(getDBValue("lid", data, dataMap, ""));
        setCriteria(getDBValue("criteria", data, dataMap, ""));
        setOwner(getDBValue("owner", data, dataMap, ""));
        setGoesId(getDBValue("goes", data, dataMap, ""));
        setReportFrequency(getDBValue("rptfreq", data, dataMap, ""));
        setReportTime(getDBValue("rptime", data, dataMap, ""));
        setNotify(getDBValue("notify", data, dataMap, ""));
        setObservationFrequency(getDBValue("obsvfreq", data, dataMap, ""));
        setRandomReport(getDBValue("randrept", data, dataMap, ""));
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
     * @return the goesId
     */
    public String getGoesId() {
        return goesId;
    }

    /**
     * @param goesId
     *            the goesId to set
     */
    public void setGoesId(String goesId) {
        this.goesId = goesId;
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

    /**
     * @return the reportTime
     */
    public String getReportTime() {
        return reportTime;
    }

    /**
     * @param reportTime
     *            the reportTime to set
     */
    public void setReportTime(String reportTime) {
        this.reportTime = reportTime;
    }

    /**
     * @return the notify
     */
    public String getNotify() {
        return notify;
    }

    /**
     * @param notify
     *            the notify to set
     */
    public void setNotify(String notify) {
        this.notify = notify;
    }

    @Override
    public String getConstrainedSelectStatement() {
        StringBuffer rval = new StringBuffer();

        String columns = "lid, criteria, owner, goes, rptfreq, rptime, notify, obsvfreq, randrept";

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
        String columns = "lid, criteria, owner, goes, rptfreq, rptime, notify, obsvfreq, randrept";

        String rval = "INSERT INTO " + dbTable + " ( " + columns
                + " ) VALUES ( %s, %s, %s, %s, %s, %s, %s, %s, %s )";

        rval = String.format(rval, getDBString(lid), getDBString(criteria),
                getDBString(owner), getDBString(goesId),
                getDBString(reportFrequency), getDBString(reportTime),
                getDBString(notify), getDBString(observationFrequency), 
                getDBString(randomReport));

        return rval;
    }

    @Override
    public String getPKStatement() {
        return "lid=" + getDBString(lid);
    }

    @Override
    public String getSelectStatement() {
        StringBuffer rval = new StringBuffer();

        String columns = "lid, criteria, owner, goes, rptfreq, rptime, notify, obsvfreq, randrept";

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
                + " SET lid=%s, criteria=%s, owner=%s, goes=%s, rptfreq=%s, rptime=%s, notify=%s, obsvfreq=%s, randrept=%s WHERE %s";

        // Populate the values
        rval = String.format(rval, getDBString(lid), getDBString(criteria),
                getDBString(owner), getDBString(goesId),
                getDBString(reportFrequency), getDBString(reportTime),
                getDBString(notify), getDBString(observationFrequency), 
                getDBString(randomReport), getPKStatement());

        return rval;
    }

    /**
     * @return the randomReport
     */
    public String getRandomReport() {
        return randomReport;
    }

    /**
     * @param randomReport the randomReport to set
     */
    public void setRandomReport(String randomReport) {
        this.randomReport = randomReport;
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
