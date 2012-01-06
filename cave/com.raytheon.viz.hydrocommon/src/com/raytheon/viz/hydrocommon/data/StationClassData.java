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
 * this class contains the StnClass data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Nov 19, 2008	1697    	askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class StationClassData extends HydroDBData implements IHydroDBData {

    /**
     * lid - Location/Station ID
     */
    private String lid;

    /**
     * disp_class - Station Type
     */
    private String stationType;

    /**
     * DCP - data source
     */
    private String dcp;

    /**
     * Observer - data source
     */
    private String observer;

    /**
     * telem_type - data source
     */
    private String telemetry;

    /**
     * Default constructor
     */
    public StationClassData() {
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public StationClassData(QueryResultRow data, Map<String, Integer> dataMap) {
        setLid(getDBValue("lid", data, dataMap, ""));
        setStationType(getDBValue("disp_class", data, dataMap, ""));
        setDcp(getDBValue("dcp", data, dataMap, ""));
        setObserver(getDBValue("observer", data, dataMap, ""));
        setTelemetry(getDBValue("telem_type", data, dataMap, ""));
    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    public String getStationType() {
        return stationType;
    }

    public void setStationType(String stationType) {
        this.stationType = stationType;
    }

    public String getDcp() {
        return dcp;
    }

    public void setDcp(String dcp) {
        this.dcp = dcp;
    }

    public String getObserver() {
        return observer;
    }

    public void setObserver(String observer) {
        this.observer = observer;
    }

    public String getTelemetry() {
        return telemetry;
    }

    public void setTelemetry(String telemetry) {
        this.telemetry = telemetry;
    }

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement() + " WHERE lid='" + lid + "'";
    }

    @Override
    public String getDeleteStatement() {
        String rval = "DELETE FROM stnclass WHERE %s";

        return String.format(rval, getPKStatement());
    }

    @Override
    public String getExistsStatement() {
        return getConstrainedSelectStatement();
    }

    @Override
    public String getInsertStatement() {
        String rval = "INSERT INTO stnclass ( lid, disp_class, dcp, observer, telem_type )"
                + " VALUES ('%s', '%s', '%s', '%s', '%s')";

        rval = String.format(rval, getLid(), getStationType(), getDcp(),
                getObserver(), getTelemetry());

        return rval;

    }

    @Override
    public String getPKStatement() {
        return "lid='" + getLid() + "'";
    }

    @Override
    public String getSelectStatement() {
        StringBuffer rval = new StringBuffer();

        rval.append("SELECT lid, disp_class, dcp, observer, telem_type ");
        rval.append("FROM stnclass");

        return rval.toString();
    }

    @Override
    public String getUpdateStatement() {
        String rval = "UPDATE stnclass SET disp_class, dcp, observer, telem_type WHERE %s";

        rval = String.format(rval, getPKStatement());

        return rval;
    }
}
