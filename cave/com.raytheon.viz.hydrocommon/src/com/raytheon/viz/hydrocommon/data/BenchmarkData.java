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
 * this class contains the Benchmark data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Dec 17, 2008	1787    	askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class BenchmarkData extends HydroDBData implements IHydroDBData {

    /**
     * lid - Location/Station ID
     */
    private String lid;

    /**
     * bnum - Benchmark Number
     */
    private String benchmarkNumber;

    /**
     * elev - Elevation
     */
    private double elevation;

    /**
     * remark
     */
    private String remark;

    /**
     * Constructor
     */
    public BenchmarkData() {
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public BenchmarkData(QueryResultRow data, Map<String, Integer> dataMap) {
        setLid(getDBValue("lid", data, dataMap, ""));
        setBenchmarkNumber(getDBValue("bnum", data, dataMap, ""));
        setElevation(getDBValue("elev", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setRemark(getDBValue("remark", data, dataMap, ""));
    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    public String getRemark() {
        return remark;
    }

    public void setRemark(String remark) {
        this.remark = remark;
    }

    /**
     * @return the benchmarkNumber
     */
    public String getBenchmarkNumber() {
        return benchmarkNumber;
    }

    /**
     * @param benchmarkNumber
     *            the benchmarkNumber to set
     */
    public void setBenchmarkNumber(String benchmarkNumber) {
        this.benchmarkNumber = benchmarkNumber;
    }

    /**
     * @return the elevation
     */
    public double getElevation() {
        return elevation;
    }

    /**
     * @param elevation
     *            the elevation to set
     */
    public void setElevation(double elevation) {
        this.elevation = elevation;
    }

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement() + " WHERE lid='" + lid
                + "' ORDER BY elev DESC";
    }

    @Override
    public String getDeleteStatement() {
        return "DELETE FROM benchmark WHERE " + getPKStatement();
    }

    @Override
    public String getExistsStatement() {
        return getSelectStatement() + " WHERE " + getPKStatement();
    }

    @Override
    public String getInsertStatement() {
        String rval = "INSERT INTO benchmark ( lid, bnum, elev, remark )"
                + " VALUES ( '%s', '%s', %s, '%s' )";

        rval = String.format(rval, lid, benchmarkNumber,
                getDBString(elevation), remark);

        return rval;
    }

    @Override
    public String getPKStatement() {
        return "lid='" + getLid() + "' AND bnum='" + benchmarkNumber + "'";
    }

    @Override
    public String getSelectStatement() {
        StringBuffer rval = new StringBuffer();

        rval.append("SELECT ");
        rval.append("lid, bnum, elev, remark");
        rval.append(" FROM benchmark");

        return rval.toString();
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE benchmark SET lid='%s', bnum='%s', elev=%s, remark='%s' WHERE %s";

        // Populate the values
        rval = String.format(rval, lid, benchmarkNumber,
                getDBString(elevation), remark, getPKStatement());

        return rval;
    }
}
