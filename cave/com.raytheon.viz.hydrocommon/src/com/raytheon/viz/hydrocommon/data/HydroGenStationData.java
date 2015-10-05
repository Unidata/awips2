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
 * this class contains the HGStation data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Dec 29, 2008 1802        askripsky   Initial creation
 * Aug 23, 2011 10482                   Fixed order priority in the query
 * Oct 01, 2015 4943        rjpeter     Fixed mapped query columns.
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class HydroGenStationData extends HydroDBData implements IHydroDBData {

    /**
     * Location id
     */
    private String lid;

    /**
     * Physical Element
     */
    private String pe;

    /**
     * TS
     */
    private String ts;

    /**
     * Fcst TS
     */
    private String forecastTs;

    /**
     * HSA
     */
    private String hsa;

    /**
     * The table that contains the data.
     */
    private String dbTable = "hgstation";

    /**
     * Constructor
     */
    public HydroGenStationData() {
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public HydroGenStationData(QueryResultRow data, Map<String, Integer> dataMap) {
        setLid(getDBValue("lid", data, dataMap, ""));
        setPe(getDBValue("pe", data, dataMap, ""));
        setTs(getDBValue("ts", data, dataMap, ""));
        setForecastTs(getDBValue("fcstts", data, dataMap, ""));
        setHsa(getDBValue("hsa", data, dataMap, ""));
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
     * @return the pe
     */
    public String getPe() {
        return pe;
    }

    /**
     * @param pe
     *            the pe to set
     */
    public void setPe(String pe) {
        this.pe = pe;
    }

    /**
     * @return the ts
     */
    public String getTs() {
        return ts;
    }

    /**
     * @param ts
     *            the ts to set
     */
    public void setTs(String ts) {
        this.ts = ts;
    }

    /**
     * @return the forecastTs
     */
    public String getForecastTs() {
        return forecastTs;
    }

    /**
     * @param forecastTs
     *            the forecastTs to set
     */
    public void setForecastTs(String forecastTs) {
        this.forecastTs = forecastTs;
    }

    /**
     * @return the hsa
     */
    public String getHsa() {
        return hsa;
    }

    /**
     * @param hsa
     *            the hsa to set
     */
    public void setHsa(String hsa) {
        this.hsa = hsa;
    }

    @Override
    public String getInsertStatement() {
        String rval = "INSERT INTO " + dbTable + " ( lid, pe, ts, fcstts"
                + " ) VALUES ( %s, %s, %s, %s )";

        rval = String.format(rval, getDBString(lid), getDBString(pe),
                getDBString(ts), getDBString(forecastTs));

        return rval;
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE " + dbTable + " SET "
                + "lid=%s, pe=%s, ts=%s, fcstts=%s" + " WHERE %s";

        // Populate the values
        rval = String.format(rval, getDBString(lid), getDBString(pe),
                getDBString(ts), getDBString(forecastTs), getPKStatement());

        return rval;
    }

    @Override
    public String getSelectStatement() {
        return "SELECT " + dbTable + ".lid as lid, pe, ts, fcstts, hsa FROM "
                + dbTable + ", location WHERE location.lid=" + dbTable
                + ".lid ORDER BY hsa,lid, pe, ts";
    }

    @Override
    public String getDeleteStatement() {
        return String.format("DELETE FROM " + dbTable + " WHERE %s",
                getPKStatement());
    }

    @Override
    public String getPKStatement() {
        String pkString = "lid=%s AND pe=%s AND ts=%s";
        return String.format(pkString, getDBString(lid), getDBString(pe),
                getDBString(ts));
    }

    @Override
    public String getExistsStatement() {
        String selectQuery = "SELECT lid FROM " + dbTable + " WHERE "
                + getPKStatement();

        return selectQuery;
    }

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement();
    }
}
