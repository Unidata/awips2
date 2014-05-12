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
package com.raytheon.viz.hydro.timeseries;

import java.sql.Timestamp;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.dataplugin.shef.tables.Fcstheight;
import com.raytheon.uf.common.dataplugin.shef.tables.FcstheightId;
import com.raytheon.uf.common.dataplugin.shef.tables.Rejecteddata;
import com.raytheon.uf.common.dataplugin.shef.tables.RejecteddataId;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.hydro.timeseries.table.DataRecord;
import com.raytheon.viz.hydro.timeseries.table.SiteInfo;
import com.raytheon.viz.hydro.timeseries.table.TabularData;
import com.raytheon.viz.hydro.timeseries.util.TimeSeriesUtil;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.ForecastData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDataManager;
import com.raytheon.viz.hydrocommon.datamanager.SqlBuilder;
import com.raytheon.viz.hydrocommon.util.DbUtils;

/**
 * Class for managing database query calls. TimeSeriesDataManager.java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03Sept2008   #1509      dhladky     Initial Creation.
 * 20Oct2008    1520       mpduff      Add Timeseries queries.
 * 04Nov2009    2639       mpduff      Fixed some queries, code cleanup.
 * 21July2010   #5964      lbousaidi   Added contingencyValue table name
 * 									   to addDataRecord routine.
 * 25Oct 2010	2640	   lbousaidi   changed obstime to productime in
 * 									   getTabularData select query
 * Apr 05, 2011 8732       jpiatt      Added product_id to getGraphData query.
 * June 01 2011 9499       djingtao    add dur in getGraphData()
 * July 25 2011 10082      djingtao    modify edit()
 * May  30 2012 14967      wkwock      overload insertRejectedData method
 * Feb  22 2013 14676      lbousaidi   check when producttime is null
 * Mar 25, 2013  1781      mpduff      Constrain time series table query with a start time.
 * May 12  2014  16705     lbousaidi   update revision and shef_qual_code in edit routine.
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class TimeSeriesDataManager extends HydroDataManager {

    private static final String TIME_SERIES_DATA_QUERY = "select lid,obstime,lid,product_id from latestobsvalue";

    private final String INGEST_FILTER_QUERY = "select lid,pe,ts,extremum,dur from ingestfilter where lid=':lid' and ingest = 'T' order by pe asc,ts_rank asc,ts asc,dur asc,extremum asc";

    private final String SHEF_PE_QUERY = "select name||' '|| eng_unit from shefpe where pe=':pe'";

    private static final String SHEF_PE_GENERIC_UNITS = "Generic Units";

    private static TimeSeriesDataManager manager = null;

    private Map<String, String> stationData;

    private boolean sortChanged = false;

    private static SimpleDateFormat dateFormat;

    /**
     * Map holding the location id and display class.
     */
    private Map<String, String> stnDisplayMap = null;

    static {
        dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /**
     * private constructor
     */
    private TimeSeriesDataManager() {
    }

    /**
     * Get an instance of this singleton object
     * 
     * @return instance of this class
     */
    public static synchronized TimeSeriesDataManager getInstance() {
        if (manager == null) {
            manager = new TimeSeriesDataManager();
        }
        return manager;
    }

    /**
     * Get TimeSeriesData from the DB
     * 
     * @return Object[]
     */
    public Object[] getTimeSeriesData() {
        List<Object[]> data;

        try {
            data = DirectDbQuery.executeQuery(TIME_SERIES_DATA_QUERY,
                    HydroConstants.IHFS, QueryLanguage.SQL);
            if (data != null) {
                return data.get(0);
            }
        } catch (VizException e) {

        }
        return null;
    }

    /**
     * Get TimeSeriesStationData from the DB
     * 
     * @return map
     */
    public Map<String, String> getTimeSeriesStationData() {
        return getTimeSeriesStationData(false);
    }

    /**
     * Get the station list data sorted by lid.
     * 
     * @param sortByName
     *            boolean
     * @return map
     */
    public Map<String, String> getTimeSeriesStationData(boolean sortByName) {
        boolean stnDisplayMapSet = true;

        StringBuilder sql = new StringBuilder("Select ");
        sql.append("distinct location.lid, ingestfilter.pe, ");
        sql.append("upper(location.name) as uppername, stnclass.disp_Class ");
        sql.append("from ingestfilter, location, stnclass ");
        sql.append("where ingest='T' and location.lid = stnclass.lid ");
        sql.append("and location.lid=ingestfilter.lid ");

        if (sortByName) {
            sql.append("order by uppername asc");
        } else {
            sql.append("order by lid asc");
        }

        // if the sort changed then requery for the data
        if (sortChanged != sortByName) {
            sortChanged = true;
        }

        if (stnDisplayMap == null) {
            stnDisplayMap = new HashMap<String, String>();
            stnDisplayMapSet = false;
        }

        if ((stationData == null) || sortChanged) {
            stationData = new LinkedHashMap<String, String>();

            List<Object[]> data;
            try {
                data = DirectDbQuery.executeQuery(sql.toString(),
                        HydroConstants.IHFS, QueryLanguage.SQL);
                for (Object[] rowData : data) {
                    if (rowData[2] == null) {
                        rowData[2] = "";
                    }
                    stationData.put((String) rowData[0], (String) rowData[2]);
                    if (!stnDisplayMapSet) {
                        stnDisplayMap.put((String) rowData[0],
                                (String) rowData[3]);
                    }
                }
            } catch (VizException e) {
                e.printStackTrace();
            }
        }

        return stationData;
    }

    /**
     * Get the site's PE data for graphical display
     * 
     * @param lid
     *            The lid to query for
     * @return ArrayList of Object[] of data
     * @throws VizException
     */
    public List<Object[]> getSitePEData(String lid) throws VizException {
        return DirectDbQuery.executeQuery(
                INGEST_FILTER_QUERY.replace(":lid", lid), HydroConstants.IHFS,
                QueryLanguage.SQL);
    }

    /**
     * Get the site's PE data for tabular display
     * 
     * @param lid
     *            The lid to query for
     * @return List of SiteInfo objects
     * @throws VizException
     */
    public List<SiteInfo> getTabularPEData(String lid) throws VizException {
        StringBuilder sql = new StringBuilder("select ");
        sql.append("lid, pe, dur, ts, extremum from Ingestfilter ");
        sql.append("where lid = '").append(lid).append("' and ingest = 'T' ");
        sql.append("order by pe, dur, ts, extremum asc");

        ArrayList<Object[]> results = (ArrayList<Object[]>) DirectDbQuery
                .executeQuery(sql.toString(), HydroConstants.IHFS,
                        QueryLanguage.SQL);

        ArrayList<SiteInfo> returnData = new ArrayList<SiteInfo>();
        for (int i = 0; i < results.size(); i++) {
            Object[] oa = results.get(i);
            SiteInfo si = new SiteInfo();
            si.setLid((String) oa[0]);
            si.setPe((String) oa[1]);
            si.setDur((Integer) oa[2]);
            si.setTs((String) oa[3]);
            si.setExt((String) oa[4]);
            returnData.add(si);
        }

        return returnData;
    }

    /**
     * Checks the record count in the Location and Stnclass tables.
     * 
     * @return true if same count, otherwise false
     * @throws VizException
     */
    public boolean checkLidCount() throws VizException {
        /* Set up a location query */
        String locationTable = "locview";
        String stnClassTable = "stnclass";

        StringBuilder errMessage = null;
        errMessage = new StringBuilder(
                "Error querying the Location table, no data returned");

        /* Execute the location query */
        long locationCount = recordCount(locationTable, "");
        if (locationCount <= 0) {
            throw new VizException(errMessage.toString());
        }
        errMessage.setLength(0);

        errMessage.setLength(0);
        errMessage
                .append("Error querying the Stnclass table, no data returned");

        /* Execute the Stnclass query */
        long stnClassCount = recordCount(stnClassTable, "");

        if (stnClassCount <= 0) {
            throw new VizException(errMessage.toString());
        }

        if (stnClassCount == locationCount) {
            return true;
        }

        return false;
    }

    /**
     * Get the shef pe data from IHFS.
     * 
     * @param pe
     *            The PE value
     * @return The units of the data
     * @throws VizException
     */
    public String getShefPE(String pe) throws VizException {

        List<Object[]> data = DirectDbQuery.executeQuery(
                SHEF_PE_QUERY.replace(":pe", pe), HydroConstants.IHFS,
                QueryLanguage.SQL);
        if ((data != null) && (data.size() > 0)) {
            Object[] sa = data.get(0);
            return (String) sa[0];

        }
        return SHEF_PE_GENERIC_UNITS;
    }

    /**
     * Query the table.
     * 
     * @param tablename
     *            The table to query
     * @param lid
     *            The location id
     * @param pe
     *            The physical element
     * @param ts
     *            The type source
     * @param startTime
     *            The start time
     * @param endTime
     *            The end time
     * @return The List of Object[] data
     * @throws VizException
     * @throws ClassNotFoundException
     */
    public ArrayList<Object[]> getGraphData(String tablename, String lid,
            String pe, String ts, String dur, String extremum, Date startTime,
            Date endTime) throws VizException, ClassNotFoundException {

        StringBuilder graphQuery = new StringBuilder(
                "select lid,obstime,value,product_id from ");
        graphQuery.append(tablename + " where lid = '" + lid + "' and pe = '"
                + pe + "' " + "and dur = '" + dur + "' ");
        graphQuery.append("and ts = '" + ts + "' and extremum = '"
                + extremum.toUpperCase() + "' and obstime ");
        graphQuery.append("between '"
                + HydroConstants.DATE_FORMAT.format(startTime) + "' ");
        graphQuery.append("and '" + HydroConstants.DATE_FORMAT.format(endTime)
                + "' ");
        graphQuery.append("order by obstime asc");

        return (ArrayList<Object[]>) DirectDbQuery.executeQuery(
                graphQuery.toString(), HydroConstants.IHFS, QueryLanguage.SQL);
    }

    /**
     * Query the Riverstat table for the flood/action stage/flow.
     * 
     * @param lid
     *            The Location Id
     * @return The list of flood stage values
     * @throws VizException
     */
    public List<Object[]> getFloodStage(String lid) throws VizException {
        /* Query the floodCat table for the flood stage data */
        String floodQuerySql = "select lid,fs,fq,wstg,action_Flow from riverstat where lid = '"
                + lid + "'";
        return DirectDbQuery.executeQuery(floodQuerySql, HydroConstants.IHFS,
                QueryLanguage.SQL);
    }

    /**
     * Query the floodcat table for the flood stage.
     * 
     * @param lid
     *            The Location Id
     * @return The flood stage value as a String
     */
    public List<Object[]> getFloodCategories(String lid) throws VizException {
        String floodQuerySql = "select " + HydroConstants.LID + ","
                + HydroConstants.MINOR_STAGE + ","
                + HydroConstants.MODERATE_STAGE + ","
                + HydroConstants.MAJOR_STAGE + "," + HydroConstants.MINOR_FLOW
                + "," + HydroConstants.MODERATE_FLOW + ","
                + HydroConstants.MAJOR_FLOW + " from floodcat where lid = '"
                + lid + "'";
        return DirectDbQuery.executeQuery(floodQuerySql, HydroConstants.IHFS,
                QueryLanguage.SQL);
    }

    /**
     * Get the Station name and river name.
     * 
     * @param lid
     *            Location Id of the station
     * @return List of Object[] containing the data
     * @throws VizException
     */
    public String[] getStnRiverName(String lid) throws VizException {
        StringBuilder sql = new StringBuilder("select ");
        sql.append("name ");
        sql.append("from location ");
        sql.append(" where lid = '" + lid + "'");

        ArrayList<Object[]> result1 = (ArrayList<Object[]>) DirectDbQuery
                .executeQuery(sql.toString(), HydroConstants.IHFS,
                        QueryLanguage.SQL);

        sql.setLength(0);
        sql.append("select stream from riverstat where lid = '" + lid + "'");

        ArrayList<Object[]> result2 = (ArrayList<Object[]>) DirectDbQuery
                .executeQuery(sql.toString(), HydroConstants.IHFS,
                        QueryLanguage.SQL);

        ArrayList<String> sa = new ArrayList<String>();

        if ((result1 != null) && (result1.size() > 0)) {
            sa.add((String) result1.get(0)[0]);
        } else {
            sa.add(HydroConstants.UNDEFINED);
        }

        if ((result2 != null) && (result2.size() > 0)) {
            sa.add((String) result2.get(0)[0]);
        } else {
            sa.add(HydroConstants.UNDEFINED);
        }

        return sa.toArray(new String[sa.size()]);
    }

    /**
     * Gets a unique list of basis times.
     * 
     * @param table
     *            The table to query
     * @param lid
     *            The lid to query on
     * @param pe
     *            The pe to query on
     * @param dur
     *            The duration to query on
     * @param ts
     *            The type source to query on
     * @param ext
     *            The extremum to query on
     * @param begin
     *            The beginning time
     * @param end
     *            The ending time
     * @return List of Object arrays
     * @throws VizException
     * @throws ClassNotFoundException
     */
    public List<Object[]> getUniqueList(String table, String lid, String pe,
            int dur, String ts, String ext, Date begin, Date end)
            throws VizException, ClassNotFoundException {
        StringBuilder sql = new StringBuilder("select ");
        sql.append("distinct(basistime) from " + table + " ");
        sql.append("where lid = '" + lid + "' and ts = '" + ts + "' and ");
        sql.append("dur = '" + dur + "' and extremum = '" + ext + "' and ");
        sql.append("validtime > '" + dateFormat.format(begin) + "' and ");
        sql.append("validtime < '" + dateFormat.format(end) + "' ");
        sql.append("order by basistime desc");

        return DirectDbQuery.executeQuery(sql.toString(), HydroConstants.IHFS,
                QueryLanguage.SQL);
    }

    /**
     * Query for the data records
     * 
     * @param tablename
     *            The table to query
     * @param lid
     *            The lid to query on
     * @param pe
     *            The pe to query on
     * @param ts
     *            The type source to query on
     * @param dur
     *            The duration to query on
     * @param ext
     *            The extremum to query on
     * @param startTime
     *            The start time of the data
     * @param endTime
     *            The end time of the data
     * @param basisTime
     *            The forecast basis time, null if not forecast data
     * @param forecastData
     *            true if forecast data
     * @return ArrayList of TabularData
     * @throws VizException
     * @throws ClassNotFoundException
     */
    public ArrayList<TabularData> getTabularData(String tablename, String lid,
            String pe, String ts, String dur, String ext, Date startTime,
            Date endTime, String basisTime, boolean forecastData)
            throws VizException, ClassNotFoundException {
        StringBuilder sql = new StringBuilder("select ");

        if (forecastData) {
            sql.append("lid, validtime, value, revision, shef_qual_code, ");
            sql.append("quality_code, product_id, producttime, postingtime, ");
            sql.append("basistime, probability ");
            sql.append("from ").append(tablename);
            sql.append(" where lid = '").append(lid).append("'");
            sql.append(" and pe = '").append(pe).append("'");
            sql.append(" and ts = '").append(ts).append("'");
            sql.append(" and dur = ").append(dur);
            sql.append(" and extremum = '").append(ext).append("'");
            sql.append(" and validtime > '")
                    .append(dateFormat.format(startTime)).append("'");
            sql.append(" and validtime < '").append(dateFormat.format(endTime))
                    .append("'");
            sql.append(" and basistime = '").append(basisTime).append("'");
            sql.append(" order by validtime desc");
        } else {
            sql.append("lid, obstime, value, revision, shef_qual_code, ");
            sql.append("quality_code, product_id, producttime, postingtime ");
            sql.append("from ").append(tablename);
            sql.append(" where lid = '").append(lid).append("'");
            sql.append(" and pe = '").append(pe).append("'");
            sql.append(" and ts = '").append(ts).append("'");
            sql.append(" and dur = ").append(dur);
            sql.append(" and extremum = '").append(ext).append("'");
            sql.append(" and obstime > '").append(dateFormat.format(startTime))
                    .append("'");
            sql.append(" and obstime < '").append(dateFormat.format(endTime))
                    .append("'");
            sql.append(" order by obstime desc");
        }

        AppsDefaults ad = AppsDefaults.getInstance();
        boolean debug = ad.getBoolean(HydroConstants.DEBUG_HYDRO_DB_TOKEN,
                false);
        if (debug) {
            System.out.println(ad.getToken(HydroConstants.PGHOST) + ":"
                    + ad.getToken(HydroConstants.PGPORT) + ":"
                    + ad.getToken(HydroConstants.DB_NAME));
            System.out.println("Query: " + sql.toString());
        }

        ArrayList<TabularData> tabularData = new ArrayList<TabularData>();
        ArrayList<Object[]> results = (ArrayList<Object[]>) DirectDbQuery
                .executeQuery(sql.toString(), HydroConstants.IHFS,
                        QueryLanguage.SQL);

        for (int i = 0; i < results.size(); i++) {
            Object[] oa = results.get(i);
            TabularData td = new TabularData();
            td.setLid((String) oa[0]);
            td.setObsTime((Date) oa[1]);
            td.setValue((Double) oa[2]);
            td.setRevision((Integer) oa[3]);
            td.setShefQualCode((String) oa[4]);
            td.setQualityCode((Integer) oa[5]);
            td.setProductId((String) oa[6]);
            td.setProductTime((Date) oa[7]);
            td.setPostingTime((Date) oa[8]);
            if (forecastData) {
                td.setValidTime((Date) oa[9]);
                td.setProbability((Float) oa[10]);
            }
            tabularData.add(td);
        }

        return tabularData;
    }

    /**
     * Get the record count of the table.
     * 
     * @param table
     *            The table to query
     * @param where
     *            The where statement
     * @return Number of rows in the table, or -1 if error
     * @throws VizException
     */
    public long recordCount(String table, String where) throws VizException {
        if ((table == null) || table.equals("")) {
            return -1;
        }

        String sql = "select count(*) from " + table + where;

        AppsDefaults ad = AppsDefaults.getInstance();
        boolean debug = ad.getBoolean(HydroConstants.DEBUG_HYDRO_DB_TOKEN,
                false);

        if (debug) {
            System.out.println(ad.getToken(HydroConstants.PGHOST) + ":"
                    + ad.getToken(HydroConstants.PGPORT) + ":"
                    + ad.getToken(HydroConstants.DB_NAME));
            System.out.println("Query: " + sql);
        }

        List<Object[]> results = DirectDbQuery.executeQuery(sql.toString(),
                HydroConstants.IHFS, QueryLanguage.SQL);

        if ((results == null) || (results.get(0) == null)) {
            return -1;
        }

        return (Long) results.get(0)[0];
    }

    /**
     * Delete the record.
     * 
     * @param tablename
     *            The tablename to query
     * @param where
     *            The where statement
     * @throws VizException
     */
    public void deleteRecord(String tablename, String where)
            throws VizException {
        String sql = "delete from " + tablename + " " + where;

        AppsDefaults ad = AppsDefaults.getInstance();
        boolean debug = ad.getBoolean(HydroConstants.DEBUG_HYDRO_DB_TOKEN,
                false);

        if (debug) {
            System.out.println(ad.getToken(HydroConstants.PGHOST) + ":"
                    + ad.getToken(HydroConstants.PGPORT) + ":"
                    + ad.getToken(HydroConstants.DB_NAME));
            System.out.println("Query: " + sql);
        }
        DirectDbQuery.executeStatement(sql, HydroConstants.IHFS,
                QueryLanguage.SQL);
    }

    /**
     * Delete a list of items.
     * 
     * @param queryList
     *            list of queries
     * @throws VizException
     */
    public void deleteRecords(ArrayList<String> queryList) throws VizException {
        StringBuilder sb = new StringBuilder();
        for (String query : queryList) {
            sb.append(query);
        }

        DirectDbQuery.executeStatement(sb.toString(), HydroConstants.IHFS,
                QueryLanguage.SQL);
    }

    /**
     * Add a data record.
     * 
     * @param tablename
     *            The tablename to query
     * @param dr
     *            The data record to add
     * @return int
     * @throws VizException
     */
    public int addDataRecord(String tablename, DataRecord dr)
            throws VizException {
        StringBuilder sb = new StringBuilder();
        Date now = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();
        if (tablename.startsWith("Fcst") || tablename.startsWith("fcst")
                || tablename.startsWith("Contingency")) {
            sb.append("insert into " + tablename
                    + "(lid, pe, dur, ts, extremum, ");
            sb.append("probability, validtime, basistime, value, quality_code, ");
            sb.append("shef_qual_code, revision, product_id, producttime, postingtime) ");
            sb.append("values ('" + dr.getLid() + "', '"
                    + dr.getPe().toUpperCase() + "', ");
            sb.append(dr.getDur() + ", '" + dr.getTs().toUpperCase() + "', '");
            sb.append(dr.getExt().toUpperCase() + "', -1, ");
            Date d;
            if (dr.getObsTime() instanceof Timestamp) {
                d = timestampToDate((Timestamp) dr.getObsTime());
                sb.append("'" + dateFormat.format(d) + "', '");
            } else {
                sb.append("'" + dateFormat.format(dr.getObsTime()) + "', '");
            }
            sb.append(dr.getBasisTime() + "', " + dr.getValue() + ", ");
            sb.append(dr.getQualityCode() + ", '" + dr.getShefQualCode()
                    + "', ");
            sb.append(dr.getRevision() + ", '" + dr.getProductId() + "', ");
            if (dr.getObsTime() instanceof Timestamp) {
                d = timestampToDate((Timestamp) dr.getProductTime());
                sb.append("'" + dateFormat.format(d) + "', '");
            } else {
                sb.append("'" + dateFormat.format(dr.getProductTime()) + "', '");
            }
            sb.append(dateFormat.format(now) + "')");

        } else {
            sb.append("insert into " + tablename
                    + "(lid, pe, dur, ts, extremum, ");
            sb.append("obstime, postingtime, value, quality_code, shef_qual_code, productTime, ");
            sb.append("revision, product_Id) ");
            sb.append("values ('" + dr.getLid() + "', '"
                    + dr.getPe().toUpperCase() + "', ");
            sb.append(dr.getDur() + ", '" + dr.getTs().toUpperCase() + "', '");
            sb.append(dr.getExt().toUpperCase() + "', ");
            Date d;
            if (dr.getObsTime() instanceof Timestamp) {
                d = timestampToDate((Timestamp) dr.getObsTime());
                sb.append("'" + dateFormat.format(d) + "', '");
            } else {
                sb.append("'" + dateFormat.format(dr.getObsTime()) + "', '");
            }

            sb.append(dateFormat.format(now) + "', ");

            sb.append(dr.getValue() + ", ");
            sb.append(dr.getQualityCode() + ", '");
            sb.append(dr.getShefQualCode() + "', '");
            sb.append(dr.getProductTime() + "', ");
            sb.append("0, '" + dr.getProductId() + "')");
        }

        AppsDefaults ad = AppsDefaults.getInstance();
        boolean debug = ad.getBoolean(HydroConstants.DEBUG_HYDRO_DB_TOKEN,
                false);

        if (debug) {
            System.out.println(ad.getToken(HydroConstants.PGHOST) + ":"
                    + ad.getToken(HydroConstants.PGPORT) + ":"
                    + ad.getToken(HydroConstants.DB_NAME));
            System.out.println("Query: " + sb.toString());
        }

        return DirectDbQuery.executeStatement(sb.toString(),
                HydroConstants.IHFS, QueryLanguage.SQL);
    }

    /**
     * Update sql.
     * 
     * @param sql
     *            The sql string
     * @return int
     * @throws VizException
     */
    public int update(String sql) throws VizException {
        AppsDefaults ad = AppsDefaults.getInstance();
        boolean debug = ad.getBoolean(HydroConstants.DEBUG_HYDRO_DB_TOKEN,
                false);

        if (debug) {
            System.out.println(ad.getToken(HydroConstants.PGHOST) + ":"
                    + ad.getToken(HydroConstants.PGPORT) + ":"
                    + ad.getToken(HydroConstants.DB_NAME));
            System.out.println("Query: " + sql);
        }

        return DirectDbQuery.executeStatement(sql, HydroConstants.IHFS,
                QueryLanguage.SQL);
    }

    /**
     * Insert rejected data record.
     * 
     * @param dr
     *            The data record
     * @return int
     * @throws VizException
     */
    public int insertRejectedData(DataRecord dr) throws VizException {
        Rejecteddata rd = new Rejecteddata();
        RejecteddataId rdid = new RejecteddataId();
        Date d = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();

        /* set basistime to obstime for observed data */
        if (dr.getBasisTime() != null) {
            try {
                rdid.setBasistime(HydroConstants.DATE_FORMAT.parse(dr
                        .getBasisTime()));
            } catch (ParseException e) {
                rdid.setBasistime(dr.getObsTime());
            }
        } else {
            rdid.setBasistime(dr.getObsTime());
        }

        rdid.setDur((short) dr.getDur());
        rdid.setExtremum(dr.getExt());
        rdid.setLid(dr.getLid());
        rdid.setPe(dr.getPe());

        /* set postingtime to current time */
        rdid.setPostingtime(d);

        rdid.setProbability(-1);
        rdid.setTs(dr.getTs());
        rdid.setValidtime(dr.getValidTime());
        rd.setId(rdid);

        rd.setProductId(dr.getProductId());
        rd.setProducttime(dr.getProductTime());
        rd.setQualityCode((int) dr.getQualityCode());

        /* set reject_type to M for Manual */
        rd.setRejectType("M");

        rd.setRevision((short) dr.getRevision());
        rd.setShefQualCode(dr.getShefQualCode());
        rd.setUserid(LocalizationManager.getInstance().getCurrentUser());
        rd.setValue(dr.getValue());

        /* set validtime for observed data */
        if (rdid.getValidtime() == null) {
            rdid.setValidtime(dr.getObsTime());
        }

        return DirectDbQuery.saveOrUpdate(rd, HydroConstants.IHFS);
    }

    /**
     * Insert a list of items into the rejected table
     * 
     * @param recordList
     *            List of DataRecord objects
     * @return
     * @throws VizException
     */
    public int insertRejectedData(ArrayList<DataRecord> recordList)
            throws VizException {
        StringBuilder sb = new StringBuilder();

        Date d = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();
        for (DataRecord dr : recordList) {
            sb.append("insert into rejecteddata(lid, pe, dur, ts, extremum, ");
            sb.append("probability, validtime, basistime, postingtime, value, ");
            sb.append("revision, shef_qual_code, product_id, producttime, quality_code, ");
            sb.append("reject_type, userid) VALUES(");

            sb.append("'" + dr.getLid() + "', ");
            sb.append("'" + dr.getPe() + "', ");
            sb.append(dr.getDur() + ", ");
            sb.append("'" + dr.getTs() + "', ");
            sb.append("'" + dr.getExt() + "', ");
            sb.append(-1 + ", ");

            /* set validtime for observed data */
            if (dr.getValidTime() != null) {
                sb.append("'"
                        + HydroConstants.DATE_FORMAT.format(dr.getValidTime())
                        + "', ");
            } else {
                sb.append("'"
                        + (HydroConstants.DATE_FORMAT.format(dr.getObsTime()))
                        + "', ");
            }

            if (dr.getBasisTime() != null) {
                try {
                    sb.append("'"
                            + (HydroConstants.DATE_FORMAT.parse(dr
                                    .getBasisTime())) + "', ");
                } catch (ParseException e) {
                    sb.append("'"
                            + (HydroConstants.DATE_FORMAT.format(dr
                                    .getObsTime())) + "', ");
                }
            } else {
                sb.append("'"
                        + (HydroConstants.DATE_FORMAT.format(dr.getObsTime()))
                        + "', ");
            }

            sb.append("'" + HydroConstants.DATE_FORMAT.format(d) + "', ");
            sb.append(dr.getValue() + ", ");
            sb.append(dr.getRevision() + ", ");
            sb.append("'" + dr.getShefQualCode() + "', ");
            sb.append("'" + dr.getProductId() + "', ");
            sb.append("'"
                    + HydroConstants.DATE_FORMAT.format(dr.getProductTime())
                    + "', ");
            sb.append(dr.getQualityCode() + ", ");
            sb.append("'M', ");
            sb.append("'" + LocalizationManager.getInstance().getCurrentUser()
                    + "');");
        }

        AppsDefaults ad = AppsDefaults.getInstance();
        boolean debug = ad.getBoolean(HydroConstants.DEBUG_HYDRO_DB_TOKEN,
                false);

        if (debug) {
            System.out.println(ad.getToken(HydroConstants.PGHOST) + ":"
                    + ad.getToken(HydroConstants.PGPORT) + ":"
                    + ad.getToken(HydroConstants.DB_NAME));
            System.out.println("Query: " + sb.toString());
        }

        return DirectDbQuery.executeStatement(sb.toString(),
                HydroConstants.IHFS, QueryLanguage.SQL);
    }

    private Object getDataFromDB(ForecastData dr, String field) {
        StringBuilder sql = new StringBuilder("select " + field + " from ");
        String tablename = DbUtils.getTableName(dr.getPe(), dr.getTs());
        sql.append(tablename + " where ");
        sql.append("lid = '" + dr.getLid() + "' ");
        sql.append("and pe = '" + dr.getPe().toUpperCase() + "' ");
        sql.append("and dur =" + dr.getDur() + " ");
        sql.append("and ts = '" + dr.getTs().toUpperCase() + "' ");
        sql.append("and extremum = '" + dr.getExtremum().toUpperCase() + "' ");
        if (dr.getTs().toUpperCase().startsWith("F")
                || dr.getTs().toUpperCase().startsWith("C")) {
            sql.append("and validtime = '" + dr.getValidTime() + "' ");
            sql.append("and basistime = '" + dr.getBasisTime() + "';");
        } else {// obs data
            sql.append("and obstime = '" + dr.getObsTime() + "';");
        }
        List<Object[]> sqlResult;
        try {
            sqlResult = (DirectDbQuery.executeQuery(sql.toString(),
                    HydroConstants.IHFS, QueryLanguage.SQL));
            if (sqlResult != null && sqlResult.size() > 0
                    && sqlResult.get(0)[0] != null) {
                return sqlResult.get(0)[0];
            }
        } catch (VizException e) {
            e.printStackTrace();
            return null;
        }

        return null;
    }

    public int insertRejectedData(List<ForecastData> deleteList)
            throws VizException {
        StringBuilder sb = new StringBuilder();

        Date currentTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"))
                .getTime();
        for (ForecastData dr : deleteList) {

            int probability = -1;
            int revision = 0;
            if (dr.getTs().toUpperCase().startsWith("F")
                    || dr.getTs().toUpperCase().startsWith("C")) {
                probability = 0;
                revision = 1;
            }

            Date productTime = dr.getProductTime();
            if (productTime == null) {
                productTime = (Date) getDataFromDB(dr, "producttime");
            }

            String productID = (String) getDataFromDB(dr, "product_id");
            if (productID == null) {
                productID = dr.getProductID();
            }

            Integer qualityCode = (Integer) getDataFromDB(dr, "quality_code");
            if (qualityCode == null) {
                qualityCode = new Integer(dr.getQualityCode());
            }

            sb.append("insert into rejecteddata(lid, pe, dur, ts, extremum, ");
            sb.append("probability, validtime, basistime, postingtime, value, ");
            sb.append("revision, shef_qual_code, product_id, producttime, quality_code, ");
            sb.append("reject_type, userid) VALUES(");

            sb.append("'" + dr.getLid() + "', ");
            sb.append("'" + dr.getPe().toUpperCase() + "', ");
            sb.append(dr.getDur() + ", ");
            sb.append("'" + dr.getTs().toUpperCase() + "', ");
            sb.append("'" + dr.getExtremum().toUpperCase() + "', ");
            sb.append(probability + ", ");

            /* set validtime for observed data */
            if (dr.getValidTime() != null) {
                sb.append("'"
                        + HydroConstants.DATE_FORMAT.format(dr.getValidTime())
                        + "', ");
            } else {
                sb.append("'"
                        + (HydroConstants.DATE_FORMAT.format(dr.getObsTime()))
                        + "', ");
            }

            if (dr.getBasisTime() != null) {
                sb.append("'" + (dr.getBasisTime()) + "', ");
            } else {
                sb.append("'"
                        + (HydroConstants.DATE_FORMAT.format(dr.getObsTime()))
                        + "', ");
            }

            sb.append("'" + HydroConstants.DATE_FORMAT.format(currentTime)
                    + "', ");
            sb.append(dr.getValue() + ", ");
            sb.append(revision + ", ");
            sb.append("'M', ");// shef_qual_code always M
            sb.append("'" + productID + "', ");
            sb.append("'" + HydroConstants.DATE_FORMAT.format(productTime)
                    + "', ");
            sb.append(qualityCode + ", ");
            sb.append("'M', ");
            sb.append("'" + LocalizationManager.getInstance().getCurrentUser()
                    + "');");
        }

        AppsDefaults ad = AppsDefaults.getInstance();
        boolean debug = ad.getBoolean(HydroConstants.DEBUG_HYDRO_DB_TOKEN,
                false);

        if (debug) {
            System.out.println(ad.getToken(HydroConstants.PGHOST) + ":"
                    + ad.getToken(HydroConstants.PGPORT) + ":"
                    + ad.getToken(HydroConstants.DB_NAME));
            System.out.println("Query: " + sb.toString());
        }

        return DirectDbQuery.executeStatement(sb.toString(),
                HydroConstants.IHFS, QueryLanguage.SQL);

    }

    /**
     * Deletes a list of Observations.
     * 
     * @param deleteList
     *            List of Observations to delete.
     * @return The number of rows modified
     * @throws VizException
     */
    public int delete(List<ForecastData> deleteList) throws VizException {
        int status = -1;

        for (int i = 0; i < deleteList.size(); i++) {
            ForecastData data = deleteList.get(i);
            String tablename = DbUtils.getTableName(data.getPe(), data.getTs());

            StringBuilder sql = new StringBuilder("delete from ");
            sql.append(tablename + " where ");
            sql.append("lid = '" + data.getLid() + "' ");
            sql.append("and pe = '" + data.getPe().toUpperCase() + "' ");
            sql.append("and dur = " + data.getDur() + " ");
            sql.append("and ts = '" + data.getTs().toUpperCase() + "' ");
            sql.append("and extremum = '" + data.getExtremum().toUpperCase()
                    + "' ");

            if (data.getValidTime() != null) {
                sql.append("and validtime = '"
                        + dbFormat.format(data.getValidTime()) + "'");
            } else {
                sql.append("and obstime = '"
                        + dbFormat.format(data.getObsTime()) + "'");
            }

            AppsDefaults ad = AppsDefaults.getInstance();
            boolean debug = ad.getBoolean(HydroConstants.DEBUG_HYDRO_DB_TOKEN,
                    false);

            if (debug) {
                System.out.println(ad.getToken(HydroConstants.PGHOST) + ":"
                        + ad.getToken(HydroConstants.PGPORT) + ":"
                        + ad.getToken(HydroConstants.DB_NAME));
                System.out.println("Query: " + sql);
            }

            status = DirectDbQuery.executeStatement(sql.toString(),
                    HydroConstants.IHFS, QueryLanguage.SQL);
        }

        return status;
    }

    /**
     * Inserts a list of Observations.
     * 
     * @param insertList
     *            List of Observations to insert.
     * @return The number of rows modified
     * @throws VizException
     */
    public int insert(List<ForecastData> insertList) throws VizException {
        int status = -1;
        Date now = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();

        for (int i = 0; i < insertList.size(); i++) {
            ForecastData data = insertList.get(i);
            String tablename = DbUtils.getTableName(data.getPe(), data.getTs());

            SqlBuilder sql = new SqlBuilder(tablename);
            sql.setSqlType(SqlBuilder.INSERT);
            sql.addString("lid", data.getLid());
            sql.addString("pe", data.getPe().toUpperCase());
            sql.addInt("dur", data.getDur());
            sql.addString("ts", data.getTs().toUpperCase());
            sql.addString("extremum", data.getExtremum().toUpperCase());
            sql.addString("product_id", data.getProductID().toUpperCase());

            if (data.getValidTime() != null) {
                sql.addString("validTime",
                        dateFormat.format(data.getValidTime()));
                sql.addInt("probability", -1);
                sql.addString("basisTime",
                        dateFormat.format(data.getBasisTime()));
                if (data.getProductTime() == null) {
                    sql.addString(
                            "producttime",
                            dateFormat.format(Calendar.getInstance(
                                    TimeZone.getTimeZone("GMT")).getTime()));
                } else {
                    sql.addString("producttime",
                            dateFormat.format(data.getProductTime()));
                }
            } else {
                sql.addString("obstime", dateFormat.format(data.getObsTime()));
                sql.addString("producttime",
                        dateFormat.format(data.getObsTime()));
            }

            sql.addDouble("value", data.getValue());
            sql.addString("shef_qual_code", "M");
            sql.addInt("quality_code", TimeSeriesUtil.DEFAULT_QC_VALUE);
            sql.addInt("revision", 0);
            sql.addString("postingtime", dateFormat.format(now));

            AppsDefaults ad = AppsDefaults.getInstance();
            boolean debug = ad.getBoolean(HydroConstants.DEBUG_HYDRO_DB_TOKEN,
                    false);

            if (debug) {
                System.out.println(ad.getToken(HydroConstants.PGHOST) + ":"
                        + ad.getToken(HydroConstants.PGPORT) + ":"
                        + ad.getToken(HydroConstants.DB_NAME));
                System.out.println("Query: " + sql.toString());
            }

            status = DirectDbQuery.executeStatement(sql.toString(),
                    HydroConstants.IHFS, QueryLanguage.SQL);
        }

        return status;
    }

    /**
     * Edits a list of Observations.
     * 
     * @param editList
     *            List of Observations to edit.
     * @return The number of rows modified
     * @throws VizException
     */
    public int edit(List<ForecastData> editList) throws VizException {
        int status = -1;
        Date now = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();

        for (int i = 0; i < editList.size(); i++) {
            ForecastData data = editList.get(i);
            String tablename = DbUtils.getTableName(data.getPe(), data.getTs());

            SqlBuilder sql = new SqlBuilder(tablename);
            sql.setSqlType(SqlBuilder.UPDATE);
            sql.addDouble("value", data.getValue());
            sql.addString("shef_qual_code", "M");
            sql.addInt("revision", 1);
            sql.addString("postingTime", HydroConstants.DATE_FORMAT.format(now));
            if (data.getProductTime() == null) {
                sql.addString(
                        "producttime",
                        HydroConstants.DATE_FORMAT.format(Calendar.getInstance(
                                TimeZone.getTimeZone("GMT")).getTime()));
            }

            StringBuilder where = new StringBuilder();
            where.append(" where lid = '" + data.getLid().toUpperCase() + "' ");
            where.append("and dur = " + data.getDur() + " ");
            where.append("and ts = '" + data.getTs().toUpperCase() + "' ");
            where.append("and extremum = '" + data.getExtremum().toUpperCase()
                    + "' ");
            if (data.getValidTime() != null) {
                where.append("and validtime = '"
                        + dateFormat.format(data.getValidTime()) + "'");
                where.append(" and basistime = '"
                        + dateFormat.format(data.getBasisTime()) + "'");
            } else {
                where.append("and obstime = '"
                        + dateFormat.format(data.getObsTime()) + "'");
            }

            sql.setWhereClause(where.toString());

            if (data.getValue() == HydroConstants.MISSING_VALUE) {
                DataRecord dr = new DataRecord();
                Date productTime = data.getProductTime();
                if (productTime == null) {
                    productTime = now;
                }

                dr.setDur(data.getDur());
                dr.setExt(data.getExtremum().toUpperCase());
                dr.setLid(data.getLid());
                dr.setObsTime(data.getObsTime());
                dr.setPe(data.getPe().toUpperCase());
                dr.setPostingTime(now);
                dr.setQualityCode(TimeSeriesUtil.DEFAULT_QC_VALUE);
                dr.setRevision(data.getRevision());
                dr.setShefQualCode("M");
                dr.setTs(data.getTs().toUpperCase());
                dr.setValue(data.getPreviousValue());
                dr.setProductTime(productTime);
                dr.setProductId(data.getProductID());
                if (data.getValidTime() != null) {
                    dr.setValidTime(data.getValidTime());
                    dr.setBasisTime(dateFormat.format(data.getBasisTime()));
                } else {
                    dr.setValidTime(data.getObsTime());
                }

                insertRejectedData(dr);
            }

            AppsDefaults ad = AppsDefaults.getInstance();
            boolean debug = ad.getBoolean(HydroConstants.DEBUG_HYDRO_DB_TOKEN,
                    false);

            if (debug) {
                System.out.println(ad.getToken(HydroConstants.PGHOST) + ":"
                        + ad.getToken(HydroConstants.PGPORT) + ":"
                        + ad.getToken(HydroConstants.DB_NAME));
                System.out.println("Query: " + sql);
            }

            DirectDbQuery.executeStatement(sql.toString(), HydroConstants.IHFS,
                    QueryLanguage.SQL);
        }

        return status;
    }

    /**
     * Get the product's productId and productTime.
     * 
     * @param where
     *            The where statement
     * @param table
     *            The table to get the data from
     * @return List of String[] productId, productTime
     * @throws VizException
     */
    public String[] getProductIdTime(final String where, final String table)
            throws VizException {

        StringBuilder sql = new StringBuilder();
        sql.append("select product_id, productTime from " + table);
        sql.append(where);

        AppsDefaults ad = AppsDefaults.getInstance();
        boolean debug = ad.getBoolean(HydroConstants.DEBUG_HYDRO_DB_TOKEN,
                false);

        if (debug) {
            System.out.println(ad.getToken(HydroConstants.PGHOST) + ":"
                    + ad.getToken(HydroConstants.PGPORT) + ":"
                    + ad.getToken(HydroConstants.DB_NAME));
            System.out.println("Query: " + sql);
        }

        List<Object[]> rs = DirectDbQuery.executeQuery(sql.toString(),
                HydroConstants.IHFS, QueryLanguage.SQL);

        String[] sa = null;
        if ((rs != null) && (rs.size() > 0)) {
            Object[] item = rs.get(0);
            sa = new String[2];
            sa[0] = (String) item[0];
            sa[1] = dateFormat.format((Date) item[1]);
        }

        return sa;
    }

    /**
     * Get the list of forecast data.
     * 
     * @param where
     *            The where statement
     * @param table
     *            The table to get the data from
     * @return List of String[] productId, productTime
     * @throws VizException
     */
    public ArrayList<Fcstheight> getForecast(final String where,
            final String table) throws VizException {
        ArrayList<Fcstheight> returnList = new ArrayList<Fcstheight>();

        StringBuilder query = new StringBuilder("select ");
        query.append("lid, pe, dur, ts, extremum, probability, validtime, ");
        query.append("basistime, value, shef_qual_code, quality_code, ");
        query.append("revision, product_id, producttime, postingtime ");
        query.append("from " + table + " " + where);

        AppsDefaults ad = AppsDefaults.getInstance();
        boolean debug = ad.getBoolean(HydroConstants.DEBUG_HYDRO_DB_TOKEN,
                false);

        if (debug) {
            System.out.println(ad.getToken(HydroConstants.PGHOST) + ":"
                    + ad.getToken(HydroConstants.PGPORT) + ":"
                    + ad.getToken(HydroConstants.DB_NAME));
            System.out.println("Query: " + query.toString());
        }

        List<Object[]> results = DirectDbQuery.executeQuery(query.toString(),
                HydroConstants.IHFS, QueryLanguage.SQL);

        if ((results == null) || (results.size() == 0)) {
            return returnList;
        }
        returnList.ensureCapacity(results.size());
        int i = 0;
        for (Object[] item : results) {
            i = 0;
            Fcstheight fh = new Fcstheight();
            FcstheightId fhid = new FcstheightId();
            fhid.setLid((String) item[i++]);
            fhid.setPe((String) item[i++]);
            Integer in = (Integer) item[i++];
            fhid.setDur(in.shortValue());
            fhid.setTs((String) item[i++]);
            fhid.setExtremum((String) item[i++]);
            fhid.setProbability((Float) item[i++]);
            fhid.setValidtime((Date) item[i++]);
            fhid.setBasistime((Date) item[i++]);
            fh.setId(fhid);
            fh.setValue((Double) item[i++]);
            fh.setShefQualCode((String) item[i++]);
            fh.setQualityCode((Integer) item[i++]);
            in = (Integer) item[i++];
            fh.setRevision(in.shortValue());
            fh.setProductId((String) item[i++]);
            fh.setProducttime((Date) item[i++]);
            fh.setPostingtime((Date) item[i++]);

            returnList.add(fh);
        }

        return returnList;
    }

    /**
     * Insert/Update the forecast tables.
     * 
     * @param dataObj
     *            The persistent data object to insert/update
     * @return The number of objects inserted/updated
     * @throws VizException
     */
    public int putForecast(PersistableDataObject dataObj) throws VizException {
        int rv = 0;

        rv = DirectDbQuery.saveOrUpdate(dataObj, HydroConstants.IHFS);

        return rv;
    }

    /**
     * Get the Use Latest Forecast flag from riverstat table.
     * 
     * @param lid
     *            The location id
     * @return The use latestes forecast flag, or null if nothing in db
     */
    public String getUseLatestForecast(String lid) {
        String useLatest = null;

        String query = "select use_latest_fcst from riverstat where lid = '"
                + lid + "'";

        ArrayList<Object[]> rs = runQuery(query);

        if ((rs != null) && (rs.size() > 0)) {
            useLatest = (String) rs.get(0)[0];
        }

        return useLatest;
    }

    /**
     * Convert Timestamp object to java.util.Date object.
     * 
     * @param timestamp
     *            The Timestamp to convert
     * @return The Date object
     */
    public Date timestampToDate(Timestamp timestamp) {
        long milliseconds = timestamp.getTime();// + (timestamp.getNanos() /
                                                // 1000000);
        return new java.util.Date(milliseconds);
    }

    /**
     * Get the station display map.
     * 
     * @return The map of station displays
     */
    public Map<String, String> getStationDisplayMap() {
        return stnDisplayMap;
    }
}
