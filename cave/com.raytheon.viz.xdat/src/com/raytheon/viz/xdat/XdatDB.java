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

package com.raytheon.viz.xdat;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * XdatDB manager. This class acquire all xdat tokens.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  2 Dec 2008             wkwock      Initial creation.
 * 16 Jan 2009  1883       Venable &amp;   Updated database calls and query methods. 
 *                         Duff
 * 10 Feb 2009             wkwock      Added functions and clean up.
 * 12 Aug 2014  3049       bkowal      Close the BufferedReader when finished.
 * 21 May 2015  4501       skorolev    Changed a way of database connection. Got rid of Vector.
 * 17 Sep 2015  4886       skorolev    Corrected updateRejecteddata.
 * 17 Dec 2015  18407      xwei        Fixed: XDAT in Hydro Perspective allows user only to view 4 days instead of 30 days
 * 04 Aug 2016  5800       mduff       Handle date objects correctly, changed return types of query methods, cleanup
 * 
 * </pre>
 * 
 * @author wkwock
 * 
 */
public class XdatDB {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(XdatDB.class);

    public static final String IHFS = "ihfs";

    private Map<String, String> peMap = null;

    /**
     * Constructor
     */
    public XdatDB() {

    }

    /**
     * Run a database query.
     * 
     * @param query
     *            SQL statement.
     * @return List Rows of data.
     */
    public List<String> runQuery(String query) {
        List<String> result = new ArrayList<String>();
        try {
            List<Object[]> rs = DirectDbQuery.executeQuery(query, IHFS,
                    QueryLanguage.SQL);
            if (!rs.isEmpty()) {
                for (Object[] obj : rs) {
                    if (obj[0] != null) {
                        result.add(((String) obj[0]).trim());
                    }
                }
            } else {
                result = null;
            }
        } catch (VizException e) {
            statusHandler.error("Error querying database", e);
        }
        return result;
    }

    /**
     * This query will return a List of String arrays so the data can be
     * accessed when the data strings contain spaces.
     * 
     * @param query
     *            SQL query.
     * @return List (rows) of Object arrays (columns of data).
     */
    private List<Object[]> runDetailedQuery(String query) {
        List<Object[]> rs = Collections.emptyList();

        try {
            rs = DirectDbQuery.executeQuery(query, IHFS, QueryLanguage.SQL);
        } catch (VizException e) {
            statusHandler.error("Error querying database", e);
        }

        return rs;
    }

    /**
     * Get number of observation days
     * 
     * @return Number of observation days.
     */
    public int get_obs_days() {
        int numHours = 0;
        int maxNumHours = 0;
        Set<String> peSet = getPeMap().keySet();

        String query = "select table_name, num_hours_host from purgedyndata";
        List<Object[]> result = runDetailedQuery(query);

        Map<String, Integer> hourMap = new HashMap<>(result.size(), 1.0f);
        for (Object[] oa : result) {
            if (oa.length > 0) {
                hourMap.put(oa[0].toString(), (Integer) oa[1]);
            }
        }

        for (String pe : peSet) {
            String table = peMap.get(pe);
            if (!("_no_table_").equalsIgnoreCase(table)) {
                numHours = hourMap.get(table);
                if (numHours > maxNumHours) {
                    maxNumHours = numHours;
                }
            }
        }

        if (maxNumHours == 0) {
            maxNumHours = 72;
        }

        return maxNumHours / 24 + 1;
    }

    /**
     * Get the list of states from database. originate from
     * xdat_dbase.ec:get_state_lits()
     * 
     * @return A String array of state names.
     */
    public String[] getStateList() {
        String states[];

        List<String> stateList = runQuery("select DISTINCT state from location where post = 1 and state !='XX' order by state");
        states = stateList.toArray(new String[stateList.size()]);

        return states;
    }

    /**
     * Search for IDs
     * 
     * @param stateName
     *            2 Letter state abbreviation.
     * @param pe
     * @return List<String[]> List (rows) of String arrays (columns) of data.
     */
    public List<String[]> searchIds(String stateName, String pe) {

        String queryStr = "select distinct location.lid, des from location, latestobsvalue "
                + "where location.lid = latestobsvalue.lid and location.state = '"
                + stateName
                + "' and latestobsvalue.pe = '"
                + pe
                + "' order by location.lid";

        List<Object[]> rs = runDetailedQuery(queryStr);
        List<String[]> result = new ArrayList<>(rs.size());

        for (Object[] oa : rs) {
            String[] sa = new String[oa.length];
            for (int i = 0; i < oa.length; i++) {
                if (oa[i] != null) {
                    sa[i] = oa[i].toString();
                }
            }
            result.add(sa);
        }

        return result;
    }

    /**
     * Search for location IDs that match the search string.
     * 
     * @param searchStr
     * @param peType
     * @return ID and Destination from location table
     */
    public List<String[]> searchStringList(String searchStr, String peType) {
        String queryStr = "select distinct location.lid, location.des from location, latestobsvalue "
                + "where location.lid = latestobsvalue.lid and (location.lid like '%"
                + searchStr
                + "%' or location.des like '%"
                + searchStr
                + "%') and latestobsvalue.pe = '"
                + peType
                + "' order by location.lid";

        List<Object[]> rs = runDetailedQuery(queryStr);
        List<String[]> result = new ArrayList<>(rs.size());

        for (Object[] oa : rs) {
            String[] sa = new String[oa.length];
            for (int i = 0; i < oa.length; i++) {
                if (oa[i] != null) {
                    sa[i] = oa[i].toString();
                }
            }
            result.add(sa);
        }

        return result;
    }

    /**
     * Get the above flood stage search data.
     * 
     * @param dateStr
     *            Date in a String format.
     * @return List<String[]> List (rows) of String arrays (columns) of data.
     */
    public List<String[]> getAboveFsSearch(String dateStr) {
        SimpleDateFormat dateFmt = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss",
                Locale.US);
        dateFmt.setTimeZone(TimeUtil.GMT_TIME_ZONE);

        String query = "select height.lid, height.pe, height.dur, height.ts, height.extremum, "
                + "height.obstime, height.value, height.product_id, riverstat.fs "
                + "from height, riverstat where height.lid =riverstat.lid and "
                + "riverstat.fs > 0.00 and height.pe in ('HG','HT') and height.obstime "
                + ">= '"
                + dateStr
                + "' and height.value >= riverstat.fs order by height.lid, "
                + "height.obstime desc";

        List<Object[]> rs = runDetailedQuery(query);
        List<String[]> result = new ArrayList<>(rs.size());

        for (Object[] oa : rs) {
            String[] sa = new String[oa.length];
            for (int i = 0; i < oa.length; i++) {
                if (i == 5) {
                    sa[i] = dateFmt.format(oa[i]);
                } else {
                    sa[i] = oa[i].toString();
                }
            }
            result.add(sa);
        }

        return result;
    }

    /**
     * Get the unknown sites.
     * 
     * @param unknownTable
     *            Unknown table name.
     * @return List<String[]> List (rows) of String arrays (columns) of data.
     */
    public List<String[]> getUnknownSites(String unknownTable) {
        SimpleDateFormat dateFmt = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss",
                Locale.US);
        dateFmt.setTimeZone(TimeUtil.GMT_TIME_ZONE);

        String query = "select lid, product_id, producttime from "
                + unknownTable + " order by lid, producttime desc";

        List<Object[]> rs = runDetailedQuery(query);
        List<String[]> result = new ArrayList<>(rs.size());

        for (Object[] oa : rs) {
            String[] sa = new String[oa.length];
            for (int i = 0; i < oa.length; i++) {
                if (i == 2) {
                    sa[i] = dateFmt.format(oa[i]);
                } else {
                    sa[i] = oa[i].toString();
                }
            }
            result.add(sa);
        }

        return result;
    }

    /**
     * get rejected data
     * 
     * @param peStr
     *            Physical element string.
     * @param startDateStr
     *            Starting date string.
     * @param endDateStr
     *            Ending date string.
     * @return List<String[]> List (rows) of String arrays (columns) of data.
     */
    public List<String[]> getRejectedData(String peStr, String startDateStr,
            String endDateStr) {

        String query = "select lid, pe, dur, ts, extremum, validtime, postingtime, value "
                + "from rejecteddata where pe = '"
                + peStr
                + "' and validtime between '"
                + startDateStr
                + " 00:00:00' and '"
                + endDateStr
                + " 23:59:59' order by lid, validtime desc";

        SimpleDateFormat dateFmt = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss",
                Locale.US);
        dateFmt.setTimeZone(TimeUtil.GMT_TIME_ZONE);

        List<Object[]> rs = runDetailedQuery(query);
        List<String[]> result = new ArrayList<>(rs.size());

        for (Object[] oa : rs) {
            String[] sa = new String[oa.length];
            for (int i = 0; i < oa.length; i++) {
                if (i == 5 || i == 6) {
                    sa[i] = dateFmt.format(oa[i]);
                } else {
                    sa[i] = oa[i].toString();
                }
            }
            result.add(sa);
        }

        return result;
    }

    /**
     * Get the sites turned off.
     * 
     * @return List<String[]> List (rows) of String arrays (columns) of data.
     */
    public List<String[]> getSitesTurnedOffData() {

        String query = "select lid, county, state, des from location where post = 0 order by state, lid";

        List<Object[]> rs = runDetailedQuery(query);
        List<String[]> result = new ArrayList<>(rs.size());

        for (Object[] oa : rs) {
            String[] sa = new String[oa.length];
            for (int i = 0; i < oa.length; i++) {
                if (oa[i] != null) {
                    sa[i] = oa[i].toString();
                }
            }
            result.add(sa);
        }

        return result;
    }

    /**
     * Get the search data.
     * 
     * @param lidStr
     *            Location ID.
     * @param peStr
     *            Physical element.
     * @param startDateStr
     *            Starting date.
     * @param endDateStr
     *            Ending date.
     * @return List<String[]> List (rows) of String arrays (columns) of data.
     */
    public String[] getSearchData(String lidStr, String peStr,
            String startDateStr, String endDateStr) {
        String searchDataBuf[] = null;

        String query = "select dur, ts, extremum, obstime, value, product_id from height where lid='"
                + lidStr
                + "' and obstime between '"
                + startDateStr
                + " 00:00:00' and '"
                + endDateStr
                + " 23:59:59' order by obstime desc";

        List<String> searchDataVector = runQuery(query);

        if (searchDataVector != null) {
            searchDataBuf = new String[searchDataVector.size()];

            for (int i = 0; i < searchDataVector.size(); i++) {
                searchDataBuf[i] = lidStr + "    " + peStr + " "
                        + searchDataVector.get(i);
            }
        }
        return searchDataBuf;
    }

    /**
     * Get the location destination.
     * 
     * @param ID
     *            The ID.
     * @return The location destination.
     */
    public String getLocationDes(String ID) {

        String locationDes = "";
        String query = "select des from location where lid = '" + ID + "'";
        List<String> result = runQuery(query);

        if (result != null && !result.isEmpty()) {
            locationDes = result.get(0);
        }

        return locationDes;
    }

    /**
     * Gets the group data from IHFS.
     * 
     * @param input
     *            Record List
     * @param startDate
     *            Starting Date
     * @param endDate
     *            Ending Date
     * @return List<String[]> List (rows) of String arrays (columns) of data.
     */
    public List<String[]> getGroupData(String[] input, String startDate,
            String endDate) {
        SimpleDateFormat dateFmt = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss",
                Locale.US);
        dateFmt.setTimeZone(TimeUtil.GMT_TIME_ZONE);

        List<String[]> result = new ArrayList<String[]>();
        String[] data = null;

        String lid = input[0];
        String pe = input[1];

        String sql = "select des from location where lid = '" + lid + "'";

        List<String> rs = runQuery(sql.toString());

        data = new String[2];
        data[0] = lid;
        if ((rs != null) && (!rs.isEmpty())) {
            data[1] = rs.get(0); // only one row returned
            if (data[1] == null) {
                data[1] = "";
            }
        } else {
            data[1] = "";
        }
        /* Add row to return List */
        result.add(data);

        // Determine the table to query
        String table = getPeMap().get(pe);

        if (table == null) {
            table = getPeMap().get(pe.substring(0, 1));

            if (table == null) {
                return null;
            }
        }

        // Get the second half of the data
        sql = "select dur, ts, extremum, obstime, product_id, value from "
                + table + " where lid = '" + lid + "' and pe = '" + pe
                + "' and obstime between '" + startDate + " 00:00:00' "
                + " and '" + endDate + " 23:59:59' order by obstime desc";

        List<Object[]> rs2 = runDetailedQuery(sql);

        double nextValue = 0.0;
        double value = 0.0;

        /* Need to calculate the change */
        if ((rs2 != null) && (!rs2.isEmpty())) {
            for (int j = 0; j < rs2.size(); j++) {
                List<String> list = new ArrayList<String>();
                list.add(lid);
                list.add(pe);
                nextValue = HydroConstants.MISSING_VALUE;

                for (int k = 0; k < rs2.get(j).length; k++) { // Each row
                    Object[] row = rs2.get(j);
                    if (k == row.length - 1) {
                        try {
                            value = (double) row[k];
                            if (j < (rs2.size() - 1)) {
                                Object[] nextRow = rs2.get(j + 1);
                                nextValue = (double) nextRow[k];
                            }
                        } catch (NumberFormatException nfe) {
                            value = HydroConstants.MISSING_VALUE;
                        }

                        /* calculate the change */
                        if (value == HydroConstants.MISSING_VALUE) {
                            list.add("");
                            list.add("");
                        } else {
                            list.add(String.format("%6.2f", value));
                            if (nextValue != HydroConstants.MISSING_VALUE) {
                                list.add(String.format("%7.2f", value
                                        - nextValue));
                            } else {
                                list.add("");
                            }
                        }
                    } else if (k == row.length - 2) {
                        if (row[k] == null) {
                            list.add("");
                        } else {
                            list.add(row[k].toString());
                        }
                    } else if (k == row.length - 3) {
                        list.add(dateFmt.format(row[k]));
                    } else {
                        list.add(row[k].toString());
                    }
                }
                result.add(list.toArray(new String[list.size()]));
            }
        }

        return result;
    }

    /**
     * Get the list data
     * 
     * @param id
     *            The ID.
     * @param peStr
     *            Physical element.
     * @param startDate
     *            Start date.
     * @param endDate
     *            End date.
     * @return List<String[]> List (rows) of String arrays (columns) of data.
     */
    public List<String[]> getListData(String id, String peStr,
            String startDate, String endDate) {

        String dbTable = getPeMap().get(peStr);

        if (dbTable == null) {
            dbTable = getPeMap().get(peStr.substring(0, 1));
            if (dbTable == null) {
                return null;
            }
        }

        String query = "select product_id, dur, ts, extremum, obstime, value from "
                + dbTable
                + " where lid = '"
                + id
                + "' and pe = '"
                + peStr
                + "' and obstime between '"
                + startDate
                + " 00:00:00' and '"
                + endDate + " 23:59:59' order by obstime desc";

        SimpleDateFormat dateFmt = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss",
                Locale.US);
        dateFmt.setTimeZone(TimeUtil.GMT_TIME_ZONE);

        List<Object[]> rs = runDetailedQuery(query);
        List<String[]> result = new ArrayList<>(rs.size());

        for (Object[] oa : rs) {
            String[] sa = new String[oa.length];
            for (int i = 0; i < oa.length; i++) {
                if (i == 4) {
                    sa[i] = dateFmt.format(oa[i]);
                } else {
                    sa[i] = oa[i].toString();
                }
            }
            result.add(sa);
        }

        return result;
    }

    /**
     * Get the SHEF parameter code.
     * 
     * @param id
     *            The ID.
     * @param pe
     *            Physical Element.
     * @return List<String> List The SHEF parameter code.
     */
    public List<String> getShefParamCode(String id, String pe) {

        String query = "select ingestfilter.ts, ingestfilter.extremum, shefdur.durcode, shefdur.name "
                + "from ingestfilter, shefdur where  ingestfilter.lid = '"
                + id
                + "' and ingestfilter.pe = '"
                + pe
                + "' and ingestfilter.ts like 'R%' and ingestfilter.dur = shefdur.dur";

        List<String> result = new ArrayList<String>();

        List<Object[]> rs = runDetailedQuery(query);
        if (!rs.isEmpty()) {
            Iterator<Object[]> shefIterator = rs.iterator();
            while (shefIterator.hasNext()) {
                Object[] row = shefIterator.next();
                String strBuf = pe + row[2].toString() + row[0].toString()
                        + row[1].toString() + " (" + row[3].toString() + ")";
                result.add(strBuf);
            }
        } else {
            return null;
        }

        return result;
    }

    /**
     * Get the coop precipitation data originated.
     * 
     * @return Vector<String> Array of coop precip data.
     */
    public List<String[]> getCoopPrecipData(String startDate) {

        String query = "select lid, dur, ts, extremum, obstime, product_id, value "
                + "from rawpp where (pe = 'PP' and obstime between '"
                + startDate
                + " 10:00:00' and"
                + " '"
                + startDate
                + " 14:00:00' and value >= 0.00 and (dur = 2001 or dur = 5004)) "
                + "order by value desc,lid";

        SimpleDateFormat dateFmt = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss",
                Locale.US);
        dateFmt.setTimeZone(TimeUtil.GMT_TIME_ZONE);

        List<Object[]> rs = runDetailedQuery(query);
        List<String[]> result = new ArrayList<>(rs.size());

        for (Object[] oa : rs) {
            String[] sa = new String[oa.length];
            for (int i = 0; i < oa.length; i++) {
                if (i == 4) {
                    sa[i] = dateFmt.format(oa[i]);
                } else {
                    sa[i] = oa[i].toString();
                }
            }
            result.add(sa);
        }

        return result;
    }

    /**
     * Get the physical element map containing data from the PE map file. .
     * 
     * @return Map of physical elements and tables.
     */
    public Map<String, String> getPeMap() {
        if (peMap == null) {
            peMap = new HashMap<String, String>();
            String line = null;

            String xdatDir = AppsDefaults.getInstance().getToken("xdat_params");
            File file = new File(xdatDir + File.separator + "pe_map");
            if (file.exists()) {
                try (BufferedReader br = new BufferedReader(
                        new FileReader(file));) {
                    while (null != (line = br.readLine())) {
                        if (line.trim().isEmpty()) {
                            continue;
                        }
                        String[] table = line.split("\\s+", 2);
                        peMap.put(table[0], table[1]);
                    }
                } catch (IOException ioe) {
                    statusHandler.error("Error reading pe_map file.  "
                            + "Check for file in " + file.getParent());
                }
            } else {
                statusHandler.error("pe_map file not found.  "
                        + "Check for file in " + file.getParent());
            }
        }

        return peMap;
    }

    /**
     * Get precipitation ID and value
     * 
     * @param obsDate
     * @return HashMap<String, Double> of ID and value from rawpc table.
     */
    public Map<String, Double> getPrecipLidAndValue(String obsDate) {
        String query = "select lid, value from rawpc where (pe = 'PC' and obstime = '"
                + obsDate + "' and value >= 0.00)";
        Map<String, Double> results = new HashMap<>();
        List<Object[]> rs = runDetailedQuery(query);
        if (!rs.isEmpty()) {
            // Get the number of columns in the row of data.
            results = new HashMap<String, Double>(rs.size(), 1.0f);
            for (Object[] oa : rs) {
                results.put(oa[0].toString(), (Double) oa[1]);
            }
        } else {
            return new HashMap<String, Double>(0);
        }

        return results;
    }

    /**
     * Update the rejected data table.
     * 
     * @param lid
     * @param pe
     * @param dur
     * @param ts
     * @param extremum
     * @param value
     * @param validtime
     * @param new_value
     * @param user_id
     */
    public void updateRejecteddata(String lid, String pe, int dur, String ts,
            String extremum, double value, String validtime, String new_value,
            String user_id) {
        double probability = -999.0;
        String basistime = validtime;
        int revision = 0;
        String product_id = "xxxxxxx";
        String producttime = validtime;
        String shef_qual_code = "M";
        String rejected_type = "D";
        if (new_value.compareTo("M") != 0) {
            rejected_type = "u";
        }
        String userid = user_id + " (Edited via xdat)";
        final int QC_MANUAL_FAILED = 123;
        Long quality_code = new Long(0);

        set_qccode(QC_MANUAL_FAILED, quality_code);
        // set_qccode should be defined in some other package

        Calendar postTime = TimeUtil.newGmtCalendar();
        SimpleDateFormat dateFmt = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss",
                Locale.US);
        dateFmt.setTimeZone(TimeUtil.GMT_TIME_ZONE);
        String postingtime = dateFmt.format(postTime.getTime());

        String query = "INSERT INTO rejecteddata VALUES (" + "'" + lid + "', '"
                + pe + "', " + dur + ", '" + ts + "', '" + extremum + "', "
                + probability + ", '" + validtime + "', '" + basistime + "', '"
                + postingtime + "', " + value + ", " + revision + ", '"
                + shef_qual_code + "', '" + product_id + "', '" + producttime
                + "', " + quality_code + ", '" + rejected_type + "', '"
                + userid + "')";

        try {
            DirectDbQuery.executeStatement(query, IHFS, QueryLanguage.SQL);
        } catch (VizException e) {
            statusHandler.error("Error to update the rejected data table.", e);
        }
    }

    /**
     * Get durcode from shefdur table
     * 
     * @param dur
     * @return durcode from shefdur table
     */
    public String getDurcode(int dur) {
        String query = "select durcode from shefdur where dur=" + dur;
        List<String> rs = runQuery(query);
        if (rs != null) {
            return rs.get(0);
        }

        return null;
    }

    /**
     * Sets the quality control code. Original code
     * /fs/hseb/ob90/ohd/whfs_lib/src/DBMSUtil/TEXT/QualityCode.c. This code
     * conversion is just enough for xdat. Future should use the class from
     * DBMSUtil.
     * 
     * @param bit_grp_descr
     * @param quality_code
     * @return quality_code
     */
    public void set_qccode(int bit_grp_descr, Long quality_code) {
        final int MANUAL_QC = 22;
        final int CERTAINTY_QC = 30;
        final int QC_MANUAL_FAILED = 123;

        switch (bit_grp_descr) {
        case QC_MANUAL_FAILED:

            /*
             * Set the quality control code so that the specific bit for manual
             * check indicates it failed, and therefore set the summary
             * certainty bit to indicate the failure.
             */
            set_qcbit(MANUAL_QC, 0, quality_code);
            set_qcbit(CERTAINTY_QC, 0, quality_code);
        }
    }

    /**
     * Sets the specified bit of the quality code to the specified value.
     * 
     * @param bit_position
     * @param setting
     * @param quality_code
     * @return status, quality_code
     */
    public int set_qcbit(int bit_position, int setting, Long quality_code) {
        final int SIGN_QC = 31;
        final int VALID_QC_REQUEST = 1;
        final int ALL_ONES = 2147483647;
        final int INVALID_QC_REQUEST = -1;

        /*
         * Variable used to set a specific bit in bit string to 1; initialized
         * as 0000000000000000 0000000000000001
         */

        int mask = 1;
        int bitwise_inclusive_OR_result;
        int bitwise_AND_result;
        int status;

        /* Ensure that a valid bit position is requested. */

        if (bit_position < SIGN_QC) {
            status = VALID_QC_REQUEST;

            /* if setting the bit ON */
            if (setting == 1) {
                /*
                 * The mask is employed to set a specific bit to the value of 1
                 * in a 32-bit string while hiding the value of the other bits.
                 * The mask is leftwardly shifted to the bit position of the bit
                 * being referenced.
                 */

                mask = mask << bit_position;

                /*
                 * The bitwise inclusive OR operation is used to set the
                 * specified bit. Upon completion, the bit is written to
                 * quality_code memory location.
                 */

                bitwise_inclusive_OR_result = (int) (quality_code.longValue() | mask);

                quality_code = new Long(bitwise_inclusive_OR_result);
            }

            /*
             * if setting the bit OFF. first build a mask that has all ones
             * except for the bit in question, then AND the mask with the
             * existing value to turn off the single bit.
             */

            else {
                mask = ALL_ONES ^ (mask << bit_position);

                bitwise_AND_result = (int) (quality_code.longValue() & mask);

                quality_code = new Long(bitwise_AND_result);
            }
        } else {
            status = INVALID_QC_REQUEST;
        }

        return (status);
    }
}
