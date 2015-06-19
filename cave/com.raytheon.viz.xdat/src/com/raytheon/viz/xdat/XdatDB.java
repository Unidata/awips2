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
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
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
 * 2 Dec 2008              wkwock      Initial creation.
 * 16 Jan 2009  1883       Venable &amp;   Updated database calls and query methods. 
 *                         Duff
 * 10 Feb 2009             wkwock      Added functions and clean up.
 * 12 Aug 2014  3049       bkowal      Close the BufferedReader when finished.
 * 21 May 2015  4501       skorolev    Changed a way of database connection. Got rid of Vector.
 * 
 * </pre>
 * 
 * @author wkwock
 * @version 1.0
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
            statusHandler
                    .handle(Priority.PROBLEM, "Error querying database", e);
        }
        return result;
    }

    /**
     * This query will return a Vector of String arrays so the data can be
     * accessed when the data strings contain spaces.
     * 
     * @param query
     *            SQL query.
     * @return List (rows) of String arrays (columns of data).
     */
    public List<String[]> runDetailedQuery(String query) {
        List<String[]> result = null;

        try {
            List<Object[]> rs = DirectDbQuery.executeQuery(query, IHFS,
                    QueryLanguage.SQL);
            if (!rs.isEmpty()) {
                if (result == null) {
                    result = new ArrayList<String[]>();
                }
                Iterator<Object[]> rsiterator = rs.iterator();
                while (rsiterator.hasNext()) {
                    Object[] row = rsiterator.next();
                    int numberOfCols = row.length;
                    String[] data = new String[numberOfCols];
                    for (int i = 0; i < numberOfCols; i++) {
                        if (row[i] != null) {
                            data[i] = row[i].toString();
                        }

                    }
                    result.add(data);
                }
            }
        } catch (VizException e) {
            statusHandler
                    .handle(Priority.PROBLEM, "Error querying database", e);
            return null;
        }
        return result;
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
        Iterator<String> iterator = peSet.iterator();
        while (iterator.hasNext()) {
            String table = peMap.get(iterator.next());
            if (table.compareTo("_no_table_") != 0) {
                String queryStr = "SELECT num_hours_host from purgedyndata where table_name = '"
                        + table + "'";
                List<Integer> hours;
                try {
                    List<Object[]> rs = DirectDbQuery.executeQuery(queryStr,
                            IHFS, QueryLanguage.SQL);
                    hours = new ArrayList<Integer>(rs.size());
                    for (Object[] obj : rs) {
                        hours.add((Integer) obj[0]);
                    }
                    if (hours != null && hours.size() > 1) {
                        numHours = hours.get(0);
                    }
                    if (numHours > maxNumHours) {
                        maxNumHours = numHours;
                    }
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error to get number of observation days", e);
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

        List<String[]> idVector = runDetailedQuery(queryStr);

        return idVector;
    }

    /**
     * Search for location IDs that match the search string.
     * 
     * @param searchStr
     * @param peType
     * @return ID and Destination from location table
     */
    public List<String[]> search_string_list(String searchStr, String peType) {
        String queryStr = "select distinct location.lid, location.des from location, latestobsvalue "
                + "where location.lid = latestobsvalue.lid and (location.lid like '%"
                + searchStr
                + "%' or location.des like '%"
                + searchStr
                + "%') and latestobsvalue.pe = '"
                + peType
                + "' order by location.lid";

        List<String[]> idVector = runDetailedQuery(queryStr);

        return idVector;
    }

    /**
     * Get the above flood stage search data.
     * 
     * @param dateStr
     *            Date in a String format.
     * @return List<String[]> List (rows) of String arrays (columns) of data.
     */
    public List<String[]> getAboveFsSearch(String dateStr) {

        String query = "select height.lid, height.pe, height.dur, height.ts, height.extremum, "
                + "height.obstime, height.value, height.product_id, riverstat.fs "
                + "from height, riverstat where height.lid =riverstat.lid and "
                + "riverstat.fs > 0.00 and height.pe in ('HG','HT') and height.obstime "
                + ">= '"
                + dateStr
                + "' and height.value >= riverstat.fs order by height.lid, "
                + "height.obstime desc";

        List<String[]> aboveFsVector = runDetailedQuery(query);

        return aboveFsVector;
    }

    /**
     * Get the unknown sites.
     * 
     * @param unknownTable
     *            Unknown table name.
     * @return List<String[]> List (rows) of String arrays (columns) of data.
     */
    public List<String[]> getUnknownSites(String unknownTable) {

        String query = "select lid, product_id, producttime from "
                + unknownTable + " order by lid, producttime desc";
        System.out.println("Query = [" + query + "]");
        List<String[]> unknownSitesVector = runDetailedQuery(query);

        return unknownSitesVector;
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

        List<String[]> rejectedDataVector = runDetailedQuery(query);

        return rejectedDataVector;
    }

    /**
     * Get the sites turned off.
     * 
     * @return List<String[]> List (rows) of String arrays (columns) of data.
     */
    public List<String[]> getSitesTurnedOffData() {

        String query = "select lid, county, state, des from location where post = 0 order by state, lid";

        List<String[]> sitesTurnedOffVector = runDetailedQuery(query);

        return sitesTurnedOffVector;

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
        List<String> searchDataVector = runQuery(query);

        if (searchDataVector != null) {

            if (searchDataVector.size() > 0) {
                locationDes = searchDataVector.get(0);
            }
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

        ArrayList<String[]> returnVec = new ArrayList<String[]>();
        String lid = null;
        String pe = null;
        StringBuffer sql = new StringBuffer();
        String[] data = null;

        String[] sa = input;
        lid = sa[0];
        pe = sa[1];

        sql.append("select des from location where lid = '" + lid + "'");

        List<String[]> rs = runDetailedQuery(sql.toString());
        sql.setLength(0);

        data = new String[2];
        data[0] = lid;
        if ((rs != null) && (rs.size() > 0)) {
            data[1] = rs.get(0)[0]; // only one row returned
            if (data[1] == null) {
                data[1] = "";
            }
        } else {
            data[1] = "";
        }
        /* Add row to return List */
        returnVec.add(data);

        // Determine the table to query
        String table = getPeMap().get(pe);

        if (table == null) {
            table = getPeMap().get(pe.substring(0, 1));

            if (table == null) {
                return null;
            }
        }

        // Get the second half of the data
        sql.append("select dur, ts, extremum, obstime, product_id, value ");
        sql.append("from " + table + " where lid = '" + lid + "' and ");
        sql.append("pe = '" + pe + "' and ");
        sql.append("obstime between '" + startDate + " 00:00:00' ");
        sql.append(" and '" + endDate + " 23:59:59' order by obstime desc");

        rs = runDetailedQuery(sql.toString());

        sql.setLength(0);
        double nextValue = 0.0;
        double value = 0.0;

        /* Need to calculate the change */
        if ((rs != null) && (rs.size() > 0)) {
            for (int j = 0; j < rs.size(); j++) { // Vector
                List<String> list = new ArrayList<String>();
                list.add(lid);
                list.add(pe);
                nextValue = HydroConstants.MISSING_VALUE;

                for (int k = 0; k < rs.get(j).length; k++) { // Each row
                    String[] row = rs.get(j);
                    if (k == row.length - 1) {
                        try {
                            value = Double.parseDouble(row[k]);
                            if (j < (rs.size() - 1)) {
                                String[] nextRow = rs.get(j + 1);
                                nextValue = Double.parseDouble(nextRow[k]);
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
                            list.add(row[k]);
                        }
                    } else {
                        list.add(row[k]);
                    }
                }
                returnVec.add(list.toArray(new String[list.size()]));
            }
        }

        return returnVec;
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

        List<String[]> listDataVector = runDetailedQuery(query);

        return listDataVector;
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

        try {
            List<Object[]> rs = DirectDbQuery.executeQuery(query, IHFS,
                    QueryLanguage.SQL);
            if (!rs.isEmpty()) {
                Iterator<Object[]> shefIterator = rs.iterator();
                while (shefIterator.hasNext()) {
                    Object[] row = shefIterator.next();
                    String strBuf = pe + row[2].toString() + row[0].toString()
                            + row[1].toString() + " (" + row[3].toString()
                            + ")";
                    result.add(strBuf);
                }
            } else {
                return null;
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error to get the SHEF parameter code.", e);
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

        List<String[]> precipDataVector = runDetailedQuery(query);

        return precipDataVector;
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
                        if (line.trim().equals("")) {
                            continue;
                        }
                        String[] table = line.split("\\s+", 2);
                        peMap.put(table[0], table[1]);
                    }
                } catch (IOException ioe) {
                    ioe.printStackTrace();
                }
            } else {
                statusHandler.handle(Priority.ERROR, "pe_map file not found.  "
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
    public HashMap<String, Double> getPrecipLidAndValue(String obsDate) {
        String query = "select lid, value from rawpc where (pe = 'PC' and obstime = '"
                + obsDate + "' and value >= 0.00)";
        HashMap<String, Double> results = new HashMap<String, Double>();
        try {
            List<Object[]> rs = DirectDbQuery.executeQuery(query, IHFS,
                    QueryLanguage.SQL);
            if (!rs.isEmpty()) {
                // Get the number of columns in the row of data.
                int numberOfCols = rs.get(0).length;
                results = new HashMap<String, Double>(numberOfCols);
            } else {
                return null;
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error to get precipitation ID and value.", e);
        }
        return results;
    }

    /**
     * Get precipitation value
     * 
     * @param obsDate
     * @return List<String[]> Array of values from rawpc table.
     */
    public List<String[]> getPrecipValues(String obstime, String lid,
            double value) {
        String query = "select value from rawpc where (lid = '" + lid
                + "' and pe = 'PC' and obstime = '" + obstime
                + "' and value <= " + value + ")";

        List<String[]> precipValueVector = runDetailedQuery(query);

        return precipValueVector;
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

        GregorianCalendar postTime = new GregorianCalendar();
        SimpleDateFormat dateFmt = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss",
                Locale.US);
        String postingtime = dateFmt.format(postTime.getTime());

        String query = "INSERT INTO rejecteddata VALUES (" + "'" + lid + "', '"
                + pe + "', " + dur + ", '" + ts + "', '" + extremum + "', "
                + probability + ", '" + validtime + "', '" + basistime + "', '"
                + postingtime + "', " + value + ", " + revision + ", '"
                + shef_qual_code + "', '" + product_id + "', '" + producttime
                + "', " + quality_code + ", '" + rejected_type + "', '"
                + userid + "')";

        try {
            DirectDbQuery.executeQuery(query, IHFS, QueryLanguage.SQL);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error to update the rejected data table.", e);
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
        List<String[]> durcodeVector = runDetailedQuery(query);
        if (durcodeVector != null) {
            return durcodeVector.get(0)[0];
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
