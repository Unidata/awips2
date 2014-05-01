/* This software was developed and / or modified by Raytheon Company,
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
package ohd.hseb.sshp;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import ohd.hseb.db.Database;
import ohd.hseb.db.DbTable;
import ohd.hseb.ihfsdb.generated.FcstDischargeTable;
import ohd.hseb.ihfsdb.generated.FcstHeightRecord;
import ohd.hseb.ihfsdb.generated.FcstHeightTable;
import ohd.hseb.ihfsdb.generated.IngestFilterTable;
import ohd.hseb.ihfsdb.generated.RiverStatusRecord;
import ohd.hseb.ihfsdb.generated.RiverStatusTable;
import ohd.hseb.ihfsdb.generated.RiverstatTable;
import ohd.hseb.ihfsdb.generated.RpfParamsTable;
import ohd.hseb.util.FileLogger;
import ohd.hseb.util.Logger;

/**
 * Provides functions to load the max fcst info into the RiverStatus table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 13, 2011  8079       jnjanga
 * 
 * </pre>
 * 
 * @see com.raytheon.edex.plugin.shef.database.PostShef
 * @author jnjanga
 * @version 1.0
 */

public class ForecastPoster {

    private long basishrs = 72;

    private long fcsthrs = 72;

    private long obshrs = 72;

    private String basis_hours_str = null;

    private boolean isHoursLoad = false;

    private FileLogger logger = null;

    private Database db = null;

    public static final String MISSING = "-9999";

    public static final int MISSING_VALUE = -9999;

    public static final int QUESTIONABLE_BAD_THRESHOLD = 1073741824;

    /**
     * Constructor
     * 
     * @param db
     * @param logger
     */
    public ForecastPoster(Database db, Logger logger) {
        this.logger = (FileLogger) logger;
        this.db = db;
    }

    /**
     * Loads the max fcst info into the RiverStatus table for the current
     * location and pe.
     * 
     * @param db
     *            The database
     * 
     * @param lid
     *            The location id
     * @param pe
     *            The physical element
     * @param ts
     *            The type source
     * @throws SQLException
     */
    public void loadMaxFcstItem(String lid, String pe, String ts)
            throws SQLException {
        boolean useLatest = false;
        long basisBeginTime = 0;
        ArrayList<Forecast> fcstList = null;
        Forecast maxFcstfcst = null;
        int qcFilter = 1;

        /*
         * get the setting for the use_latest_fcst field for the current
         * location from the riverstat table.
         */

        String useLatestFcst = getUseLatestForecast(lid);

        if (useLatestFcst == null) {
            useLatest = true;
        } else {
            if (useLatestFcst.equalsIgnoreCase("F")) {
                useLatest = false;
            } else {
                useLatest = true;
            }
        }

        /*
         * get the forecast time series for this location, pe, and ts using any
         * instructions on any type-source to screen and whether to use only the
         * latest basis time
         */
        basisBeginTime = getBasisTime();

        fcstList = (ArrayList<Forecast>) buildTsFcstRiv(lid, pe, ts, qcFilter,
                useLatest, basisBeginTime);

        /*
         * find the data for this location, pe, and ts given the forecast
         * time-series and the count of values in it. if data found, determine
         * its max and load the value
         */
        if ((fcstList != null) && (fcstList.size() > 0)) {
            maxFcstfcst = findMaxFcst(fcstList);
            boolean updateFlag = updateRiverStatus(lid, pe, ts);
            /* load the maxim data into the RiverStatus table */
            loadRiverStatus(maxFcstfcst, updateFlag);
        }

        return;
    }

    private long getBasisTime() {

        String hourQuery = "select obshrs,fcsthrs from RpfParams";
        long currentTime = System.currentTimeMillis();
        long basisBeginTime = 0;
        Statement stmt = null;
        ResultSet rs = null;

        /*
         * This code sets the time values
         */
        try {
            if (!isHoursLoad) {
                RpfParamsTable rpfTb = new RpfParamsTable(db);
                stmt = db.newStmtForScrollableRset();
                rs = rpfTb.executeQuery(stmt, hourQuery);
                if (rs.next()) {
                    obshrs = rs.getLong("obshrs");
                    fcsthrs = rs.getLong("fcsthrs");
                } else {
                    logger.log("No fcsts in RpfParams table, using defaults");
                }

                rs.close();
                stmt.close();

                if (basis_hours_str != null) {
                    basishrs = Long.parseLong(basis_hours_str);
                    if ((basishrs <= 0) || (basishrs > 480)) {
                        logger.log("invalid value for basis_hours_filter token: "
                                + basishrs);
                        basishrs = 72;
                    }
                }
                isHoursLoad = true;
            }
        } catch (Exception e) {
            logger.log(" - PostgresSQL error in getBasisTime()");
            logger.printStackTrace(e);
        }

        basisBeginTime = currentTime - (basishrs * 3600 * 1000);
        return basisBeginTime;
    }

    /**
     * Determines if the lid,pe,ts already exists and whether or not the
     * riverstatus table needs to be updated.
     * 
     * @param lid
     *            - locationId
     * @param pe
     *            - Physical Element
     * @param ts
     *            - Timestamp
     * @return
     */

    private boolean updateRiverStatus(String lid, String pe, String ts) {
        Statement stmt = null;
        boolean rval = false;

        String query = "select lid " + "from riverstatus where lid = '" + lid
                + "' and " + "pe = '" + pe + "' and " + "ts = '" + ts + "'";

        try {
            RiverStatusTable rst = new RiverStatusTable(db);
            stmt = db.newStmtForScrollableRset();
            ResultSet rs = rst.executeQuery(stmt, query);

            if (rs.next())
                rval = true;
            rs.close();
            stmt.close();
        } catch (Exception e) {
            logger.log(" - PostgresSQL error searching riverstatus");
            logger.log("Query = [" + query + "]");
            logger.printStackTrace(e);

        }

        return rval;
    }

    /**
     * Loads the max fcst info into the RiverStatus table for the current
     * location and pe.
     * 
     * @param fcst
     *            - Forecast fcst to load
     * @param updateFlag
     *            - the boolean value if an update versus an insert should be
     *            performed
     * @return - status of action, 1 is good, 0 is bad
     */
    public synchronized int loadRiverStatus(Forecast fcst, Boolean updateFlag) {

        int status = -1;

        // turn fcst into a riverstatus record
        RiverStatusRecord record = new RiverStatusRecord();
        record.setLid(fcst.getLid());
        record.setPe(fcst.getPe());
        record.setDur(fcst.getDur());
        record.setTs(fcst.getTs());
        record.setExtremum(fcst.getExtremum());
        record.setProbability(fcst.getProbability());
        record.setValidtime(fcst.getValidTime().getTime());
        record.setBasistime(fcst.getBasisTime().getTime());
        record.setValue(fcst.getValue());
        try {
            RiverStatusTable rst = new RiverStatusTable(db);

            if (updateFlag) {
                status = rst.insertOrUpdate(record);
            } else {
                status = rst.insert(record);
            }

        } catch (Exception e) {
            logger.log("lid = [" + fcst.getLid() + "]");
            logger.log("PE = [" + fcst.getPe() + "]");
            logger.log("Duration = [" + fcst.getDur() + "]");
            logger.log("TS = [" + fcst.getTs() + "]");
            logger.log("Extremum = [" + fcst.getExtremum() + "]");
            logger.log("Probability = [" + fcst.getProbability() + "]");
            logger.log("valid timestamp = [" + fcst.getValidTime() + "]");
            logger.log("basis timestamp = [" + fcst.getBasisTime() + "]");
            logger.log("Data Value = [" + fcst.getValue() + "]");

            logger.printStackTrace(e);
            if (updateFlag) {
                logger.log(" - PostgresSQL error updating into riverstatus");
            } else {
                logger.log(" - PostgresSQL error inserting into riverstatus");
            }

        }

        return status;
    }

    /**
     * Get the Use Latest Forecast flag from riverstat table.
     * 
     * @param lid
     *            - The location id
     * @return The use latest forecast flag, or null if nothing in db
     */
    private String getUseLatestForecast(String lid) {
        String sql = "select use_latest_fcst from riverstat where lid = '"
                + lid + "'";
        String useLatest = null;
        Statement stmt = null;
        ResultSet rs = null;

        try {
            RiverstatTable rstb = new RiverstatTable(db);
            stmt = db.newStmtForScrollableRset();
            rs = rstb.executeQuery(stmt, sql);
            if (rs.next())
                useLatest = rs.getString("use_latest_fcst");
            rs.close();
            stmt.close();
        } catch (SQLException e) {
            logger.log("Error executing getUseLatestForecast(" + lid + ")");
            logger.printStackTrace(e);
        }
        return useLatest;
    }

    /**
     * This gets the max forecast value from a forecast time-series that has
     * already been prepared. This function returns the ts, value, basistime,
     * and validtime for the maximum forecast value.
     * 
     * @param fcstList
     *            ArrayList of Forecast data
     * @return The max Forecast object
     */
    private Forecast findMaxFcst(ArrayList<Forecast> fcstList) {
        Forecast max = null;
        double maxValue = MISSING_VALUE;
        int maxIndex = MISSING_VALUE;

        /* just in case */
        if ((fcstList == null) || (fcstList.size() == 0)) {
            return null;
        }

        /* loop and get the max */
        for (int i = 0; i < fcstList.size(); i++) {
            if (fcstList.get(i).getValue() > maxValue) {
                maxValue = fcstList.get(i).getValue();
                maxIndex = i;
            }
        }

        /* if for some bizarre reason, load the first fcst */
        if (maxIndex == MISSING_VALUE) {
            logger.log("ERROR - find_maxfcst couldn't find max?!\n");
            maxIndex = 0;
        }

        max = fcstList.get(maxIndex);

        return max;
    }

    /**
     * Processes forecast data for the given table, lid and pe.
     * 
     * @param table
     *            - The table
     * @param lid
     *            - The location id
     * @param pe
     *            - The physical element
     */
    public boolean loadMaxFcstData(String table, String lid, String pe) {
        Statement stmt = null;
        ResultSet rs = null;
        boolean status = true;

        String sql = "select DISTINCT(ts) " + "from " + table
                + " where lid = '" + lid + "' and " + "pe = '" + pe + "' and "
                + "validtime > CURRENT_TIMESTAMP and " + "probability < 0.0";
        try {
            FcstHeightTable fht = new FcstHeightTable(db);
            stmt = db.newStmtForScrollableRset();
            rs = fht.executeQuery(stmt, sql);
            while (rs.next()) {
                loadMaxFcstItem(lid, pe, rs.getString("ts"));
            }
            rs.close();
            stmt.close();
        } catch (SQLException se) {
            status = !status;
            logger.log("Error executing loadMaxFcsftData(" + table + ", " + lid
                    + ", " + pe + ")");
            logger.printStackTrace(se);
        }
        return status;
    }

    /**
     * This function assembles a forecast time series for a given location and
     * pe. The data are retrieved for: 1) either the specified type-source or
     * for the type-source defined in the ingest filter as the one to use, based
     * on its rank; and for 2) either all forecast values regardless of basis
     * time or only those forecasts with the latest basis time. 3) for
     * non-probabilistic values only.
     * 
     * It returns a times series of values in an array of structures, and also
     * returns the count of values.
     * 
     * Presuming that the duration and extremum values in the forecast table
     * never yield duplicates, then there can only be duplicates for the same
     * validtime due to multiple basis times.
     * 
     * There is a passed in limit regarding how far in the future data is
     * considered, and how old the forecast (basistime) can be.
     * 
     * This function is needed since some locations have short-term forecasts
     * and long-term forecasts, both of which are valid and do not prempt the
     * other. This avoids problems with the previous method where the software
     * always used the forecast with the latest creation time and ignored all
     * other forecasts, for certain purposes.
     * 
     * The approach herein does NOT assume that the creation data corresponds to
     * the valid time covered - i.e. it does NOT require that long-term forecast
     * have the latest creation time. The heart of the logic for this function
     * is contained in the adjust_startend() function.
     * 
     * @param lid
     * @param pe
     * @param tsFilter
     * @param qcFilter
     * @param useLatest
     * @param basisBegintime
     * @return A list of forecast records
     */
    private List<Forecast> buildTsFcstRiv(String lid, String pe,
            String tsFilter, int qcFilter, boolean useLatest,
            long basisBegintime) {
        int fcstCount = 0;
        int keepCount = 0;
        String useTs = null;
        String tableName = null;
        String sql = null;
        String sqlForecast = null;
        java.sql.Timestamp basisTimeAnsi = null;

        int[] doKeep = null;
        Statement stmt = null;
        Statement stmt2 = null;
        ResultSet ulHead = null;
        int ulCount = 0;
        List<Forecast> fcstHead = null;
        Forecast fcstHght = null;

        List<Forecast> fcstList = new ArrayList<Forecast>();
        Forecast forecast = null;
        DbTable dbTable = null;

        if ((tsFilter == null) || (tsFilter.length() == 0)) {
            useTs = getBestTs(lid, pe, "F%", 0);
            if (useTs == null)
                return null;
        } else {
            useTs = tsFilter;
        }

        try {
            if (pe.startsWith("H") || pe.startsWith("h"))
                tableName = "FcstHeight";
            else
                tableName = "FcstDischarge";

            dbTable = getDbTableInstance(db, tableName);

            basisTimeAnsi = new Timestamp(basisBegintime);
            /*
             * retrieve a list of unique basis times; use descending sort. only
             * consider forecast data before some ending time, and with some
             * limited basis time ago
             */

            sql = "SELECT DISTINCT(basistime) FROM " + tableName + " "
                    + "WHERE lid = '" + lid + "' and " + "pe = '" + pe
                    + "' and " + "ts = '" + useTs + "' and "
                    + "validtime >= CURRENT_TIMESTAMP and " + "basistime >= '"
                    + basisTimeAnsi + "' and " + "value != " + MISSING_VALUE
                    + " and " + "quality_code >= " + QUESTIONABLE_BAD_THRESHOLD
                    + " " + "ORDER BY basistime DESC ";

            stmt = db.newStmtForScrollableRset();
            ulHead = dbTable.executeQuery(stmt, sql);
            ulCount = getResultSetSize(ulHead);

            /*
             * retrieve the data; the ordering by validtime is important. as
             * before, limit the forecast time valid time window and as needed,
             * the age of the forecast (basistime).
             */

            sqlForecast = "SELECT lid,pe,dur,ts,extremum,probability,validtime,basistime,value "
                    + "FROM "
                    + tableName
                    + " "
                    + "WHERE lid = '"
                    + lid
                    + "' AND "
                    + "pe = '"
                    + pe
                    + "' AND "
                    + "ts = '"
                    + useTs
                    + "' AND "
                    + "validtime >= CURRENT_TIMESTAMP AND "
                    + "probability < 0.0 AND ";

            if (useLatest || ulCount == 1) {
                java.sql.Timestamp tempStamp = null;
                ulHead.next();
                tempStamp = ulHead.getTimestamp("basistime");
                sqlForecast += "basistime >= '" + tempStamp + "' AND ";

            } else {
                sqlForecast += "basistime >= '" + basisTimeAnsi + "' AND ";
            }

            sqlForecast += "value != " + Integer.parseInt(MISSING) + " AND "
                    + "quality_code >= " + Integer.parseInt(MISSING) + " "
                    + "ORDER BY validtime ASC";

            stmt2 = db.newStmtForScrollableRset();
            ResultSet oa = dbTable.executeQuery(stmt2, sqlForecast);

            int soa = getResultSetSize(oa);

            if ((oa != null) && soa > 0) {
                fcstHead = new ArrayList<Forecast>(soa);
                while (oa.next()) {
                    fcstHght = new Forecast();
                    Date tmpDate = null;
                    fcstHght.setLid(oa.getString("lid"));
                    fcstHght.setPe(oa.getString("Pe"));
                    fcstHght.setDur(oa.getShort("dur"));
                    fcstHght.setTs(oa.getString("ts"));
                    fcstHght.setExtremum(oa.getString("extremum"));
                    fcstHght.setProbability(oa.getFloat("probability"));
                    tmpDate = new Date(oa.getTimestamp("validtime").getTime());
                    fcstHght.setValidTime(tmpDate);
                    tmpDate = new Date(oa.getTimestamp("basistime").getTime());
                    fcstHght.setBasisTime(tmpDate);
                    fcstHght.setValue(oa.getDouble("value"));
                    fcstHead.add(fcstHght);
                }
            }

            if (fcstHead != null) {
                fcstCount = fcstHead.size();
            }

            /*
             * define a local array to determine which items in the time series
             * to keep and return
             */
            if (fcstCount > 0) {
                doKeep = new int[fcstCount];
            } else {
                return null;
            }

            /*
             * if only getting the latest basis time's data or only one basis
             * time was found, then consider all; otherwise, need to adjoin/butt
             * the time series together for the multiple basis times.
             */

            if (useLatest || ulCount <= 1) {
                for (int i = 0; i < doKeep.length; i++) {
                    doKeep[i] = 1;
                }
            } else {
                doKeep = setFcstKeep(ulHead, ulCount, fcstHead);
            }

            /*
             * now load the values and info to return, knowing which items to
             * keep since all the values have been tagged. first get the count
             * of the number of values to keep and allocate the data
             */

            for (int j = 0; j < fcstCount; j++) {
                if (doKeep[j] == 1) {
                    keepCount++;
                }
            }

            for (int y = 0; y < fcstCount; y++) {
                forecast = new Forecast();
                if (doKeep[y] == 1) {
                    forecast.setLid(fcstHead.get(y).getLid());
                    forecast.setPe(fcstHead.get(y).getPe());
                    convertDur(fcstHead.get(y).getDur(), forecast);
                    forecast.setTs(fcstHead.get(y).getTs());
                    forecast.setExtremum(fcstHead.get(y).getExtremum());
                    forecast.setValidTime(fcstHead.get(y).getValidTime());
                    forecast.setBasisTime(fcstHead.get(y).getBasisTime());
                    forecast.setValue(fcstHead.get(y).getValue());
                }
                fcstList.add(forecast);
            }

            stmt.close();
            stmt2.close();
        } catch (Error e) {
            logger.log(" - SQL error in buildTsFcstRiv");
            logger.printStackTrace(e);
        } catch (Exception e) {
            logger.log("Query = [" + sql + "]");
            logger.log("Query = [" + sqlForecast + "]");
            logger.log(" - SQL Exception in buildTsFcstRiv");
            logger.printStackTrace(e);
        }

        return fcstList;
    }

    private int getResultSetSize(ResultSet rs) {
        int rowcount = 0;
        try {
            if (rs.last()) {
                rowcount = rs.getRow();
                rs.beforeFirst();
            }
        } catch (SQLException e) {
            logger.log("Error computing Resultset size !");
            logger.printStackTrace(e);
        }
        return rowcount;
    }

    /**
     * Converts duration int to String character.
     * 
     * @param dur
     *            - The duration value
     * @return The single character duration value
     */
    private void convertDur(short dur, Forecast data) {
        String value = null;
        String durationCode = null;

        switch (dur) {
        case 0:
            value = "I";
            break;
        case 1:
            value = "U";
            break;
        case 5:
            value = "E";
            break;
        case 10:
            value = "G";
            break;
        case 15:
            value = "C";
            break;
        case 30:
            value = "J";
            break;
        case 1001:
            value = "H";
            break;
        case 1002:
            value = "B";
            break;
        case 1003:
            value = "T";
            break;
        case 1004:
            value = "F";
            break;
        case 1006:
            value = "Q";
            break;
        case 1008:
            value = "A";
            break;
        case 1012:
            value = "K";
            break;
        case 1018:
            value = "L";
            break;
        case 2001:
            value = "D";
            break;
        case 2007:
            value = "W";
            break;
        // case 'N':
        // Not sure what to return. Shef maunal explanation:
        // N Mid month, duration for the period from the 1st day of the
        // month to and ending on the
        // 15th day of the same month
        // break;
        case 3001:
            value = "M";
            break;
        case 4001:
            value = "Y";
            break;
        case 5004:
            value = "P";
            break;
        case 5000: {
            value = "Z";
            break;
        }
        case 5001:
            value = "S";
            break;
        case 5002:
            value = "R";
            break;
        case 5005:
            value = "X";
            break;
        default: {
            // Anything that didn't get picked up above is
            // probably a variable duration.
            if (dur >= 7000) {
                value = "V";
                durationCode = "S";
            } else if (dur < 1000) {
                value = "V";
                durationCode = "N";
            } else if (dur < 2000) {
                value = "V";
                durationCode = "H";
            } else if (dur < 3000) {
                value = "V";
                durationCode = "D";
            } else if (dur < 4000) {
                value = "V";
                durationCode = "M";
            } else if (dur < 5000) {
                value = "V";
                durationCode = "Y";
            } else {
                // Not sure what value this would be.
                value = "Z";
            }
        }
        }

        data.setDur(dur);
    }

    /**
     * Determine which items in the forecast time series to keep, as there may
     * be overlap due to multiple time_series.
     * 
     * @param ulHead
     * @param count
     * @param fcstHead
     * @return
     * @throws SQLException
     */
    private int[] setFcstKeep(ResultSet ulHead, int count,
            List<Forecast> fcstHead) throws SQLException {
        int fcstCount = fcstHead.size();
        int ulCount = count;
        int[] doKeep = new int[fcstCount];
        int[] basisIndex = new int[fcstCount];
        int[] tsFirstChk = new int[ulCount];
        Timestamp[] startTime = new Timestamp[ulCount];
        Timestamp[] endTime = new Timestamp[ulCount];
        Timestamp[] basisTime = new Timestamp[ulCount];
        Timestamp fcstBasisTime = null;
        Timestamp fcstValidTime = null;
        Timestamp ulBasisTime = null;

        for (int i = 0; i < ulCount; i++) {
            tsFirstChk[i] = 0;
        }

        Timestamp validTime = null;
        ulHead.beforeFirst();
        for (int i = 0; i < fcstCount; i++) {

            /* find out which basis time's time series this value belongs to */

            fcstBasisTime = new Timestamp(fcstHead.get(i).getBasisTime()
                    .getTime());

            basisIndex[i] = MISSING_VALUE;

            for (int j = 0; ((j < ulCount) && (basisIndex[i] == MISSING_VALUE)); j++) {
                ulBasisTime = ulHead.getTimestamp("ts");

                if (ulBasisTime.compareTo(fcstBasisTime) == 0) {
                    basisIndex[i] = j;
                }
            }

            if (basisIndex[i] == MISSING_VALUE) {
                logger.log("Unexpected error assigning basis_index for " + i);
            }

            /*
             * check if the values constitute the start or end times for the
             * time series and fcst these times if they do
             */

            validTime = new Timestamp(fcstHead.get(i).getValidTime().getTime());

            if (tsFirstChk[basisIndex[i]] == 1) {
                if (validTime.before(startTime[basisIndex[i]])) {
                    startTime[basisIndex[i]] = validTime;
                } else if (validTime.after(endTime[basisIndex[i]])) {
                    endTime[basisIndex[i]] = validTime;
                }
            } else {
                startTime[basisIndex[i]] = validTime;
                endTime[basisIndex[i]] = validTime;
                tsFirstChk[basisIndex[i]] = 1;
            }
        }

        /*
         * for each of the unique basis times, assign the basis time in a
         * convenient array for use in the adjust_startend function.
         */

        ulHead.beforeFirst();
        ArrayList<Timestamp> bst = new ArrayList<Timestamp>();
        while (ulHead.next()) {
            bst.add(ulHead.getTimestamp("ts"));
        }
        basisTime = (Timestamp[]) bst.toArray();

        /*
         * knowing the actual start and end times for the multiple time series,
         * loop thru the time series and adjust the start and end times so that
         * they reflect the time span to use; i.e. there is no overlap. THIS IS
         * THE KEY STEP IN THE PROCESS OF DEFINING AN AGGREGATE VIRTUAL TIME
         * SERIES!!!
         */
        Object[] tmp = adjustStartEnd(ulCount, basisTime, startTime, endTime);
        startTime = (Timestamp[]) tmp[0];
        endTime = (Timestamp[]) tmp[1];

        /*
         * loop thru the complete retrieved time series and only keep the value
         * if it lies between the start and end time for this basis time
         */
        for (int i = 0; i < fcstCount; i++) {
            fcstValidTime = new Timestamp(fcstHead.get(i).getValidTime()
                    .getTime());
            if ((fcstValidTime.compareTo(startTime[basisIndex[i]]) >= 0)
                    && (fcstValidTime.compareTo(endTime[basisIndex[i]]) <= 0)) {
                doKeep[i] = 1;
            } else {
                doKeep[i] = 0;
            }
        }
        return doKeep;
    }

    /**
     * This method uses the time series with the latest basis time first, and
     * uses it in its entirety. Then the time series with the next latest basis
     * time is used. If it overlaps portions of the already saved time series,
     * then only that portion which doesn't overlap is used. This process
     * continues until all time series have been considered. In essences, this
     * method adjoins adjacent time series.
     * 
     * @param count
     * @param basisTime
     * @param startValidTime
     * @param endValidTime
     * @return
     */
    private Object[] adjustStartEnd(int count, Timestamp[] basisTime,
            Timestamp[] startValidTime, Timestamp[] endValidTime) {
        boolean found = false;
        int currentIndex = 0;
        int[] basisOrder = new int[count];
        Timestamp fullStartValidTime = null;
        Timestamp fullEndValidTime = null;
        Timestamp tmpTime = null;
        Timestamp zero = new Timestamp((new Date(0)).getTime());
        Object[] rval = new Object[2]; // [startValidTime[]] [endValidTime[]]

        for (int i = 0; i < count; i++) {
            basisOrder[i] = -1;
        }

        /*
         * find the order of the time series by their latest basis time. if two
         * time series have the same basis time, use the one that has the
         * earlier starting time. note that the order is such that the latest
         * basis time is last in the resulting order array.
         */

        for (int i = 0; i < count; i++) {
            tmpTime = zero;
            currentIndex = 0;

            for (int j = 0; j < count; j++) {
                /*
                 * only consider the time series if it hasn't been accounted for
                 * in the order array
                 */
                found = false;

                for (int k = 0; k < i; k++) {
                    if (j == basisOrder[k]) {
                        found = true;
                        break;
                    }
                }

                if (!found) {
                    if (basisTime[j].compareTo(tmpTime) > 0) {
                        currentIndex = j;
                        tmpTime = basisTime[j];
                    } else if (basisTime[j].compareTo(tmpTime) == 0) {
                        if (startValidTime[j]
                                .compareTo(startValidTime[currentIndex]) < 0) {
                            currentIndex = j;
                            tmpTime = basisTime[j];
                        }
                    }
                }
            }

            basisOrder[i] = currentIndex;
        }

        /*
         * do NOT adjust the start and end time of the time series with the
         * latest ending time. loop through all the other time series and adjust
         * their start and end times as necessary so that they do not overlap
         * the time limits of the being-built aggregate time series.
         */

        currentIndex = basisOrder[0];
        fullStartValidTime = startValidTime[currentIndex];
        fullEndValidTime = endValidTime[currentIndex];

        for (int i = 1; i < count; i++) {
            currentIndex = basisOrder[i];

            /*
             * each additional time series being considered is checked to see if
             * it falls outside the time window already encompassed by the
             * assembled time series. there are four cases that can occur; each
             * is handled below.
             */

            if ((startValidTime[currentIndex].compareTo(fullStartValidTime) >= 0)
                    && (endValidTime[currentIndex].compareTo(fullEndValidTime) <= 0)) {
                /*
                 * if the basis time series being considered is fully within the
                 * time of the already existing time series, then ignore it
                 * completely, and reset its times.
                 */
                startValidTime[currentIndex] = zero;
                endValidTime[currentIndex] = zero;
            } else if ((startValidTime[currentIndex]
                    .compareTo(fullStartValidTime) <= 0)
                    && (endValidTime[currentIndex].compareTo(fullEndValidTime) >= 0)) {
                /*
                 * if the basis time series being considered covers time both
                 * before and after the existing time series, use the portion of
                 * it that is before the time series. it is not desirable to use
                 * both the before and after portion (this results in a
                 * non-contiguous time-series that is weird), and given a choice
                 * it is better to use the forecast data early on than the later
                 * forecast data, so use the before portion
                 */
                endValidTime[currentIndex] = new Timestamp(
                        fullStartValidTime.getTime() - 1000);
                fullStartValidTime = startValidTime[currentIndex];

            } else if ((startValidTime[currentIndex]
                    .compareTo(fullStartValidTime) <= 0)
                    && (endValidTime[currentIndex].compareTo(fullEndValidTime) <= 0)) {
                /*
                 * if the basis time series being considered straddles the
                 * beginning or is completely before the existing time series,
                 * then use the portion of it that is before the time series.
                 */
                endValidTime[currentIndex] = new Timestamp(
                        fullStartValidTime.getTime() - 1000);
                fullStartValidTime = startValidTime[currentIndex];
            } else if ((startValidTime[currentIndex]
                    .compareTo(fullStartValidTime) >= 0)
                    && (endValidTime[currentIndex].compareTo(fullEndValidTime) >= 0)) {
                /*
                 * if the basis time series being considered straddles the end
                 * or is completely after the existing time series, then use the
                 * portion of it that is after the time series.
                 */
                startValidTime[currentIndex] = new Timestamp(
                        fullEndValidTime.getTime() + 1000);
                fullEndValidTime = endValidTime[currentIndex];

            }
        } /* end for loop on the unique ordered basis times */

        // Need to find a better way to do this
        rval[0] = startValidTime;
        rval[1] = endValidTime;
        return rval;
    }

    /**
     * For a given location and pe code and type-source prefix, this function
     * returns the type-source code with the lowest rank in IngestFilter.
     * Alternatively, if a specific ordinal number is passed, then the Nth
     * ranking ts is returned. If no (<= 0) ordinal number (i.e. 1st, 2nd) is
     * requested, then the highest rank (1st) is returned. The type-source
     * prefix is normally given as a one-character string, R for observed data
     * and F for forecast data.
     * 
     * The function argument returns a status variable indicating whether the
     * request was satisfied.
     * 
     * @param lid
     * @param pe
     * @param tsPrefix
     * @param ordinal
     * @return
     */
    private String getBestTs(String lid, String pe, String tsPrefix, int ordinal) {

        int count = 0;
        String tsFound = null;
        String query = "SELECT ts_rank,ts FROM ingestfilter " + "WHERE lid = '"
                + lid + "' AND " + "pe = '" + pe + "' AND " + "ts like '"
                + tsPrefix + "' AND " + "ingest = 'T' "
                + "ORDER BY ts_rank, ts";

        try {
            /*
             * get the ingest filter entries for this location. note that the
             * retrieval is ordered so that if multiple best ranks exist, there
             * is some predicatibility for the identified best one. also note
             * that this approach ignores the duration, extremum, and probabilty
             * code.
             */

            IngestFilterTable ift = new IngestFilterTable(db);
            Statement stmt = db.newStmtForScrollableRset();
            ResultSet rs = ift.executeQuery(stmt, query);

            if (rs.next()) {
                /*
                 * if no specific ordinal number was requested, return with the
                 * highest rank.
                 */
                if (ordinal <= 0) {
                    tsFound = rs.getString("ts");
                } else {
                    /*
                     * get a count of the number of matching ts entries. if the
                     * requested ordinal number is greater than the number
                     * available then return with a not found status.
                     */

                    count = getResultSetSize(rs);

                    if (ordinal <= count) {
                        rs.relative(ordinal - 1);
                        tsFound = rs.getString("ts");
                    }
                }
            }
            rs.close();
            stmt.close();
        } catch (Exception e) {
            logger.log(" - PostgresSQL error retrieving from ingestfilter");
            logger.log("Query = [" + query + "]");
            logger.printStackTrace(e);

        }

        return tsFound;
    }

    private class Forecast extends FcstHeightRecord {

        private Date validTime = null;

        private Date basisTime = null;

        public void setValidTime(Date validTime) {
            this.validTime = validTime;
        }

        public Date getValidTime() {
            return validTime;
        }

        public void setBasisTime(Date basisTime) {
            this.basisTime = basisTime;
        }

        public Date getBasisTime() {
            return basisTime;
        }

        public boolean equals(Object other) {
            if ((this == other))
                return true;
            if ((other == null))
                return false;
            if (!(other instanceof Forecast))
                return false;
            Forecast castOther = (Forecast) other;

            return ((this.getLid() == castOther.getLid()) || (this.getLid() != null
                    && castOther.getLid() != null && this.getLid().equals(
                    castOther.getLid())))
                    && ((this.getPe() == castOther.getPe()) || (this.getPe() != null
                            && castOther.getPe() != null && this.getPe()
                            .equals(castOther.getPe())))
                    && (this.getDur() == castOther.getDur())
                    && ((this.getTs() == castOther.getTs()) || (this.getTs() != null
                            && castOther.getTs() != null && this.getTs()
                            .equals(castOther.getTs())))
                    && ((this.getExtremum() == castOther.getExtremum()) || (this
                            .getExtremum() != null
                            && castOther.getExtremum() != null && this
                            .getExtremum().equals(castOther.getExtremum())))
                    && (this.getProbability() == castOther.getProbability())
                    && ((this.getValidTime() == castOther.getValidTime()) || (this
                            .getValidTime() != null
                            && castOther.getValidTime() != null && this
                            .getValidTime().equals(castOther.getValidTime())))
                    && ((this.getBasisTime() == castOther.getBasisTime()) || (this
                            .getBasisTime() != null
                            && castOther.getBasisTime() != null && this
                            .getBasisTime().equals(castOther.getBasisTime())));
        }

        public int hashCode() {
            int result = 17;

            result = 37 * result
                    + (getLid() == null ? 0 : this.getLid().hashCode());
            result = 37 * result
                    + (getPe() == null ? 0 : this.getPe().hashCode());
            result = 37 * result + this.getDur();
            result = 37 * result
                    + (getTs() == null ? 0 : this.getTs().hashCode());
            result = 37
                    * result
                    + (getExtremum() == null ? 0 : this.getExtremum()
                            .hashCode());
            result = 37 * result + (int) this.getProbability();
            result = 37
                    * result
                    + (getValidTime() == null ? 0 : this.getValidTime()
                            .hashCode());
            result = 37
                    * result
                    + (getBasisTime() == null ? 0 : this.getBasisTime()
                            .hashCode());
            return result;
        }
    }

    /**
     * A utility DbTable factory
     * 
     * @param db
     * @param table
     * @return
     */
    private DbTable getDbTableInstance(Database db, String table) {
        if (table.equalsIgnoreCase("FcstHeight"))
            return new FcstHeightTable(db);
        else if (table.equalsIgnoreCase("FcstDischarge"))
            return new FcstDischargeTable(db);
        // Default for now is FcstHeight so we're never returning null.
        return new FcstHeightTable(db);
    }

}
