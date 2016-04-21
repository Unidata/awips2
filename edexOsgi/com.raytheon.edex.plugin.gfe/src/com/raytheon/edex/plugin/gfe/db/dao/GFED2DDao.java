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

package com.raytheon.edex.plugin.gfe.db.dao;

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hibernate.Session;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.plugin.grid.dao.GridDao;

/**
 * Data access object for manipulating GFE Records
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 20, 2013 1774       randerso    Refactored out of GFEDao
 * Apr 04, 2013 1787       randerso    Fixed to support changes to D2D grid
 *                                     location  Additional cleanup to move the
 *                                     D2D to GFE translation  logic into
 *                                     D2DGridDatabase.
 * May 03, 2013 1974       randerso    Changed queryByParmId to look for parm
 *                                     with duration  suffix first.
 * May 22, 2013 1974       randerso    Fix bug introduced by the previous fix
 *                                     where query for  T (T%hr) returned TP6hr
 * Jun 13, 2013 2044       randerso    Cleaned up JavaDoc
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * 10/16/2014   3454       bphillip    Upgrading to Hibernate 4
 * Aug 14, 2015 17801      bhunderm    Fixed logic to choose the parm with lesser
 *                                     duration when have multiple grids for same fcsthr.
 * Dec 03, 2015 5168       randerso    Added ability to retrieve D2D data by fcsthr or timerange
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GFED2DDao extends GridDao {
    private static final String FCSTTIME_ID = PluginDataObject.DATATIME_ID
            + ".fcstTime";

    private static final String REFTIME_ID = PluginDataObject.DATATIME_ID
            + ".refTime";

    /**
     * Constructor
     * 
     * @throws PluginException
     */
    public GFED2DDao() throws PluginException {
        super();
    }

    /**
     * Retrieves a list of available forecast times
     * 
     * @param d2dModelName
     * @param refTime
     * 
     * @return The list of forecast times associated with the specified
     *         DatabaseID
     * @throws DataAccessLayerException
     *             If errors occur while querying the metadata database
     */
    public List<Integer> getForecastTimes(String d2dModelName, Date refTime)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(GridRecord.class.getName());
        query.addDistinctParameter(FCSTTIME_ID);
        query.addQueryParam(GridConstants.DATASET_ID, d2dModelName);
        query.addQueryParam(REFTIME_ID, refTime);
        query.addOrder(FCSTTIME_ID, true);

        @SuppressWarnings("unchecked")
        List<Integer> vals = (List<Integer>) this.queryByCriteria(query);
        return vals;
    }

    /**
     * Retrieves a GridRecord from the grib metadata database based on a ParmID,
     * forecastTime, and GridParmInfo.
     * 
     * @param d2dModelName
     * @param refTime
     * @param d2dParmName
     * @param d2dLevel
     * @param forecastTime
     *            The forecast time of the desired GridRecord, null for any
     *            record
     * @param info
     *            The GridParmInfo for the requested d2d grid.
     * @return The GridRecord from the grib metadata database
     * @throws DataAccessLayerException
     *             If errors occur while querying the metadata database
     */
    public GridRecord getGrid(String d2dModelName, Date refTime,
            String d2dParmName, Level d2dLevel, Integer forecastTime,
            GridParmInfo info) throws DataAccessLayerException {
        Session s = null;

        try {
            s = getSession();
            // TODO: clean up so we only make one db query
            SortedMap<DataTime, Integer> rawTimes = queryByParmId(d2dModelName,
                    refTime, d2dParmName, d2dLevel, s);

            // if forecastTime is null just pick one,
            // this is for static data since all times are the same
            Integer id = null;
            if (forecastTime == null) {
                id = rawTimes.values().iterator().next();
            } else {
                for (Entry<DataTime, Integer> entry : rawTimes.entrySet()) {
                    if (entry.getKey().getFcstTime() == forecastTime) {
                        id = entry.getValue();
                        break;
                    }
                }
            }

            GridRecord retVal = (GridRecord) s.get(GridRecord.class, id);
            return retVal;

        } finally {
            if (s != null) {
                try {
                    s.close();
                } catch (Exception e) {
                    logger.error("Error occurred closing database session", e);
                }
            }
        }
    }

    /**
     * Retrieves a GridRecord from the grib metadata database based on a ParmID,
     * TimeRange, and GridParmInfo.
     * 
     * @param d2dModelName
     * @param refTime
     * @param d2dParmName
     * @param d2dLevel
     * @param timeRange
     *            The valid period of the desired GridRecord, null for any
     *            record
     * @param info
     *            The GridParmInfo for the requested d2d grid.
     * @return The GridRecord from the grib metadata database
     * @throws DataAccessLayerException
     *             If errors occur while querying the metadata database
     */
    public GridRecord getGrid(String d2dModelName, Date refTime,
            String d2dParmName, Level d2dLevel, TimeRange timeRange,
            GridParmInfo info) throws DataAccessLayerException {
        Session s = null;

        try {
            s = getSession();
            // TODO: clean up so we only make one db query
            SortedMap<DataTime, Integer> rawTimes = queryByParmId(d2dModelName,
                    refTime, d2dParmName, d2dLevel, s);

            // if forecastTime is null just pick one,
            // this is for static data since all times are the same
            Integer id = null;
            if (timeRange == null) {
                id = rawTimes.values().iterator().next();
            } else {
                for (Entry<DataTime, Integer> entry : rawTimes.entrySet()) {
                    if (entry.getKey().getValidPeriod().equals(timeRange)) {
                        id = entry.getValue();
                        break;
                    }
                }
            }

            GridRecord retVal = (GridRecord) s.get(GridRecord.class, id);
            return retVal;

        } finally {
            if (s != null) {
                try {
                    s.close();
                } catch (Exception e) {
                    logger.error("Error occurred closing database session", e);
                }
            }
        }
    }

    /**
     * Gets a SortedMap of DataTime and GridRecord ids from the grib metadata
     * database which match the given ParmID. Session passed to allow reuse
     * across multiple calls.
     * 
     * @param d2dModelName
     * @param refTime
     * @param d2dParmName
     * @param d2dLevel
     * @param s
     *            The database session to use
     * @return The list of GridRecords from the grib metadata database which
     *         match the given ParmID
     * @throws DataAccessLayerException
     *             If errors occur while querying the metadata database
     */
    public SortedMap<DataTime, Integer> queryByParmId(String d2dModelName,
            Date refTime, String d2dParmName, Level d2dLevel, Session s)
            throws DataAccessLayerException {

        DatabaseQuery query;
        query = new DatabaseQuery(GridRecord.class.getName());
        query.addReturnedField(PluginDataObject.DATATIME_ID);
        query.addReturnedField("id");
        query.addReturnedField(GridConstants.PARAMETER_ABBREVIATION);
        query.addQueryParam(GridConstants.DATASET_ID, d2dModelName);
        query.addQueryParam(REFTIME_ID, refTime);
        query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION, d2dParmName
                + "%hr", QueryOperand.LIKE);
        query.addQueryParam(GridConstants.LEVEL_ID, d2dLevel.getId());
        query.addOrder(FCSTTIME_ID, true);
        query.addOrder(GridConstants.PARAMETER_ABBREVIATION, true);

        @SuppressWarnings("unchecked")
        List<Object[]> firstTry = (List<Object[]>) this.queryByCriteria(query);

        // TODO use a regular expression match in the query to eliminate the
        // need to remove the false matches below

        // remove false matches
        Pattern pattern = Pattern.compile("^" + d2dParmName + "(\\d+)hr$");

        Iterator<Object[]> iter = firstTry.iterator();
        while (iter.hasNext()) {
            Object[] row = iter.next();
            Matcher matcher = pattern.matcher((String) row[2]);
            if (!matcher.matches()) {
                iter.remove();
            }
        }

        SortedMap<DataTime, Integer> dataTimes = new TreeMap<DataTime, Integer>();
        if (firstTry.isEmpty()) {
            query = new DatabaseQuery(GridRecord.class.getName());
            query.addReturnedField(PluginDataObject.DATATIME_ID);
            query.addReturnedField("id");
            query.addQueryParam(GridConstants.DATASET_ID, d2dModelName);
            query.addQueryParam(REFTIME_ID, refTime);
            query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION,
                    d2dParmName);
            query.addQueryParam(GridConstants.LEVEL_ID, d2dLevel.getId());
            query.addOrder(FCSTTIME_ID, true);

            @SuppressWarnings("unchecked")
            List<Object[]> secondTry = (List<Object[]>) this
                    .queryByCriteria(query);

            for (Object[] row : secondTry) {
                dataTimes.put((DataTime) row[0], (Integer) row[1]);
            }
        } else {
            int i = 0;
            while (i < firstTry.size()) {
                Object[] row = firstTry.get(i++);
                DataTime dataTime = (DataTime) row[0];
                Integer id = (Integer) row[1];
                Matcher matcher = pattern.matcher((String) row[2]);
                int dur = Integer.MAX_VALUE;
                if (matcher.matches()) {
                    dur = Integer.parseInt(matcher.group(1));
                }

                while (i < firstTry.size()) {
                    Object[] nextRow = firstTry.get(i);
                    DataTime nextDataTime = (DataTime) nextRow[0];
                    if (dataTime.getFcstTime() == nextDataTime.getFcstTime()) {
                        i++;
                        String nextParam = (String) nextRow[2];
                        Matcher nextMatcher = pattern.matcher(nextParam);
                        int nextDur = Integer.MAX_VALUE;
                        if (nextMatcher.matches()) {
                            nextDur = Integer.parseInt(nextMatcher.group(1));
                        }
                        if (nextDur < dur) {
                            id = (Integer) nextRow[1];
                        }
                    } else {
                        break;
                    }
                }
                dataTimes.put(dataTime, id);
            }
        }
        return dataTimes;
    }

    /**
     * Retrieve the available Data Times by D2D parm id.
     * 
     * @param d2dModelName
     * @param refTime
     * @param d2dParmName
     * @param d2dLevel
     * @return the list of data times, empty if none
     * @throws DataAccessLayerException
     */
    public List<DataTime> queryDataTimeByParmId(String d2dModelName,
            Date refTime, String d2dParmName, Level d2dLevel)
            throws DataAccessLayerException {
        List<DataTime> timeList = new ArrayList<>();
        Session s = null;
        try {
            s = getSession();

            SortedMap<DataTime, Integer> results = queryByParmId(d2dModelName,
                    refTime, d2dParmName, d2dLevel, s);
            for (DataTime o : results.keySet()) {
                timeList.add(o);
            }
        } finally {
            if (s != null) {
                try {
                    s.close();
                } catch (Exception e) {
                    logger.error("Error occurred closing database session", e);
                }
            }
        }

        return timeList;
    }

    /**
     * Retrieves model run times for the n most recent model runs of a given
     * d2dModelName
     * 
     * @param d2dModelName
     * @param maxRecords
     * @return list of model run times, empty if none
     * @throws DataAccessLayerException
     */
    public List<Date> getModelRunTimes(String d2dModelName, int maxRecords)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(GridRecord.class.getName());
        query.addDistinctParameter(REFTIME_ID);
        query.addQueryParam(GridConstants.DATASET_ID, d2dModelName);
        query.addOrder(REFTIME_ID, false);
        if (maxRecords > 0) {
            query.setMaxResults(maxRecords);
        }
        List<?> result = this.queryByCriteria(query);

        List<Date> inventory = new ArrayList<Date>(result.size());
        for (Object obj : result) {
            // convert returned "Dates" (actually java.sql.TimeStamps) to actual
            // java.util.Dates so equals comparisons work correctly
            Date date = new Date(((Date) obj).getTime());
            inventory.add(date);
        }

        return inventory;
    }
}
