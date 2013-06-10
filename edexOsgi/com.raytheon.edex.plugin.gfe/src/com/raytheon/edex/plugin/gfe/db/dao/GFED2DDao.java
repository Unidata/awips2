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
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hibernate.Session;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
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
 * 03/20/13     #1774      randerso    Refactored out of GFEDao
 * 04/04/13     #1787      randerso    Fixed to support changes to D2D grid location
 *                                     Additional cleanup to move the D2D to GFE translation
 *                                     logic into D2DGridDatabase.
 * 05/03/13     #1974      randerso    Changed queryByParmId to look for parm with duration
 *                                     suffix first.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

// **********************************************************************
// TODO: this was moved out of GFEDao and needs to be cleaned up to better
// use the inherited GridDao functionality and hibernate instead of
// SQL/HQL queries. Some parts of the queries could be pushed up to
// GridDao
// **********************************************************************
public class GFED2DDao extends GridDao {
    private static final String FCST_TIME = "dataTime.fcstTime";

    private static final String REF_TIME = "dataTime.refTime";

    public GFED2DDao() throws PluginException {
        super();
    }

    /**
     * Retrieves a list of available forecast times
     * 
     * @param dbId
     *            The database ID to get the times for
     * @return The list of forecast times associated with the specified
     *         DatabaseID
     * @throws DataAccessLayerException
     *             If errors occur while querying the metadata database
     */
    public List<Integer> getForecastTimes(String d2dModelName, Date refTime)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(GridRecord.class.getName());
        query.addDistinctParameter(FCST_TIME);
        query.addQueryParam(GridConstants.DATASET_ID, d2dModelName);
        query.addQueryParam(REF_TIME, refTime);
        query.addOrder(FCST_TIME, true);

        @SuppressWarnings("unchecked")
        List<Integer> vals = (List<Integer>) this.queryByCriteria(query);
        return vals;
    }

    /**
     * Retrieves a GridRecord from the grib metadata database based on a ParmID,
     * TimeRange, and GridParmInfo.
     * 
     * @param id
     *            The parmID of the desired GridRecord
     * @param forecastTime
     *            The foreCast time of the desired GridRecord, null for any
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
            s = getHibernateTemplate().getSessionFactory().openSession();
            // TODO: clean up so we only make one db query
            SortedMap<Integer, Integer> rawTimes = queryByParmId(d2dModelName,
                    refTime, d2dParmName, d2dLevel, s);

            // if forecastTime is null just pick one,
            // this is for static data since all times are the same
            if (forecastTime == null) {
                forecastTime = rawTimes.keySet().iterator().next();
            }

            GridRecord retVal = (GridRecord) s.get(GridRecord.class,
                    rawTimes.get(forecastTime));
            retVal.setPluginName(GridConstants.GRID);
            return retVal;

        } finally {
            if (s != null) {
                try {
                    s.close();
                } catch (Exception e) {
                    statusHandler.error(
                            "Error occurred closing database session", e);
                }
            }
        }
    }

    /**
     * Gets a SortedMap of DataTime and GridRecord ids from the grib metadata
     * database which match the given ParmID. Session passed to allow reuse
     * across multiple calls.
     * 
     * @param id
     *            The ParmID to search with
     * @param s
     *            The database session to use
     * @return The list of GridRecords from the grib metadata database which
     *         match the given ParmID
     * @throws DataAccessLayerException
     *             If errors occur while querying the metadata database
     */
    public SortedMap<Integer, Integer> queryByParmId(String d2dModelName,
            Date refTime, String d2dParmName, Level d2dLevel, Session s)
            throws DataAccessLayerException {

        DatabaseQuery query;
        query = new DatabaseQuery(GridRecord.class.getName());
        query.addReturnedField(FCST_TIME);
        query.addReturnedField("id");
        query.addReturnedField(GridConstants.PARAMETER_ABBREVIATION);
        query.addQueryParam(GridConstants.DATASET_ID, d2dModelName);
        query.addQueryParam(REF_TIME, refTime);
        query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION, d2dParmName
                + "%hr", QueryOperand.LIKE);
        query.addQueryParam(GridConstants.LEVEL_ID, d2dLevel.getId());
        query.addOrder(FCST_TIME, true);
        query.addOrder(GridConstants.PARAMETER_ABBREVIATION, true);

        @SuppressWarnings("unchecked")
        List<Object[]> firstTry = (List<Object[]>) this.queryByCriteria(query);

        SortedMap<Integer, Integer> dataTimes = new TreeMap<Integer, Integer>();
        if (firstTry.isEmpty()) {
            query = new DatabaseQuery(GridRecord.class.getName());
            query.addReturnedField(FCST_TIME);
            query.addReturnedField("id");
            query.addQueryParam(GridConstants.DATASET_ID, d2dModelName);
            query.addQueryParam(REF_TIME, refTime);
            query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION,
                    d2dParmName);
            query.addQueryParam(GridConstants.LEVEL_ID, d2dLevel.getId());
            query.addOrder(FCST_TIME, true);

            @SuppressWarnings("unchecked")
            List<Object[]> secondTry = (List<Object[]>) this
                    .queryByCriteria(query);

            for (Object[] row : secondTry) {
                dataTimes.put((Integer) row[0], (Integer) row[1]);
            }
        } else {
            Pattern p = Pattern.compile("^" + d2dParmName + "(\\d+)hr$");
            int i = 0;
            while (i < firstTry.size()) {
                Object[] row = firstTry.get(i++);
                Integer fcstHr = (Integer) row[0];
                Integer id = (Integer) row[1];
                Matcher matcher = p.matcher((String) row[2]);
                int dur = Integer.MAX_VALUE;
                if (matcher.matches()) {
                    dur = Integer.parseInt(matcher.group(1));
                }

                for (int j = i; j < firstTry.size(); j++) {
                    Object[] nextRow = firstTry.get(j);
                    if (fcstHr.equals(nextRow[0])) {
                        i = j;
                        String nextParam = (String) nextRow[2];
                        Matcher nextMatcher = p.matcher(nextParam);
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
                dataTimes.put(fcstHr, id);
            }
        }
        return dataTimes;
    }

    /**
     * Retrieve the available Forecast Hours by D2D parm id.
     * 
     * @param id
     * @return the list of forecast hours
     * @throws DataAccessLayerException
     */
    public List<Integer> queryFcstHourByParmId(String d2dModelName,
            Date refTime, String d2dParmName, Level d2dLevel)
            throws DataAccessLayerException {
        List<Integer> timeList = new ArrayList<Integer>();
        Session s = null;
        try {
            s = getHibernateTemplate().getSessionFactory().openSession();

            SortedMap<Integer, Integer> results = queryByParmId(d2dModelName,
                    refTime, d2dParmName, d2dLevel, s);
            for (Integer o : results.keySet()) {
                timeList.add(o);
            }
        } finally {
            if (s != null) {
                try {
                    s.close();
                } catch (Exception e) {
                    statusHandler.error(
                            "Error occurred closing database session", e);
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
     * @return
     * @throws DataAccessLayerException
     */
    public List<Date> getModelRunTimes(String d2dModelName, int maxRecords)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(GridRecord.class.getName());
        query.addDistinctParameter(REF_TIME);
        query.addQueryParam(GridConstants.DATASET_ID, d2dModelName);
        query.addOrder(REF_TIME, false);
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
