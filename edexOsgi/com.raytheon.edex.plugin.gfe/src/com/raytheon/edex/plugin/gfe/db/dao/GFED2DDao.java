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
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hibernate.Query;
import org.hibernate.SQLQuery;
import org.hibernate.Session;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.edex.plugin.gfe.util.GridTranslator;
import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridInfoConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.parameter.mapping.ParameterMapper;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.mapping.MultipleMappingException;
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

    // hibernate query to find grid info record for the given datasetId and
    // parameter
    private static final String SQL_D2D_GRID_PARM_QUERY = "select parameter_abbreviation, id "
            + "FROM grid_info WHERE "
            + GridInfoConstants.DATASET_ID
            + " = :"
            + GridInfoConstants.DATASET_ID
            + " AND "
            + "level_id = :level_id  AND "
            + "(lower(parameter_abbreviation) = :abbrev OR lower(parameter_abbreviation) like :hourAbbrev)";

    // hibernate query to find the times for the GridRecord for the given
    // info.id, id returned to allow easy lookup of the record associated with
    // the time
    private static final String HQL_D2D_GRID_TIME_QUERY = "select dataTime.fcstTime, id from GridRecord "
            + "where "
            + GridConstants.INFO_ID
            + " = :info_id AND dataTime.refTime = :refTime order by dataTime.fcstTime";

    private static final Pattern WIND_PATTERN = Pattern.compile("wind");

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
    public List<Integer> getD2DForecastTimes(DatabaseID dbId)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(GridRecord.class.getName());
        query.addDistinctParameter(FCST_TIME);
        try {
            IFPServerConfig config = IFPServerConfigManager
                    .getServerConfig(dbId.getSiteId());
            query.addQueryParam(GridConstants.DATASET_ID,
                    config.d2dModelNameMapping(dbId.getModelName()));
        } catch (GfeConfigurationException e) {
            throw new DataAccessLayerException(
                    "Error occurred looking up model name mapping", e);
        }
        query.addQueryParam(REF_TIME, dbId.getModelTimeAsDate());
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
    public GridRecord getD2DGrid(ParmID id, Integer forecastTime,
            GridParmInfo info) throws DataAccessLayerException {
        Session s = null;

        try {
            s = getHibernateTemplate().getSessionFactory().openSession();
            // TODO: clean up so we only make one db query
            SortedMap<Integer, Integer> rawTimes = queryByD2DParmId(id, s);

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
    public SortedMap<Integer, Integer> queryByD2DParmId(ParmID id, Session s)
            throws DataAccessLayerException {
        String levelName = GridTranslator.getLevelName(id.getParmLevel());

        double[] levelValues = GridTranslator.getLevelValue(id.getParmLevel());
        boolean levelOnePresent = (levelValues[0] != Level
                .getInvalidLevelValue());
        boolean levelTwoPresent = (levelValues[1] != Level
                .getInvalidLevelValue());
        Level level = null;

        // to have a level 2, must have a level one
        try {
            if (levelOnePresent && levelTwoPresent) {
                level = LevelFactory.getInstance().getLevel(levelName,
                        levelValues[0], levelValues[1]);
            } else if (levelOnePresent) {
                level = LevelFactory.getInstance().getLevel(levelName,
                        levelValues[0]);
            } else {
                level = LevelFactory.getInstance().getLevel(levelName, 0.0);
            }
        } catch (CommunicationException e) {
            logger.error(e.getLocalizedMessage(), e);
        }
        if (level == null) {
            logger.warn("Unable to query D2D parms, ParmID " + id
                    + " does not map to a level");
            return new TreeMap<Integer, Integer>();
        }

        SQLQuery modelQuery = s.createSQLQuery(SQL_D2D_GRID_PARM_QUERY);
        modelQuery.setLong("level_id", level.getId());
        DatabaseID dbId = id.getDbId();

        try {
            IFPServerConfig config = IFPServerConfigManager
                    .getServerConfig(dbId.getSiteId());
            modelQuery.setString(GridInfoConstants.DATASET_ID,
                    config.d2dModelNameMapping(dbId.getModelName()));
        } catch (GfeConfigurationException e) {
            throw new DataAccessLayerException(
                    "Error occurred looking up model name mapping", e);
        }

        String abbreviation = null;
        try {
            abbreviation = ParameterMapper.getInstance().lookupBaseName(
                    id.getParmName(), "gfeParamName");
        } catch (MultipleMappingException e) {
            statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);
            abbreviation = e.getArbitraryMapping();
        }

        abbreviation = abbreviation.toLowerCase();
        modelQuery.setString("abbrev", abbreviation);
        modelQuery.setString("hourAbbrev", abbreviation + "%hr");

        @SuppressWarnings("unchecked")
        List<Object[]> results = modelQuery.list();

        Integer modelId = null;
        if (results.size() == 0) {
            return new TreeMap<Integer, Integer>();
        } else if (results.size() > 1) {
            // hours matched, take hour with least number that matches exact
            // param
            Pattern p = Pattern.compile("^" + abbreviation + "(\\d+)hr$");
            int lowestHr = -1;
            for (Object[] rows : results) {
                String param = ((String) rows[0]).toLowerCase();
                if (param.equals(abbreviation) && (lowestHr < 0)) {
                    modelId = (Integer) rows[1];
                } else {
                    Matcher matcher = p.matcher(param);
                    if (matcher.matches()) {
                        int hr = Integer.parseInt(matcher.group(1));
                        if ((lowestHr < 0) || (hr < lowestHr)) {
                            modelId = (Integer) rows[1];
                            lowestHr = hr;
                        }
                    }
                }
            }
        } else {
            modelId = (Integer) (results.get(0))[1];
        }

        Query timeQuery = s.createQuery(HQL_D2D_GRID_TIME_QUERY);
        timeQuery.setInteger("info_id", modelId);
        timeQuery.setParameter("refTime", dbId.getModelTimeAsDate());

        @SuppressWarnings("unchecked")
        List<Object[]> timeResults = timeQuery.list();

        if (timeResults.isEmpty()) {
            return new TreeMap<Integer, Integer>();
        }

        SortedMap<Integer, Integer> dataTimes = new TreeMap<Integer, Integer>();
        for (Object[] rows : timeResults) {
            dataTimes.put((Integer) rows[0], (Integer) rows[1]);
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
    public List<Integer> queryFcstHourByD2DParmId(ParmID id)
            throws DataAccessLayerException {
        List<Integer> timeList = new ArrayList<Integer>();
        Session s = null;
        try {
            s = getHibernateTemplate().getSessionFactory().openSession();

            if (id.getParmName().equalsIgnoreCase("wind")) {
                String idString = id.toString();
                Matcher idWindMatcher = WIND_PATTERN.matcher(idString);

                ParmID uWindId = new ParmID(idWindMatcher.replaceAll("uW"));
                SortedMap<Integer, Integer> results = queryByD2DParmId(uWindId,
                        s);
                List<Integer> uTimeList = new ArrayList<Integer>(results.size());
                for (Integer o : results.keySet()) {
                    uTimeList.add(o);
                }

                ParmID vWindId = new ParmID(idWindMatcher.replaceAll("vW"));
                results = queryByD2DParmId(vWindId, s);
                Set<Integer> vTimeList = new HashSet<Integer>(results.size(), 1);
                for (Integer o : results.keySet()) {
                    vTimeList.add(o);
                }

                for (Integer tr : uTimeList) {
                    if (vTimeList.contains(tr)) {
                        timeList.add(tr);
                    }
                }

                if (!timeList.isEmpty()) {
                    return timeList;
                }

                ParmID sWindId = new ParmID(idWindMatcher.replaceAll("ws"));
                results = queryByD2DParmId(sWindId, s);
                List<Integer> sTimeList = new ArrayList<Integer>(results.size());
                for (Integer o : results.keySet()) {
                    sTimeList.add(o);
                }

                ParmID dWindId = new ParmID(idWindMatcher.replaceAll("wd"));
                results = queryByD2DParmId(dWindId, s);
                Set<Integer> dTimeList = new HashSet<Integer>(results.size(), 1);
                for (Integer o : results.keySet()) {
                    dTimeList.add(o);
                }

                for (Integer tr : sTimeList) {
                    if (dTimeList.contains(tr)) {
                        timeList.add(tr);
                    }
                }
            } else {
                SortedMap<Integer, Integer> results = queryByD2DParmId(id, s);
                for (Integer o : results.keySet()) {
                    timeList.add(o);
                }
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
    public List<Date> getD2DModelRunTimes(String d2dModelName, int maxRecords)
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
