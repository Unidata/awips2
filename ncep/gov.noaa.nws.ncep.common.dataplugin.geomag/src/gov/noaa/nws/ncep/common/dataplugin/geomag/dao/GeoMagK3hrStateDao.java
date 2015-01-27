/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.common.dataplugin.geomag.dao;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagK3hrState;
import gov.noaa.nws.ncep.common.dataplugin.geomag.table.GeoMagStation;
import gov.noaa.nws.ncep.common.dataplugin.geomag.util.GeoMagStationLookup;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.transaction.TransactionException;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;

import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Dao class for GeoMagK3hrState.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer           Description
 * ------------ ---------- ----------------   --------------------------
 * 06/27/2014   R4078       sgurung            Initial creation.
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
public class GeoMagK3hrStateDao extends CoreDao {

    /** The logger */
    protected transient Log logger = LogFactory.getLog(getClass());

    /**
     * Creates a new GeoMagK3hrStateDao
     */
    public GeoMagK3hrStateDao() {
        super(DaoConfig.forClass(GeoMagK3hrState.class));
    }

    /**
     * Retrieves a GeoMagK3hrState based on the given id
     * 
     * @param id
     *            The given ID number
     * @return The GeoMagK3hrState object associated with the given id
     */
    public GeoMagK3hrState queryById(int id) {
        return (GeoMagK3hrState) super.queryById(id);
    }

    /**
     * Retrieves list of all GeoMagK3hrState objects
     * 
     * @return list of GeoMagK3hrState objects
     */
    @SuppressWarnings("unchecked")
    public List<GeoMagK3hrState> getAllStationStateChanges() {
        return (List<GeoMagK3hrState>) txTemplate
                .execute(new TransactionCallback() {
                    @Override
                    public Object doInTransaction(TransactionStatus status) {
                        Session sess = getCurrentSession();
                        Criteria crit = sess
                                .createCriteria(GeoMagK3hrState.class);
                        return crit.list();
                    }
                });
    }

    /**
     * Returns the processing states for a list of magnetometer stations (for a
     * given synoptic period, if provided)<br>
     * 
     * @param stations
     *            A list of stations
     * @param reftime
     *            time tag
     * @return List of data map containing "reftime", "stationCode",
     *         "processingState"
     * @throws Exception
     */
    public List<Map<String, Object>> getKpStationsStates(
            final List<String> stations, final Date reftime) throws Exception {

        StringBuffer sql = new StringBuffer();
        sql.append(" SELECT a.reftime as reftime, a.stationcode as stationcode, c.processingState as processingstate FROM geomag_k3hr AS a"
                + " INNER JOIN  geomag_k3hr_state AS b ON b.k3hrId = a.id "
                + " INNER JOIN geomag_states AS c ON c.stateid =  b.stateid");

        sql.append(" WHERE ");

        if (stations != null) {
            sql.append(" a.stationcode IN ('");

            String station = null;
            for (int i = 0; i < stations.size(); i++) {
                station = stations.get(i);
                sql.append(station).append("'");
                if (i != (stations.size() - 1)) {
                    sql.append(",'");
                }

            }
            sql.append(") AND");
        }
        sql.append(" a.reftime = '" + reftime + "'");
        // sql.append(" AND a.kestreal <= 9.0 ");
        sql.append("ORDER BY a.stationcode ASC, c.processingState ASC");

        logger.info("Inside GeoMagK3hrStateDao.getKpStationsStates(), sql = "
                + sql.toString());

        Object[] results = executeSQLQuery(sql.toString());

        if (results.length == 0) {
            return new ArrayList<Map<String, Object>>();
        }

        String[] fieldNames = { "reftime", "stationcode", "processingstate" };
        List<Map<String, Object>> resultMaps = new ArrayList<Map<String, Object>>(
                results.length);
        for (Object obj : results) {
            if (obj instanceof Object[] == false) {
                obj = new Object[] { obj };
            }
            Object[] objs = (Object[]) obj;
            if (objs.length != fieldNames.length) {
                throw new Exception(
                        "Column count returned does not match expected column count");
            }
            Map<String, Object> resultMap = new HashMap<String, Object>(
                    objs.length * 2);
            for (int i = 0; i < fieldNames.length; ++i) {
                resultMap.put(fieldNames[i], objs[i]);
            }
            resultMaps.add(resultMap);
        }

        return resultMaps;

    }

    /**
     * Returns the processing states for a given magnetometer station (for a
     * given synoptic period, if provided)<br>
     * 
     * @param station
     *            Station code
     * @param reftime
     *            time tag
     * @return List of strings containing "processingstate"
     * @throws Exception
     */
    public List<String> getKpStationStates(final String station,
            final Date reftime) throws Exception {

        StringBuffer sql = new StringBuffer();
        sql.append(" SELECT c.processingState as processingstate FROM geomag_k3hr AS a"
                + " INNER JOIN  geomag_k3hr_state AS b ON b.k3hrId = a.id "
                + " INNER JOIN geomag_states AS c ON c.stateid =  b.stateid");
        sql.append(" WHERE stationcode = '" + station + "'");
        sql.append(" AND");
        sql.append(" a.reftime = '" + reftime + "'");
        // sql.append(" AND a.kestreal <= 9.0 ");
        sql.append("ORDER BY c.processingState ASC");

        logger.info("Inside GeoMagK3hrStateDao.getKpStationStates(), sql = "
                + sql.toString());

        Object[] results = executeSQLQuery(sql.toString());

        if (results.length == 0) {
            return new ArrayList<String>();
        }

        String[] fieldNames = { "processingstate" };
        List<String> resultLst = new ArrayList<String>(results.length);
        for (Object obj : results) {
            if (obj instanceof Object[] == false) {
                obj = new Object[] { obj };
            }
            Object[] objs = (Object[]) obj;
            if (objs.length != fieldNames.length) {
                throw new Exception(
                        "Column count returned does not match expected column count");
            }
            resultLst.add((String) objs[0]);
        }

        return resultLst;

    }

    /**
     * Returns the processing state id for a given station code, reftime and
     * processing state.<br>
     * 
     * @param stationCode
     *            Station code
     * @param prevTime
     *            time tag
     * @param state
     *            processing state
     * @return stateId
     * @throws Exception
     */
    public Integer getStationK3hrStateId(final String stationCode,
            final Date prevTime, String state) throws Exception {
        Integer stateId = null;

        StringBuffer sql = new StringBuffer();
        sql.append(" SELECT b.k3hrId FROM geomag_k3hr AS a"
                + " INNER JOIN  geomag_k3hr_state AS b ON b.k3hrId = a.id "
                + " INNER JOIN geomag_states AS c ON c.stateid =  b.stateid");
        sql.append(" WHERE a.stationcode = '" + stationCode + "'");
        sql.append(" AND a.reftime = '" + prevTime + "'");

        if (state != null) {
            sql.append(" AND c.processingState = '" + state + "'");
        }

        logger.info("Inside GeoMagK3hrStateDao.getStationK3hrStateId(), sql = "
                + sql.toString());

        Object[] results = executeSQLQuery(sql.toString());

        if (results.length == 0) {
            return null;
        }

        String[] fieldNames = { "stateid" };
        Object obj = results[0];
        if (obj instanceof Object[] == false) {
            obj = new Object[] { obj };
        }
        Object[] objs = (Object[]) obj;
        if (objs.length != fieldNames.length) {
            throw new Exception(
                    "Column count returned does not match expected column count");
        }
        stateId = (Integer) objs[0];

        return stateId;
    }

    /**
     * Returns the processing state id for a given station code, reftime and
     * processing state.<br>
     * 
     * @param stationCode
     *            Station code
     * @param prevTime
     *            time tag
     * @param state
     *            processing state
     * @return stateId
     * @throws Exception
     */
    public Integer getStationStateId(final String stationCode,
            final Date prevTime, String state) throws Exception {
        Integer stateId = null;

        StringBuffer sql = new StringBuffer();
        sql.append(" SELECT c.stateid FROM geomag_k3hr AS a"
                + " INNER JOIN  geomag_k3hr_state AS b ON b.k3hrId = a.id "
                + " INNER JOIN geomag_states AS c ON c.stateid =  b.stateid");
        sql.append(" WHERE a.stationcode = '" + stationCode + "'");
        sql.append(" AND a.reftime = '" + prevTime + "'");

        if (state != null) {
            sql.append(" AND c.processingState = '" + state + "'");
        }

        logger.info("Inside GeoMagK3hrStateDao.getStationStateId(), sql = "
                + sql.toString());

        Object[] results = executeSQLQuery(sql.toString());

        if (results.length == 0) {
            return null;
        }

        System.out.println(" results.length = " + results.length);

        String[] fieldNames = { "stateid" };
        Object obj = results[0];
        if (obj instanceof Object[] == false) {
            obj = new Object[] { obj };
        }
        Object[] objs = (Object[]) obj;
        if (objs.length != fieldNames.length) {
            throw new Exception(
                    "Column count returned does not match expected column count");
        }
        stateId = (Integer) objs[0];

        return stateId;
    }

    /**
     * Updates the processing states when a new record is inserted for a
     * magnetometer station in the geomag_k3hr table.<br>
     * 
     * @param k3hrId
     *            k3hrId
     * @param stationCode
     *            stationCode
     * @return reftime time tag
     * @throws Exception
     */
    public void updateStates(final Integer k3hrId, final String stationCode,
            final Date refTime) throws TransactionException {

        GeoMagK3hrDao k3hrDao = new GeoMagK3hrDao();
        GeoMagStateDao stateDao = new GeoMagStateDao();

        boolean kpStation = false;

        Map<String, ArrayList<GeoMagStation>> allStations = GeoMagStationLookup
                .getInstance().getStationsByCodeMap();
        if (allStations != null) {
            ArrayList<GeoMagStation> geomagStnsSameCode = allStations
                    .get(stationCode);

            if (geomagStnsSameCode != null) {
                for (int i = 0; i < geomagStnsSameCode.size(); i++) {
                    GeoMagStation stn = geomagStnsSameCode.get(i);
                    if (stn.getKpStation() == 1) {
                        kpStation = true;
                    }
                }
            }
        }

        try {
            Date previousTime = k3hrDao.getStationMaxPrevTime(stationCode,
                    refTime);

            // logger.info("Inside GeoMagK3hrStateDao.updateStates(), previousTime = "
            // + previousTime);

            Integer stateId = stateDao.getStateByProcessingState("Process")
                    .getStateId();

            if (stateId != null) {

                // To get here it would have already been set to Process.
                GeoMagK3hrState k3hrStateRec = new GeoMagK3hrState();
                k3hrStateRec.setK3hrId(k3hrId);
                k3hrStateRec.setStateId(stateId);
                persist(k3hrStateRec);

                if (kpStation) {
                    // if station is a kpstation, set to Algorithm
                    stateId = stateDao.getStateByProcessingState("Algorithm")
                            .getStateId();

                    k3hrStateRec = new GeoMagK3hrState();
                    k3hrStateRec.setK3hrId(k3hrId);
                    k3hrStateRec.setStateId(stateId);
                    persist(k3hrStateRec);

                    if (previousTime != null) {
                        // if previous period exists and if station state is
                        // active for that period,
                        // insert as active
                        stateId = getStationStateId(stationCode, previousTime,
                                "Active");

                        // logger.info("Inside GeoMagK3hrStateDao.updateStates(), stateId = "
                        // + stateId);

                        // if there is no data for previous synoptic period,
                        // then look for stateId
                        // based on the last data date
                        // if (stateId == null) {
                        // List<String> stnList = new ArrayList<String>();
                        // stnList.add(stationCode);
                        // GeoMagK1minDao dao = new GeoMagK1minDao();
                        // Date lastDataDate = dao.getLastDataDate(stnList,
                        // null, null);
                        // logger.info("Inside GeoMagK3hrStateDao.updateStates(), lastDataDate = "
                        // + lastDataDate);
                        //
                        // if (lastDataDate != null) {
                        // stateId = getStationStateId(stationCode,
                        // lastDataDate, "Active");
                        //
                        // logger.info("Inside GeoMagK3hrStateDao.updateStates(), new stateId = "
                        // + stateId);
                        //
                        // }
                        // }

                        if (stateId != null) {
                            k3hrStateRec = new GeoMagK3hrState();
                            k3hrStateRec.setK3hrId(k3hrId);
                            k3hrStateRec.setStateId(stateId);
                            persist(k3hrStateRec);
                        }
                    }

                }

            }

        } catch (DataIntegrityViolationException de) {
            // logger.info("Record with given stateId and k3hr combination exists.");
        } catch (Exception e) {
            logger.error("Error while updating processing states.", e);
        }

    }

    public int delete(Integer stateId, Integer k3hrId)
            throws DataAccessLayerException {
        DatabaseQuery deleteStmt = new DatabaseQuery(this.daoClass);
        deleteStmt.addQueryParam("stateId", stateId);
        deleteStmt.addQueryParam("k3hrId", k3hrId);

        return this.deleteByCriteria(deleteStmt);
    }

    public int purgeDataByK3hrId(Integer k3hrId)
            throws DataAccessLayerException {
        DatabaseQuery deleteStmt = new DatabaseQuery(this.daoClass);
        deleteStmt.addQueryParam("k3hrId", k3hrId);

        return this.deleteByCriteria(deleteStmt);
    }

}
