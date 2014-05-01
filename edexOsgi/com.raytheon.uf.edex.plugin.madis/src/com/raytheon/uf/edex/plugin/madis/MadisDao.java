package com.raytheon.uf.edex.plugin.madis;

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

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.madis.MadisRecord;
import com.raytheon.uf.common.dataquery.db.QueryParam;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.pointdata.PointDataDbDescription;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;
import com.raytheon.uf.edex.pointdata.spatial.ObStationDao;

/**
 * MadisDao MADIS data DAO
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * MAR 27, 2013 1746       dhladky     MADIS data record creation
 * Jun 11, 2013 2090       djohnson    Override purgeDataByRefTime to improve purge performance.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class MadisDao extends PointDataPluginDao<MadisRecord> {

    /** The station dao */
    private ObStationDao obDao = new ObStationDao();

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MadisDao.class);

    public List<?> queryBySpatialBox(double upperLeftLat, double upperLeftLon,
            double lowerRightLat, double lowerRightLon)
            throws DataAccessLayerException {

        List<ObStation> stationList = obDao.queryBySpatialBox(upperLeftLat,
                upperLeftLon, lowerRightLat, lowerRightLon);

        List<String> stationNames = new ArrayList<String>();
        for (ObStation ob : stationList) {
            stationNames.add(ob.getIcao());
        }

        DatabaseQuery query = new DatabaseQuery(MadisRecord.class);
        query.addQueryParam("location.stationId", stationNames,
                QueryParam.QueryOperand.IN);
        return queryByCriteria(query);
    }

    public List<?> queryByState(String state, Integer count)
            throws DataAccessLayerException {

        ArrayList<String> icaos = new ArrayList<String>();
        DatabaseQuery query = new DatabaseQuery(MadisRecord.class, count);
        query.addQueryParam("location.stationId", icaos,
                QueryParam.QueryOperand.IN);
        return queryByCriteria(query);
    }

    /**
     * Queries for to determine if a given data uri exists on the sfcobs table.
     * 
     * @param dataUri
     *            The DataURI to find.
     * @return An array of objects. If not null, there should only be a single
     *         element.
     */
    public Object[] queryDataUriColumn(final String dataUri) {

        String sql = "select datauri from awips.madis where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }

    /**
     * Constructs a new obs dao.
     * 
     * @throws PluginException
     */
    public MadisDao(String pluginName) throws PluginException {

        super(pluginName);

        try {
            hdf5DataDescription = PointDataDescription.fromStream(this
                    .getClass().getResourceAsStream(
                            "/res/pointdata/" + pluginName + ".xml"));
        } catch (SerializationException e) {
            statusHandler.error("Unable to load madis Point Data Description",
                    e);
            throw new PluginException(
                    "Unable to load madis Point Data Description!", e);
        }

    }

    @Override
    public PointDataDescription getPointDataDescription(Map<String, Object> obj) {
        return hdf5DataDescription;
    }

    public ObStationDao getObDao() {
        return obDao;
    }

    public void setObDao(ObStationDao obDao) {
        this.obDao = obDao;
    }

    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime" };
    }

    @Override
    public String getPointDataFileName(MadisRecord p) {
        return "madis.h5";
    }

    @Override
    public MadisRecord newObject() {
        return new MadisRecord();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.pointdata.PointDataPluginDao#getPointDataDbDescription
     * ()
     */
    @Override
    public PointDataDbDescription getPointDataDbDescription() {
        if (dbDataDescription == null) {
            InputStream stream = this.getClass().getResourceAsStream(
                    "/res/pointdata/madisdb.xml");
            if (stream != null) {
                try {
                    dbDataDescription = PointDataDbDescription
                            .fromStream(stream);
                } catch (JAXBException e) {
                    statusHandler.error("Unable to load " + pluginName
                            + " Point Data Database Description", e);
                } finally {
                    if (stream != null) {
                        try {
                            stream.close();
                        } catch (IOException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage(), e);
                        }
                    }
                }
            }
        }
        return dbDataDescription;
    }

    /**
     * Overridden because {@link PluginDao} retrieves all PDO instances prior to
     * purging them, in order to calculute HDF5 paths for each one. In the case
     * of {@link Madis} objects, the granularity is only down to the hour level
     * therefore we can just pull one, calculate the HDF5 path, and purge all
     * entities without retrieving them.
     */
    @Override
    public int purgeDataByRefTime(Date refTime,
            Map<String, String> productKeys, boolean trackHdf5,
            boolean trackToUri, Map<String, List<String>> hdf5FileToUriPurged)
            throws DataAccessLayerException {

        DatabaseQuery dataQuery = new DatabaseQuery(this.daoClass);

        if (refTime != null) {
            dataQuery.addQueryParam(PURGE_VERSION_FIELD, refTime);
        }

        if ((productKeys != null) && (productKeys.size() > 0)) {
            for (Map.Entry<String, String> pair : productKeys.entrySet()) {
                dataQuery.addQueryParam(pair.getKey(), pair.getValue());
            }
        }

        // Keep track of the old max results so we can use the same query to
        // find an example pdo to get the HDF5 path
        final Integer oldMaxResults = dataQuery.getMaxResults();
        dataQuery.setMaxResults(1);
        @SuppressWarnings("unchecked")
        final List<PluginDataObject> pdos = (List<PluginDataObject>) this
                .queryByCriteria(dataQuery);
        if (CollectionUtil.isNullOrEmpty(pdos)) {
            return 0;
        }

        // Restore the old max results so it targets all entities
        dataQuery.setMaxResults(oldMaxResults);
        int numberDeleted = this.deleteByCriteria(dataQuery);

        if (trackHdf5 && (hdf5FileToUriPurged != null)) {
            purgeHdf5ForPdos(trackToUri, hdf5FileToUriPurged, pdos);
        }

        return numberDeleted;
    }

}
