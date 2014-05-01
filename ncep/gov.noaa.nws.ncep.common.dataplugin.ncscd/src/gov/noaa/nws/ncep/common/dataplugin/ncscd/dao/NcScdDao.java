/**
 * NcScdDao
 * 
 * This java class defines data access object for SCD data. 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    	Description
 * ------------ ---------- ----------- 	--------------------------
 * 06/30/2011				F. J. Yen	Renamed and converted ScdDao to HDF5.
 * 09/13/2011   457         S. Gurung   Renamed H5 to Nc and h5 to nc
 * 
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system. 
 * @author T. Lee
 * @version 1.0 
 */

package gov.noaa.nws.ncep.common.dataplugin.ncscd.dao;

import gov.noaa.nws.ncep.common.dataplugin.ncscd.NcScdRecord;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataquery.db.QueryParam;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.pointdata.PointDataDbDescription;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;
import com.raytheon.uf.edex.pointdata.spatial.ObStationDao;

public class NcScdDao extends PointDataPluginDao<NcScdRecord> {

    /** The station dao */
    private ObStationDao obDao = new ObStationDao();

    /**
     * Creates a new NcScdDao
     * 
     * @throws PluginException
     */
    public NcScdDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        // TODO Auto-generated method stub
        return null;
    }

    public List<?> queryBySpatialBox(double upperLeftLat, double upperLeftLon,
            double lowerRightLat, double lowerRightLon)
            throws DataAccessLayerException {

        List<ObStation> stationList = obDao.queryBySpatialBox(upperLeftLat,
                upperLeftLon, lowerRightLat, lowerRightLon);

        List<String> stationNames = new ArrayList<String>();
        // for(ObStation station:stationList){
        // stationNames.add((String)station.getIdentifier());
        // }
        stationList.clear();

        DatabaseQuery query = new DatabaseQuery(NcScdRecord.class);
        query.addQueryParam("location.stationId", stationNames,
                QueryParam.QueryOperand.IN);
        return queryByCriteria(query);
    }

    public List<?> queryByState(String state, Integer count)
            throws DataAccessLayerException {

        // List<ObStation> results = obDao.queryByState(state);

        ArrayList<String> icaos = new ArrayList<String>();
        // for (ObStation station : results) {
        // //icaos.add((String)station.getIdentifier());
        // }

        DatabaseQuery query = new DatabaseQuery(NcScdRecord.class, count);
        query.addQueryParam("location.stationId", icaos,
                QueryParam.QueryOperand.IN);
        return queryByCriteria(query);
    }

    /**
     * Retrieves an ncscd report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public NcScdRecord queryByDataURI(String dataURI) {
        NcScdRecord report = null;
        List<?> ncscd = null;
        try {
            ncscd = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if ((ncscd != null) && (ncscd.size() > 0)) {
            report = (NcScdRecord) ncscd.get(0);
        }
        return report;
    }

    /**
     * Queries for to determine if a given data uri exists on the ncscd table.
     * 
     * @param dataUri
     *            The DataURI to find.
     * @return An array of objects. If not null, there should only be a single
     *         element.
     */
    public Object[] queryDataUriColumn(final String dataUri) {

        String sql = "select datauri from awips.ncscd where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
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
    public String getPointDataFileName(NcScdRecord p) {
        return "ncscd.h5";
    }

    @Override
    public NcScdRecord newObject() {
        return new NcScdRecord();
    }

    @Override
    public PointDataDescription getPointDataDescription(Map<String, Object> obj) {
        if (hdf5DataDescription == null) {
            try {
                hdf5DataDescription = PointDataDescription.fromStream(this
                        .getClass().getResourceAsStream(
                                "/res/pointdata/ncscd.xml"));
            } catch (SerializationException e) {
                logger.error("Unable to load ncscd Point Data Description", e);
            }
        }
        return hdf5DataDescription;
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
                    "/res/pointdata/ncscddb.xml");
            if (stream != null) {
                try {
                    dbDataDescription = PointDataDbDescription
                            .fromStream(stream);
                } catch (JAXBException e) {
                    logger.error("Unable to load " + pluginName
                            + " Point Data Database Description", e);
                }
            }
        }
        return dbDataDescription;
    }

}
