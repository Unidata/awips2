/**
 * This software was modified from Raytheon's taf plugin by
 * NOAA/NWS/NCEP/NCO in order to output point data in HDF5.
 **/
package gov.noaa.nws.ncep.common.dataplugin.nctaf.dao;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBException;

import gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafRecord;

import com.raytheon.edex.db.dao.spatial.ObStationDao;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataquery.db.QueryParam;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.pointdata.PointDataDbDescription;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.spatial.ObStation;

/**
 * Set of DAO methods for TAF data.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 09/09/2011   458			sgurung	   Initial Creation from Raytheon's taf plugin
 * 09/23/2011   458			sgurung	   Converted to HDF5
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class NcTafDao extends PointDataPluginDao<NcTafRecord> {

	/** The station dao */
    private ObStationDao obDao = new ObStationDao();
      
    /**
     * Creates a new NcTafDao
     * @throws PluginException 
     */
    public NcTafDao(String pluginName) throws PluginException {
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

        DatabaseQuery query = new DatabaseQuery(NcTafRecord.class);
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

        DatabaseQuery query = new DatabaseQuery(NcTafRecord.class, count);
        query.addQueryParam("location.stationId", icaos,
                QueryParam.QueryOperand.IN);
        return queryByCriteria(query);
    } 
    
    /**
     * Retrieves an nctaf report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public NcTafRecord queryByDataURI(String dataURI) {
    	NcTafRecord report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if((obs != null)&&(obs.size() > 0)) {
            report = (NcTafRecord) obs.get(0);
        }
        return report;
    }
    
    /**
     * Queries for to determine if a given data uri exists on the nctaf table.
     * 
     * @param dataUri
     *            The DataURI to find.
     * @return An array of objects. If not null, there should only be a single
     * element.
     */
    public Object[] queryDataUriColumn(final String dataUri) {

        String sql = "select datauri from awips.nctaf where datauri='"
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
    public String getPointDataFileName(NcTafRecord p) {
        return "nctafs.h5";
    }

    @Override
    public NcTafRecord newObject() {
        return new NcTafRecord();
    }

    public PointDataDescription getPointDataDescription() throws JAXBException {
        if (hdf5DataDescription == null) {
        	hdf5DataDescription = PointDataDescription.fromStream(this.getClass()
                    .getResourceAsStream("/res/pointdata/nctaf.xml"));
            
        }
        return hdf5DataDescription;
    }
    
    @Override
    public PointDataDbDescription getPointDataDbDescription() {
        if (dbDataDescription == null) {
            InputStream stream = this.getClass().getResourceAsStream(
                    "/res/pointdata/nctafdb.xml");
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
