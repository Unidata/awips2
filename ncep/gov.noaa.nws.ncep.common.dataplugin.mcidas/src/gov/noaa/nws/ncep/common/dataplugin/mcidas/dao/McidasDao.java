/**
 * This is a Data Access Object (DAO) driver to interact with McIDAS image 
 * properties (satellite name and image type) and HDF5 data store.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * ------------ ---------- 	----------- --------------------------
 * 10/2009		144			T. Lee		Created
 * 11/2009		144			T. Lee		Implemented area name DAO
 * </pre>
 * 
 * @author tlee
 * @version 1.0
 */

package gov.noaa.nws.ncep.common.dataplugin.mcidas.dao;

import java.util.List;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.records.AbstractStorageRecord;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.edex.database.plugin.PluginDao;

import gov.noaa.nws.ncep.common.dataplugin.mcidas.McidasRecord;
import gov.noaa.nws.ncep.common.dataplugin.mcidas.fixed.McidasAreaName;
import gov.noaa.nws.ncep.common.dataplugin.mcidas.fixed.McidasSatelliteName;
import gov.noaa.nws.ncep.common.dataplugin.mcidas.fixed.McidasImageType;

public class McidasDao extends PluginDao {
    private McidasSatelliteNameDao satelliteNameDao = new McidasSatelliteNameDao();    
    private McidasImageTypeDao imageTypeDao = new McidasImageTypeDao();
    private McidasAreaNameDao areaNameDao = new McidasAreaNameDao();


    public McidasDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore, IPersistable record) 
    		throws StorageException {
        McidasRecord satRecord = (McidasRecord) record;

        /*
         * Write McIDAS Area file header block.
         */
        if (satRecord.getHeaderBlock() != null ) {
        	AbstractStorageRecord storageRecord = new ByteDataRecord("Header", 
        			satRecord.getDataURI(), (byte[]) satRecord.getHeaderBlock(), 1, 
        			new long[] {satRecord.getHeaderBlock().length});
        	storageRecord.setCorrelationObject(satRecord);
        	dataStore.addDataRecord(storageRecord);        
        }
 
        /*
         * Write McIDAS image data block to HDF5.
         */
        if ( satRecord.getMessageData() != null ) {
        	long xdim = satRecord.getCoverage().getNx();
        	long ydim = satRecord.getCoverage().getNy();
        	long[] sizes = new long[] { xdim, ydim };        
        	AbstractStorageRecord storageRecord = new ByteDataRecord("Data", 
        			satRecord.getDataURI(), (byte[]) satRecord.getMessageData(), 2, sizes);
        	
        	StorageProperties props = new StorageProperties();
        	
        	props.setDownscaled(true);
//        	props.setChunked(true);
//        	props.setCompressed(true);
        	
        	storageRecord.setProperties(props);
        	storageRecord.setCorrelationObject(satRecord);
        	dataStore.addDataRecord(storageRecord);        	
        }
        return dataStore;
    }

    /*
     * Get satellite name from satellite ID
     */
    		
    public McidasSatelliteName getSatelliteId(int satelliteId) {
    	return satelliteNameDao.queryById(satelliteId);
    }

    public McidasSatelliteNameDao getSatellitenNameDao() {
    	return satelliteNameDao;
    }

    public void setSatellitenNameDao(McidasSatelliteNameDao satellitenNameDao) {
    	this.satelliteNameDao = satellitenNameDao;
    }

    /*
     * Get image type from satellite ID and image number
     */
    public List<McidasImageType> getImageType(String satelliteId, String imageNumber) {
    	return imageTypeDao.queryBySatelliteIdAndImageNumber(satelliteId, imageNumber);
    }
    
    public McidasImageTypeDao getImageTypeDao() {
    	return imageTypeDao;
    }

    public void setImageTypeDao(
    		McidasImageTypeDao imageTypeDao) {
    	this.imageTypeDao = imageTypeDao;
    }   
    
    /*
     * Get area name from area file number
     */
    public McidasAreaName getAreaId(int areaId) {
    	return areaNameDao.queryById(areaId);
    }
    
    public McidasAreaNameDao getAreaNameDao() {
    	return areaNameDao;
    }

    public void setMcidasAreaNameDao(
    		McidasAreaNameDao areaNameDao) {
    	this.areaNameDao = areaNameDao;
    }   
}