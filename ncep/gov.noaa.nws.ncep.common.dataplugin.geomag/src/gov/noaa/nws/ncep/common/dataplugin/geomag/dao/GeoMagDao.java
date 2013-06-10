package gov.noaa.nws.ncep.common.dataplugin.geomag.dao;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.AbstractStorageRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginDao;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagRecord;
import gov.noaa.nws.ncep.common.dataplugin.geomag.fixed.GeoMagSource;
import gov.noaa.nws.ncep.common.dataplugin.geomag.fixed.GeoMagSourcePreference;

/**
 * This is a Data Access Object (DAO) driver to interact with geomag database table and HDF5 data store.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * ------------ ---------- 	----------- --------------------------
 * 04/2013		975			S. Gurung   Initial Creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
public class GeoMagDao extends PluginDao {
	
	 /** The source data access object */
	private GeoMagSourceDao sourceDao = new GeoMagSourceDao();
	
	 /** The source preference data access object */
	private GeoMagSourcePreferenceDao sourcePrefDao = new GeoMagSourcePreferenceDao();

    public GeoMagDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore, IPersistable record) 
    		throws StorageException {
        GeoMagRecord magRecord = (GeoMagRecord) record;

        /*
         * Write observation times to HDF5.
         */
        if (magRecord.getObsTimes() != null ) {
        	AbstractStorageRecord storageRecord = new LongDataRecord(GeoMagRecord.OBS_TIME, 
        			magRecord.getDataURI(), (long[]) magRecord.getObsTimes(), 1, 
        			new long[] {magRecord.getObsTimes().length});
        	storageRecord.setCorrelationObject(magRecord);
        	dataStore.addDataRecord(storageRecord);        
        }
 
        /*
         * Write component1 data to HDF5.
         */
        if ( magRecord.getComp1Data() != null ) {
        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.component1, 
        			magRecord.getDataURI(), (float[]) magRecord.getComp1Data(), 1, 
        			new long[] {magRecord.getComp1Data().length});
        	storageRecord.setCorrelationObject(magRecord);
        	dataStore.addDataRecord(storageRecord);       
        }
        
        /*
         * Write component2 data to HDF5.
         */
        if ( magRecord.getComp2Data() != null ) {
        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.component2, 
        			magRecord.getDataURI(), (float[]) magRecord.getComp2Data(), 1, 
        			new long[] {magRecord.getComp2Data().length});
        	storageRecord.setCorrelationObject(magRecord);
        	dataStore.addDataRecord(storageRecord);       
        }
        
        /*
         * Write component3 data to HDF5.
         */
        if ( magRecord.getComp3Data() != null ) {
        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.component3, 
        			magRecord.getDataURI(), (float[]) magRecord.getComp3Data(), 1, 
        			new long[] {magRecord.getComp3Data().length});
        	storageRecord.setCorrelationObject(magRecord);
        	dataStore.addDataRecord(storageRecord);       
        }
        
        /*
         * Write component4 data to HDF5.
         */
        if ( magRecord.getComp4Data() != null ) {
        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.component4, 
        			magRecord.getDataURI(), (float[]) magRecord.getComp4Data(), 1, 
        			new long[] {magRecord.getComp4Data().length});
        	storageRecord.setCorrelationObject(magRecord);
        	dataStore.addDataRecord(storageRecord);       
        }
        
        return dataStore;
    }
    
    /*
     * Get GeoMagSource from source ID
     */
    public GeoMagSource getGeoMagSource(int sourceId) {
    	return sourceDao.queryById(sourceId);
    }
    
    public Integer getGeoMagSourceId(String sourceName) throws DataAccessLayerException {
    	return sourceDao.getSourceId(sourceName);
    }
    
    public GeoMagSourceDao getGeoMagSource() {
    	return sourceDao;
    }

    public void setGeoMagSourceDao(
    		GeoMagSourceDao sourceDao) {
    	this.sourceDao = sourceDao;
    }   
    
    /*
     * Get GeoMagSourcePreference from source preference ID
     */
    public GeoMagSourcePreference getGeoMagSourcePreference(int prefId) {
    	return sourcePrefDao.queryById(prefId);
    }
    
    public GeoMagSourcePreferenceDao getGeoMagSourcePreference() {
    	return sourcePrefDao;
    }

    public void setGeoMagSourcePreference(
    		GeoMagSourcePreferenceDao sourcePrefDao) {
    	this.sourcePrefDao = sourcePrefDao;
    }   
   
}