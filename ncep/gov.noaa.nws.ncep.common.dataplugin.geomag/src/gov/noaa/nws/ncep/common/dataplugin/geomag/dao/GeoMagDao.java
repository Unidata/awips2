package gov.noaa.nws.ncep.common.dataplugin.geomag.dao;


import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.AbstractStorageRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginDao;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagAvg;
import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagK1min;
import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagK3hr;
import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagRecord;

/**
 * This is a Data Access Object (DAO) driver to interact with geomag database table and HDF5 data store.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * ------------ ---------- 	----------- --------------------------
 * 04/2013		975			S. Gurung   Initial Creation
 * 07/16/2013   975         Q. Zhou     Added fields.
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
public class GeoMagDao extends PluginDao {
	
	 /** The source data access object */
	private GeoMagAvgDao avgDao = new GeoMagAvgDao();
	private GeoMagK3hrDao k3hrDao = new GeoMagK3hrDao();
	private GeoMagK1minDao k1minDao = new GeoMagK1minDao();

    public GeoMagDao(String pluginName) throws PluginException {
        super(pluginName);
    }

//    @Override
//    public StorageStatus persistToHDF5(PluginDataObject... records)
//            throws PluginException {
//        long t0 = System.currentTimeMillis();
//
//        // NOTE: currently making the assumption that models aren't
//        // mixed in the records aggregate. If this isn't true,
//        // some pre-processing will be needed.
//        Map<PointDataContainer, List<PointDataView>> containerMap = new HashMap<PointDataContainer, List<PointDataView>>(
//                records.length);
//        Map<PointDataContainer, File> fileMap = new HashMap<PointDataContainer, File>();
//
//        for (PluginDataObject p : records) {
//            if (p instanceof IPointData) {
//                PointDataView pdv = ((IPointData) p).getPointDataView();
//                List<PointDataView> views = containerMap
//                        .get(pdv.getContainer());
//                if (views == null) {
//                    views = new ArrayList<PointDataView>();
//                    containerMap.put(pdv.getContainer(), views);
//                }
//                views.add(pdv);
//                File file = fileMap.get(pdv.getContainer());
//                if (file == null) {
//                    file = getFullFilePath(p);
//                    System.out.println("***file " +file.toString());
//                    fileMap.put(pdv.getContainer(), file);
//                }
//
//            }DataTime
//        }
//
//        List<StorageStatus> ssList = new ArrayList<StorageStatus>();
//        try {
//            for (PointDataContainer container : containerMap.keySet()) {
//                IDataStore ds = DataStoreFactory.getDataStore(fileMap
//                        .get(container));
//                StorageProperties sp = new StorageProperties();
//                String compression = PluginRegistry.getInstance()
//                        .getRegisteredObject(pluginName).getCompression();
//                if (compression != null) {
//                    sp.setCompression(StorageProperties.Compression
//                            .valueOf(compression));
//                }
//
//                Set<String> params = container.getParameters();
//                for (String param : params) {
//                    try {
//                        IDataRecord idr = container.getParameterRecord(param);
//                        ds.addDataRecord(idr, sp);
//                    } catch (StorageException e) {
//                        throw new PluginException("Error adding record", e);
//                    }
//                }
//
//                try {
//                    StorageStatus ss = ds.store(StoreOp.APPEND);
//                    if (ss.getOperationPerformed() == StoreOp.APPEND) {
//                        // increment the indices
//                        List<PointDataView> views = containerMap.get(container);
//                        int idx = (int) ss.getIndexOfAppend()[0];
//                        container.incrementIds(idx, views);
//                    }
//                    ssList.add(ss);
//                } catch (StorageException e) {
//                    throw new PluginException("Error updating point file", e);
//                }
//            }
//            // Aggregate the storage status errors
//            StorageStatus aggregatedStatus = new StorageStatus();
//            List<StorageException> se = new ArrayList<StorageException>();
//            for (StorageStatus ss : ssList) {
//                StorageException[] seArr = ss.getExceptions();
//                if (seArr != null) {
//                    se.addAll(Arrays.asList(seArr));
//                }
//            }
//
//            aggregatedStatus.setExceptions(se.toArray(new StorageException[se
//                    .size()]));
//            return aggregatedStatus;
//        }
//
//        finally {
//            System.out.println("Time spent in persist: "
//                    + (System.currentTimeMillis() - t0));
//        }
//    }
//    
//    public File getFullFilePath(PluginDataObject p) {
//        File file;
//        String directory = p.getPluginName() + File.separator
//                + pathProvider.getHDFPath(p.getPluginName(), (IPersistable) p);
//        file = new File(directory
//                + File.separator
//                + pathProvider.getHDFFileName(p.getPluginName(),
//                        (IPersistable) p));
//        return file;
//    }
    
    /**
     * Retrieves an geomag report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public GeoMagRecord queryByDataURI(String dataURI) {
    	GeoMagRecord report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if ((obs != null) && (obs.size() > 0)) {
            report = (GeoMagRecord) obs.get(0);
        }
        return report;
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

        String sql = "select datauri from awips.geomag where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }

    @Override
    public IDataStore populateDataStore(IDataStore dataStore, IPersistable record) 
    		throws StorageException {
    	//return null;
        GeoMagRecord magRecord = (GeoMagRecord) record;

        // change to 00:00:00.0.    "/geomag/2013-04-01_00:00:00.0/BOU/102/GEOMAG";
//        String headUri = magRecord.getDataURI();        
//        headUri = headUri.substring(0, 18) +"_00:00:00.0/" + headUri.substring(30);
        
        /*
         * Write observation times to HDF5.
         */
//        if (magRecord.getObsTimes() != null ) {
//        	AbstractStorageRecord storageRecord = new LongDataRecord(GeoMagRecord.OBS_TIME, 
//        			headUri, (long[]) magRecord.getObsTimes(), 1, 
//        			new long[] {magRecord.getObsTimes().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	
//        }

//        if (magRecord.getCompIdx() != null) {
//		    AbstractStorageRecord storageRecord = new IntegerDataRecord(GeoMagRecord.CompIdx, 
//		    		magRecord.getDataURI(), (int[]) magRecord.getCompIdx(), 1, 
//	    			new long[] {magRecord.getCompIdx().length});
//		    
//	    	storageRecord.setCorrelationObject(record);
//	    	dataStore.addDataRecord(storageRecord);
//        }
 
        /*
         * Write component1 data to HDF5.
         */
        if ( magRecord.getComp1Data() != null ) {
        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.component1, 
        			magRecord.getDataURI(), (float[]) magRecord.getComp1Data(), 1, 
        			new long[] {magRecord.getComp1Data().length});
        	storageRecord.setCorrelationObject(magRecord);
        	dataStore.addDataRecord(storageRecord);       
        	//StorageStatus ss = dataStore.store(StoreOp.APPEND);       
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
        
//        /*
//         * Write component3 data to HDF5.
//         */
//        if ( magRecord.getComp3Data() != null ) {
//        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.component3, 
//        			magRecord.getDataURI(), (float[]) magRecord.getComp3Data(), 1, 
//        			new long[] {magRecord.getComp3Data().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//        
//        /*
//         * Write component4 data to HDF5.
//         */
//        if ( magRecord.getComp4Data() != null ) {
//        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.component4, 
//        			magRecord.getDataURI(), (float[]) magRecord.getComp4Data(), 1, 
//        			new long[] {magRecord.getComp4Data().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//        if (magRecord.getHrAvgIdx() != null) {
//		    AbstractStorageRecord storageRecord = new IntegerDataRecord(GeoMagRecord.HrAvgIdx, 
//		    		magRecord.getDataURI(), (int[]) magRecord.getHrAvgIdx(), 1, 
//	    			new long[] {magRecord.getHrAvgIdx().length});
//		    
//	    	storageRecord.setCorrelationObject(record);
//	    	dataStore.addDataRecord(storageRecord);
//        }
//
//        /*
//         * Write H_HR_AVG data to HDF5.
//         */
//        if ( magRecord.getHrAvgH() != null ) {
//        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.H_HR_AVG, 
//        			magRecord.getDataURI(), (float[]) magRecord.getHrAvgH(), 1, 
//        			new long[] {magRecord.getHrAvgH().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//        
//        /*
//         * Write D_HR_AVG data to HDF5.
//         */
//        if ( magRecord.getHrAvgD() != null ) {
//        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.D_HR_AVG, 
//        			magRecord.getDataURI(), (float[]) magRecord.getHrAvgD(), 1, 
//        			new long[] {magRecord.getHrAvgD().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//        /*
//         * Write K_Index data to HDF5.
//         */
//        // 3hr
//        if ( magRecord.getKKIndex() != null ) {
//        	AbstractStorageRecord storageRecord = new IntegerDataRecord(GeoMagRecord.K_Index, 
//        			magRecord.getDataURI(), (int[]) magRecord.getKKIndex(), 1, 
//        			new long[] {magRecord.getKKIndex().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//        
//        if ( magRecord.getKKGamma() != null ) {
//        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.K_Gamma, 
//        			magRecord.getDataURI(), (float[]) magRecord.getKKGamma(), 1, 
//        			new long[] {magRecord.getKKGamma().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//        
//        if ( magRecord.getKKReal() != null ) {
//        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.K_Real, 
//        			magRecord.getDataURI(), (float[]) magRecord.getKKReal(), 1, 
//        			new long[] {magRecord.getKKReal().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//        
//        if ( magRecord.getKestIndex() != null ) {
//        	AbstractStorageRecord storageRecord = new IntegerDataRecord(GeoMagRecord.Kest_Index, 
//        			magRecord.getDataURI(), (int[]) magRecord.getKestIndex(), 1, 
//        			new long[] {magRecord.getKestIndex().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//        
//        if ( magRecord.getKestGamma() != null ) {
//        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.Kest_Gamma, 
//        			magRecord.getDataURI(), (float[]) magRecord.getKestGamma(), 1, 
//        			new long[] {magRecord.getKestGamma().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//        
//        if ( magRecord.getKestReal() != null ) {
//        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.Kest_Real, 
//        			magRecord.getDataURI(), (float[]) magRecord.getKestReal(), 1, 
//        			new long[] {magRecord.getKestReal().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//        
//        if ( magRecord.getHKGamma() != null ) {
//        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.KH_Gamma, 
//        			magRecord.getDataURI(), (float[]) magRecord.getHKGamma(), 1, 
//        			new long[] {magRecord.getHKGamma().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//        
//        if ( magRecord.getHKReal() != null ) {
//        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.KH_Real, 
//        			magRecord.getDataURI(), (float[]) magRecord.getHKReal(), 1, 
//        			new long[] {magRecord.getHKReal().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//
//        if ( magRecord.getDKGamma() != null ) {
//        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.KD_Gamma, 
//        			magRecord.getDataURI(), (float[]) magRecord.getDKGamma(), 1, 
//        			new long[] {magRecord.getDKGamma().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//        
//        if ( magRecord.getDKReal() != null ) {
//        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.KD_Real, 
//        			magRecord.getDataURI(), (float[]) magRecord.getDKReal(), 1, 
//        			new long[] {magRecord.getDKReal().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//
//        
//        
//        // 1 min    
//        if ( magRecord.getKestIndex1m() != null ) {
//        	AbstractStorageRecord storageRecord = new IntegerDataRecord(GeoMagRecord.Kest_Index_1m, 
//        			magRecord.getDataURI(), (int[]) magRecord.getKestIndex1m(), 1, 
//        			new long[] {magRecord.getKestIndex1m().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//        
//        if ( magRecord.getKestGamma1m() != null ) {
//        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.Kest_Gamma_1m, 
//        			magRecord.getDataURI(), (float[]) magRecord.getKestGamma1m(), 1, 
//        			new long[] {magRecord.getKestGamma1m().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//		    
//        if ( magRecord.getKestReal1m() != null ) {
//        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.Kest_Real_1m, 
//        			magRecord.getDataURI(), (float[]) magRecord.getKestReal1m(), 1, 
//        			new long[] {magRecord.getKestReal1m().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//        
//        if ( magRecord.getHKGamma1m() != null ) {
//        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.KH_Gamma_1m, 
//        			magRecord.getDataURI(), (float[]) magRecord.getHKGamma1m(), 1, 
//        			new long[] {magRecord.getHKGamma1m().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//        
//        if ( magRecord.getHKReal1m() != null ) {
//        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.KH_Real_1m, 
//        			magRecord.getDataURI(), (float[]) magRecord.getHKReal1m(), 1, 
//        			new long[] {magRecord.getHKReal1m().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//
//        if ( magRecord.getDKGamma1m() != null ) {
//        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.KD_Gamma_1m, 
//        			magRecord.getDataURI(), (float[]) magRecord.getDKGamma1m(), 1, 
//        			new long[] {magRecord.getDKGamma1m().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//        
//        if ( magRecord.getDKReal1m() != null ) {
//        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.KD_Real_1m, 
//        			magRecord.getDataURI(), (float[]) magRecord.getDKReal1m(), 1, 
//        			new long[] {magRecord.getDKReal1m().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//        if ( magRecord.getHKIndex1m() != null ) {
//        	AbstractStorageRecord storageRecord = new IntegerDataRecord(GeoMagRecord.KH_Index_1m, 
//        			magRecord.getDataURI(), (int[]) magRecord.getHKIndex1m(), 1, 
//        			new long[] {magRecord.getHKIndex1m().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//        
//        if ( magRecord.getDKIndex1m() != null ) {
//        	AbstractStorageRecord storageRecord = new IntegerDataRecord(GeoMagRecord.KD_Index_1m, 
//        			magRecord.getDataURI(), (int[]) magRecord.getDKIndex1m(), 1, 
//        			new long[] {magRecord.getDKIndex1m().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//        
//        if ( magRecord.getHCount() != null ) {
//        	AbstractStorageRecord storageRecord = new IntegerDataRecord(GeoMagRecord.KH_Count, 
//        			magRecord.getDataURI(), (int[]) magRecord.getHCount(), 1, 
//        			new long[] {magRecord.getHCount().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//     
//        if ( magRecord.getDCount() != null ) {
//        	AbstractStorageRecord storageRecord = new IntegerDataRecord(GeoMagRecord.KD_Count, 
//        			magRecord.getDataURI(), (int[]) magRecord.getDCount(), 1, 
//        			new long[] {magRecord.getDCount().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//
//        if ( magRecord.getHDev() != null ) {
//        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.KH_Dev, 
//        			magRecord.getDataURI(), (float[]) magRecord.getHDev(), 1, 
//        			new long[] {magRecord.getHDev().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//     
//        if ( magRecord.getDDev() != null ) {
//        	AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.KD_Dev, 
//        			magRecord.getDataURI(), (float[]) magRecord.getDDev(), 1, 
//        			new long[] {magRecord.getDDev().length});
//        	storageRecord.setCorrelationObject(magRecord);
//        	dataStore.addDataRecord(storageRecord);  
//        	       
//        }
//        
//        if (magRecord.getKs() != null) {
//			AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.K_s, 
//					magRecord.getDataURI(), (float[]) magRecord.getKs(), 1, 
//	    			new long[] {magRecord.getKs().length});
//	    	storageRecord.setCorrelationObject(record);
//	    	dataStore.addDataRecord(storageRecord);
//        				
//	    }
//		
//        if (magRecord.getAest() != null) {
//			AbstractStorageRecord storageRecord = new IntegerDataRecord(GeoMagRecord.A_est, 
//					magRecord.getDataURI(), (int[]) magRecord.getAest(), 1, 
//	    			new long[] {magRecord.getAest().length});
//	    	storageRecord.setCorrelationObject(record);
//	    	dataStore.addDataRecord(storageRecord);  
//        				
//	    }
//		
//	    if (magRecord.getLastUpdate() != null) {
//			AbstractStorageRecord storageRecord = new FloatDataRecord(GeoMagRecord.Last_Update_1m, 
//					magRecord.getDataURI(), (float[]) magRecord.getLastUpdate(), 1, 
//	    			new long[] {magRecord.getLastUpdate().length});
//	    	storageRecord.setCorrelationObject(record);
//	    	dataStore.addDataRecord(storageRecord);  
//        				
//        }
        return dataStore;
    }
    
    /*
     * Get GeoMagAvg from  ID
     */
    public GeoMagAvg getGeoMagAvg(Date avgTime) {
    	return (GeoMagAvg) avgDao.queryById(avgTime);
    }
    
//    public Integer getGeoMagSourceId(String sourceName) throws DataAccessLayerException {
//    	return avgDao.getSourceId(sourceName);
//    }
    
    public GeoMagAvgDao getGeoMagAvgDao() {
    	return avgDao;
    }

    public void setGeoMagAvgDao(
    		GeoMagAvgDao avgDao) {
    	this.avgDao = avgDao;
    }   
    
    /*
     * Get GeoMagK1min from  ID
     */
    public GeoMagK1min getGeoMagDateK1min(int id) {
    	return k1minDao.queryById(id);
    }
    
    public GeoMagK1minDao getGeoMagK1minDao() {
    	return k1minDao;
    }
    
    public void setGeoMagK1minDao(
    		GeoMagK1minDao k1minDao) {
    	this.k1minDao = k1minDao;
    }

    /*
     * Get GeoMagK1min from  ID
     */
    public GeoMagK3hr getGeoMagDateK3hr(int id) {
    	return k3hrDao.queryById(id);
    }   
   
    public GeoMagK3hrDao getGeoMagK3hrDao() {
    	return k3hrDao;
    }

    public void setGeoMagK3hrDao(
    		GeoMagK3hrDao k3hrDao) {
    	this.k3hrDao = k3hrDao;
    }   
}