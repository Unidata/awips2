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
 * Nov 14, 2013 2393        bclement    added in-java interpolation
 * Mar 07, 2014 2791        bsteffen    Move Data Source/Destination to numeric
 *                                      plugin.
 * </pre>
 * 
 * @author tlee
 * @version 1.0
 */

package gov.noaa.nws.ncep.common.dataplugin.mcidas.dao;

import gov.noaa.nws.ncep.common.dataplugin.mcidas.McidasMapCoverage;
import gov.noaa.nws.ncep.common.dataplugin.mcidas.McidasRecord;
import gov.noaa.nws.ncep.common.dataplugin.mcidas.fixed.McidasAreaName;
import gov.noaa.nws.ncep.common.dataplugin.mcidas.fixed.McidasImageType;
import gov.noaa.nws.ncep.common.dataplugin.mcidas.fixed.McidasSatelliteName;

import java.awt.Rectangle;
import java.util.List;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.records.AbstractStorageRecord;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.interpolation.GridDownscaler;
import com.raytheon.uf.common.numeric.buffer.BufferWrapper;
import com.raytheon.uf.edex.database.plugin.DownscaleStoreUtil;
import com.raytheon.uf.edex.database.plugin.DownscaleStoreUtil.IDataRecordCreator;
import com.raytheon.uf.edex.database.plugin.PluginDao;

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
        final McidasRecord satRecord = (McidasRecord) record;

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
            McidasMapCoverage coverage = satRecord.getCoverage();
            int xdim = coverage.getNx();
            int ydim = coverage.getNy();
        	long[] sizes = new long[] { xdim, ydim };        
            AbstractStorageRecord storageRecord = new ByteDataRecord(
                    DataStoreFactory.DEF_DATASET_NAME, satRecord.getDataURI(),
                    (byte[]) satRecord.getMessageData(), 2, sizes);
        	
            final StorageProperties props = new StorageProperties();

            GridGeometry2D gridGeom;
            try {
                gridGeom = coverage.getGridGeometry();
            } catch (Exception e) {
                throw new StorageException(
                        "Unable to create grid geometry for record: "
                                + satRecord, storageRecord, e);
            }
            GridDownscaler downScaler = new GridDownscaler(gridGeom);
        	
        	storageRecord.setProperties(props);
        	storageRecord.setCorrelationObject(satRecord);
            // Store the base record.
            dataStore.addDataRecord(storageRecord);

            BufferWrapper dataSource = BufferWrapper.wrapArray(
                    storageRecord.getDataObject(), xdim, ydim);
            // this way of interpolating does not create the Data-interpolated/0
            // link to the full sized data. This shouldn't be an issue since the
            // retrieval code checks for level 0 and requests the full sized
            // data.
            DownscaleStoreUtil.storeInterpolated(dataStore, downScaler,
                    dataSource, new IDataRecordCreator() {

                        @Override
                        public IDataRecord create(Object data,
                                int downScaleLevel, Rectangle size)
                                throws StorageException {
                            long[] sizes = new long[] { size.width, size.height };
                            String group = DataStoreFactory.createGroupName(
                                    satRecord.getDataURI(), null, true);
                            String name = String.valueOf(downScaleLevel);
                            IDataRecord rval = DataStoreFactory
                                    .createStorageRecord(name, group, data, 2,
                                            sizes);
                            rval.setProperties(props);
                            rval.setCorrelationObject(satRecord);
                            return rval;
                        }

                        @Override
                        public double getFillValue() {
                            return Double.NaN;
                        }

                        @Override
                        public boolean isSigned() {
                            return false;
                        }

                    });
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