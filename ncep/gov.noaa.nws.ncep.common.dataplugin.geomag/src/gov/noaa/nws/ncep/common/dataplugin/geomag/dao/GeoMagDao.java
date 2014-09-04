package gov.noaa.nws.ncep.common.dataplugin.geomag.dao;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagAvg;
import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagK1min;
import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagK3hr;
import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagRecord;

import java.util.Date;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.AbstractStorageRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.purge.PurgeLogger;

/**
 * This is a Data Access Object (DAO) driver to interact with geomag database
 * table and HDF5 data store.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 04/2013      975         S. Gurung   Initial Creation
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

    /**
     * Overridden to purge expired data from geomag_k1min, geomag_houravg and
     * geomag_k3hr tables as well
     */
    @Override
    public int purgeDataByRefTime(Date refTime,
            Map<String, String> productKeys, boolean trackHdf5,
            boolean trackToUri, Map<String, List<String>> hdf5FileToUriPurged)
            throws DataAccessLayerException {

        int results = super.purgeDataByRefTime(refTime, productKeys, trackHdf5,
                trackToUri, hdf5FileToUriPurged);

        // delete expired data from geomag_k1min, geomag_houravg and geomag_k3hr
        // tables
        try {
            avgDao.purgeDataByRefTime(refTime);
            k1minDao.purgeDataByRefTime(refTime);
            k3hrDao.purgeDataByRefTime(refTime);
        } catch (Exception e) {
            PurgeLogger
                    .logError(
                            "Purging expired data from the secondary tables for this plugin failed.",
                            this.pluginName);
        }

        return results;

    }

    // @Override
    // public StorageStatus persistToHDF5(PluginDataObject... records)
    // throws PluginException {
    // long t0 = System.currentTimeMillis();
    //
    // // NOTE: currently making the assumption that models aren't
    // // mixed in the records aggregate. If this isn't true,
    // // some pre-processing will be needed.
    // Map<PointDataContainer, List<PointDataView>> containerMap = new
    // HashMap<PointDataContainer, List<PointDataView>>(
    // records.length);
    // Map<PointDataContainer, File> fileMap = new HashMap<PointDataContainer,
    // File>();
    //
    // for (PluginDataObject p : records) {
    // if (p instanceof IPointData) {
    // PointDataView pdv = ((IPointData) p).getPointDataView();
    // List<PointDataView> views = containerMap
    // .get(pdv.getContainer());
    // if (views == null) {
    // views = new ArrayList<PointDataView>();
    // containerMap.put(pdv.getContainer(), views);
    // }
    // views.add(pdv);
    // File file = fileMap.get(pdv.getContainer());
    // if (file == null) {
    // file = getFullFilePath(p);
    // System.out.println("***file " +file.toString());
    // fileMap.put(pdv.getContainer(), file);
    // }
    //
    // }DataTime
    // }
    //
    // List<StorageStatus> ssList = new ArrayList<StorageStatus>();
    // try {
    // for (PointDataContainer container : containerMap.keySet()) {
    // IDataStore ds = DataStoreFactory.getDataStore(fileMap
    // .get(container));
    // StorageProperties sp = new StorageProperties();
    // String compression = PluginRegistry.getInstance()
    // .getRegisteredObject(pluginName).getCompression();
    // if (compression != null) {
    // sp.setCompression(StorageProperties.Compression
    // .valueOf(compression));
    // }
    //
    // Set<String> params = container.getParameters();
    // for (String param : params) {
    // try {
    // IDataRecord idr = container.getParameterRecord(param);
    // ds.addDataRecord(idr, sp);
    // } catch (StorageException e) {
    // throw new PluginException("Error adding record", e);
    // }
    // }
    //
    // try {
    // StorageStatus ss = ds.store(StoreOp.APPEND);
    // if (ss.getOperationPerformed() == StoreOp.APPEND) {
    // // increment the indices
    // List<PointDataView> views = containerMap.get(container);
    // int idx = (int) ss.getIndexOfAppend()[0];
    // container.incrementIds(idx, views);
    // }
    // ssList.add(ss);
    // } catch (StorageException e) {
    // throw new PluginException("Error updating point file", e);
    // }
    // }
    // // Aggregate the storage status errors
    // StorageStatus aggregatedStatus = new StorageStatus();
    // List<StorageException> se = new ArrayList<StorageException>();
    // for (StorageStatus ss : ssList) {
    // StorageException[] seArr = ss.getExceptions();
    // if (seArr != null) {
    // se.addAll(Arrays.asList(seArr));
    // }
    // }
    //
    // aggregatedStatus.setExceptions(se.toArray(new StorageException[se
    // .size()]));
    // return aggregatedStatus;
    // }
    //
    // finally {
    // System.out.println("Time spent in persist: "
    // + (System.currentTimeMillis() - t0));
    // }
    // }
    //
    // public File getFullFilePath(PluginDataObject p) {
    // File file;
    // String directory = p.getPluginName() + File.separator
    // + pathProvider.getHDFPath(p.getPluginName(), (IPersistable) p);
    // file = new File(directory
    // + File.separator
    // + pathProvider.getHDFFileName(p.getPluginName(),
    // (IPersistable) p));
    // return file;
    // }

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
    public IDataStore populateDataStore(IDataStore dataStore,
            IPersistable record) throws StorageException {

        GeoMagRecord magRecord = (GeoMagRecord) record;

        /*
         * Write component1 data to HDF5.
         */
        if (magRecord.getComp1Data() != null) {
            AbstractStorageRecord storageRecord = new FloatDataRecord(
                    GeoMagRecord.component1, magRecord.getDataURI(),
                    (float[]) magRecord.getComp1Data(), 1,
                    new long[] { magRecord.getComp1Data().length });
            storageRecord.setCorrelationObject(magRecord);
            dataStore.addDataRecord(storageRecord);
            // StorageStatus ss = dataStore.store(StoreOp.APPEND);
        }

        /*
         * Write component2 data to HDF5.
         */
        if (magRecord.getComp2Data() != null) {
            AbstractStorageRecord storageRecord = new FloatDataRecord(
                    GeoMagRecord.component2, magRecord.getDataURI(),
                    (float[]) magRecord.getComp2Data(), 1,
                    new long[] { magRecord.getComp2Data().length });
            storageRecord.setCorrelationObject(magRecord);
            dataStore.addDataRecord(storageRecord);

        }

        return dataStore;
    }

    /*
     * Get GeoMagAvg from ID
     */
    public GeoMagAvg getGeoMagAvg(Date avgTime) {
        return (GeoMagAvg) avgDao.queryById(avgTime);
    }

    public GeoMagAvgDao getGeoMagAvgDao() {
        return avgDao;
    }

    public void setGeoMagAvgDao(GeoMagAvgDao avgDao) {
        this.avgDao = avgDao;
    }

    /*
     * Get GeoMagK1min from ID
     */
    public GeoMagK1min getGeoMagDateK1min(int id) {
        return (GeoMagK1min) k1minDao.queryById(id);
    }

    public GeoMagK1minDao getGeoMagK1minDao() {
        return k1minDao;
    }

    public void setGeoMagK1minDao(GeoMagK1minDao k1minDao) {
        this.k1minDao = k1minDao;
    }

    /*
     * Get GeoMagK1min from ID
     */
    public GeoMagK3hr getGeoMagDateK3hr(int id) {
        return (GeoMagK3hr) k3hrDao.queryById(id);
    }

    public GeoMagK3hrDao getGeoMagK3hrDao() {
        return k3hrDao;
    }

    public void setGeoMagK3hrDao(GeoMagK3hrDao k3hrDao) {
        this.k3hrDao = k3hrDao;
    }
}