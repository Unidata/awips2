package gov.noaa.nws.ncep.edex.plugin.mosaic.common.dao;

/**
 * Data Access Object implementation for accessing mosaic data
 * 
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 09/2009      143				L. Lin     	Initial coding
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

import gov.noaa.nws.ncep.edex.plugin.mosaic.common.MosaicRecord;
import gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3.SymbologyBlock;
import gov.noaa.nws.ncep.edex.plugin.mosaic.util.MosaicConstants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

public class MosaicDao extends PluginDao {

    /**
     * Creates a new mosaic dao
     * 
     * @param pluginName
     *            "mosaic"
     * @throws PluginException
     *             If the dao cannot be initialized
     */
    public MosaicDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {

        MosaicRecord mosaicRec = (MosaicRecord) obj;
        if (mosaicRec.getHeaderBlock() != null) {
        	//System.out.println("In decoderDao populateDataStore - store HeaderBlock");
            IDataRecord rec = new ByteDataRecord("Header", mosaicRec.getDataURI(),
                    mosaicRec.getHeaderBlock(), 1, new long[] { 120 });
            rec.setCorrelationObject(mosaicRec);
            dataStore.addDataRecord(rec);
        }
        
        if (mosaicRec.getRawData() != null) {
            IDataRecord rec = new ByteDataRecord("Data", mosaicRec.getDataURI(),
                    mosaicRec.getRawData(), 2, new long[] {
                            mosaicRec.getNx(), mosaicRec.getNy() });
            rec.setCorrelationObject(mosaicRec);
            dataStore.addDataRecord(rec);
        }

        if (mosaicRec.getThresholds() != null && mosaicRec.getProductCode() != 2) {
            IDataRecord rec = new ShortDataRecord("Thresholds", mosaicRec
                    .getDataURI(), mosaicRec.getThresholds(), 1,
                    new long[] { 16 });
            rec.setCorrelationObject(mosaicRec);
            dataStore.addDataRecord(rec);
        }

        if (mosaicRec.getSymbologyBlock() != null) {
            byte[] data = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).serialize(
                    mosaicRec.getSymbologyBlock());
            ByteDataRecord bdr = new ByteDataRecord("Symbology", mosaicRec
                    .getDataURI(), data);
            dataStore.addDataRecord(bdr);
        }

        if (mosaicRec.getProductDependentValues() != null) {
            IDataRecord rec = new ShortDataRecord("DependentValues", mosaicRec
                    .getDataURI(), mosaicRec.getProductDependentValues(), 1,
                    new long[] { mosaicRec.getProductDependentValues().length });
            rec.setCorrelationObject(mosaicRec);
            dataStore.addDataRecord(rec);
        }

        if (mosaicRec.getRecordVals().isEmpty()) {
            byte[] data = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).serialize(
                    mosaicRec.getRecordVals());
            ByteDataRecord bdr = new ByteDataRecord("RecordVals", mosaicRec
                    .getDataURI(), data);
            dataStore.addDataRecord(bdr);
        }

        return dataStore;
    }

    @Override
    public List<IDataRecord[]> getHDF5Data(List<PluginDataObject> objects,
            int tileSet) throws PluginException {
        List<IDataRecord[]> retVal = new ArrayList<IDataRecord[]>();

        for (PluginDataObject obj : objects) {
            IDataRecord[] record = null;

            if (obj instanceof IPersistable) {
                /* connect to the data store and retrieve the data */
                try {
                    record = getDataStore((IPersistable) obj).retrieve(
                            obj.getDataURI());
                } catch (Exception e) {
                    throw new PluginException(
                            "Error retrieving mosaic HDF5 data", e);
                }
                retVal.add(record);
            }
        }

        return retVal;
    }

    @Override
    public PluginDataObject[] getFullRecord(DatabaseQuery query, int tile)
            throws PluginException {
        PluginDataObject[] queryResults = getMetadata(query);

        for (PluginDataObject obj : queryResults) {
            MosaicRecord record = (MosaicRecord) obj;
            record.setPluginName(pluginName);
            IDataRecord[] hdf5Data = getHDF5Data(record, tile);
            record.setMessageData(hdf5Data[0].getDataObject());
            record.setThresholds((short[]) hdf5Data[2].getDataObject());
            record.setProductDependentValues((short[]) hdf5Data[6]
                    .getDataObject());

            record
                    .setProductVals((HashMap<MosaicConstants.MapValues, Map<String, Map<MosaicConstants.MapValues, String>>>) hdf5Data[5]
                            .getDataObject());
            try {
                record.setSymbologyBlock((SymbologyBlock) SerializationUtil
                        .transformFromThrift((byte[]) hdf5Data[3]
                                .getDataObject()));
            } catch (SerializationException e) {
                throw new PluginException(
                        "Error deserializing symbology block", e);
            }
        }
        return queryResults;
    }

}
