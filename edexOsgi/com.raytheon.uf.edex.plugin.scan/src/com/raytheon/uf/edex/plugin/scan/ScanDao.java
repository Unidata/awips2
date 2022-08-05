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

package com.raytheon.uf.edex.plugin.scan;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.scan.ScanRecord;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.StorageProperties.Compression;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.DataUriMetadataIdentifier;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IMetadataIdentifier;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginDao;

/**
 * Scan DAO
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ------------------------------------------
 * Feb 24, 2009  2027     dhladky   Initial Creation
 * Feb 01, 2013  1649     dhladky   removed XML where not needed, compression
 * Jul 31, 2018  6685     randerso  Added getLatestTime(). Code cleanup.
 * Sep 23, 2021  8608     mapeters  Add metadata id handling
 * Jun 22, 2022  8865     mapeters  Update populateDataStore to return boolean
 *
 * </pre>
 *
 * @author dhladky
 */
public class ScanDao extends PluginDao {

    /**
     * Nullary constructor for convenience
     *
     * @throws PluginException
     */
    public ScanDao() throws PluginException {
        this("scan");
    }

    /**
     * Required constructor taking plugin name
     *
     * @param pluginName
     * @throws PluginException
     */
    public ScanDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected boolean populateDataStore(IDataStore dataStore, IPersistable obj)
            throws Exception {
        boolean populated = false;

        ScanRecord scanRec = (ScanRecord) obj;
        String table = null;

        IMetadataIdentifier metaId = new DataUriMetadataIdentifier(scanRec);
        if (scanRec.getType().equals(ScanTables.CELL.name())) {
            table = ScanTables.CELL.name();

            StorageProperties sp = null;
            String compression = PluginRegistry.getInstance()
                    .getRegisteredObject(scanRec.getPluginName())
                    .getCompression();
            if (compression != null) {
                sp = new StorageProperties();
                sp.setCompression(Compression.valueOf(compression));
            }

            if (scanRec.getModelData() != null) {

                byte[] data = DynamicSerializationManager
                        .getManager(SerializationType.Thrift)
                        .serialize(scanRec.getTableData());
                ByteDataRecord bdr = new ByteDataRecord(table + "/model",
                        scanRec.getDataURI(), data);
                dataStore.addDataRecord(bdr, metaId, sp);
                populated = true;
            }

            if (scanRec.getSoundingData() != null) {

                byte[] data = DynamicSerializationManager
                        .getManager(SerializationType.Thrift)
                        .serialize(scanRec.getTableData());
                ByteDataRecord bdr = new ByteDataRecord(table + "/sounding",
                        scanRec.getDataURI(), data);
                dataStore.addDataRecord(bdr, metaId, sp);
                populated = true;
            }

        } else if (scanRec.getType().equals(ScanTables.DMD.name())) {
            table = ScanTables.DMD.name();

        } else if (scanRec.getType().equals(ScanTables.TVS.name())) {
            table = ScanTables.TVS.name();

        } else if (scanRec.getType().equals(ScanTables.MESO.name())) {
            table = ScanTables.MESO.name();

        }

        if (scanRec.getTableData() != null) {
            // All of them have a table to write
            byte[] data = DynamicSerializationManager
                    .getManager(SerializationType.Thrift)
                    .serialize(scanRec.getTableData());
            ByteDataRecord bdr = new ByteDataRecord(table, scanRec.getDataURI(),
                    data);
            dataStore.addDataRecord(bdr, metaId);
            populated = true;
        }

        return populated;
    }

    @Override
    public List<IDataRecord[]> getHDF5Data(List<PluginDataObject> objects,
            int tileSet) throws PluginException {
        List<IDataRecord[]> retVal = new ArrayList<>();

        for (PluginDataObject obj : objects) {
            IDataRecord[] record = null;

            if (obj instanceof IPersistable) {
                /* connect to the data store and retrieve the data */
                try {
                    record = getDataStore((IPersistable) obj)
                            .retrieve(obj.getDataURI());
                } catch (Exception e) {
                    throw new PluginException("Error retrieving SCAN HDF5 data",
                            e);
                }
                retVal.add(record);
            }
        }
        return retVal;
    }

    /**
     * @param type
     *            the desired type
     * @return the latest ref time for scan record of specified type
     */
    public Date getLatestTime(ScanTables type) {
        Map<String, String> productKeys = new HashMap<>(1, 1.0f);
        productKeys.put("type", type.name());
        try {
            return getMaxRefTime(productKeys);
        } catch (DataAccessLayerException e) {
            logger.error("Error retreiving last time for Scan type: " + type,
                    e);
            return null;
        }
    }
}