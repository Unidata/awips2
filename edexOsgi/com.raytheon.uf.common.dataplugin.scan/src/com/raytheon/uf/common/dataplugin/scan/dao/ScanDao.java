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

package com.raytheon.uf.common.dataplugin.scan.dao;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.scan.ScanRecord;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.StorageProperties.Compression;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.database.plugin.PluginDao;

/**
 * Scan DAO
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 02/24/09     2027         dhladky    Initial Creation
 * 02/01/13     1649        D. Hladky   removed XML where not needed, compression
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public class ScanDao extends PluginDao {

    public ScanDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        
        ScanRecord scanRec = (ScanRecord) obj;
        String table = null;

        if (scanRec.getType().equals(ScanTables.CELL.name())) {
            table = ScanTables.CELL.name();

            StorageProperties sp = null;
            String compression = PluginRegistry.getInstance()
                    .getRegisteredObject(scanRec.getPluginName()).getCompression();
            if (compression != null) {
                sp = new StorageProperties();
                sp.setCompression(Compression.valueOf(compression));
            }
            
            if (scanRec.getModelData() != null) {
                
                byte[] data = DynamicSerializationManager.getManager(
                        SerializationType.Thrift).serialize(
                        scanRec.getTableData());
                ByteDataRecord bdr = new ByteDataRecord(table + "/model",
                        scanRec.getDataURI(), data);
                dataStore.addDataRecord(bdr, sp);
            }
            
            if (scanRec.getSoundingData() != null) {
                
                
                byte[] data = DynamicSerializationManager.getManager(
                        SerializationType.Thrift).serialize(
                        scanRec.getTableData());
                ByteDataRecord bdr = new ByteDataRecord(table + "/sounding",
                        scanRec.getDataURI(), data);
                dataStore.addDataRecord(bdr, sp);
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
            byte[] data = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).serialize(scanRec.getTableData());
            ByteDataRecord bdr = new ByteDataRecord(table,
                    scanRec.getDataURI(), data);
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
                            "Error retrieving SCAN HDF5 data", e);
                }
                retVal.add(record);
            }
        }
        return retVal;
    }
}