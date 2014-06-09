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

package com.raytheon.edex.plugin.binlightning.dao;

import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.binlightning.LightningConstants;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.edex.database.plugin.PluginDao;

/**
 * Data access object for access binlightning data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 1/08/09      1674       bphillip    Initial creation
 * Jun 05, 2014 3226       bclement    record now contains maps for data arrays
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class BinLightningDao extends PluginDao {

    /**
     * 
     * @param pluginName The name of this plugin.
     * @throws PluginException
     */
    public BinLightningDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * Copy data from a Persistable object into a given DataStore container.
     * @param dataStore DataStore instance to receive the Persistable data.
     * @param obj The Persistable object to be stored.
     * @throws Exception Any general exception thrown in this method.
     */
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        BinLightningRecord binLightningRec = (BinLightningRecord) obj;
        Map<String, Object> strikeDataArrays = binLightningRec
                .getStrikeDataArrays();
        populateFromMap(dataStore, obj, binLightningRec.getDataURI(),
                strikeDataArrays);
        Map<String, Object> pulseDataArrays = binLightningRec
                .getPulseDataArrays();
        String pulseGroup = binLightningRec.getDataURI() + DataURI.SEPARATOR
                + LightningConstants.PULSE_HDF5_GROUP_SUFFIX;
        populateFromMap(dataStore, obj, pulseGroup, pulseDataArrays);
        return dataStore;
    }

    /**
     * Adds each primitive data array object in map to the datastore using the
     * provided group and the key of the map entry as the name
     * 
     * @param dataStore
     * @param obj
     * @param group
     * @param data
     * @throws StorageException
     */
    private void populateFromMap(IDataStore dataStore, IPersistable obj,
            String group, Map<String, Object> data)
            throws StorageException {
        for (Entry<String, Object> e : data.entrySet()) {
            String name = e.getKey();
            Object dataArray = e.getValue();
            IDataRecord record = DataStoreFactory.createStorageRecord(name,
                    group, dataArray);
            record.setCorrelationObject(obj);
            dataStore.addDataRecord(record);
        }
    }
}
