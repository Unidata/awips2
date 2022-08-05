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
import com.raytheon.uf.common.datastorage.records.DataUriMetadataIdentifier;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IMetadataIdentifier;
import com.raytheon.uf.edex.database.plugin.PluginDao;

/**
 * Data access object for accessing binlightning data
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 1/08/09      1674       bphillip    Initial creation
 * Jun 05, 2014 3226       bclement    record now contains maps for data arrays
 * Sep 23, 2021 8608       mapeters    Add metadata id handling
 * Jun 22, 2022 8865       mapeters    Update populateDataStore to return boolean
 * </pre>
 *
 * @author bphillip
 */
public class BinLightningDao extends PluginDao {

    /**
     *
     * @param pluginName
     *            The name of this plugin.
     * @throws PluginException
     */
    public BinLightningDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected boolean populateDataStore(IDataStore dataStore, IPersistable obj)
            throws Exception {
        boolean populated = false;

        BinLightningRecord binLightningRec = (BinLightningRecord) obj;
        Map<String, Object> strikeDataArrays = binLightningRec
                .getStrikeDataArrays();
        if (populateFromMap(dataStore, binLightningRec,
                binLightningRec.getDataURI(), strikeDataArrays)) {
            populated = true;
        }
        Map<String, Object> pulseDataArrays = binLightningRec
                .getPulseDataArrays();
        String pulseGroup = binLightningRec.getDataURI() + DataURI.SEPARATOR
                + LightningConstants.PULSE_HDF5_GROUP_SUFFIX;
        if (populateFromMap(dataStore, binLightningRec, pulseGroup,
                pulseDataArrays)) {
            populated = true;
        }
        return populated;
    }

    /**
     * Adds each primitive data array object in map to the data store using the
     * provided group and the key of the map entry as the name
     *
     * @param dataStore
     * @param obj
     * @param group
     * @param data
     * @return true if any data was added to the data store, false otherwise
     * @throws StorageException
     */
    private boolean populateFromMap(IDataStore dataStore,
            BinLightningRecord binLightningRec, String group,
            Map<String, Object> data) throws StorageException {
        IMetadataIdentifier metaId = new DataUriMetadataIdentifier(
                binLightningRec);

        boolean populated = false;
        for (Entry<String, Object> e : data.entrySet()) {
            String name = e.getKey();
            Object dataArray = e.getValue();
            IDataRecord record = DataStoreFactory.createStorageRecord(name,
                    group, dataArray);
            record.setCorrelationObject(binLightningRec);
            dataStore.addDataRecord(record, metaId);
            populated = true;
        }
        return populated;
    }
}
