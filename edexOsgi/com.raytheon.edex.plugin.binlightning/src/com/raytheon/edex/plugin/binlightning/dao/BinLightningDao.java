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

import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
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

        for (int i = 0; i < binLightningRec.getDataArrays().length; i++) {
            IDataRecord record = DataStoreFactory.createStorageRecord(
                    binLightningRec.getDataNames()[i], binLightningRec
                            .getDataURI(), binLightningRec.getDataArrays()[i]);
            record.setCorrelationObject(binLightningRec);
            dataStore.addDataRecord(record);
        }

        return dataStore;
    }
}
