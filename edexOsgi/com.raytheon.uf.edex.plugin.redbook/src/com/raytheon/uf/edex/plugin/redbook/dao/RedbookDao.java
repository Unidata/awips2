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
package com.raytheon.uf.edex.plugin.redbook.dao;

import java.io.FileNotFoundException;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.redbook.RedbookRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.DataUriMetadataIdentifier;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.edex.database.plugin.PluginDao;

/**
 * Set of DAO methods for Surface Observation data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2008 1131       jkorman     Initial implementation.
 * Mar 13, 2014 2907       njensen     split edex.redbook plugin into common and
 *                                     edex redbook plugins
 * Sep 23, 2021 8608       mapeters    Add metadata id handling
 * Jun 22, 2022 8865       mapeters    Update populateDataStore to return boolean,
 *                                     remove unused methods
 * </pre>
 *
 * @author jkorman
 */
public class RedbookDao extends PluginDao {

    /**
     * Creates a new RedbookDao
     *
     * @throws PluginException
     */
    public RedbookDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected boolean populateDataStore(IDataStore dataStore, IPersistable obj)
            throws Exception {
        RedbookRecord redBookRec = (RedbookRecord) obj;
        if (dataStore != null) {
            IDataRecord rec = DataStoreFactory.createStorageRecord(
                    RedbookRecord.REDBOOK_DATA, redBookRec.getDataURI(),
                    redBookRec.getRedBookData());
            rec.setCorrelationObject(this);
            dataStore.addDataRecord(rec,
                    new DataUriMetadataIdentifier(redBookRec));
            return true;
        }
        return false;
    }

    public RedbookRecord getFullRecord(RedbookRecord record)
            throws StorageException, FileNotFoundException {
        IDataStore dataStore = getDataStore(record);
        IDataRecord data = dataStore.retrieve(record.getDataURI(),
                RedbookRecord.REDBOOK_DATA, Request.ALL);
        if (data instanceof ByteDataRecord) {
            record.setRedBookData(((ByteDataRecord) data).getByteData());
        } else {
            throw new StorageException("expected byte data", data);
        }
        return record;
    }
}
