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
package com.raytheon.uf.edex.plugin.mpe.dao.metadata.impl;

import java.util.Collections;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.mpe.PrecipRecord;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.DataUriMetadataIdentifier;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.mpe.fieldgen.PrecipField;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Custom {@link PluginDao} implementation used for the data management of
 * {@link PrecipRecord}s.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 19, 2017 6407       bkowal      Initial creation
 * Sep 23, 2021 8608       mapeters    Add metadata id handling
 * Jun 22, 2022 8865       mapeters    Update populateDataStore to return boolean
 *
 * </pre>
 *
 * @author bkowal
 */
public class PrecipDao extends PluginDao {

    public PrecipDao() throws PluginException {
        super(PrecipRecord.PLUGIN_NAME);
    }

    public PrecipDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected boolean populateDataStore(IDataStore dataStore, IPersistable obj)
            throws Exception {
        PrecipRecord precipRecord = (PrecipRecord) obj;
        IDataRecord dataRecord = (IDataRecord) precipRecord.getMessageData();
        if (dataRecord != null) {
            dataStore.addDataRecord(dataRecord,
                    new DataUriMetadataIdentifier(precipRecord));
            return true;
        }
        return false;
    }

    public List<Date> getPrecipInventory(final PrecipField precipField)
            throws DataAccessLayerException {
        if (precipField == null) {
            throw new IllegalArgumentException(
                    "Required argument 'precipField' cannot be NULL.");
        }
        DatabaseQuery query = new DatabaseQuery(this.daoClass);
        query.addQueryParam(PrecipRecord.Fields.PRECIP_FIELD, precipField);
        query.addReturnedField(PluginDataObject.REFTIME_ID);
        query.addOrder(PluginDataObject.REFTIME_ID, true);
        List<?> results = queryByCriteria(query);
        if (CollectionUtils.isEmpty(results)) {
            return Collections.emptyList();
        }
        List<Date> inventory = new LinkedList<>();
        for (Object result : results) {
            inventory.add((Date) result);
        }
        return inventory;
    }

    public PrecipRecord getLatestRecordForHour(final PrecipField precipField,
            final Date date, final int hour) throws PluginException {
        if (precipField == null) {
            throw new IllegalArgumentException(
                    "Required argument 'precipField' cannot be NULL.");
        }
        if (date == null) {
            throw new IllegalArgumentException(
                    "Required argument 'date' cannot be NULL.");
        }
        DatabaseQuery query = new DatabaseQuery(this.daoClass);
        query.addQueryParam(PrecipRecord.Fields.PRECIP_FIELD, precipField);
        query.addQueryParam(PrecipRecord.Fields.DATE, date);
        query.addQueryParam(PrecipRecord.Fields.HOUR, hour);
        query.addOrder(PluginDataObject.REFTIME_ID, false);
        query.setMaxResults(1);
        PluginDataObject[] results = getFullRecord(query, 0);
        if (results == null || results.length == 0) {
            return null;
        }
        PrecipRecord precipRecord = (PrecipRecord) results[0];
        precipRecord.setMessageData(
                ((IDataRecord[]) precipRecord.getMessageData())[0]);
        return precipRecord;
    }
}