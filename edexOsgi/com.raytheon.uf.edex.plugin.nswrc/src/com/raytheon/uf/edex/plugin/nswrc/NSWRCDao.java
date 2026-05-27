/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2013 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 2120 South 72nd Street
 * Omaha Tower, Suite 900
 * Omaha, NE 68124 USA
 * 402.291.0100
 *
 */
package com.raytheon.uf.edex.plugin.nswrc;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.nswrc.NSWRCConstants;
import com.raytheon.uf.common.dataplugin.nswrc.NSWRCDataRetriever;
import com.raytheon.uf.common.dataplugin.nswrc.NSWRCRadialRecord;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.records.DataUriMetadataIdentifier;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IMetadataIdentifier;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * NSWRC (NextGen Surveillance and Weather Radar Capability) data access object.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2013            ekladstrup  Initial creation
 * Apr 22, 2014  3048      mweeks      Updates for peer review and 13.5.4 baseline.
 * Sep 23, 2021  8608      mapeters    Add metadata id handling
 * Jun 22, 2022  8865      mapeters    Update populateDataStore to return boolean
 *
 * </pre>
 *
 * @author ekladstrup
 */
public class NSWRCDao extends PluginDao {

    /**
     * @param pluginName
     * @throws PluginException
     */
    public NSWRCDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected boolean populateDataStore(IDataStore dataStore, IPersistable obj)
            throws Exception {
        boolean populated = false;

        NSWRCRadialRecord radarRec = (NSWRCRadialRecord) obj;
        StorageProperties sp = null;
        String compression = PluginRegistry.getInstance()
                .getRegisteredObject(pluginName).getCompression();

        if (compression != null) {
            sp = new StorageProperties();
            sp.setCompression(
                    StorageProperties.Compression.valueOf(compression));
        }

        IMetadataIdentifier metaId = new DataUriMetadataIdentifier(radarRec);
        if (radarRec.getData() != null) {
            IDataRecord rec = new FloatDataRecord("Data", radarRec.getDataURI(),
                    radarRec.getData(), 2, new long[] {
                            radarRec.getNumRadials(), radarRec.getNumBins() });
            rec.setCorrelationObject(radarRec);
            dataStore.addDataRecord(rec, metaId, sp);
            populated = true;
        }

        if (radarRec.getAngleData() != null) {
            IDataRecord rec = new FloatDataRecord("AngleData",
                    radarRec.getDataURI(), radarRec.getAngleData(), 1,
                    new long[] { radarRec.getNumRadials() });
            rec.setCorrelationObject(radarRec);
            dataStore.addDataRecord(rec, metaId, sp);
            populated = true;
        }

        if (radarRec.getSignal_to_noise() != null) {
            IDataRecord rec = new FloatDataRecord(NSWRCConstants.NOISE_TITLE,
                    radarRec.getDataURI(), radarRec.getSignal_to_noise(), 2,
                    new long[] { radarRec.getNumRadials(),
                            radarRec.getNumBins() });
            rec.setCorrelationObject(radarRec);
            dataStore.addDataRecord(rec, metaId, sp);
            populated = true;
        }

        if (radarRec.getNormalized_coherent_power() != null) {
            IDataRecord rec = new FloatDataRecord(NSWRCConstants.POWER_TITLE,
                    radarRec.getDataURI(),
                    radarRec.getNormalized_coherent_power(), 2, new long[] {
                            radarRec.getNumRadials(), radarRec.getNumBins() });
            rec.setCorrelationObject(radarRec);
            dataStore.addDataRecord(rec, metaId, sp);
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
                    throw new PluginException(
                            "Error retrieving radar HDF5 data", e);
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
            NSWRCRadialRecord record = (NSWRCRadialRecord) obj;
            IDataRecord[] hdf5Data = getHDF5Data(record, tile);
            record.setMessageData(hdf5Data[0].getDataObject());
            record.setAngleData((float[]) hdf5Data[1].getDataObject());
        }
        return queryResults;
    }

    public void populateData(NSWRCRadialRecord record) throws Exception {
        NSWRCDataRetriever.populateRadarRecord(getDataStore(record), record);
    }
}