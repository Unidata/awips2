package com.raytheon.uf.edex.plugin.fog;

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

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.fog.FogRecord;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.DataUriMetadataIdentifier;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IMetadataIdentifier;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.edex.database.plugin.PluginDao;

/**
 * FOG specified data access object.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 12/12/09     2037        dhladky     Initial Creation
 * Sep 23, 2021 8608        mapeters    Add metadata id handling
 *
 * </pre>
 *
 * @author dhladky
 */
public class FogDao extends PluginDao {

    public FogDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        FogRecord fogRec = (FogRecord) obj;

        String group = fogRec.getDataURI();
        IMetadataIdentifier metaId = new DataUriMetadataIdentifier(fogRec);
        if (fogRec.getVisArray() != null) {
            IDataRecord rec = new IntegerDataRecord(
                    FogRecord.CHANNEL.VIS.getChannel(), group,
                    fogRec.getVisArray(), 2,
                    new long[] { fogRec.getNx(), fogRec.getNy() });
            rec.setCorrelationObject(fogRec);
            dataStore.addDataRecord(rec, metaId);
        }

        if (fogRec.getIR_3_9Array() != null) {
            IDataRecord rec = new IntegerDataRecord(
                    FogRecord.CHANNEL.IR3_9.getChannel(), group,
                    fogRec.getIR_3_9Array(), 2,
                    new long[] { fogRec.getNx(), fogRec.getNy() });
            rec.setCorrelationObject(fogRec);
            dataStore.addDataRecord(rec, metaId);
        }

        if (fogRec.getIR_10_7Array() != null) {
            IDataRecord rec = new IntegerDataRecord(
                    FogRecord.CHANNEL.IR10_7.getChannel(), group,
                    fogRec.getIR_10_7Array(), 2,
                    new long[] { fogRec.getNx(), fogRec.getNy() });
            rec.setCorrelationObject(fogRec);
            dataStore.addDataRecord(rec, metaId);
        }

        return dataStore;
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
                    throw new PluginException("Error retrieving FOG HDF5 data",
                            e);
                }
                retVal.add(record);
            }
        }
        return retVal;
    }
}
