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
package com.raytheon.uf.edex.plugin.preciprate;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.preciprate.PrecipRateRecord;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.edex.database.plugin.PluginDao;

/**
 * PrecipRate specified data access object.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 01/27/10     4236        dhladky    Initial Creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public class PrecipRateDao extends PluginDao {

    public PrecipRateDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        PrecipRateRecord prRec = (PrecipRateRecord) obj;

        if (prRec.getRawData() != null) {
            IDataRecord rec = new ByteDataRecord("Data", prRec.getDataURI(),
                    prRec.getRawData(), 2, new long[] { prRec.getNumRadials(),
                            prRec.getNumBins() });
            rec.setCorrelationObject(prRec);
            dataStore.addDataRecord(rec);
        }

        if (prRec.getAngleData() != null) {
            IDataRecord rec = new FloatDataRecord("Angles", prRec.getDataURI(),
                    prRec.getAngleData(), 1,
                    new long[] { prRec.getNumRadials() });
            rec.setCorrelationObject(prRec);
            dataStore.addDataRecord(rec);
        }

        if (prRec.getDhrMap() != null) {
            byte[] data = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).serialize(prRec.getDhrMap());
            ByteDataRecord bdr = new ByteDataRecord("DHRMap",
                    prRec.getDataURI(), data);
            dataStore.addDataRecord(bdr);
        }

        logger.debug("PrecipRateDao: writing " + prRec.toString());

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
                            "Error retrieving Precipitation Rate HDF5 data", e);
                }
                retVal.add(record);
            }
        }
        return retVal;
    }
}
