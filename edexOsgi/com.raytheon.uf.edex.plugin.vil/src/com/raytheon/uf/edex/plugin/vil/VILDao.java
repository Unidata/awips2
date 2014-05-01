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

package com.raytheon.uf.edex.plugin.vil;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.vil.VILRecord;
import com.raytheon.uf.common.dataplugin.vil.VILRecord.DATA_TYPE;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.database.plugin.PluginDao;

/**
 * vil specified data access object, modeled off grid & radar.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 11/11/09     2027         dhladky    Initial Creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public class VILDao extends PluginDao {

    public VILDao(final String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected IDataStore populateDataStore(final IDataStore dataStore,
            final IPersistable obj) throws Exception {
        VILRecord VILRec = (VILRecord) obj;

        String compression = PluginRegistry.getInstance()
                .getRegisteredObject(pluginName).getCompression();
        StorageProperties sp = null;
        if (compression != null) {
            sp = new StorageProperties();
            sp.setCompression(StorageProperties.Compression
                    .valueOf(compression));
        }

        if ((VILRec.getDataArray() != null)
                && VILRec.getFieldName().equals(DATA_TYPE.VILD.name())) {

            IDataRecord rec = new FloatDataRecord("Data", VILRec.getDataURI(),
                    VILRec.getDataArray(), 2, new long[] { VILRec.getNx(),
                            VILRec.getNy() });
            rec.setCorrelationObject(VILRec);
            dataStore.addDataRecord(rec, sp);
        }

        if ((VILRec.getDataArray() != null)
                && VILRec.getFieldName().equals(DATA_TYPE.DVILD.name())) {
            IDataRecord rec = new FloatDataRecord("Data", VILRec.getDataURI(),
                    VILRec.getDataArray(), 2, new long[] { VILRec.getNx(),
                            VILRec.getNy() });
            rec.setCorrelationObject(VILRec);
            dataStore.addDataRecord(rec, sp);
        }

        if ((VILRec.getDataArray() != null)
                && VILRec.getFieldName().equals(DATA_TYPE.EDVILD.name())) {
            IDataRecord rec = new FloatDataRecord("Data", VILRec.getDataURI(),
                    VILRec.getDataArray(), 2, new long[] { VILRec.getNx(),
                            VILRec.getNy() });
            rec.setCorrelationObject(VILRec);
            dataStore.addDataRecord(rec, sp);
        }

        logger.debug("VILDao: writing " + VILRec.toString());

        return dataStore;
    }

    @Override
    public List<IDataRecord[]> getHDF5Data(
            final List<PluginDataObject> objects, final int tileSet)
            throws PluginException {
        List<IDataRecord[]> retVal = new ArrayList<IDataRecord[]>();

        for (PluginDataObject obj : objects) {
            IDataRecord[] record = null;

            if (obj instanceof IPersistable) {
                /* connect to the data store and retrieve the data */
                try {
                    record = getDataStore((IPersistable) obj).retrieve(
                            obj.getDataURI());
                } catch (Exception e) {
                    throw new PluginException("Error retrieving vil HDF5 data",
                            e);
                }
                retVal.add(record);
            }
        }
        return retVal;
    }
}
