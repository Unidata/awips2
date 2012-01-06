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

package com.raytheon.uf.common.dataplugin.qpf.dao;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.qpf.QPFRecord;
import com.raytheon.uf.common.dataplugin.qpf.QPFRecord.DATA_TYPE;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.edex.database.plugin.PluginDao;

/**
 * QPF specified data access object, modeled off grid & radar.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 02/24/09     2027         dhladky    Initial Creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public class QPFDao extends PluginDao {

    public QPFDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        QPFRecord qpfRec = (QPFRecord) obj;

        if (qpfRec.getDataArray() != null
                && qpfRec.getFieldName().equals(DATA_TYPE.P10.name())) {

            IDataRecord rec = new FloatDataRecord("Data", qpfRec.getDataURI(),
                    qpfRec.getDataArray(), 2, new long[] { qpfRec.getNx(),
                            qpfRec.getNy() });
            rec.setCorrelationObject(qpfRec);
            dataStore.addDataRecord(rec);
        }

        if (qpfRec.getDataArray() != null
                && qpfRec.getFieldName().equals(DATA_TYPE.P25.name())) {
            IDataRecord rec = new FloatDataRecord("Data", qpfRec.getDataURI(),
                    qpfRec.getDataArray(), 2, new long[] { qpfRec.getNx(),
                            qpfRec.getNy() });
            rec.setCorrelationObject(qpfRec);
            dataStore.addDataRecord(rec);
        }

        if (qpfRec.getDataArray() != null
                && qpfRec.getFieldName().equals(DATA_TYPE.P50.name())) {
            IDataRecord rec = new FloatDataRecord("Data", qpfRec.getDataURI(),
                    qpfRec.getDataArray(), 2, new long[] { qpfRec.getNx(),
                            qpfRec.getNy() });
            rec.setCorrelationObject(qpfRec);
            dataStore.addDataRecord(rec);
        }

        if (qpfRec.getDataArray() != null
                && qpfRec.getFieldName().equals(DATA_TYPE.P75.name())) {
            IDataRecord rec = new FloatDataRecord("Data", qpfRec.getDataURI(),
                    qpfRec.getDataArray(), 2, new long[] { qpfRec.getNx(),
                            qpfRec.getNy() });
            rec.setCorrelationObject(qpfRec);
            dataStore.addDataRecord(rec);
        }

        if (qpfRec.getDataArray() != null
                && qpfRec.getFieldName().equals(DATA_TYPE.PRECIP_AMT.name())) {
            IDataRecord rec = new FloatDataRecord("Data", qpfRec.getDataURI(),
                    qpfRec.getDataArray(), 2, new long[] { qpfRec.getNx(),
                            qpfRec.getNy() });
            rec.setCorrelationObject(qpfRec);
            dataStore.addDataRecord(rec);
        }

        if (qpfRec.getDataArray() != null
                && qpfRec.getFieldName().equals(DATA_TYPE.AV_VIL.name())) {
            IDataRecord rec = new FloatDataRecord("Data", qpfRec.getDataURI(),
                    qpfRec.getDataArray(), 2, new long[] { qpfRec.getNx(),
                            qpfRec.getNy() });
            rec.setCorrelationObject(qpfRec);
            dataStore.addDataRecord(rec);
        }
       
        logger.debug("QPFDao: writing " + qpfRec.toString());

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
                    throw new PluginException("Error retrieving qpf HDF5 data",
                            e);
                }
                retVal.add(record);
            }
        }
        return retVal;
    }
}
