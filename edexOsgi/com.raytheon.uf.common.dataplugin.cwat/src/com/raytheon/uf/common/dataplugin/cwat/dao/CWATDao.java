package com.raytheon.uf.common.dataplugin.cwat.dao;
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
import com.raytheon.uf.common.dataplugin.cwat.CWATRecord;
import com.raytheon.uf.common.dataplugin.cwat.CWATRecord.DATA_TYPE;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.edex.database.plugin.PluginDao;

/**
 * CWAT specified data access object.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 06/03/09     2037         dhladky    Initial Creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public class CWATDao extends PluginDao {

    public CWATDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        CWATRecord cwatRec = (CWATRecord) obj;

        if (cwatRec.getDataArray() != null
                && cwatRec.getFieldName().equals(DATA_TYPE.CWAT.name())) {

            IDataRecord rec = new ShortDataRecord("Data", cwatRec.getDataURI(),
            		cwatRec.getDataArray(), 2, new long[] { cwatRec.getNx(),
            	cwatRec.getNy() });
            rec.setCorrelationObject(cwatRec);
            dataStore.addDataRecord(rec);
        }
        
        if (cwatRec.getThreats() != null) {
            byte[] data = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).serialize(
                            cwatRec.getThreats());
            ByteDataRecord bdr = new ByteDataRecord(CWATRecord.THREATS, cwatRec
                    .getDataURI(), data);
            dataStore.addDataRecord(bdr);
        }

        logger.debug("CWATDao: writing " + cwatRec.toString());

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
                    throw new PluginException("Error retrieving CWAT HDF5 data",
                            e);
                }
                retVal.add(record);
            }
        }
        return retVal;
    }
}

