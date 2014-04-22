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
package com.raytheon.uf.viz.vil;

import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.dataplugin.HDF5Util;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.vil.VILRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;


/**
 * VILResourceData
 * 
 * Implements contouring for vil data
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    11Nov2009    2037        dhladky    Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "vilResourceData")
public class VILResourceData extends AbstractRequestableResourceData {

    public VILRecord[] records;
   
    public Map<DataTime, VILRecord> dataObjectMap;
  
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects) {
        records = new VILRecord[objects.length];
        dataObjectMap = new HashMap<DataTime, VILRecord>();
        
        for (int i = 0; i < objects.length; i++) {
            records[i] = (VILRecord) objects[i];
            dataObjectMap.put(records[i].getDataTime(), records[i]);
        }

        return new VILResource(this, loadProperties);
    }

    /**
     * @return the records
     */
    public VILRecord[] getRecords() {
        return records;
    }

    /**
     * @param records
     *            the records to set
     */
    public void setRecords(VILRecord[] records) {
        this.records = records;
    }
    
    /**
     * populate VIL Record
     * 
     * @param record
     */
    public VILRecord populateRecord(VILRecord record)
            throws VizException {
        IDataStore dataStore = getDataStore(record);
        record.retrieveFromDataStore(dataStore);
        return record;
    }
    
    /**
     * Get the data store
     * 
     * @param record
     * @return
     */
    private IDataStore getDataStore(VILRecord record) {
        return DataStoreFactory.getDataStore(HDF5Util.findHDF5Location(record));
    }
}
