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
package com.raytheon.uf.viz.cwat;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.cwat.CWATRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;


/**
 * CWATResourceData
 * 
 * Implements contouring for cwat data
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    15Mar2009    2037        dhladky    Initial Creation.
 *    Feb 28, 2013 1731        bsteffen    Remove unneccessary query in
 *                                         getDataStore.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "cwatResourceData")
public class CWATResourceData extends AbstractRequestableResourceData {

    public CWATRecord[] records;
   
    public Map<DataTime, CWATRecord> dataObjectMap;
  
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects) {
        records = new CWATRecord[objects.length];
        dataObjectMap = new HashMap<DataTime, CWATRecord>();
        
        for (int i = 0; i < objects.length; i++) {
            records[i] = (CWATRecord) objects[i];
            dataObjectMap.put(records[i].getDataTime(), records[i]);
        }

        return new CWATResource(this, loadProperties);
    }

    /**
     * @return the records
     */
    public CWATRecord[] getRecords() {
        return records;
    }

    /**
     * @param records
     *            the records to set
     */
    public void setRecords(CWATRecord[] records) {
        this.records = records;
    }
    
    /**
     * populate CWAT Record
     * 
     * @param record
     */
    public CWATRecord populateRecord(CWATRecord record)
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
    private IDataStore getDataStore(CWATRecord record) {
        File loc = HDF5Util.findHDF5Location(record);
        return DataStoreFactory.getDataStore(loc);
    }
}