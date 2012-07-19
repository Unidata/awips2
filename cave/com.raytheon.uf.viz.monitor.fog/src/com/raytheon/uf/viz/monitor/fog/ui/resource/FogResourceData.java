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
package com.raytheon.uf.viz.monitor.fog.ui.resource;

import java.io.File;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.fog.FogRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.comm.Loader;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.monitor.fog.FogMonitor;
import com.raytheon.uf.viz.monitor.fog.threshold.FogAlgorithmMgr;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay;


/**
 * FogResourceData
 * 
 * Implements contouring for fog data
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    12Dec2009    3963        dhladky    Initial Creation.
 *    16Jun2012    14386       zhao       Modified to keep only latest fog record for each frame
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "fogResourceData")
public class FogResourceData extends AbstractRequestableResourceData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FogResourceData.class);
    public FogRecord[] records;
   
    public Map<Date, FogRecord> dataObjectMap;
    
    public Map<Date, GriddedImageDisplay> gridImageMap;
    
    protected FogMonitor monitor;
    
    protected FogAlgorithmMgr fogAlgMgr;
    
    protected FogThreat fogThreat;
   
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects) {
     
        records = new FogRecord[objects.length];
        dataObjectMap = new HashMap<Date, FogRecord>();
        gridImageMap = new HashMap<Date, GriddedImageDisplay>();
        
        for (int i = 0; i < objects.length; i++) {
            records[i] = (FogRecord) objects[i];
            try {
                records[i] = populateRecord(records[i]);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            }
            if ( dataObjectMap.containsKey(records[i].getRefHour().getTime()) ) {
            	if ( dataObjectMap.get(records[i].getRefHour().getTime()).getDataTime().greaterThan(records[i].getDataTime()) ) {
            		continue;
            	} else {
            		dataObjectMap.remove(records[i].getRefHour().getTime());
            		gridImageMap.remove(records[i].getRefHour().getTime());
            	}
            }
            dataObjectMap.put(records[i].getRefHour().getTime(), records[i]);
            gridImageMap.put(records[i].getRefHour().getTime(), null);
        }

        FogResource fogRes = new FogResource(this, loadProperties);
        getFogMonitor().addFogResourceListener(fogRes);
        
        return fogRes;
    }

    /**
     * @return the records
     */
    public FogRecord[] getRecords() {
        return records;
    }
    
    /**
     * @param records
     *            the records to set
     */
    public void setRecords(FogRecord[] records) {
        this.records = records;
    }
    
    /**
     * populate Fog Record
     * 
     * @param record
     */
    public FogRecord populateRecord(FogRecord record)
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
    private IDataStore getDataStore(FogRecord record) {
        IDataStore dataStore = null;
        try {
            Map<String, Object> vals = new HashMap<String, Object>();
            vals.put("dataURI", record.getDataURI());
            vals.put("pluginName", record.getPluginName());

            record = (FogRecord) Loader.loadData(vals);

            File loc = HDF5Util.findHDF5Location(record);
            dataStore = DataStoreFactory.getDataStore(loc);

        } catch (VizException e) {
            e.printStackTrace();
        }

        return dataStore;
    }
    
    /** Get the Fog Algorithm manager **/
    protected FogAlgorithmMgr getAlgorithmManager() {
        
        if (fogAlgMgr == null) {
            fogAlgMgr = FogAlgorithmMgr.getInstance();
        }
        return fogAlgMgr;
    }
    
    /**
     * Gets the fog Threat generator
     * @return
     */
    protected FogThreat getFogThreat() {
        if (fogThreat == null) {
            fogThreat = new FogThreat(getAlgorithmManager().getAlgorithmXML());
        }
        return fogThreat;
    }
        
    /** Get the Fog monitor **/
    protected FogMonitor getFogMonitor() {
        if (monitor == null) {
            monitor = FogMonitor.getInstance();
        }
        return monitor;
    }
       
    public void resetGridImgMap() {
        for(Date key: dataObjectMap.keySet()){
            gridImageMap.put(key, null);
        }
    }

    
}
