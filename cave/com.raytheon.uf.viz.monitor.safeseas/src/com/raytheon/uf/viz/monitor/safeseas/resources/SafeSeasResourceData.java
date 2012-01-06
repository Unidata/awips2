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
 */
package com.raytheon.uf.viz.monitor.safeseas.resources;

import java.io.File;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

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
import com.raytheon.uf.viz.monitor.fog.threshold.FogAlgorithmMgr;
import com.raytheon.uf.viz.monitor.safeseas.SafeSeasMonitor;

/**
 * SafeSeasResourceData
 * 
 * Implements empty display for SAFESEAS
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date             Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    July 21, 2010    4891        skorolev    Initial Creation.
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1
 */
/**
 * @author root
 * 
 */
@XmlAccessorType(XmlAccessType.NONE)
public class SafeSeasResourceData extends AbstractRequestableResourceData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SafeSeasResourceData.class);

	@XmlAttribute
	protected String plotSource = "SAFESEAS Table";

	public FogRecord[] records;

    public Map<Date, FogRecord> dataObjectMap;

	protected SafeSeasMonitor monitor;

    public HashMap<Date, Boolean> plotted;

	protected FogAlgorithmMgr fogAlgMgr;

	protected SSFogThreat fogThreatSS;


    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon
     * .uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
	protected AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties, PluginDataObject[] objects) {

		records = new FogRecord[objects.length];
        dataObjectMap = new HashMap<Date, FogRecord>();
        plotted = new HashMap<Date, Boolean>();

		for (int i = 0; i < objects.length; i++) {
			records[i] = (FogRecord) objects[i];
            try {
                records[i] = populateRecord(records[i]);
            } catch (VizException e) {
                // TODO Auto-generated catch block. Please revise as
                // appropriate.
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
            dataObjectMap.put(records[i].getRefHour().getTime(), records[i]);
            plotted.put(records[i].getRefHour().getTime(), new Boolean(false));
		}

		SafeSeasResource ssRes = new SafeSeasResource(this,
                loadProperties);
		getSafeSeasMonitor().addSSResourceListener(ssRes);
		return ssRes;
    }

    /**
     * @return monitor
     */
    public SafeSeasMonitor getSafeSeasMonitor() {
        if (monitor == null) {
            monitor = SafeSeasMonitor.getInstance();
        }
        return monitor;
    }


    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#update(java.lang.Object
     * )
     */
    @Override
    public void update(Object updateData) {
        // TODO Auto-generated method stub

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
	public FogRecord populateRecord(FogRecord record) throws VizException {
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
	 * 
	 * @return
	 */
	protected SSFogThreat getSSFogThreat() {
		if (fogThreatSS == null) {
			fogThreatSS = new SSFogThreat(getAlgorithmManager()
					.getAlgorithmXML());
		}
		return fogThreatSS;
	}

	/** Get the SafeSeasMonitor monitor **/
	protected SafeSeasMonitor getFogMonitor() {
		if (monitor == null) {
			monitor = SafeSeasMonitor.getInstance();
		}
		return monitor;
	}
	
    /**
     * @return plotSource
     */
    public String getPlotSource() {
        return plotSource;
    }

    /**
     * @param plotSource
     */
    public void setPlotSource(String plotSource) {
        this.plotSource = plotSource;
    }

}
