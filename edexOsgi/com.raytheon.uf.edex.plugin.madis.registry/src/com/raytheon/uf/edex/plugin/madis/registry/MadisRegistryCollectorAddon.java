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
package com.raytheon.uf.edex.plugin.madis.registry;

import java.util.Date;

import com.raytheon.uf.common.datadelivery.harvester.ConfigLayer;
import com.raytheon.uf.common.datadelivery.harvester.OGCAgent;
import com.raytheon.uf.common.dataplugin.madis.MadisRecord;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.edex.ogc.common.util.PluginIngestFilter;
import com.raytheon.uf.edex.ogc.registry.WfsRegistryCollectorAddon;
import com.raytheon.uf.edex.plugin.madis.ogc.MadisDimension;
import com.raytheon.uf.edex.plugin.madis.ogc.MadisLayer;

/**
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2013            bclement     Initial creation
 * Aug 18, 2013 #2097      dhladky      Restored original functionality before renaming of this class
 * Aug 30, 2013 #2098      dhladky      Incorrect time returned
 * Sept 2, 2013 #2098      dhladky      Improved time management.
 * Sept 9, 2013 #2351      dhladky      Speed improvements
 * Jan 13, 2014 #2679      dhladky      multiple ingest layers for a single request window.
 *
 * </pre>
 *
 * @author bclement
 * @version 1.0	
 */
public class MadisRegistryCollectorAddon extends
		WfsRegistryCollectorAddon<MadisDimension, MadisLayer, MadisRecord>
		implements PluginIngestFilter {

	/**
	 * @param layerNames
	 */
	public MadisRegistryCollectorAddon() {

	    super();
	    OGCAgent agent = getAgent();
	    
		for (ConfigLayer clayer: agent.getLayers()) {
		    MadisLayer layer = new MadisLayer();
		    layer.setName(clayer.getName());
		    layers.put(clayer.getName(), layer);
		    initializeLayer(layer);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.ogc.registry.WfsRegistryCollectorAddon#getTime(com
	 * .raytheon.uf.common.dataplugin.PluginDataObject)
	 */
	@Override
	protected Date getTime(MadisRecord record) {
		Date time = record.getTimeObs();
		return time;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.ogc.registry.WfsRegistryCollectorAddon#copy(com.
	 * raytheon.uf.edex.ogc.common.db.SimpleLayer)
	 */
	@Override
	protected MadisLayer copy(MadisLayer layer) {
		return new MadisLayer(layer);
	}

    @Override
    public SurfaceObsLocation getSpatial(MadisRecord record) {
        
        if (record.getLocation() != null) {
            return record.getLocation();
        }
        return null;
    }

	

}
