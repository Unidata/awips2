/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.plugin.obs.registry;

import java.util.Calendar;
import java.util.Date;

import com.raytheon.uf.common.datadelivery.harvester.ConfigLayer;
import com.raytheon.uf.common.datadelivery.harvester.OGCAgent;
import com.raytheon.uf.common.dataplugin.obs.metar.MetarRecord;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.edex.ogc.common.db.DefaultPointDataDimension;
import com.raytheon.uf.edex.ogc.common.db.LayerCollector;
import com.raytheon.uf.edex.ogc.common.util.PluginIngestFilter;
import com.raytheon.uf.edex.ogc.registry.WfsRegistryCollectorAddon;
import com.raytheon.uf.edex.plugin.obs.ogc.metar.MetarLayer;

/**
 * Registry Collector for Observations
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2013            bclement     Initial creation
 * Jan 13, 2014  #2679     dhladky      multiple layers
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class ObsRegistryCollectorAddon
        extends
        WfsRegistryCollectorAddon<DefaultPointDataDimension, MetarLayer, MetarRecord>
        implements PluginIngestFilter {

    public ObsRegistryCollectorAddon() {

        super();
        OGCAgent agent = getAgent();
        
        for (ConfigLayer clayer: agent.getLayers()) {
            MetarLayer layer = new MetarLayer();
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
    protected Date getTime(MetarRecord record) {
        Calendar time = record.getTimeObs();
        return LayerCollector.roundToHour(time.getTime(), roundCutoff);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.registry.WfsRegistryCollectorAddon#copy(com.
     * raytheon.uf.edex.ogc.common.db.SimpleLayer)
     */
    @Override
    protected MetarLayer copy(MetarLayer layer) {
        return new MetarLayer(layer);
    }

    @Override
    public SurfaceObsLocation getSpatial(MetarRecord record) {
        
        if (record.getLocation() != null) {
            return record.getLocation();
        }
        return null;
    }

}
