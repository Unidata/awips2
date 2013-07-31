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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.obs.metar.MetarRecord;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.ogc.common.db.DefaultPointDataDimension;
import com.raytheon.uf.edex.ogc.common.db.LayerCollector;
import com.raytheon.uf.edex.ogc.common.util.PluginIngestFilter;
import com.raytheon.uf.edex.ogc.registry.WfsRegistryCollectorAddon;
import com.raytheon.uf.edex.plugin.obs.ogc.metar.MetarLayer;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

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

    /**
     * @param layerName
     */
    public ObsRegistryCollectorAddon(String layerName) {
        super(layerName);
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

    /**
     * Filter geographically
     */
    public PluginDataObject[] filter(PluginDataObject[] pdos) {

        Collection<MetarRecord> withInGeoConstraint = new ArrayList<MetarRecord>();
        PluginDataObject[] pdor = null;

        for (PluginDataObject record : pdos) {

            MetarRecord rec = (MetarRecord) record;

            if (rec != null) {

                Envelope e = getCoverage().getEnvelope();

                if (rec.getLocation() != null) {

                    Coordinate c = rec.getLocation().getLocation()
                            .getCoordinate();

                    if (c != null) {

                        if (e.contains(c)) {
                            withInGeoConstraint.add(rec);
                        } else {
                            statusHandler.handle(
                                    Priority.DEBUG,
                                    "Obs record discarded:  outside of range: "
                                            + rec.getLatitude() + " "
                                            + rec.getLongitude());
                        }
                    }
                }
            }
        }

        if (!withInGeoConstraint.isEmpty()) {
            int size = withInGeoConstraint.size();
            pdor = withInGeoConstraint.toArray(new PluginDataObject[size]);
        }

        return pdor;
    }

}
