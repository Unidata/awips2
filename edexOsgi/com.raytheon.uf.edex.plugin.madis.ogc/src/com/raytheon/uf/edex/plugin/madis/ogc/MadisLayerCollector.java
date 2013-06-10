package com.raytheon.uf.edex.plugin.madis.ogc;

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
import java.util.Collection;
import java.util.Date;
import java.util.SortedSet;
import java.util.TreeSet;

import org.geotools.geometry.jts.JTS;
import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.datadelivery.harvester.ConfigLayer;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.madis.MadisRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.ogc.common.db.WFSLayerCollector;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * 
 * Madis Layer Collector
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/01/2013   1746       dhladky      Initial creation
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class MadisLayerCollector extends WFSLayerCollector<MadisDimension, MadisLayer, MadisRecord> {

    private static final String MADIS_LAYER_NAME = "madis";

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MadisLayerCollector.class);

    private final int roundCutoff = 45;

    private volatile boolean alreadyInitialized; 
    
    public MadisLayerCollector(
            LayerTransformer<MadisDimension, MadisLayer> transformer) {
        super(transformer, MadisLayer.class, MadisRecord.class);
        initializeLayer(layer, new MadisRecord());
    }

    @Override
    public void addAll(Collection<MadisRecord> coll) {
        for (MadisRecord rec : coll) {
            addToTimes(layer, rec);
            addToDims(layer, rec);
        }
    }

    private Date getTime(MadisRecord record) {
        Date timeObs = record.getTimeObs();
        return roundToHour(timeObs, roundCutoff);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void addToTimes(MadisLayer layer, MadisRecord record) {
        synchronized (layer) {
            if (record != null) {
                SortedSet<Date> times = layer.getTimes();
                times.add(getTime(record));
                statusHandler.handle(Priority.DEBUG, "Adding madis layer: "
                        + record.toString());
            }
        }
    }

    /**
     * Filter geographically
     */
    public PluginDataObject[] geoFilter(PluginDataObject[] pdos) {

        Collection<MadisRecord> withInGeoConstraint = new ArrayList<MadisRecord>();
        PluginDataObject[] pdor = null;
        
        synchronized (layer) {

            for (PluginDataObject record : pdos) {

                MadisRecord rec = (MadisRecord) record;
                
                if (rec != null) {

                    Envelope e = getCoverage().getEnvelope();

                    if (rec.getLocation() != null) {

                        Coordinate c = rec.getLocation().getLocation()
                                .getCoordinate();

                        if (c != null) {

                            if (e.contains(c)) {
                                withInGeoConstraint.add(rec);
                            } else {
                                statusHandler.handle(Priority.DEBUG,
                                        "Madis record discarded:  outside of range: "
                                                + rec.getLatitude() + " "
                                                + rec.getLongitude());
                            }
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

    /**
     * {@inheritDoc}
     */
    @Override
    protected boolean initializeLayer(MadisLayer layer, MadisRecord rec) {
        if (!alreadyInitialized) {
            synchronized (layer) {
                layer.setName(MADIS_LAYER_NAME);
                // create the main point data set
                setDataSet(layer);
                ConfigLayer configLayer = getAgent().getLayer(layer.getName());
                Coordinate lowerRight = getCoverage().getLowerRight();
                Coordinate upperLeft = getCoverage().getUpperLeft();
                ReferencedEnvelope env = new ReferencedEnvelope(upperLeft.x,
                        lowerRight.x, lowerRight.y, upperLeft.y,
                        MapUtil.LATLON_PROJECTION);
                layer.setCrs84Bounds(JTS.toGeometry((Envelope) env));
                layer.setTargetCrsCode(configLayer.getCrs());
                layer.setTargetMaxx(env.getMaxX());
                layer.setTargetMaxy(env.getMaxY());
                layer.setTargetMinx(env.getMinX());
                layer.setTargetMiny(env.getMinY());
                layer.setTimes(new TreeSet<Date>());
                // install main dataset name on registry
                storeDataSet(getDataSet());
            }
            alreadyInitialized = true;
        }
        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getLayerName(MadisRecord rec) {
        return MADIS_LAYER_NAME;
    }

}
