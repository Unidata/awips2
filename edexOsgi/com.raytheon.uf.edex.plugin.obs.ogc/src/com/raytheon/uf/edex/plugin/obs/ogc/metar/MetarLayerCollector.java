package com.raytheon.uf.edex.plugin.obs.ogc.metar;

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
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.SortedSet;
import java.util.TreeSet;

import org.geotools.geometry.jts.JTS;
import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.obs.metar.MetarRecord;
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
 * Metar Layer Collector
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/09/2012   753       dhladky      Modified from a class written by Brian Clements
 * 04/01/2013   1746      dhladky      Updated for MADIS impl.
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class MetarLayerCollector extends
        WFSLayerCollector<MetarDimension, MetarLayer, MetarRecord> {

    private static final String METAR_LAYER_NAME = "metar";

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MetarLayerCollector.class);

    private final boolean truncate = true;

    private final int roundCutoff = 45;

    private volatile boolean alreadyInitialized;

    public MetarLayerCollector(
            LayerTransformer<MetarDimension, MetarLayer> transformer) {
        super(transformer, MetarLayer.class, MetarRecord.class);
        initializeLayer(layer, new MetarRecord());
    }

    @Override
    public void addAll(Collection<MetarRecord> coll) {
        for (MetarRecord rec : coll) {
            addToTimes(layer, rec);
            addToDims(layer, rec);
        }
    }

    private Calendar getTime(MetarRecord record) {
        Calendar timeObs = record.getTimeObs();
        if (truncate) {
            return truncateToHour(timeObs);
        } else {
            return roundToHour(timeObs, roundCutoff);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void addToTimes(MetarLayer layer, MetarRecord record) {
        synchronized (layer) {
            if (record != null) {
                SortedSet<Date> times = layer.getTimes();
                times.add(getTime(record).getTime());
                statusHandler.handle(Priority.DEBUG, "Adding metar layer: "
                        + record.toString());
            }

        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected boolean initializeLayer(MetarLayer layer, MetarRecord rec) {
        if (!alreadyInitialized) {
            synchronized (layer) {
                layer.setName(METAR_LAYER_NAME);
                // create the main point data set
                setDataSet(layer);
                Coordinate lowerRight = getCoverage().getLowerRight();
                Coordinate upperLeft = getCoverage().getUpperLeft();
                ReferencedEnvelope env = new ReferencedEnvelope(upperLeft.x,
                        lowerRight.x, lowerRight.y, upperLeft.y,
                        MapUtil.LATLON_PROJECTION);
                layer.setCrs84Bounds(JTS.toGeometry((Envelope) env));
                layer.setTargetCrsCode("CRS:84");
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
    protected String getLayerName(MetarRecord rec) {
        return METAR_LAYER_NAME;
    }

    /**
     * Filter geographically
     */

    public PluginDataObject[] geoFilter(PluginDataObject[] pdos) {

        Collection<MetarRecord> withInGeoConstraint = new ArrayList<MetarRecord>();
        PluginDataObject[] pdor = null;

        synchronized (layer) {

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
      
}
