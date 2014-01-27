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
package com.raytheon.uf.edex.ogc.registry;

import java.lang.reflect.Constructor;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeSet;

import org.geotools.geometry.jts.JTS;
import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.datadelivery.harvester.ConfigLayer;
import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.EnvelopeUtils;
import com.raytheon.uf.common.datadelivery.registry.Levels;
import com.raytheon.uf.common.datadelivery.registry.PointDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.PointTime;
import com.raytheon.uf.common.datadelivery.registry.WFSPointDataSet;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ImmutableDate;
import com.raytheon.uf.edex.ogc.common.db.SimpleDimension;
import com.raytheon.uf.edex.ogc.common.db.SimpleLayer;
import com.raytheon.uf.edex.ogc.common.interfaces.IWFSMetaData;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Collector addon for WFS collectors with a single layer that adds time
 * information to registry
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 23, 2013            bclement     Initial creation
 * Aug 08, 2013 2097       dhladky      Made operational
 * Aug 30, 2013 2098       dhladky      Improved
 * Sept 2, 2013 2098       dhladky      Updated how times are managed.
 * Sept 30, 2013 1797      dhladky      Generics
 * Oct 10, 2013 1797       bgonzale     Refactored registry Time objects.
 * Nov 6,  2013 2525       dhladky      Stop appending "/wfs"
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class WfsRegistryCollectorAddon<D extends SimpleDimension, L extends SimpleLayer<D>, R extends PluginDataObject>
        extends RegistryCollectorAddon<D, L, R> implements IWFSMetaData<L> {

    protected final IUFStatusHandler statusHandler = UFStatus.getHandler(this
            .getClass());

    protected WFSPointDataSet wpds = new WFSPointDataSet();

    protected PointDataSetMetaData pdsmd = new PointDataSetMetaData();

    protected PointTime time = new PointTime();

    protected Coverage coverage = new Coverage();

    protected volatile L _layer = null;

    protected int roundCutoff = 45;

    protected final String layerName;
    
    protected Date previousTime = null;

    /**
     * @param layerName
     */
    public WfsRegistryCollectorAddon(String layerName) {
        this.layerName = layerName;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.db.CollectorAddon#onCollect(com.raytheon
     * .uf.edex.ogc.common.db.SimpleLayer,
     * com.raytheon.uf.common.dataplugin.PluginDataObject)
     */
    @Override
    public void onCollect(L layer, R record) {
        ensureLayer(layer);
        synchronized (_layer) {
            Date time = record.getDataTime().getRefTime();
            _layer.getTimes().add(time);
        }
    }

    /**
     * extract point data time from record
     * 
     * @param record
     * @return
     */
    protected Date getTime(R record) {
        Date refTime = record.getDataTime().getRefTime();
        return refTime;
    }

    /**
     * Ensure that local layer has been initialized
     * 
     * @param layer
     */
    private void ensureLayer(L layer) {
        if (_layer == null) {
            synchronized (layerName) {
                if (_layer != null) {
                    // another thread got here first
                    return;
                }
                L copy = copy(layer);
                copy.getTimes().clear();
                initializeLayer(copy);
                _layer = copy;
            }
        } else {
            if (!layer.getName().equals(layerName)) {
                statusHandler
                        .warn("WFS Registry collector used with multiple layers, unexpected behavior may occur");
            }
        }
    }

    /**
     * Return copy of layer
     * 
     * @param layer
     * @return
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    protected L copy(L layer) {
        Class<?> lclass = layer.getClass();
        try {
            Constructor<?> constructor = lclass.getConstructor(lclass);
            return (L) constructor.newInstance(layer);
        } catch (Exception e) {
            statusHandler.error("Unable to clone layer", e);
            return (L) new SimpleLayer<SimpleDimension>((SimpleLayer) layer) {
                private static final long serialVersionUID = -7014438001156256263L;

                @Override
                public Set<SimpleDimension> getDimensions() {
                    return new TreeSet<SimpleDimension>();
                }
            };
        }
    }

    /**
     * @param layer
     */
    protected void initializeLayer(L layer) {
        synchronized (layer) {
            // create the main point data set
            layer.setName(layerName);
            setDataSet(layer);
            Coordinate lowerRight = getCoverage().getLowerRight();
            Coordinate upperLeft = getCoverage().getUpperLeft();
            ReferencedEnvelope env = new ReferencedEnvelope(upperLeft.x,
                    lowerRight.x, lowerRight.y, upperLeft.y,
                    MapUtil.LATLON_PROJECTION);
            layer.setCrs84Bounds(JTS.toGeometry((Envelope) env));
            ConfigLayer configLayer = getAgent().getLayer(layer.getName());
            layer.setTargetCrsCode(configLayer.getCrs());
            layer.setTargetMaxx(configLayer.getMaxx());
            layer.setTargetMaxy(configLayer.getMaxy());
            layer.setTargetMinx(configLayer.getMinx());
            layer.setTargetMiny(configLayer.getMiny());
            layer.setTimes(new TreeSet<Date>());
            // install main dataset name on registry

            storeDataSet(getDataSet());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.ogc.common.db.CollectorAddon#onFinish()
     */
    @Override
    public void onFinish() {

    }

    @Override
    public WFSPointDataSet getDataSet() {
        return wpds;
    }

    /**
     * Sets the times
     * 
     * @param layer
     */
    protected void setTime(L layer) {
        synchronized (layer) {
            // always creating a new time object for transfer
            List<Date> times = new ArrayList<Date>();
            // for dataset we just create a place holder object
            // DataSetMetaData gets the actual times
            if (layer.getTimes() != null) {
                for (Date time : layer.getTimes()) {
                    times.add(time);
                }

                time.setTimes(times);
                time.setNumTimes(times.size());
                // apply date format
                SimpleDateFormat dateFormat = new SimpleDateFormat(
                        getConfiguration().getAgent().getDateFormat());
                dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
                time.setFormat(dateFormat.toPattern());
                if (!times.isEmpty()) {
                    if (previousTime == null) {
                        time.setStart(times.get(0));
                    } else {
                        try {
                            time.setStartDate(dateFormat.format(previousTime));
                        } catch (ParseException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "Failed to parse date.", e);
                        }
                    }
                    Date lastTime = times.get(times.size() - 1);
                    try {
                        time.setEndDate(dateFormat.format(lastTime));
                    } catch (ParseException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Failed to parse date.", e);
                    }
                    setPreviousTime(lastTime);
                }
            }
        }
    }

    protected PointTime getTime() {
        return time;
    }

    @Override
    protected void setDataSet(L layer) {
        // set general settings
        synchronized (layer) {
            // set coverage if not set
            setCoverage(layer);
            wpds.setCoverage(getCoverage());
            wpds.setDataSetName(layer.getName());
            wpds.setProviderName(getConfiguration().getProvider().getName());
            wpds.setCollectionName(layer.getName());
            // set parameters if not already
            setParameters(layer);
            wpds.setParameters(getParameters());
            // set time if not already
            setTime(layer);
            wpds.setTime(getTime());
        }
    }

    @Override
    public PointDataSetMetaData getDataSetMetaData() {
        return pdsmd;
    }

    @Override
    protected void setDataSetMetaData(L layer) {
        // set the dataset first
        synchronized (layer) {
            setDataSet(layer);
            StringBuilder sb = new StringBuilder();
            sb.append(layer.getName()).append(" ");
            sb.append(layer.getTargetMaxx()).append(", ");
            sb.append(layer.getTargetMiny()).append(" : ");
            sb.append(layer.getTargetMinx()).append(", ");
            sb.append(layer.getTargetMaxy());
            pdsmd.setDataSetDescription(sb.toString());
            pdsmd.setDataSetName(layer.getName());
            pdsmd.setProviderName(getConfiguration().getProvider().getName());
            StringBuilder sb2 = new StringBuilder();
            sb2.append(getConfiguration().getProvider().getConnection()
                    .getUrl());
            pdsmd.setUrl(sb2.toString());
        }
    }

    /**
     * Sets the coverage
     */
    @Override
    protected void setCoverage(L layer) {
        synchronized (layer) {
            setCoverage(layer.getName());
        }
    }

    /**
     * @param layerName
     */
    private void setCoverage(String layerName) {
        ConfigLayer configLayer = getAgent().getLayer(layerName);
        Coordinate lowerRight = new Coordinate(configLayer.getMaxx(),
                configLayer.getMiny());
        Coordinate upperLeft = new Coordinate(configLayer.getMinx(),
                configLayer.getMaxy());
        ReferencedEnvelope re = EnvelopeUtils.createLatLonEnvelope(lowerRight,
                upperLeft);
        coverage.setEnvelope(re);
    }

    /**
     * Gets the coverage
     * 
     * @param layer
     * @return
     */
    @Override
    protected Coverage getCoverage() {
        return coverage;
    }

    /**
     * WFS uses Point Data type
     * 
     * @return
     */
    @Override
    public DataType getDataType() {
        return DataType.POINT;
    }

    /**
     * Send the metaData to the registry
     */
    @Override
    public void sendMetaData(L layer) {
        // place into a WFS metadata object.
        synchronized (layer) {
            // creates a new PointTime object
            time = new PointTime();
            // harvests the times from the layer
            setTime(layer);
            // make sure you populate the metadata
            setDataSetMetaData(layer);
            getDataSetMetaData().setTime(getTime());
            ImmutableDate date = null;
            date = new ImmutableDate(getTime().getEnd());
            getDataSetMetaData().setDate(date);
            storeMetaData(getDataSetMetaData());
        }
    }

    /**
     * Adds the new times to the layer, transmit meta data
     */
    public void buildLayerUpdate() {
        if (_layer == null) {
            return;
        }
        synchronized (_layer) {
            try {
                if (!_layer.getTimes().isEmpty()) {
                    sendMetaData(_layer);
                    statusHandler.info("MetaData update " + _layer.getName()
                            + ": times: " + _layer.getTimes().size());
                    _layer.getTimes().clear();
                } else {
                    statusHandler.info("No new metadata to update "
                            + _layer.getName());
                }
            } catch (Exception e) {
                statusHandler.error("problem updating " + _layer.getName()
                        + " layer. " + e);
            }
        }
    }

    @Override
    public Levels getLevels(DataLevelType type, String collectionName) {
        // not implement in point data yet
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.ogc.common.db.CollectorAddon#onPurgeAll()
     */
    @Override
    public void onPurgeAll() {
        // TODO Auto-generated method stub
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.db.CollectorAddon#onPurgeExpired(java
     * .util.Set)
     */
    @Override
    public void onPurgeExpired(Set<Date> timesToKeep) {
        // TODO Auto-generated method stub
    }

    /**
     * Marker time for sending back and requesting data
     * From this date to last will be the next query
     * @return
     */
    public Date getPreviousTime() {
        return previousTime;
    }

    /**
     * Sets the marker time for previousDate
     * @param previousTime
     */
    public void setPreviousTime(Date previousTime) {
        this.previousTime = previousTime;
    }

}
