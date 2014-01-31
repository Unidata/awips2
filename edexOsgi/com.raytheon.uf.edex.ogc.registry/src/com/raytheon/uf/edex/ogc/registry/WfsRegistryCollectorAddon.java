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
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
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
import com.raytheon.uf.common.geospatial.ISpatialObject;
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
 * Jan 13, 2014 2679       dhladky      Multiple ingest layer windows for a single request window.
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class WfsRegistryCollectorAddon<D extends SimpleDimension, L extends SimpleLayer<D>, R extends PluginDataObject>
        extends RegistryCollectorAddon<D, L, R> implements IWFSMetaData<L> {

    protected final IUFStatusHandler statusHandler = UFStatus.getHandler(this
            .getClass());

    protected final Map<String, WFSPointDataSet> wpds = new HashMap<String, WFSPointDataSet>(1);

    protected final Map<String, PointDataSetMetaData> pdsmds = new HashMap<String, PointDataSetMetaData>(1);

    protected final Map<String, PointTime> times = new HashMap<String, PointTime>(1);

    protected final Map<String, Coverage> coverages = new HashMap<String, Coverage>(1);

    protected final Map<String, L> layers = new HashMap<String, L>(1);
        
    protected final Map<String, Date> previousTimes = new HashMap<String, Date>(1);
    
    protected int roundCutoff = 45;
    
    /**  Used to identify breaking character for URL and unique naming key **/
    public static final String UNIQUE_ID_SEPARATOR = ",";

    public WfsRegistryCollectorAddon() {
        
        super();
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
        synchronized (layers) {
            // find enclosing layer to assign to
            String layerName = isWithinLayer(record);
            L _layer = layers.get(layerName);
            copy(_layer);
            Date time = getTime(record);
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

        /**
         * this interfaces with the WfsSource, in registry collectors there
         * exists a one to many relationship for featureType to layers. In this
         * case we just re-init if they are null, not caring what got passed in;
         */

        synchronized (layers) {
            for (Entry<String, L> entry : layers.entrySet()) {

                L _layer = entry.getValue();

                if (_layer == null) {
                    synchronized (_layer) {
                        initializeLayer(_layer);
                        layers.put(layer.getName(), _layer);
                    }
                }
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
            
            Coverage coverage = getCoverage(layer.getName());
            Coordinate lowerRight = coverage.getLowerRight();
            Coordinate upperLeft = coverage.getUpperLeft();
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

            // create the main point data set
            setDataSet(layer);
            // install main dataset name on registry
            WFSPointDataSet pds = getDataSet(layer.getName());
            storeDataSet(pds);
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
    public WFSPointDataSet getDataSet(String layerName) {
        
        WFSPointDataSet wpd = wpds.get(layerName);
        
        if (wpd == null) {
            wpd = new WFSPointDataSet();
            wpds.put(layerName, wpd);
        }
      
        return wpd;
    }

    /**
     * Sets the times
     * 
     * @param layer
     */
    protected void setTime(L layer) {
        synchronized (layer) {
            // always creating a new time object for transfer
            List<Date> timeList = new ArrayList<Date>();
            // for dataset we just create a place holder object
            // DataSetMetaData gets the actual times
            if (layer.getTimes() != null) {
                for (Date time : layer.getTimes()) {
                    timeList.add(time);
                }

                PointTime time = getTime(layer.getName());
                time.setTimes(timeList);
                time.setNumTimes(timeList.size());
                // apply date format
                SimpleDateFormat dateFormat = new SimpleDateFormat(
                        getConfiguration().getAgent().getDateFormat());
                dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
                time.setFormat(dateFormat.toPattern());
                if (!timeList.isEmpty()) {
                    
                    Date previousTime = previousTimes.get(layer.getName());
                    
                    if (previousTime == null) {
                        time.setStart(timeList.get(0));
                    } else {
                        try {
                            time.setStartDate(dateFormat.format(previousTime));
                        } catch (ParseException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "Failed to parse date.", e);
                        }
                    }
                    Date lastTime = timeList.get(timeList.size() - 1);
                    try {
                        time.setEndDate(dateFormat.format(lastTime));
                    } catch (ParseException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Failed to parse date.", e);
                    }
                    setPreviousTime(layer.getName(), lastTime);
                }
            }
        }
    }

    /**
     * Get a point time
     * 
     * @param layerName
     * @return PointTime
     */
    protected PointTime getTime(String layerName) {
        
        PointTime time = times.get(layerName);
        
        if (time == null) {
           time = new PointTime();
           times.put(layerName, time);
        }
        
        return time;
    }

    @Override
    protected void setDataSet(L layer) {
        // set general settings
        synchronized (layer) {
            // set coverage if not set
            setCoverage(layer);
            WFSPointDataSet wpd = getDataSet(layer.getName());
            Coverage coverage = getCoverage(layer.getName());
            wpd.setCoverage(coverage);
            wpd.setDataSetName(layer.getName());
            wpd.setProviderName(getConfiguration().getProvider().getName());
            wpd.setCollectionName(layer.getName());
            // set parameters if not already
            setParameters(layer);
            wpd.setParameters(getParameters());
            // set time if not already
            setTime(layer);
            wpd.setTime(getTime(layer.getName()));
        }
    }

    @Override
    public PointDataSetMetaData getDataSetMetaData(String layerName) {
        
        PointDataSetMetaData pdsmd = pdsmds.get(layerName);
        
        if (pdsmd == null) {
            pdsmd = new PointDataSetMetaData();
            pdsmds.put(layerName, pdsmd);
        }
        
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
            
            PointDataSetMetaData pdsmd = getDataSetMetaData(layer.getName());
            pdsmd.setDataSetDescription(sb.toString());
            pdsmd.setDataSetName(layer.getName());
            pdsmd.setProviderName(getConfiguration().getProvider().getName());
            StringBuilder sb2 = new StringBuilder();
            sb2.append(getConfiguration().getProvider().getConnection()
                    .getUrl());
            /**
             * this is added to the URL because the URL is what the registry
             * uses as it's unique identifier. All WFS/WCS requests use the same
             * URL, we have to add the dataset name to ensure uniqueness of the
             * key in the registry.  This is stripped off and discarded in retrieval.
             **/
            sb2.append(UNIQUE_ID_SEPARATOR);
            sb2.append(layer.getName());
            pdsmd.setUrl(sb2.toString());
        }
    }

    /**
     * Sets the coverage
     */
    @Override
    protected void setCoverage(L layer) {
        synchronized (layer) {
            getCoverage(layer.getName());
        }
    }

    /**
     * Make me a new coverage
     * 
     * @param layerName
     * @return Coverage
     */
    private Coverage getNewCoverage(String layerName) {
        
        Coverage coverage = new Coverage();
        ConfigLayer configLayer = getAgent().getLayer(layerName);
        Coordinate lowerRight = new Coordinate(configLayer.getMaxx(),
                configLayer.getMiny());
        Coordinate upperLeft = new Coordinate(configLayer.getMinx(),
                configLayer.getMaxy());
        ReferencedEnvelope re = EnvelopeUtils.createLatLonEnvelope(lowerRight,
                upperLeft);
        coverage.setEnvelope(re);
        
        return coverage;
    }

    /**
     * Gets the coverage
     * 
     * @param layer
     * @param layername
     * @return
     */
    @Override
    protected Coverage getCoverage(String layerName) {
        
        Coverage coverage = coverages.get(layerName);
        
        if (coverage == null) {
            
            coverage = getNewCoverage(layerName);
            coverages.put(layerName, coverage);
        }
        
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
            times.put(layer.getName(), new PointTime());
            // harvests the times from the layer
            setTime(layer);
            // make sure you populate the metadata
            setDataSetMetaData(layer);
            getDataSetMetaData(layer.getName()).setTime(getTime(layer.getName()));
            ImmutableDate date = null;
            date = new ImmutableDate(getTime(layer.getName()).getEnd());
            getDataSetMetaData(layer.getName()).setDate(date);
            storeMetaData(getDataSetMetaData(layer.getName()));
        }
    }

    /**
     * Adds the new times to the layers, transmit meta data objects
     */
    public void buildLayerUpdate() {
  
        synchronized (layers) {
            for (String name : layers.keySet()) {
                // grab each layer to evaluate
                L layer = layers.get(name);

                try {
                    if (!layer.getTimes().isEmpty()) {
                        sendMetaData(layer);
                        statusHandler.info("MetaData update " + layer.getName()
                                + ": times: " + layer.getTimes().size());
                        layer.getTimes().clear();
                    } else {
                        statusHandler.info("No new metadata to update "
                                + layer.getName());
                    }
                } catch (Exception e) {
                    statusHandler.error("problem updating " + layer.getName()
                            + " layer. " + e);
                }
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
    public Date getPreviousTime(String layerName) {
        
        Date previous = previousTimes.get(layerName);
        return previous;
    }

    /**
     * Sets the marker time for previousDate
     * @param layername
     * @param previousTime
     */
    public void setPreviousTime(String layerName, Date previousTime) {
        previousTimes.put(layerName, previousTime);
    }
    
    /**
     * Filter geographically
     * @param pdos
     */
    public PluginDataObject[] filter(PluginDataObject[] pdos) {

        Collection<PluginDataObject> withInGeoConstraint = new ArrayList<PluginDataObject>();
        PluginDataObject[] pdor = null;

        for (PluginDataObject record : pdos) {

            if (record != null) {

                @SuppressWarnings("unchecked")
                ISpatialObject spatial = getSpatial((R) record);
                Coordinate c = spatial.getGeometry().getCoordinate();

                // Have to loop over the layers in order to separate them.
                for (String name : layers.keySet()) {

                    Coverage coverage = getCoverage(name);

                    if (coverage != null) {

                        Envelope e = coverage.getEnvelope();

                        if (c != null) {

                            if (e.contains(c)) {
                                withInGeoConstraint.add(record);
                                // once it is found to be within one, you are done.
                                break;
                            } else {
                                statusHandler.handle(
                                        Priority.DEBUG,
                                        "Record discarded:  outside of range: "
                                                + c.x + " "
                                                + c.y);
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
    
    @Override
    public String isWithinLayer(R record) {

        String layerName = null;

        if (record != null) {

            ISpatialObject spatial = getSpatial(record);
            Coordinate c = spatial.getGeometry().getCoordinate();

            // Figure out which layer to tally this record too.
            for (String name : layers.keySet()) {

                Coverage coverage = getCoverage(name);

                if (coverage != null) {

                    Envelope e = coverage.getEnvelope();

                    if (c != null) {

                        if (e.contains(c)) {
                            layerName = name;
                            break;
                        }
                    }
                }
            }
        }

        return layerName;
    }

}
