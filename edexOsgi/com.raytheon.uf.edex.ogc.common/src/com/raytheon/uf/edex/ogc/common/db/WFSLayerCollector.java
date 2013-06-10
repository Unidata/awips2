package com.raytheon.uf.edex.ogc.common.db;

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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.datadelivery.harvester.ConfigLayer;
import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.EnvelopeUtils;
import com.raytheon.uf.common.datadelivery.registry.Levels;
import com.raytheon.uf.common.datadelivery.registry.PointDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.PointTime;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.registry.WFSPointDataSet;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ImmutableDate;
import com.raytheon.uf.edex.ogc.common.interfaces.IWFSMetaData;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * WFS Layer Collector
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/09/2012   754       dhladky      initial creation
 * 04/01/2013   1746      dhladky      Updated for MADIS implementation
 * 05/29/2013   753       dhladky      Updated Point Data Datasets
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public abstract class WFSLayerCollector<DIMENSION extends SimpleDimension, L extends SimpleLayer<DIMENSION>, R extends PluginDataObject>
        extends DefaultLayerCollector<DIMENSION, L, R> implements
        IWFSMetaData<L> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(WFSLayerCollector.class);

    protected WFSPointDataSet wpds = wpds = new WFSPointDataSet();

    protected PointDataSetMetaData pdsmd = new PointDataSetMetaData();

    protected PointTime time = new PointTime();

    protected Coverage coverage = new Coverage();

    protected final L layer;
    
    public abstract  PluginDataObject[] geoFilter(PluginDataObject[] pdos);

    public WFSLayerCollector(LayerTransformer<DIMENSION, L> transformer,
            Class<L> layerClass, Class<R> recordClass) {
        super(transformer, layerClass, recordClass);
        this.layer = newLayer();
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
                time.setFormat(getConfiguration().getAgent().getDateFormat());
                SimpleDateFormat dateFormat = new SimpleDateFormat(
                        getConfiguration().getAgent().getDateFormat());
                dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
                time.setStart(dateFormat.format(times.get(0)));
                time.setEnd(dateFormat.format(times.size() - 1));
            }
        }
    }

    protected Time getTime() {
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
            sb2.append("/");
            sb2.append(getAgent().getWfs());
            sb2.append("?");
            sb2.append("request=GetFeature&");
            sb2.append("typename=");
            sb2.append(getAgent().getLayer(layer.getName()).getNamespace());
            sb2.append(":");
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
            ConfigLayer configLayer = getAgent().getLayer(layer.getName());
            Coordinate lowerRight = new Coordinate(configLayer.getMaxx(),
                    configLayer.getMiny());
            Coordinate upperLeft = new Coordinate(configLayer.getMinx(),
                    configLayer.getMaxy());
            ReferencedEnvelope re = EnvelopeUtils.createLatLonEnvelope(
                    lowerRight, upperLeft);
            coverage.setEnvelope(re);
        }
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
            try {
                date = new ImmutableDate(getTime().getEndDate());
            } catch (ParseException e) {
                statusHandler.handle(Priority.ERROR, "Date failed to parse!");
            }
            getDataSetMetaData().setDate(date);
            storeMetaData(getDataSetMetaData());
        }
    }
    
    /**
     * Adds the new times to the layer, transmit meta data
     */
    public void buildLayerUpdate() {
        synchronized (layer) {
            try {
                if (!layer.getTimes().isEmpty()) {
                    sendMetaData(layer);
                    statusHandler.info("MetaData update " + layer.getName()
                            + ": times: " + layer.getTimes().size());
                    layer.getTimes().clear();
                } else {
                    statusHandler.info("No new metadata to update " + layer.getName());
                }
            } catch (Exception e) {
                statusHandler.error("problem updating " + layer.getName()
                        + " layer. " + e);
            }
        }
    }

    @Override
    public Levels getLevels(DataLevelType type, String collectionName) {
        // not implement in point data yet
        return null;
    }
   
}
