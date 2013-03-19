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
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public abstract class WFSLayerCollector<L extends SimpleLayer> extends LayerCollector<L> implements IWFSMetaData<L> {

    protected WFSPointDataSet wpds = wpds = new WFSPointDataSet();
    
    protected PointDataSetMetaData pdsmd = new PointDataSetMetaData();
    
    protected PointTime time = new PointTime();

    protected Coverage coverage = new Coverage();
    
    private static final transient IUFStatusHandler statusHandler = UFStatus
    .getHandler(WFSLayerCollector.class);


    public WFSLayerCollector(LayerTransformer transformer) {
        super(transformer);
    }
        
    public WFSPointDataSet getDataSet() {
        return wpds;
    }

    /**
     * Sets the times
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
    
    protected void setDataSet(L layer) {
        // set general settings
        synchronized (layer) {
            // set coverage if not set
            setCoverage(layer);
            wpds.setCoverage(getCoverage());
            wpds.setDataSetName(layer.getName());
            wpds.setProviderName(getConfiguration().getProvider().getName());
            wpds.setCollectionName(getConfiguration().getProvider()
                    .getServiceType().name());
            // set parameters if not already
            setParameters(layer);
            wpds.setParameters(getParameters());
            // set time if not already
            setTime(layer);
            wpds.setTime(getTime());
        }
    }

    public PointDataSetMetaData getDataSetMetaData() {
        return pdsmd;
    }

    protected void setDataSetMetaData(L layer) {
        // set the dataset first
        synchronized(layer) {
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
            sb2.append(getConfiguration().getProvider().getConnection().getUrl());
            sb2.append("/");
            sb2.append(getAgent().getWcs());
            pdsmd.setUrl(sb2.toString());
        }
    }

    /**
     * Sets the coverage
     */
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
     * @param layer
     * @return
     */
    protected Coverage getCoverage() {
        return coverage;
    }
     
    /**
     * WFS uses Point Data type
     * @return
     */
    public DataType getDataType() {
        return DataType.POINT;
    }
        
    /**
     * Send the metaData to the registry
     */
    public void sendMetaData(L layer) {
        
        //TODO:  A lot of this is preliminary.
        // As we start doing retrievals (Redmine #752)
        // we will better figure out exactly what we need to
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
        statusHandler.info("*************** Sending MetaData for "+layer.getName()+" to Registry ***************");
    }
    
    public Levels getLevels(DataLevelType type, String collectionName) {
        // not implement in point data yet
        return null;
    }

}
