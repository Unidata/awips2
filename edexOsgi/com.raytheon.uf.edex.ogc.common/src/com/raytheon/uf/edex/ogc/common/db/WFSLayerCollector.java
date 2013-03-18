package com.raytheon.uf.edex.ogc.common.db;
/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2010 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 6825 Pine Street, Suite 340
 * Mail Stop B8
 * Omaha, NE 68106
 * 402.291.0100
 *
 */

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
import com.raytheon.uf.common.datadelivery.registry.DataLevelType.LevelType;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.EnvelopeUtils;
import com.raytheon.uf.common.datadelivery.registry.Levels;
import com.raytheon.uf.common.datadelivery.registry.PointDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.PointTime;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.registry.WFSPointDataSet;
import com.raytheon.uf.common.datadelivery.retrieval.util.LookupManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
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

    protected WFSPointDataSet wpds = null;
    
    protected PointDataSetMetaData pdsmd = null;
    
    protected PointTime time = null;
    
    private static final transient IUFStatusHandler statusHandler = UFStatus
    .getHandler(WFSLayerCollector.class);


    public WFSLayerCollector(LayerTransformer transformer) {
        super(transformer);
    }
        
    public WFSPointDataSet getDataSet() {
        return wpds;
    }

    protected void setPointTime(L layer) {
        // Get the times first
        time = new PointTime();
        List<Date> times = new ArrayList<Date>();
        for (Date time : layer.getTimes()) {
            times.add(time);
        }
        time.setTimes(times);
        time.setNumTimes(times.size());
        time.setFormat(getConfiguration().getAgent().getDateFormat());
        SimpleDateFormat dateFormat = new SimpleDateFormat(getConfiguration().getAgent().getDateFormat());
        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        time.setStart(dateFormat.format(times.get(0)));
        time.setEnd(dateFormat.format(times.size()-1));
    }
    
    protected Time getPointTime() {
        return time;
    }
    
    protected void setPointDataSet(L layer) {
        // set general settings
        wpds = new WFSPointDataSet();
        wpds.setCoverage(getCoverage());
        wpds.setDataSetName(layer.getName());
        wpds.setProviderName(getConfiguration().getProvider().getName());
        wpds.setCollectionName(getConfiguration().getProvider().getServiceType().name());
        // set parameters
        setParameters(layer);
        wpds.setParameters(getParameters());
        // place holder time object
        PointTime time = new PointTime();
        ArrayList<Date> dates = new ArrayList<Date>();
        dates.add(new Date(System.currentTimeMillis()));
        // TODO set times and format
        time.setTimes(dates);
        time.setFormat(getConfiguration().getAgent().getDateFormat());
        SimpleDateFormat dateFormat = new SimpleDateFormat(getConfiguration().getAgent().getDateFormat());
        time.setStart(dateFormat.format(dates.get(0)));
        time.setEnd(dateFormat.format(dates.size()-1));
        wpds.setTime(time);
    }
    
    public PointDataSetMetaData getDataSetMetaData() {
        return pdsmd;
    }

    protected void setPointDataSetMetaData(L layer) {
        // set the dataset first
        setPointDataSet(layer);
        pdsmd = new PointDataSetMetaData();
        StringBuffer sb = new StringBuffer();
        sb.append(layer.getName()).append(" ");
        sb.append(layer.getTargetMaxx()).append(", ");
        sb.append(layer.getTargetMiny()).append(" : ");
        sb.append(layer.getTargetMinx()).append(", ");
        sb.append(layer.getTargetMaxy());
        pdsmd.setDataSetDescription(sb.toString());
        pdsmd.setDataSetName(layer.getName());
        pdsmd.setProviderName(getConfiguration().getProvider().getName());
        StringBuffer sb2 = new StringBuffer();
        sb2.append(getConfiguration().getProvider().getConnection().getUrl());
        sb2.append("/");
        sb2.append(getAgent().getWcs());
        pdsmd.setUrl(sb2.toString());
    }

    protected void setCoverage(String name) {
        coverage = new Coverage();
        ConfigLayer configLayer = getAgent().getLayer(name);
        Coordinate lowerRight = new Coordinate(configLayer.getMaxx(),configLayer.getMiny());
        Coordinate upperLeft = new Coordinate(configLayer.getMinx(),configLayer.getMaxy());
        ReferencedEnvelope re = EnvelopeUtils.createLatLonEnvelope(lowerRight, upperLeft);
        coverage.setEnvelope(re);
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
        
        // updates the pointTime object
        setPointTime(layer);
        //getDataSet().setTime(getPointTime());
        getDataSetMetaData().setTime(getPointTime());
        SimpleDateFormat dateFormat = new SimpleDateFormat(getPointTime().getFormat());
        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        ImmutableDate date = null;
        try {
            date = new ImmutableDate(dateFormat.parse(getPointTime().getEnd()));
        } catch (ParseException e) {
            e.printStackTrace();
        }
        getDataSetMetaData().setDate(date);
        
        List<DataSetMetaData> dataSetMetaDatas = new ArrayList<DataSetMetaData>();
        dataSetMetaDatas.add(getDataSetMetaData());

        //storeDataSet(getDataSet());
        storeMetaData(dataSetMetaDatas, getDataSet());
        statusHandler.info("*************** Sending MetaData to Registry ***************");
    }
    
    public Levels getLevels(DataLevelType type, String collectionName) {

        Levels levels = new Levels();

        try {
            //TODO: We'll better figure this out when we do retrievals as well
            double dz = 0.0;
            
            levels.setName(type.getType().getLevelType());
            levels.setLevelType(type.getId());

            if (type.getType().equals(LevelType.MB.getLevelType())) {
                List<Double> levelList = LookupManager.getInstance()
                        .getLevels(collectionName).getLevelXml();
                levels.setLevel(levelList);

            } else if (type.getType().equals(LevelType.SEAB.getLevelType())) {
                List<Double> levelList = LookupManager.getInstance()
                        .getLevels(collectionName).getLevelXml();
                levels.setLevel(levelList);
            } else {
                // default added when only one
                levels.addLevel(Double.NaN);
            }

            levels.setDz(dz);

        } catch (Exception e) {
            statusHandler.error("Level info" + collectionName + " url: "
                    + getDataSetMetaData().getUrl(), e);
        }

        return levels;
    }
     
    
}
