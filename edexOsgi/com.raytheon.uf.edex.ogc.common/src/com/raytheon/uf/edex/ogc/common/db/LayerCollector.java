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
package com.raytheon.uf.edex.ogc.common.db;

import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang.time.DateUtils;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.criterion.Restrictions;

import com.raytheon.uf.common.datadelivery.harvester.HarvesterConfig;
import com.raytheon.uf.common.datadelivery.harvester.HarvesterConfigurationManager;
import com.raytheon.uf.common.datadelivery.harvester.OGCAgent;
import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.DataSetName;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.Levels;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.IDataSetHandler;
import com.raytheon.uf.common.datadelivery.registry.handlers.IDataSetMetaDataHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * 
 * Layer Collector
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/09/2012   754       dhladky      initial creation, based on B Clements original
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public abstract class LayerCollector<L extends SimpleLayer> {

    protected LayerTransformer transformer;
    
    protected L layer;
    
    protected Coverage coverage;
    
    protected HashMap<String,Parameter> parameters = null;

    protected HarvesterConfig config = null;
    
    protected OGCAgent agent = null;
    
    private static final transient IUFStatusHandler statusHandler = UFStatus
    .getHandler(LayerCollector.class);

    public LayerCollector(LayerTransformer transformer) {
        setTransformer(transformer);
        this.config = (HarvesterConfig) HarvesterConfigurationManager.getOGCConfiguration();
        setAgent((OGCAgent)config.getAgent());
        storeProvider(config.getProvider());
    }

    public HarvesterConfig getConfig() {
        return config;
    }

    public void setConfig(HarvesterConfig config) {
        this.config = config;
    }

    public <T extends SimpleLayer> void clearLayers(Class<T> c)
            throws DataAccessLayerException {
        DaoConfig conf = DaoConfig.forClass(c);
        CoreDao dao = new CoreDao(conf);
        List<?> res = dao.queryByCriteria(new DatabaseQuery(c));
        dao.deleteAll(res);
    }

    public <T extends SimpleLayer> void replaceTimes(T layer)
            throws DataAccessLayerException {
        DaoConfig conf = DaoConfig.forClass(layer.getClass());
        CoreDao dao = new CoreDao(conf);
        Session sess = dao.getSessionFactory().openSession();
        Transaction tx = null;
        try {
            tx = sess.beginTransaction();
            List<T> list = query(sess, layer);
            if (list.isEmpty()) {
                sess.save(layer);
            } else {
                if (list.size() > 1) {
                    statusHandler.warn("Multiple layers found with same name, using first");
                }
                T old = list.get(0);
                Set<Date> times = old.getTimes();
                times.clear();
                Set<Date> newTimes = layer.getTimes();
                if (newTimes != null) {
                    times.addAll(newTimes);
                }
            }
            tx.commit();
        } catch (Exception e) {
            tx.rollback();
            throw new DataAccessLayerException("Problem replacing layer times",
                    e);
        } finally {
            sess.close();
        }
    }

    public <T extends SimpleLayer> void updateLayer(T layer)
            throws DataAccessLayerException {
        DaoConfig conf = DaoConfig.forClass(layer.getClass());
        CoreDao dao = new CoreDao(conf);
        Session sess = dao.getSessionFactory().openSession();
        Transaction tx = null;
        try {
            tx = sess.beginTransaction();
            List<T> list = query(sess, layer);
            if (list.isEmpty()) {
                Set<? extends SimpleDimension> dims = layer.getDimensions();
                if (dims != null) {
                    for (SimpleDimension d : dims) {
                        sess.save(d);
                    }
                }
                sess.save(layer);
            } else {
                if (!list.isEmpty()) {
                    statusHandler.warn("Multiple layers found with same name, using first");
                }
                T old = list.get(0);
                update(old, layer);
            }
            tx.commit();
        } catch (Exception e) {
            tx.rollback();
            throw new DataAccessLayerException("Problem updating layer", e);
        } finally {
            sess.close();
        }
    }

    public <T extends SimpleLayer> void update(T old, T shiny) {
        updateDates(old, shiny);
        updateDims(old, shiny);
    }

    public <T extends SimpleLayer> void updateDates(T old, T shiny) {
        Set<Date> shinyTimes = shiny.getTimes();
        Set<Date> oldTimes = old.getTimes();
        if (shinyTimes != null && shinyTimes != null) {
            for (Date time : shinyTimes) {
                oldTimes.add(time);
            }
        }
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    public <T extends SimpleLayer> void updateDims(T old, T shiny) {
        Set oldDims = old.getDimensions();
        Set shinyDims = shiny.getDimensions();
        if (oldDims != null) {
            if (shinyDims != null) {
                updateDimLists(oldDims, shinyDims);
            }
        }
    }

    public <T extends SimpleDimension> HashMap<String, T> getDimMap(Set<T> dims) {
        HashMap<String, T> rval = new HashMap<String, T>(dims.size());
        for (T sd : dims) {
            rval.put(sd.getName(), sd);
        }
        return rval;
    }

    public <T extends SimpleDimension> void updateDimLists(Set<T> oldDims,
            Set<T> shinyDims) {
        HashMap<String, T> oldMap = getDimMap(oldDims);
        HashMap<String, T> shinyMap = getDimMap(shinyDims);
        for (String name : shinyMap.keySet()) {
            T shinyDim = shinyMap.get(name);
            T oldDim = oldMap.get(name);
            if (oldDim == null) {
                oldDims.add(shinyDim);
            } else {
                updateDimValues(oldDim, shinyDim);
            }
        }
    }

    public void updateDimValues(SimpleDimension oldDim, SimpleDimension shinyDim) {
        Set<String> oldValues = oldDim.getValues();
        Set<String> shinyValues = shinyDim.getValues();
        if (oldValues != null && shinyValues != null) {
            for (String val : shinyValues) {
                oldValues.add(val);
            }
        }
    }

    @SuppressWarnings("unchecked")
    public <T extends SimpleLayer> List<T> query(Session sess, T layer)
            throws DataAccessLayerException {
        Class<? extends SimpleLayer> layerClass = transformer.getLayerClass();
        Criteria criteria = sess.createCriteria(layerClass);
        criteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
        criteria.add(Restrictions.eq("name", layer.getName()));
        return criteria.list();
    }

    @SuppressWarnings("unchecked")
    public <T extends SimpleLayer> List<T> query(Session sess, String layerName)
            throws DataAccessLayerException {
        Class<? extends SimpleLayer> layerClass = transformer.getLayerClass();
        Criteria criteria = sess.createCriteria(layerClass);
        criteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
        criteria.add(Restrictions.eq("name", layerName));
        return criteria.list();
    }

    /**
     * Take the Calendar back to the first instant of the current hour. This is
     * equivalent to calling roundToHour with cutoff of 59
     * 
     * @param cal
     * @return
     */
    public static Calendar truncateToHour(Calendar cal) {
        return DateUtils.truncate(cal, Calendar.HOUR);
    }

    /**
     * Take the Date back to the first instant of the current hour. This is
     * equivalent to calling roundToHour with cutoff of 59
     * 
     * @param d
     * @return
     */
    public static Date truncateToHour(Date d) {
        return DateUtils.truncate(d, Calendar.HOUR);
    }

    /**
     * Round the Calendar to the nearest hour determined by cutoff
     * 
     * @param cal
     * @param cutoff
     *            if cal's minute value is greater than cutoff, the return will
     *            be rounded up to the next hour, else rounded down
     * @return
     */
    public static Calendar roundToHour(Calendar cal, int cutoff) {
        if (cutoff < 0 || cutoff > 59) {
            cutoff %= 60;
        }
        if (cal.get(Calendar.MINUTE) > cutoff) {
            cal = (Calendar) cal.clone();
            cal.add(Calendar.HOUR, 1);
        }
        return truncateToHour(cal);
    }

    /**
     * Round the Date to the nearest hour determined by cutoff
     * 
     * @param d
     * @param cutoff
     *            if d's minute value is greater than cutoff, the return will be
     *            rounded up to the next hour, else rounded down
     * @return
     */
    public static Date roundToHour(Date d, int cutoff) {
        Calendar tmp = Calendar.getInstance();
        tmp.setTime(d);
        tmp = roundToHour(tmp, cutoff);
        return tmp.getTime();
    }
    
    /**
     * Find the DPA config
     * @return
     */
    public HarvesterConfig getConfiguration() {
        return config;
    }
        
    protected Coverage getCoverage() {
        return coverage;
    }
   
    protected abstract L newLayer();
    
    protected abstract void setCoverage(String name);

    /**
     * Store the DataSetMetaData Object to the registry.
     * 
     * @param metaDatas
     *            The DataSetMetaData Object to store.
     */
    public void storeMetaData(final List<DataSetMetaData> metaDatas,
            final DataSet dataSet) {

        IDataSetMetaDataHandler handler = DataDeliveryHandlers
                .getDataSetMetaDataHandler();
        Iterator<DataSetMetaData> iter = metaDatas.iterator();
        int size = metaDatas.size();
        for (int i = 1; i <= size; i++) {
            statusHandler.info(String.format(
                    "Attempting store of DataSetMetaData[%s/%s]", i, size));
            final DataSetMetaData dsmd = iter.next();
            final String url = dsmd.getUrl();

            try {
                handler.update(dsmd);
                statusHandler.info("DataSetMetaData [" + url
                        + "] successfully stored in Registry");
            } catch (RegistryHandlerException e) {
                statusHandler.info("DataSetMetaData [" + url
                        + "] failed to store in Registry");

            }
        }
    }

    /**
     * 
     * @param dataSetToStore
     */
    protected void storeDataSetName(DataSet dataSetToStore) {

        DataSetName dsn = new DataSetName();
        // Set the RegistryObject Id keys for this Object
        // using the values from the DataSetMetaData Object.
        dsn.setProviderName(dataSetToStore.getProviderName());
        dsn.setDataSetType(dataSetToStore.getDataSetType());
        dsn.setDataSetName(dataSetToStore.getDataSetName());

        // Now add the parameter Objects so we can associate
        // the DataSetName with parameters..
        dsn.setParameters(dataSetToStore.getParameters());

        try {
            DataDeliveryHandlers.getDataSetNameHandler().update(dsn);
            statusHandler.info("DataSetName object store complete, dataset ["
                    + dsn.getDataSetName() + "]");
        } catch (RegistryHandlerException e) {
            statusHandler.error("DataSetName object store failed:", e);
        }
    }

    /**
     * @param dataSet
     */
    protected void storeDataSet(final DataSet dataSet) {

        DataSet dataSetToStore = getDataSetToStore(dataSet);
        final String dataSetName = dataSetToStore.getDataSetName();
        IDataSetHandler handler = DataDeliveryHandlers.getDataSetHandler();
        
        try {
            handler.update(dataSetToStore);
            statusHandler.info("Dataset [" + dataSetName
                    + "] successfully stored in Registry");
            storeDataSetName(dataSet);

        } catch (RegistryHandlerException e) {
            statusHandler.info("Dataset [" + dataSetName
                    + "] failed to store in Registry");
        }
    }
    
    /**
     * Make sure our provider is contained in the Registry
     * @param provider
     */
    protected void storeProvider(final Provider provider) {
        
        try {
            DataDeliveryHandlers.getProviderHandler().update(provider);
        } catch (RegistryHandlerException e) {
            statusHandler.info("Provider [" + provider.getName()
                    + "] failed to store in Registry");
        }
    }

    /**
     * Checks for a {@link DataSet} already existing with the same name in the
     * Registry. If so, then combine the objects.
     * 
     * @param dataSet
     *            the dataSet
     * @return the dataSet instance that should be stored to the registry
     */
    protected DataSet getDataSetToStore(DataSet dataSet) {
        try {
            DataSet result = DataDeliveryHandlers.getDataSetHandler()
                    .getByNameAndProvider(dataSet.getDataSetName(),
                            dataSet.getProviderName());
            if (result != null) {
                dataSet.combine(result);
            }
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve dataset.", e);
        }
        return dataSet;
    }

    /**
     * Store a parameter object to the registry. If necessary, also store the
     * ParameterLevel Objects needed to successfully store the Parameter Object.
     * 
     * @param parameter
     *            The Parameter Object to store.
     */
    protected void storeParameter(Parameter parameter) {

        try {
            DataDeliveryHandlers.getParameterHandler().update(parameter);
        } catch (RegistryHandlerException e) {
            statusHandler.info("Failed to store parameter ["
                    + parameter.getName() + "]");
        }
    }

    /**
     * Get me my level types for this param
     * 
     * @param cp
     * @return
     */
    public List<DataLevelType> getDataLevelTypes(Parameter cp) {
        return cp.getLevelType();
    }
    
    public abstract void setParameters(L layer);
    
    public abstract DataSet getDataSet();
    
    public abstract DataSetMetaData getDataSetMetaData();
    
    public abstract DataType getDataType();
    
    public abstract Levels getLevels(DataLevelType type, String collectionName);

    public LayerTransformer getTransformer() {
        return transformer;
    }

    public void setTransformer(LayerTransformer transformer) {
        this.transformer = transformer;
    }

    public OGCAgent getAgent() {
        return agent;
    }

    public void setAgent(OGCAgent agent) {
        this.agent = agent;
    }

    public void setCoverage(Coverage coverage) {
        this.coverage = coverage;
    }

    public HashMap<String, Parameter> getParameters() {
        return parameters;
    }

}
