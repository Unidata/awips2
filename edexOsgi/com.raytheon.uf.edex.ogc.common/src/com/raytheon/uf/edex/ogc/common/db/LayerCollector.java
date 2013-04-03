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
import java.util.List;
import java.util.Set;

import org.apache.commons.lang.time.DateUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.criterion.Restrictions;

import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

public class LayerCollector {

    protected LayerTransformer transformer;

    protected Log log = LogFactory.getLog(this.getClass());

    public LayerCollector(LayerTransformer transformer) {
        this.transformer = transformer;
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
                    log.warn("Multiple layers found with same name, using first");
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
                if (list.size() > 1) {
                    log.warn("Multiple layers found with same name, using first");
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

}
