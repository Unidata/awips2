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
package com.raytheon.uf.edex.ogc.common.db;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.ogc.common.OgcException;

/**
 * Collects layer metadata from data records. Designed for use with records that
 * contain all data for a layer at a specific time and level.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 * @param <D>
 * @param <L>
 * @param <R>
 */
public abstract class DefaultLayerCollector<D extends SimpleDimension, L extends SimpleLayer<D>, R extends PluginDataObject>
        extends LayerCollector<D, L, R> {

    protected static final IUFStatusHandler log = UFStatus
            .getHandler(DefaultLayerCollector.class);

    protected Map<String, L> layerMap;

    protected ReadWriteLock lock = new ReentrantReadWriteLock();

    public DefaultLayerCollector(Class<L> layerClass, Class<R> recordClass,
            ILayerStore store) {
        super(layerClass, recordClass, store);
    }

    public void add(R... pdos) {
        if (pdos != null && pdos.length > 0) {
            addAll(Arrays.asList(pdos));
        }
    }

    @SuppressWarnings("unchecked")
    public void addAll(Collection<? extends PluginDataObject> coll) {
        if (layerMap == null) {
            initMap();
        }
        Lock write = lock.writeLock();
        write.lock();
        ICollectorAddon<D, L, R> addon = getAddon();
        for (PluginDataObject pdo : coll) {
            if (recordClass.equals(pdo.getClass())) {
                R rec = (R) pdo;
                String name = getLayerName(rec);
                L layer = newLayer();
                layer.setName(name);
                if (!initializeLayer(layer, rec)) {
                    continue;
                }
                addToTimes(layer, rec);
                addToDims(layer, rec);
                L oldLayer = layerMap.get(name);
                if (oldLayer == null) {
                    oldLayer = newLayer();
                    oldLayer.setName(name);
                    initializeLayer(oldLayer, rec);
                    layerMap.put(name, oldLayer);
                }
                oldLayer.update(layer);
                addon.onCollect(layer, (R) pdo);
            }
        }
        addon.onFinish();
        write.unlock();
    }

    protected void addToTimes(L layer, R rec) {
        Date refTime = rec.getDataTime().getRefTime();
        layer.getTimes().add(refTime);
    }

    protected void addToDims(L layer, R rec) {
        // default is to do nothing
    }

    protected abstract boolean initializeLayer(L layer, R rec);

    public void purgeExpired() {
        try {
            clearLayersInternal();
            addFromDb();
            getAddon().onPurgeExpired(new TreeSet<Date>());
        } catch (Exception e) {
            log.error("Problem purging layers", e);
        }
    }

    protected void clearLayersInternal() throws DataAccessLayerException {
        Lock write = lock.writeLock();
        write.lock();
        if (layerMap != null) {
            layerMap.clear();
        }
        write.unlock();
    }

    @SuppressWarnings("unchecked")
    protected void addFromDb() throws DataAccessLayerException {
        DaoConfig conf = DaoConfig.forClass(recordClass);
        CoreDao dao = new CoreDao(conf);
        DatabaseQuery q = new DatabaseQuery(recordClass);
        List<R> recs = (List<R>) dao.queryByCriteria(q);
        addAll(recs);
    }

    public void purgeAll() {
        try {
            clearLayersInternal();
            getAddon().onPurgeAll();
        } catch (Exception e) {
            log.error("problem purging layers", e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.ogc.common.db.LayerCache#getLayers()
     */
    @Override
    public List<L> getLayers() throws OgcException {
        List<L> rval;
        if (layerMap == null) {
            initMap();
        }
        Lock read = lock.readLock();
        read.lock();
        rval = new ArrayList<L>(layerMap.size());
        for (String key : layerMap.keySet()) {
            rval.add(copy(layerMap.get(key)));
        }
        read.unlock();
        return rval;
    }

    protected void initMap() {
        Lock write = lock.writeLock();
        write.lock();
        if (layerMap == null) {
            layerMap = new ConcurrentHashMap<String, L>();
            try {
                addFromDb();
            } catch (DataAccessLayerException e) {
                log.error("Problem loading layers from db", e);
                // if we throw an internal server exception here, it would kill
                // the ogc request, better to just not add those layers to
                // response
            }
        }
        write.unlock();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.db.LayerCache#getLayer(java.lang.String)
     */
    @Override
    public L getLayer(String name) throws OgcException {
        if (layerMap == null) {
            initMap();
        }
        L rval;
        Lock read = lock.readLock();
        read.lock();
        rval = layerMap.get(name);
        if (rval != null) {
            rval = copy(layerMap.get(name));
        }
        read.unlock();
        return rval;
    }

}
