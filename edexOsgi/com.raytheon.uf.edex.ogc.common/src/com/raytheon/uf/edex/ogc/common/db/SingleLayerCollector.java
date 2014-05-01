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
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 11, 2012            bclement     Initial creation
 * Aug 18, 2013  #2097     dhladky      Updates for interfaces etc.
 *
 */
package com.raytheon.uf.edex.ogc.common.db;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.geotools.geometry.jts.JTS;
import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.vividsolutions.jts.geom.Envelope;


/**
 * Collects layer metadata from data records. Designed for use with plugins that
 * have a single layer which all records contribute to (like pointdata)
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
public abstract class SingleLayerCollector<D extends SimpleDimension, L extends SimpleLayer<D>, R extends PluginDataObject>
        extends LayerCollector<D, L, R> {

    private L _layer;

    protected ReadWriteLock lock = new ReentrantReadWriteLock();

    protected String layerName;

    /**
     * @param transformer
     */
    public SingleLayerCollector(Class<L> layerClass, Class<R> recordClass,
            String layerName, ILayerStore store) {
        super(layerClass, recordClass, store);
        this.layerName = layerName;
    }

    @SuppressWarnings("unchecked")
    public void add(PluginDataObject... pdos) {
        Lock write = lock.writeLock();
        write.lock();
        
        try {
            ICollectorAddon<D, L, R> addon = getAddon();
            L layer = getLayer();
            for (PluginDataObject pdo : pdos) {
                if (recordClass.equals(pdo.getClass())) {
                    R record = (R) pdo;
                    Calendar time = getTime(record);
                    replaceRange(layer, time);
                    addon.onCollect(layer, (R) pdo);
                }
            }
            addon.onFinish();
            
        } finally {
            write.unlock();
        }
    }

    private void replaceRange(L layer, Calendar time) {
        Date floor = truncateToMinute(time.getTime());
        Date ceil = roundUpToMinute(time.getTime());
        SortedSet<Date> times = layer.getTimes();
        Date earliest;
        Date latest;
        if (times.isEmpty()) {
            earliest = floor;
            latest = ceil;
        } else {
            Date lfirst = times.first();
            Date llast = times.last();
            earliest = lfirst.before(floor) ? lfirst : floor;
            latest = llast.after(ceil) ? llast : ceil;
        }
        times.clear();
        times.add(earliest);
        times.add(latest);
    }

    private L getLayer() {
        if (_layer == null) {
            try {
                List<L> res = store.getAll(layerClass);
                if (!res.isEmpty()) {
                    _layer = res.get(0);
                }
            } catch (OgcException e) {
                log.error("Problem reading layer from database", e);
            }
            if (_layer == null) {
                _layer = newLayer();
            }
        }
        return _layer;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.ogc.common.db.LayerCollector#newLayer()
     */
    @Override
    protected L newLayer() {
        L rval = super.newLayer();
        initLayer(rval);
        return rval;
    }

    protected void initLayer(L layer) {
        layer.setName(layerName);
        ReferencedEnvelope env = new ReferencedEnvelope(-180, 180, -90, 90,
                MapUtil.LATLON_PROJECTION);
        layer.setCrs84Bounds(JTS.toGeometry((Envelope) env));
        layer.setTargetCrsCode("CRS:84");
        layer.setTargetMaxx(env.getMaxX());
        layer.setTargetMaxy(env.getMaxY());
        layer.setTargetMinx(env.getMinX());
        layer.setTargetMiny(env.getMinY());
    }

    public void updateDB() {
        Lock read = lock.readLock();
        read.lock();
        try {
            L layer = getLayer();
            if (!layer.getTimes().isEmpty()) {
                updateLayer(layer);
            }
        } catch (OgcException e) {
            log.error("problem updating layer", e);
        }
        read.unlock();
    }

    protected abstract Calendar getTime(R record);

    public void purgeExpired(Set<Date> timesToKeep) {
        Lock write = lock.writeLock();
        write.lock();
        try {
            L layer = getLayer();
            SortedSet<Date> ltimes = layer.getTimes();
            ltimes.clear();
            if (!timesToKeep.isEmpty()) {
                SortedSet<Date> incoming = new TreeSet<Date>(timesToKeep);
                Date earliest = truncateToMinute(incoming.first());
                Date latest = roundUpToMinute(incoming.last());
                ltimes.add(earliest);
                ltimes.add(latest);
            }
            try {
                replaceTimes(layer);
            } catch (OgcException e) {
                log.error("problem purging expired layer times", e);
            }
            getAddon().onPurgeExpired(timesToKeep);
        } finally {
            write.unlock();
        }
    }

    public void purgeAll() {
        Lock write = lock.writeLock();
        write.lock();
        try {
            L layer = getLayer();
            layer.getTimes().clear();
            try {
                clearLayers(layerClass);
            } catch (Exception e) {
                log.error("problem purging layers", e);
            }
        } finally {
            write.unlock();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.ogc.common.db.LayerCache#getLayers()
     */
    @Override
    public List<L> getLayers() {
        ArrayList<L> rval = new ArrayList<L>(1);
        L copy = getCopy();
        if (copy != null) {
            rval.add(copy);
        }
        return rval;
    }

    private L getCopy() {
        L rval = null;
        Lock read = lock.readLock();
        read.lock();
        try {
            L layer = getLayer();
            if (!layer.getTimes().isEmpty()) {
                rval = copy(layer);
            }
        } finally {
            read.unlock();
        }
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.db.LayerCollector#copy(com.raytheon.uf
     * .edex.ogc.common.db.SimpleLayer)
     */
    @Override
    protected L copy(L orig) {
        L rval = newLayer();
        // assume that only times are different
        rval.getTimes().addAll(orig.getTimes());
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.db.LayerCache#getLayer(java.lang.String)
     */
    @Override
    public L getLayer(String name) {
        if (layerName.equals(name)) {
            return getCopy();
        } else {
            return null;
        }
    }

    @Override
    public String getLayerName(R rec) {
        return this.layerName;
    }

}
