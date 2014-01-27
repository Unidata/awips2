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

import java.lang.reflect.Constructor;
import java.util.Calendar;
import java.util.Date;
import java.util.Set;

import org.apache.commons.lang.time.DateUtils;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcException.Code;

/**
 * Abstract base for layer collectors. Provides common utility methods for child
 * classes.
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
public abstract class LayerCollector<D extends SimpleDimension, L extends SimpleLayer<D>, R extends PluginDataObject>
        implements ILayerCache<D, L> {

    protected Class<R> recordClass;

    protected Class<L> layerClass;

    protected IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    protected final ILayerStore store;

    protected CollectorAddonFactory<D, L, R> addonFactory = new CollectorAddonFactory<D, L, R>() {
        @Override
        public ICollectorAddon<D, L, R> create() {
            return new ICollectorAddon<D, L, R>() {
                @Override
                public void onCollect(L layer, R record) {
                }

                @Override
                public void onFinish() {
                }

                @Override
                public void onPurgeAll() {
                }

                @Override
                public void onPurgeExpired(Set<Date> timesToKeep) {
                }

            };
        }
    };

    public LayerCollector(Class<L> layerClass, Class<R> recordClass,
            ILayerStore store) {
        this.recordClass = recordClass;
        this.layerClass = layerClass;
        this.store = store;
    }

    /**
     * Returns object for additional tasks at layer collection
     * 
     * @return
     */
    public ICollectorAddon<D, L, R> getAddon() {
        return addonFactory.create();
    }

    protected L copy(final L orig) throws OgcException {
        L rval;
        try {
            Constructor<L> constructor = layerClass.getConstructor(layerClass);
            rval = constructor.newInstance(orig);
        } catch (Exception e) {
            log.error("Unable to copy layer: " + layerClass, e);
            throw new OgcException(Code.InternalServerError);
        }
        return rval;
    }

    protected L newLayer() {
        try {
            return layerClass.newInstance();
        } catch (Exception e) {
            log.error("Unable to instantiate class: " + layerClass, e);
            throw new RuntimeException(e);
        }
    }

    public void clearLayers(Class<L> c) throws OgcException {
        store.deleteAll(c);
    }

    public void replaceTimes(L layer) throws OgcException {
        L old = store.get(layer.getIdentifier(), layerClass);
        if (old == null) {
            store.createOrUpdate(layer);
        } else {
            Set<Date> times = old.getTimes();
            times.clear();
            Set<Date> newTimes = layer.getTimes();
            if (newTimes != null) {
                times.addAll(newTimes);
            }
            store.createOrUpdate(layer);
        }
    }

    public void updateLayer(L layer) throws OgcException {
        L old = store.get(layer.getIdentifier(), layerClass);
        if (old == null) {
            store.createOrUpdate(layer);
        } else {
            old.update(layer);
            store.createOrUpdate(layer);
        }
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

    public static Date truncateToMinute(Date d) {
        return DateUtils.truncate(d, Calendar.MINUTE);
    }

    public static Date roundUpToMinute(Date d) {
        Date rval = DateUtils.add(d, Calendar.MINUTE, 1);
        return truncateToMinute(rval);
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
     * From a record build and return the layer name. Used to make sure a proper
     * layer name can always be retrieved for any record object used by a layer
     * collector.
     * 
     * @param rec
     * @return
     */
    public abstract String getLayerName(R rec);

    /**
     * @param addonFactory
     *            the addonFactory to set
     */
    public void setAddonFactory(CollectorAddonFactory<D, L, R> addonFactory) {
        this.addonFactory = addonFactory;
    }

}
