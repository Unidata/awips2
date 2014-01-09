/**
 * 
 */
package com.raytheon.uf.edex.datadelivery.bandwidth.util;

import java.util.Calendar;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.IDataSetMetaDataHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.util.CollectionUtil;

/**
 * 
 * Implementation of {@link IDataSetAvailablityCalculator}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 9, 2012  1286       djohnson     Add SW history.
 * Jan 06, 2014 2636       mpduff       Changed how offset is determined.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class AveragingAvailablityCalculator {
    LoadingCache<NameProviderKey, List<DataSetMetaData>> cache = CacheBuilder
            .newBuilder().maximumSize(1000)
            .expireAfterWrite(30, TimeUnit.MINUTES)
            .build(new CacheLoader<NameProviderKey, List<DataSetMetaData>>() {
                @Override
                public List<DataSetMetaData> load(NameProviderKey key)
                        throws RegistryHandlerException {
                    return handler.getByDataSet(key.getName(),
                            key.getProvider());
                }
            });

    private final IDataSetMetaDataHandler handler;

    /**
     * Constructor.
     * 
     * @param handler
     *            The DataSetMetaDataHandler
     */
    public AveragingAvailablityCalculator(IDataSetMetaDataHandler handler) {
        this.handler = handler;
    }

    /**
     * Get the average dataset offset for the provided subscription
     * 
     * @param subscription
     *            The subscription
     * 
     * @param referenceTime
     *            The base reference time
     * 
     * @return The number of minutes of latency to expect.
     * @throws RegistryHandlerException
     */
    public int getDataSetAvailablityOffset(Subscription subscription,
            Calendar referenceTime) throws RegistryHandlerException {
        int offset = 0;
        NameProviderKey key = new NameProviderKey(
                subscription.getDataSetName(), subscription.getProvider());
        List<DataSetMetaData> records = null;

        try {
            records = cache.get(key);
            if (!CollectionUtil.isNullOrEmpty(records)) {
                DataSetMetaData md = records.get(0);
                if (md instanceof GriddedDataSetMetaData) {
                    offset = getOffsetForGrid(records, referenceTime);
                }
                // No availability delay for point data.
            }
        } catch (ExecutionException e) {
            throw new RegistryHandlerException(e);
        }

        return offset;
    }

    /**
     * Get the availability offset for gridded data.
     * 
     * @param records
     *            List of DataSetMetaData records
     * @param referenceTime
     *            The data's base reference time
     * @return The offset in minutes
     */
    private int getOffsetForGrid(List<DataSetMetaData> records,
            Calendar referenceTime) {
        int cycle = referenceTime.get(Calendar.HOUR_OF_DAY);
        int count = 0;
        int total = 0;
        for (DataSetMetaData md : records) {
            GriddedDataSetMetaData gmd = (GriddedDataSetMetaData) md;
            if (gmd.getCycle() == cycle) {
                total += gmd.getAvailabilityOffset();
                count++;
                if (count == 10) {
                    break;
                }
            }
        }

        if (count > 0) {
            return total / count;
        }

        return 0;
    }
}
