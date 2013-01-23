/**
 * 
 */
package com.raytheon.uf.edex.datadelivery.bandwidth.util;

import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.DataSetMetaDataDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;

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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class AveragingAvailablityCalculator implements
        IDataSetAvailablityCalculator {

    private static final int DATASET_AVAILABLE_DEFAULT = 60;

    private final IBandwidthDao bandwidthDao;

    /**
     * Constructor.
     * 
     * @param dao
     *            dao
     */
    public AveragingAvailablityCalculator(IBandwidthDao dao) {
        this.bandwidthDao = dao;
    }

    /**
     * Retrieve the DataSet availability latency for a particular dataset. This
     * time encompasses the number of minutes after the dataset base time that
     * data is typically available.
     * 
     * @param providerName
     *            The provider name for the dataset.
     * 
     * @param dataSetName
     *            The dataset name.
     * 
     * @return The number of minutes of latency to expect.
     */
    @Override
    public int getDataSetAvailablityDelay(Subscription subscription) {
        // Use a default of 60 minutes..
        int delay = DATASET_AVAILABLE_DEFAULT;
        List<DataSetMetaDataDao> recs = bandwidthDao
                .getDataSetMetaDataDao(subscription.getProvider(),
                        subscription.getDataSetName());

        long totalLatency = 0;
        int recordcount = 0;
        // Average out a maximum of twenty records
        for (DataSetMetaDataDao rec : recs) {
            long diff = (rec.getUpdateTime().getTimeInMillis() - rec
                    .getDataSetBaseTime().getTimeInMillis()) / 60000;
            // Make sure some funky dates don't mess with the average..
            totalLatency += Math.max(0, diff);
            if (recordcount++ > 20) {
                break;
            }
        }

        // Make sure we did some kind of calculation.
        if (totalLatency > 0) {
            delay = (int) (totalLatency / recordcount);
        }
        return delay;
    }
}
