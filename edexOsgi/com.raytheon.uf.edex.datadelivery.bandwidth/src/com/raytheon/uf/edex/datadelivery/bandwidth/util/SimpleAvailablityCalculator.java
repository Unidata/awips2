/**
 * 
 */
package com.raytheon.uf.edex.datadelivery.bandwidth.util;

import com.raytheon.uf.common.datadelivery.registry.Subscription;

/**
 * @author jspinks
 * 
 */
public class SimpleAvailablityCalculator implements
        IDataSetAvailablityCalculator {

    private int delay;

    public int getDelay() {
        return delay;
    }

    public void setDelay(int delay) {
        this.delay = delay;
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
        return delay;
    }
}
