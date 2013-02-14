package com.raytheon.uf.edex.datadelivery.bandwidth.notification;

/**
 * Configuration bean for the BandwidthEventBus.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 3, 2012  0726      jspinks     Initial creation
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
public class BandwidthEventBusConfig {

    // Set reasonable default values
    private int dataSetMetaDataPoolSize = 2;

    private int retrievalPoolSize = 3;

    private int subscriptionPoolSize = 2;

    /**
     * Get attribute dataSetMetaDataPoolSize.
     * 
     * @return The value of attribute dataSetMetaDataPoolSize.
     */
    public int getDataSetMetaDataPoolSize() {
        return dataSetMetaDataPoolSize;
    }

    /**
     * Get attribute retrievalPoolSize.
     * 
     * @return The value of attribute retrievalPoolSize.
     */
    public int getRetrievalPoolSize() {
        return retrievalPoolSize;
    }

    /**
     * Get attribute subscriptionPoolSize.
     * 
     * @return The value of attribute subscriptionPoolSize.
     */
    public int getSubscriptionPoolSize() {
        return subscriptionPoolSize;
    }

    /**
     * Set the dataSetMetaDataPoolSize.
     * 
     * @param dataSetMetaDataPoolSize
     *            The value to set attribute dataSetMetaDataPoolSize to.
     */
    public void setDataSetMetaDataPoolSize(int dataSetMetaDataPoolSize) {
        this.dataSetMetaDataPoolSize = dataSetMetaDataPoolSize;
    }

    /**
     * Set the retrievalPoolSize.
     * 
     * @param retrievalPoolSize
     *            The value to set attribute retrievalPoolSize to.
     */
    public void setRetrievalPoolSize(int retrievalPoolSize) {
        this.retrievalPoolSize = retrievalPoolSize;
    }

    /**
     * Set the subscriptionPoolSize.
     * 
     * @param subscriptionPoolSize
     *            The value to set attribute subscriptionPoolSize to.
     */
    public void setSubscriptionPoolSize(int subscriptionPoolSize) {
        this.subscriptionPoolSize = subscriptionPoolSize;
    }
}
