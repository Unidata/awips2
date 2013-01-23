/**
 * 
 */
package com.raytheon.uf.edex.datadelivery.bandwidth.util;

import com.raytheon.uf.common.datadelivery.registry.Subscription;

/**
 * Interface for a pluggable implementation for calculating a data sets
 * availability.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 9, 2012  1286      djohnson     Added SW histor, rename to comply with AWIPS standards.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
public interface IDataSetAvailablityCalculator {

    /**
     * Calculate the number of minutes of delay between a Subscriptions base
     * reference time and the time the data should be available.
     * 
     * @param subscription
     *            The Subscription Object to obtain the availability for.
     * 
     * @return The delay in minutes.
     */
    int getDataSetAvailablityDelay(Subscription subscription);
}
