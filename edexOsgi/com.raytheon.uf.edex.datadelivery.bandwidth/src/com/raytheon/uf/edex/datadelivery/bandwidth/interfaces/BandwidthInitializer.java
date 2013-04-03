package com.raytheon.uf.edex.datadelivery.bandwidth.interfaces;

import com.raytheon.uf.edex.datadelivery.bandwidth.IBandwidthManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDbInit;

/**
 * * An interface for initialization of the BandwidthManager instance. The
 * implementation of this interface will be passed a reference to the instance
 * of the BandwidthManager.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2012 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public interface BandwidthInitializer {

    /**
     * Initialize the instance of BandwidthManager.
     * 
     * @param instance
     *            A reference to the instance of the BandwidthManager to
     *            initialize.
     * @param dbInit
     *            a reference to the {@link IBandwidthDbInit} instance
     * 
     * @return Whether or not the initialization completed successfully.
     */
    boolean init(IBandwidthManager instance, IBandwidthDbInit dbInit);
}
