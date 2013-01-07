package com.raytheon.uf.common.registry.ebxml;

import com.raytheon.uf.common.registry.RegistryManager;


/**
 * 
 * Factory interface used by RegistryManager to obtains a particular client 
 * implementation for the registry's transaction management services.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012            jspinks     Initial creation
 *
 * </pre>
 *
 * @author jspinks
 * @version 1.0
 * 
 * @see RegistryManager
 */
public interface RegistryTxManager {

    /**
     * Get a client implementation of the registry's transaction services.
     *  
     * @return An implementation of TxManager.
     * 
     * @see TxManager
     */
    TxManager getTxManager();
}
