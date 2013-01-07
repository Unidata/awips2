package com.raytheon.uf.common.registry.ebxml;

import com.raytheon.uf.common.registry.RegistryManager;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;

/**
 * Interface for controlling the transaction management of the
 * RegistryManager Class.  RegistryManager will call methods on
 * this interface to control transactions with the registry services
 * provided by LifecycleManager and QueryManager clients.
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
 * @see LifecycleManager
 * @see QueryManager
 * @see RegistryManager
 * @see RegistryTxManager
 */
public interface TxManager {

    /**
     * Start a transaction with the registry.
     * 
     * @throws Exception
     */
    public void startTransaction() throws Exception;
    
    /**
     *  Close a transaction with the registry.
     */
    public void closeTransaction();
}
