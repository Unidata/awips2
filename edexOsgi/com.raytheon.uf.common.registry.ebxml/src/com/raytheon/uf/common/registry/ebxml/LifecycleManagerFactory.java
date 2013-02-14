package com.raytheon.uf.common.registry.ebxml;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;

/**
 * 
 * Factory interface used by RegistryManager to obtain a particular client 
 * implementation for the registry's life cycle management services.
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
 */
public interface LifecycleManagerFactory {

    /**
     * Get a client implementation of the registry's LifeCycleManager services.
     *  
     * @return An implementation of LifeCycleManager.
     */
    LifecycleManager getLifeCycleManager();

}
