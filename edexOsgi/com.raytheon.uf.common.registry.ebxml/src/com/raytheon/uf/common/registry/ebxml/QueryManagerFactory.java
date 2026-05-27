package com.raytheon.uf.common.registry.ebxml;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;

/**
 * 
 * Factory interface used by RegistryManager to obtain a particular client 
 * implementation for the registry's query manager services.
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
public interface QueryManagerFactory {

    /**
     * Get a client implementation of the registry's QueryManager service.
     *  
     * @return An implementation of QueryManager.
     * 
     */
    QueryManager getQueryManager();

}
