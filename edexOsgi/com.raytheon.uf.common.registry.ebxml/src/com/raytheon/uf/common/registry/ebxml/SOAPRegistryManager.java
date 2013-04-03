package com.raytheon.uf.common.registry.ebxml;

import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.namespace.QName;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManagerSOAPService;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManagerSOAPService;


/**
 * 
 * A SOAP client implementation for use with the RegistryManager Class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 27, 2012 356        jspinks     Initial creation
 * Sep 12, 2012 1167       djohnson    Moved to common plugin so it is reusable outside of CAVE (e.g. dataprovideragent).
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
public abstract class SOAPRegistryManager implements LifecycleManagerFactory,
        QueryManagerFactory, RegistryTxManager {

    private static QName queryServiceName = new QName(
            "urn:oasis:names:tc:ebxml-regrep:wsdl:registry:interfaces:4.0",
            "QueryManagerService");
    
    private static QName lcmServiceName = new QName(
            "urn:oasis:names:tc:ebxml-regrep:wsdl:registry:interfaces:4.0",
            "LifecycleManagerService");

    /**
     * Constructor to conform to bean pattern.
     */
    public SOAPRegistryManager() {
    }
    
    /**
     * Get an implementation of LifeCycleManager that uses SOAP to submit 
     * requests to the registry.
     *  
     * @return A SOAP implementation of LifeCycleManager.
     * 
     * @see LifecycleManagerFactory
     */
    @Override
    public LifecycleManager getLifeCycleManager() {
        // Retrieve a WebSevice client for the LifecycleManager.
        LifecycleManagerSOAPService lcm;
        LifecycleManager a = null;
        try {
            lcm = new LifecycleManagerSOAPService(new URL(
                    getLifecycleManagerUrl()), lcmServiceName);
            a = lcm.getLifecycleManagerPort();
        } catch (MalformedURLException e) {

        }

        return a;

    }

    /**
     * Get an implementation of TxManager to manage transactions
     * with the registry.
     *  
     * @return A implementation of TxManager.
     * 
     * @see RegisryTxManager
     */
    @Override
    public TxManager getTxManager() {
        return this.new NoTxManager();
    }

    /**
     * Get an implementation of QueryManager that uses SOAP to submit 
     * requests to the registry.
     *  
     * @return A SOAP implementation of QueryManager.
     * 
     * @see QueryManagerFactory
     */
    @Override
    public QueryManager getQueryManager() {
        
        QueryManagerSOAPService qm;
        QueryManager a = null;
        try {
            qm = new QueryManagerSOAPService(new URL(getQueryManagerUrl()),
                    queryServiceName);
            a = qm.getQueryManagerPort();
        } catch (MalformedURLException e) {

        }
        
        return a;
    }
    
    /**
     * Inner class to implement the TxManager interface.  The transaction
     * management for the SOAP client is handled by the webservice itself,
     * so no additional transaction management is required.  As such, this 
     * NoTxManager implementation simply fulfills the RegistryManager 
     * requirement that a TxManager be specified.
     */
    public class NoTxManager implements TxManager {

        /**
         *  Empty method since no additional transaction management
         *  with the registry is required.
         */
        @Override
        public void startTransaction() {
        }

        /**
         *  Empty method since no additional transaction management
         *  with the registry is required.
         */
         @Override
        public void closeTransaction() {
        }
    }

    /**
     * Retrieve the url to the query manager service.
     * 
     * @return the url
     */
    protected abstract String getQueryManagerUrl();

    /**
     * Retrieve the url to the lifecycle manager service.
     * 
     * @return the url
     */
    protected abstract String getLifecycleManagerUrl();
}
