package com.raytheon.uf.edex.registry.ebxml.services.soap;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;

import com.raytheon.uf.common.registry.ebxml.LifecycleManagerFactory;
import com.raytheon.uf.common.registry.ebxml.QueryManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

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
 * Jun 06, 2014 #3141      dhladky     creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public abstract class RemoteSOAPRegistryManager implements
        LifecycleManagerFactory, QueryManagerFactory {

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RemoteSOAPRegistryManager.class);

    /** SOAP service provider **/
    protected RegistrySOAPServices rss;

    /**
     * default constructor.
     */
    public RemoteSOAPRegistryManager() {

    }

    /**
     * Constructor to conform to bean pattern.
     */
    public RemoteSOAPRegistryManager(RegistrySOAPServices rss) {
        setRss(rss);
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
        LifecycleManager a = null;
        try {
            a = getRss().getLifecycleManagerServiceForHost(getRemoteHost());
        } catch (Exception e) {
            statusHandler.error(
                    "Can't find the Remote LifeCycleManager SOAP service!", e);
        }

        return a;
    }

    /**
     * Get an implementation of QueryManager that uses SOAP to submit requests
     * to the registry.
     * 
     * @return A SOAP implementation of QueryManager.
     * 
     * @see QueryManagerFactory
     */
    @Override
    public QueryManager getQueryManager() {

        QueryManager qm = null;
        try {
            qm = getRss().getQueryServiceForHost(getRemoteHost());
        } catch (Exception e) {
            statusHandler.error(
                    "Can't find the Remote QueryManager SOAP service!", e);
        }

        return qm;
    }

    /**
     * Get the name of the remote host
     * 
     * @return
     */
    protected abstract String getRemoteHost();

    /**
     * get the registry SOAP service helper setup
     * 
     * @return
     */
    public RegistrySOAPServices getRss() {
        return rss;
    }

    /**
     * Sets the registry SOAP service helper
     * 
     * @param rss
     */
    public void setRss(RegistrySOAPServices rss) {
        this.rss = rss;
    }
}
