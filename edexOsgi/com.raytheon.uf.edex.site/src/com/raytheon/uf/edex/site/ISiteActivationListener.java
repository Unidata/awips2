package com.raytheon.uf.edex.site;

import java.util.Set;

/**
 * Interface for managing a single server as multiple sites
 * 
 * This was originally SiteActivation in com.raytheon.edex.site
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 14, 2009            njensen     Initial creation
 * Oct 26, 2010  #6811     jclark      changed typename and package
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public interface ISiteActivationListener {

    public void activateSite(String siteID) throws Exception;

    public void deactivateSite(String siteID) throws Exception;

    public Set<String> getActiveSites();
    
    public String validateConfig(String siteID);

    /**
     * Called after the listener has been registered.
     */
    public void registered();
}
