package com.raytheon.uf.edex.datadelivery.harvester.interfaces;

/**
 * Store Link objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2011    218      dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

import edu.uci.ics.crawler4j.url.WebURL;

public interface IStoreLink {

    public void storeLink(WebURL url);

}
