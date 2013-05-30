package com.raytheon.uf.edex.datadelivery.retrieval.util;

import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * 
 * WFS Connection.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2013 753        dhladky     created.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */


public class WfsConnectionUtil {
    
    private static final IUFStatusHandler statusHandler = UFStatus
    .getHandler(WfsConnectionUtil.class);
    
    public static String wfsConnect(String address, String message) {
        String xmlResponse = null;
        // sets up any proxy info that might be necessary
        ConnectionUtil.getProxyParameters();
        HttpClient http = HttpClient.getInstance();
        
        try {
            xmlResponse = http.post(address, message);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Couldn't connect to WFS server: "+address+" with posted message of: "+message, e);
        }
        
        return xmlResponse;
    }

}
