/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.esb.camel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.apache.camel.Header;
import org.apache.camel.RecipientList;

import com.raytheon.uf.edex.core.EdexException;

/**
 * Provides mechanism to route messages from a central pool
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2008            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class BasicThreadPoolRouter {

    private Map<String, String> routes;

    private static Map<String, BasicThreadPoolRouter> instanceMap = new HashMap<String, BasicThreadPoolRouter>();
    
    private static Pattern pluginSeparator = Pattern.compile(",");

    public BasicThreadPoolRouter(String poolName) {
        synchronized (BasicThreadPoolRouter.class) {
            routes = new HashMap<String, String>();
            instanceMap.put(poolName, this);
        }

    }

    public static BasicThreadPoolRouter getInstance(String poolName,
            String pluginName, String routeName) throws EdexException {
        BasicThreadPoolRouter router = instanceMap.get(poolName);
        if (router == null) {
            throw new EdexException("Thread Pool " + poolName
                    + " has not been registered through spring");
        }
        router.routes.put(pluginName, routeName);
        return router;
    }

    @RecipientList
    public List <String> route(@Header(value = "pluginName") String pluginName)
            throws EdexException {
        // due to the fact that the exchange message cannot pass a list, a
        // comma separated string is used to indicate multiple destinations
        String[] plugins = pluginSeparator.split(pluginName);
        ArrayList<String> toList = new ArrayList<String>();
        for (String plugin : plugins) {
            String to = routes.get(plugin);
            if (to != null) {
                toList.add(to);
            }
        }
        if (toList.size() == 0)
            throw new EdexException(
                    "No route registered for the following plugin names: "
                            + plugins.toString());
        return toList;
    }
}
