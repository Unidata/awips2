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
package com.raytheon.uf.edex.distribution;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.apache.camel.Exchange;
import org.apache.camel.Message;
import org.apache.camel.RecipientList;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * The purpose of this bean is to load a series of XML files from localization
 * for each plugin registering itself with this bean and route messages based on
 * those XML files. The route method will provide a list of destinations based
 * on a header (or filename) and the associated plugin specified regex patterns.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 16, 2009            brockwoo    Initial creation
 * 6/8/2010     4647       bphillip    Added automatic pattern refreshing
 * 09/01/2010   4293       cjeanbap    Logging of unknown Weather Products.
 * Feb 27, 2013 1638       mschenke    Cleaned up localization code to fix null pointer
 *                                     when no distribution files present
 * Mar 19, 2013 1794       djohnson    PatternWrapper is immutable, add toString() to it for debugging.
 * Aug 19, 2013 2257       bkowal      edexBridge to qpid 0.18 upgrade
 * Aug 30, 2013 2163       bkowal      edexBridge to qpid 0.18 RHEL6 upgrade
 * Sep 06, 2013 2327       rjpeter     Updated to use DistributionPatterns.
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class DistributionSrv {
    private static final String HEADER_QPID_SUBJECT = "qpid.subject";

    private static final String MESSAGE_HEADER = "header";
    protected Log logger = LogFactory.getLog("Ingest");


    protected Log routeFailedLogger = LogFactory.getLog("RouteFailedLog");

    private final ConcurrentMap<String, String> pluginRoutes = new ConcurrentHashMap<String, String>();

    /**
     * Allows a plugin to register itself with this bean. Note: if the plugin
     * does not provide an XML or it is malformed, this will throw an exception.
     * 
     * @param pluginName
     *            the plugin name
     * @param destination
     *            a destination to send this message to
     * @return an instance of this bean
     * @throws DistributionException
     */
    public DistributionSrv register(String pluginName, String destination)
            throws DistributionException {
        if (!DistributionPatterns.getInstance()
                .hasPatternsForPlugin(pluginName)) {
            throw new DistributionException(
                    "Plugin "
                            + pluginName
                            + " does not have an accompanying patterns file in localization.");
        }
        pluginRoutes.put(pluginName, destination);
        return this;
    }

    /**
     * Generates a list of destinations for this message based on the header (or
     * filename if the header is not available).
     * 
     * @param exchange
     *            The exchange object
     * @return a list of destinations
     */
    @RecipientList
    public List<String> route(Exchange exchange) {
        Message in = exchange.getIn();
        // determine if the header is in the qpid subject field?
        String header = (String) in.getHeader(HEADER_QPID_SUBJECT);
        if (header != null) {
            // make the qpid subject the header so that everything downstream
            // will be able to read it as the header.
            in.setHeader(MESSAGE_HEADER, header);
        }

        header = (String) in.getHeader(MESSAGE_HEADER);
        Object payload = in.getBody();
        String bodyString = null;
        if (payload instanceof byte[]) {
            bodyString = new String((byte[]) payload);
        } else if (payload instanceof String) {
            bodyString = (String) payload;
        }
        File file = new File(bodyString);
        if (!file.exists()) {
            logger.error("File does not exist : " + bodyString);
            exchange.getOut().setFault(true);
        } else {
            in.setHeader("ingestFileName", file.toString());
        }
        boolean unroutedFlag = true;
        if (header == null) {
            // No header entry so will try and use the filename instead
            header = (String) exchange.getIn().getBody();
        }

        List<String> plugins = DistributionPatterns.getInstance()
                .getMatchingPlugins(header, pluginRoutes.keySet());
        List<String> routes = new ArrayList<String>(plugins.size());
        StringBuilder pluginNames = new StringBuilder(plugins.size() * 8);
        for (String plugin : plugins) {
            String route = pluginRoutes.get(plugin);
            if (route != null) {
                if (pluginNames.length() != 0) {
                    pluginNames.append(",");
                }
                pluginNames.append(plugin);
                routes.add(route);
                unroutedFlag = false;
            } else if (logger.isDebugEnabled()) {
                logger.debug("No route registered for plugin: " + plugin);
            }
        }

        if (unroutedFlag) {
            // append WMO header/filename to failed route logger
            // using warn instead of error; app can continue
            routeFailedLogger.warn(header);
        }

        in.setHeader("pluginName", pluginNames.toString());
        return routes;
    }
}
