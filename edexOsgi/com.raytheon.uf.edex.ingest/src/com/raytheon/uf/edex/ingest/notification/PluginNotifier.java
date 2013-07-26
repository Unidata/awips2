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
package com.raytheon.uf.edex.ingest.notification;

import java.util.ArrayList;
import java.util.List;

import org.apache.camel.Exchange;
import org.apache.camel.Message;
import org.apache.camel.RecipientList;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Plugins can register routes with this and then be generically fired to. Helps
 * to reduce dependencies as we no longer need to call a route directly.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 13, 2013            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class PluginNotifier {

    private static final IUFStatusHandler theHandler = UFStatus
            .getHandler(PluginNotifier.class);

    private static final Multimap<String, String> routes = ArrayListMultimap
            .create();

    private static final PluginNotifier distributor = new PluginNotifier();

    private PluginNotifier() {
    }

    public PluginNotifier getInstance() {
        return distributor;
    }

    /**
     * Normally called from Spring, this allows a set of commands to be
     * registered for a specific plugin.
     * 
     * @param name
     * @param command
     * @return
     */
    public PluginNotifier register(String name, String command) {
        routes.put(name, command);
        return this;
    }

    /**
     * Takes the pluginName, which is what was registered for, and sends to all
     * the routes that are registered as that.
     * 
     * @param exchange
     * @return
     */
    @RecipientList
    public List<String> send(Exchange exchange) {
        Message in = exchange.getIn();
        String name = (String) in.getHeader("pluginName");
        List<String> list = new ArrayList<String>();
        for (String route : routes.get(name)) {
            try {
                EDEXUtil.getMessageProducer().sendAsyncUri(route, in.getBody());
            } catch (EdexException e) {
                theHandler.handle(Priority.PROBLEM,
                        "Unable to send message to " + route, e);
            }
        }
        return list;
    }
}
