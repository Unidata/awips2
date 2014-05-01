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
package com.raytheon.uf.viz.core.comm;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.jms.JMSException;

import org.apache.activemq.ActiveMQConnectionFactory;
import org.apache.http.client.methods.HttpGet;

import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.localization.msgs.GetServersRequest;
import com.raytheon.uf.common.localization.msgs.GetServersResponse;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * Class for checking connectivity of http servers, currently only used for
 * localization, ie that is why it pops up a localization dialog. This class can
 * ultimately become a manager for checking connectivy of servers periodically
 * and notifying listeners of connectivity changes. should be made asynchronous
 * from all other things. Should be handlers passed in for hasConnectivity
 * function. Each class using hasConnectivity probably has their own method of
 * handling the check.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 12, 2009            mschenke    Initial creation
 * Mar 22, 2013 1786       mpduff      Changed to use HttpClient for
 *                                     connectivity.
 * Aug 02, 2013 2202       bsteffen    Add edex specific connectivity checking.
 * Jan 15, 2014            njensen     Added printConnectivityProblems()
 * Feb 04, 2014 2704       njensen     Check JMS capability, return exceptions with results
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ConnectivityManager {

    /*
     * Since a get servers request is used for checking localization server
     * connectivity, this map will cache the result in case it is needed later.
     */
    private static final Map<String, GetServersResponse> getServersResponseCache = Collections
            .synchronizedMap(new HashMap<String, GetServersResponse>(2));

    public static class ConnectivityResult {
        public boolean hasConnectivity;

        public String server;

        public Exception exception;

        public ConnectivityResult(boolean hc, String server) {
            this(hc, server, null);
        }

        public ConnectivityResult(boolean hc, String server, Exception e) {
            this.hasConnectivity = hc;
            this.server = server;
            this.exception = e;
        }
    }

    /**
     * Checks the connectivity of the given server
     * 
     * @param server
     *            server to check
     * @return whether quit was selected. TODO: need to return two booleans, one
     *         for quit and one for connectivity
     */
    public static void checkHttpServer(String server,
            IConnectivityCallback callback) {
        boolean good = false;
        Exception exc = null;
        try {
            HttpClient client = HttpClient.getInstance();
            HttpGet request = new HttpGet();
            request.setURI(new URI(server));
            client.executeRequest(request);
            good = true;
        } catch (Exception e) {
            exc = e;
        }
        callback.connectionChecked(new ConnectivityResult(good, server, exc));
    }

    /**
     * Checks the connectivity of the given localization server
     * 
     * @param server
     *            server to check
     */
    public static void checkLocalizationServer(String server,
            IConnectivityCallback callback) {
        boolean good = false;
        Exception exc = null;
        try {
            good = checkLocalizationServer(server, true) != null;
        } catch (Exception e) {
            exc = e;
        }
        callback.connectionChecked(new ConnectivityResult(good, server, exc));
    }

    /**
     * Returns a GetServersResponse for the provided server. If force is false
     * and this localization server has already been contacted then a cached
     * result is returned, otherwise the localization server is contacted to get
     * the response.
     */
    public static GetServersResponse checkLocalizationServer(String server,
            boolean force) throws VizException {
        if (!force) {
            GetServersResponse resp = getServersResponseCache.get(server);
            if (resp != null) {
                return resp;
            }
        }
        GetServersRequest req = new GetServersRequest();
        GetServersResponse resp = (GetServersResponse) ThriftClient
                .sendRequest(req, server);
        getServersResponseCache.put(server, resp);
        return resp;
    }

    /**
     * Checks the connectivity of the given alert service
     * 
     * @param server
     *            server to check
     * @return whether quit was selected. TODO: need to return two booleans, one
     *         for quit and one for connectivity
     */
    public static void checkAlertService(String server,
            IConnectivityCallback callback) {
        boolean good = true;
        Exception exc = null;
        try {
            ActiveMQConnectionFactory f = new ActiveMQConnectionFactory(server);
            f.createConnection().close();
        } catch (JMSException e) {
            exc = e;
            good = false;
        }
        callback.connectionChecked(new ConnectivityResult(good, server, exc));
    }

    /**
     * Checks the connectivity of the given JMS server
     * 
     * @param connectionString
     * @param callback
     */
    public static void checkJmsServer(String connectionString,
            IConnectivityCallback callback) {
        boolean good = true;
        Exception exc = null;
        try {
            JMSConnection jms = new JMSConnection(connectionString);
            jms.getFactory().createConnection().close();
        } catch (JMSException e) {
            exc = e;
            good = false;
        }
        callback.connectionChecked(new ConnectivityResult(good,
                connectionString, exc));
    }

}
