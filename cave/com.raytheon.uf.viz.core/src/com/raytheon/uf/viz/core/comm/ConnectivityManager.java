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

import javax.jms.JMSException;

import org.apache.activemq.ActiveMQConnectionFactory;
import org.apache.http.client.methods.HttpGet;

import com.raytheon.uf.common.comm.HttpClient;

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
 * Aug 12, 2009            mschenke     Initial creation
 * Mar 22, 2013   1786     mpduff       Changed to use HttpClient for connectivity.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ConnectivityManager {

    public static class ConnectivityResult {
        public boolean hasConnectivity;

        public String server;

        public ConnectivityResult(boolean hc, String server) {
            this.hasConnectivity = hc;
            this.server = server;
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
    public static void checkHttpServer(String server, IConnectivityCallback callback) {
        boolean good = false;
        try {
            HttpClient client = HttpClient.getInstance();
            HttpGet request = new HttpGet();
            request.setURI(new URI(server));
            client.executeRequest(request);
            good = true;
        } catch (Exception e) {
            // ignore
        }
        callback.connectionChecked(new ConnectivityResult(good, server));
    }

    /**
     * Checks the connectivity of the given server
     * 
     * @param server
     *            server to check
     * @return whether quit was selected. TODO: need to return two booleans, one
     *         for quit and one for connectivity
     */
    public static void checkJmsServer(String server,
            IConnectivityCallback callback) {
        boolean good = true;
        try {
            ActiveMQConnectionFactory f = new ActiveMQConnectionFactory(server);
            f.createConnection().close();
        } catch (JMSException e) {
            good = false;
        }
        callback.connectionChecked(new ConnectivityResult(good, server));
    }
}
