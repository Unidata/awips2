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
package com.raytheon.openfire.plugin.configuration.collaboration.http;

import java.io.IOException;
import java.util.TimerTask;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.http.HttpStatus;
import org.jivesoftware.openfire.SessionManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;

import com.raytheon.openfire.plugin.configuration.collaboration.configuration.ConfigurationPacket;
import com.raytheon.openfire.plugin.configuration.collaboration.iq.AbstractConfigHandler;
import com.raytheon.openfire.plugin.configuration.collaboration.iq.HttpAddressHandler;

/**
 * Runs a series of checks to determine if http collaboration is still running
 * on-demand and on a scheduled basis. The checks include: verifying that a url
 * is configured for the primary dataserver and executing an http GET request
 * against the http dataserver.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2012            bkowal      Initial creation
 * Jan 06, 2013  2563      bclement    replaced chat message with packet extension
 * Feb 14, 2013 2756       bclement    rename and refactor for operation with generic http
 *                                     server configured over XMPP
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class HttpStatusMonitor extends TimerTask {
    private static final Logger logger = LoggerFactory
            .getLogger(HttpStatusMonitor.class);

    private final SessionManager sessionManager;

    private static final HttpClient httpClient = new HttpClient();

    private GetMethod _getMethod = null;

    private String _previousUrl = null;

    private static enum Status {
        URL_UNSET, SERVER_DOWN, SERVER_UP
    }

    private Status status = Status.URL_UNSET;

    private final HttpAddressHandler addressHandler;

    private final JID serverId;

    /**
     * @param addressHandler
     *            IQ hander responsible for http url configuration
     * @param serverId
     *            id of server used as from address in messages
     */
    public HttpStatusMonitor(HttpAddressHandler addressHandler, JID serverId) {
        this.sessionManager = SessionManager.getInstance();
        this.addressHandler = addressHandler;
        this.serverId = serverId;
    }

    /**
     * Get the current URL configuration string.
     * 
     * @return null if no valid configuration is available
     */
    public String getCurrentUrlConfig() {
        String primary = AbstractConfigHandler.getPrimaryDataServer();
        if (primary == null) {
            logger.debug("No primary dataserver found");
            urlUnset();
            return null;
        } else {
            String url = addressHandler.getHttpAddress(primary);
            if (url == null) {
                logger.debug("No url set for dataserver: " + primary);
                urlUnset();
                return null;
            } else {
                return verifyHttpProcess(url);
            }
        }
    }

    /**
     * Verify that the http server is online and accepting requests. Returns the
     * valid URL configuration string.
     * 
     * @param url
     *            url to verify
     * @return null if successful request cannot be made
     */
    private String verifyHttpProcess(String url) {
        synchronized (httpClient) {
            int statusCode = -1;
            try {
                GetMethod get = getCachedMethod(url);
                statusCode = httpClient.executeMethod(get);
            } catch (HttpException e1) {
                String msg = "Unable to execute GET against the collaboration dataserver at "
                        + url;
                logger.error(msg, e1);
                setOffline(msg);
            } catch (IOException e2) {
                String msg = "Unable to read the response from the collaboration dataserver at "
                        + url;
                logger.error(msg, e2);
                setOffline(msg);
            }

            if ((statusCode == HttpStatus.SC_OK) == false) {
                String msg = "Dataserver not is not available - received status "
                        + statusCode;
                logger.error(msg);
                setOffline(msg);
                return null;
            } else {
                String urlConfig = "sessionDataHttpURL : " + url;
                setOnline(urlConfig);
                return urlConfig;
            }
        }
    }

    /**
     * Get the http GET method for url. Uses a simple cache.
     * 
     * @param url
     * @return
     */
    private GetMethod getCachedMethod(String url) {
        if (_previousUrl == null || !_previousUrl.equals(url)) {
            logger.debug("Dataserver url changed from " + _previousUrl + " to "
                    + url);
            _getMethod = new GetMethod(url);
            _previousUrl = url;
        }
        return _getMethod;
    }

    @Override
    public void run() {
        logger.debug("Verifying that httpd-collaboration is still available ...");
        getCurrentUrlConfig();
    }

    /**
     * Broadcast configuration message to all users
     * 
     * @param body
     *            url or error configuration string
     */
    private synchronized void broadcastMessage(String body) {
        logger.debug("Broadcasting message: " + body);
        Message message = ConfigurationPacket.createMessage(body);
        message.setFrom(serverId);
        this.sessionManager.broadcast(message);
    }

    /**
     * Method to call when http server cannot be reached
     * 
     * @param message
     */
    private void setOffline(String message) {
        switch (status) {
        case SERVER_UP:
            logger.debug("Status changing from " + status + " to "
                    + Status.SERVER_DOWN);
            broadcastMessage(formatError(message));
            status = Status.SERVER_DOWN;
        default:
            logger.debug("SetOffline: Status not changed from " + status);
            // no action since we don't want to spam with error messages
            break;
        }
    }

    /**
     * Format error message to configuration format
     * 
     * @param message
     * @return
     */
    private String formatError(String message) {
        return "error : " + message;
    }

    /**
     * Method to call when http server is configured and running
     * 
     * @param urlConfig
     */
    private void setOnline(String urlConfig) {
        switch (status) {
        case SERVER_DOWN:
        case URL_UNSET:
            logger.debug("Status changing from " + status + " to "
                    + Status.SERVER_UP);
            broadcastMessage(urlConfig);
            status = Status.SERVER_UP;
            break;
        default:
            logger.debug("SetOnline: Status not changed from " + status);
            // no action
        }
    }

    /**
     * Method to call when http server url is not configured
     */
    private void urlUnset() {
        switch (status) {
        case SERVER_UP:
            logger.debug("Status changing from " + status + " to "
                    + Status.URL_UNSET);
            broadcastMessage(formatError("Dataserver does not have URL configured in chat server."));
            status = Status.URL_UNSET;
        default:
            logger.debug("UrlUnset: Status not changed from " + status);
            // no action since we don't want to spam with error messages
            break;
        }
    }
}