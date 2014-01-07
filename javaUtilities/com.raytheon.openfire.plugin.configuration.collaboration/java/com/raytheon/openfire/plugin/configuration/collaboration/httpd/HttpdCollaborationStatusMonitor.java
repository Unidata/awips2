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
package com.raytheon.openfire.plugin.configuration.collaboration.httpd;

import java.io.File;
import java.io.IOException;
import java.util.TimerTask;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.http.HttpStatus;
import org.jivesoftware.openfire.SessionManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xmpp.packet.Message;

import com.raytheon.openfire.plugin.configuration.collaboration.configuration.ConfigurationPacket;
import com.raytheon.openfire.plugin.configuration.collaboration.exception.HttpdCollaborationNotRunningException;
import com.raytheon.openfire.plugin.configuration.collaboration.exception.HttpdCollaborationStatusException;
import com.raytheon.openfire.plugin.configuration.collaboration.util.HttpdCollaborationUtil;

/**
 * Runs a series of checks to determine if httpd-collaboration is still running
 * on-demand and on a scheduled basis. The checks include: verifying that a pid
 * file exists for httpd-collaboration and executing an http GET request against
 * the httpd-collaboration server.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2012            bkowal      Initial creation
 * Jan 06, 2013  2563      bclement    replaced chat message with packet extension
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class HttpdCollaborationStatusMonitor extends TimerTask {
    private static final Logger logger = LoggerFactory
            .getLogger(HttpdCollaborationStatusMonitor.class);

    private SessionManager sessionManager;

    private boolean httpdCollaborationBecameUnavailable;

    // provided by the configuration
    private String HTTPD_COLLABORATION_CONFIGURATION;

    private static String HTTPD_PID_FILE;

    private static File pidFile;

    private static final HttpClient httpClient = new HttpClient();

    private GetMethod getMethod;

    /**
     * 
     */
    public HttpdCollaborationStatusMonitor(String _location,
            String _httpdCollaborationURL,
            String _httpdCollaborationConfiguration) {
        HTTPD_PID_FILE = HttpdCollaborationUtil.endPathIfNecessary(_location)
                + "var/run/httpd.pid";
        HTTPD_COLLABORATION_CONFIGURATION = _httpdCollaborationConfiguration;
        this.sessionManager = SessionManager.getInstance();
        this.httpdCollaborationBecameUnavailable = false;

        pidFile = new File(HTTPD_PID_FILE);
        this.getMethod = new GetMethod(
                HttpdCollaborationUtil
                        .endPathIfNecessary(_httpdCollaborationURL));
    }

    public void statusHttpdCollaboration()
            throws HttpdCollaborationNotRunningException,
            HttpdCollaborationStatusException {
        this.doesPidFileExist();

        // "ping" the httpd process to ensure that it is actually running
        this.verifyHttpdProcess();
    }

    private void doesPidFileExist()
            throws HttpdCollaborationNotRunningException {
        // verify the httpd-collaboration pid file exists.

        if (pidFile.exists() == false) {
            // httpd-collaboration is not running
            throw new HttpdCollaborationNotRunningException(
                    "the httpd-collaboration pid file does not exist: "
                            + pidFile.getAbsolutePath());
        }
    }

    private void verifyHttpdProcess()
            throws HttpdCollaborationNotRunningException,
            HttpdCollaborationStatusException {
        synchronized (this.getMethod) {
            int statusCode = -1;
            try {
                statusCode = httpClient.executeMethod(getMethod);
            } catch (HttpException e1) {
                throw new HttpdCollaborationStatusException(
                        "Unable to execute GET against the httpd-collaboration server",
                        e1);
            } catch (IOException e2) {
                throw new HttpdCollaborationStatusException(
                        "Unable to read the response from the httpd-collaboration server",
                        e2);
            }

            if ((statusCode == HttpStatus.SC_OK) == false) {
                throw new HttpdCollaborationNotRunningException(
                        "httpd-collaboration is not available - received status = "
                                + statusCode);
            }
        }
    }

    @Override
    public void run() {
        logger.debug("Verifying that httpd-collaboration is still available ...");
        String errorMessage = null;

        try {
            this.statusHttpdCollaboration();
        } catch (HttpdCollaborationNotRunningException e1) {
            logger.error("httpd-collaboration is not available", e1);
            this.httpdCollaborationBecameUnavailable = true;
            errorMessage = HttpdCollaborationUtil.encodeErrorMessage(e1);
        } catch (HttpdCollaborationStatusException e2) {
            logger.error(
                    "unable to determine if httpd-collaboration is still available!!!",
                    e2);
            this.httpdCollaborationBecameUnavailable = true;
            errorMessage = HttpdCollaborationUtil.encodeErrorMessage(e2);
        }

        if (errorMessage == null && this.httpdCollaborationBecameUnavailable) {
            // If we reach this point, httpd-collaboration became
            // unavailable at some point in time; re-enable shared displays
            // in CAVE for all users.
            this.broadcastMessage(HTTPD_COLLABORATION_CONFIGURATION);
            this.httpdCollaborationBecameUnavailable = false;
        } else {
            // Broadcast to all users that httpd-collaboration is no longer
            // available; shared displays in CAVE will be disabled
            this.broadcastMessage(errorMessage);
        }
    }

    private synchronized void broadcastMessage(String body) {
        Message message = ConfigurationPacket.createMessage(body);
        this.sessionManager.broadcast(message);
    }
}