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

import java.net.URLEncoder;

import javax.jms.ConnectionFactory;
import org.apache.qpid.client.AMQConnectionFactory;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.VizApp;

/**
 * 
 * Common JMS connection code
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2008            chammack    Moved to uf.viz.core
 * Nov 2, 2009  #3067      chammack    Send all jms connections through failover:// to properly reconnect
 * Nov 2, 2011  #7391      bkowal      Ensure that the generated WsId is properly formatted to be
 *                                     included in a url.
 * May 09, 2013 1814       rjpeter     Updated prefetch to 10.
 * Aug 16, 2013 2169       bkowal      CAVE will now synchronously acknowledge messages.
 * Aug 27, 2013 2295       bkowal      The entire connection string is now provided by EDEX; so, it
 *                                     no longer needs to be constructed. Replaced stacktrace
 *                                     printing with UFStatus.
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class JMSConnection {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(JMSConnection.class);

    private static final String WSID_PLACEHOLDER = "__WSID__";

    private static JMSConnection instance;

    private final String connectionUrl;

    private AMQConnectionFactory factory;

    public static synchronized JMSConnection getInstance() {
        if (instance == null) {
            instance = new JMSConnection();
        }

        return instance;
    }

    public JMSConnection() {
        this(VizApp.getJmsConnectionString());
    }

    public JMSConnection(String connectionUrl) {
        this.connectionUrl = connectionUrl;
        try {
            String wsid = URLEncoder.encode(VizApp.getWsId().toString(),
                    "UTF-8");
            this.factory = new AMQConnectionFactory(this.connectionUrl.replace(
                    WSID_PLACEHOLDER, wsid));
        } catch (Exception e) {
            statusHandler.fatal("Failed to connect to the JMS Server!", e);
        }
    }

    /**
     * 
     * @return the jms connection url
     */
    public String getConnectionUrl() {
        return connectionUrl;
    }

    /**
     * @return the factory
     */
    public ConnectionFactory getFactory() {
        return factory;
    }

}
