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

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

import javax.jms.ConnectionFactory;

import org.apache.qpid.client.AMQConnectionFactory;
import org.apache.qpid.url.URLSyntaxException;

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
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class JMSConnection {

    private static JMSConnection instance;

    private final String jndiProviderUrl;

    private AMQConnectionFactory factory;

    public static synchronized JMSConnection getInstance() {
        if (instance == null) {
            instance = new JMSConnection();
        }

        return instance;
    }

    public JMSConnection() {
        this(VizApp.getJmsServer());
    }

    public JMSConnection(String jndiProviderUrl) {
        this.jndiProviderUrl = jndiProviderUrl;
        try {
            // do not enable retry/connectdelay connection and factory will
            // silently reconnect and user will never be notified qpid is down
            // and cave/text workstation will just act like they are hung
            // up to each individual component that opens a connection to handle
            // reconnect
            this.factory = new AMQConnectionFactory(
                    "amqp://guest:guest@"
                            + URLEncoder.encode(VizApp.getWsId().toString(),
                                    "UTF-8")
                            + "/edex?brokerlist='"
                            + this.jndiProviderUrl
                            + "?connecttimeout='5000'&heartbeat='0''&maxprefetch='10'&sync_publish='all'&failover='nofailover'");
        } catch (URLSyntaxException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (UnsupportedEncodingException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }
    }

    /**
     * @return the jndiProviderUrl
     */
    public String getJndiProviderUrl() {
        return jndiProviderUrl;
    }

    /**
     * @return the factory
     */
    public ConnectionFactory getFactory() {
        return factory;
    }

}
