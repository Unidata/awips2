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
package com.raytheon.uf.viz.alertviz.internal;

import java.util.HashMap;
import java.util.Map;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Session;

import org.apache.activemq.ActiveMQConnection;
import org.apache.activemq.ActiveMQConnectionFactory;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.alertviz.AlertvizJob;
import com.raytheon.uf.viz.alertviz.Container;

/**
 * Shared JMS Connection properties (only one JMS connection per instance)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 15, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class JMSConnectionManager {

    private Connection connection;

    private Session session;

    private ActiveMQConnectionFactory factory;

    private String host;

    private static Map<String, JMSConnectionManager> managerMap = new HashMap<String, JMSConnectionManager>();

    private JMSConnectionManager(String host) throws JMSException {
        this.host = host;
        if (session == null) {
            factory = new ActiveMQConnectionFactory(host);
        }
        connect();
    }

    public static JMSConnectionManager getConnectionManager(boolean local,
            int port) throws JMSException {
        String host = local ? "vm://" + AlertvizJob.LOCAL_SERVICE_NAME
                : AlertvizJob.TCP_CONNECTION + port;
        return getConnectionManager(host);
    }

    public static JMSConnectionManager getConnectionManager(String host)
            throws JMSException {
        JMSConnectionManager manager = managerMap.get(host);

        if (manager == null) {
            manager = new JMSConnectionManager(host);
            managerMap.put(host, manager);
        }

        return manager;
    }

    private void connect() throws JMSException {
        connection = factory.createConnection();
        session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);
    }

    public boolean isConnectionClosed() {
        if (connection instanceof ActiveMQConnection) {
            return (((ActiveMQConnection) connection).isClosed());
        }
        return false;
    }

    public void resetConnection() throws JMSException {
        try {
            connection.close();
        } catch (JMSException e) {
            // ignore
        }
        connect();
    }

    public void startProcessing() throws JMSException {
        connection.start();
    }

    public Session getSession() {
        return session;
    }

    public Connection getConnection() {
        return connection;
    }

    /**
     * 
     */
    public void closeSession() {
        try {
            session.close();
            connection.close();
        } catch (Exception e) {
            // Log to internal Log4j log
            Container.logInternal(Priority.ERROR,
                    "JMSConnnectionManager: exception when closing "
                            + "the JMS session and connection.", e);
        }
        managerMap.remove(host);
    }

}
