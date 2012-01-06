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

package com.raytheon.edex.jmx;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.Iterator;
import java.util.Set;

import javax.management.MBeanInfo;
import javax.management.MBeanOperationInfo;
import javax.management.MBeanParameterInfo;
import javax.management.MBeanServerConnection;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * A client wrapper to access JMX.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         PR#    Engineer         Description
 * ------------ ------ ---------------- -------------------------------------------------
 * Apr 26, 2007 #261   garmendariz      Initial creation
 * </pre>
 * 
 * @author garmendariz
 */
public class Client {

    /** The logger */
    private final static transient Log logger = LogFactory.getLog(Client.class);

    public static final String MULE = "server";

    public static final String ACTIVEMQ = "jmxrmi";

    public static void main(String[] args) throws Exception {

        MBeanServerConnection connection = null;

        System.out.println("Server is: "
                + (Client.checkServer(Client.MULE, "localhost") == true ? "Up"
                        : "Down"));
        connection = Client.createConnection(Client.MULE, "localhost");

        String domainNames[] = Client.queryDomainNames(connection);

        // ObjectName obj = null;

        Set<ObjectName> sets = null;

        for (int i = 0; i < domainNames.length; i++) {
            System.out.println("\n****DOMAIN: " + domainNames[i]);
            sets = Client.queryObjectNames(connection, domainNames[i]);

            for (Iterator<ObjectName> iter = sets.iterator(); iter.hasNext();) {
                ObjectName element = (ObjectName) iter.next();
                // System.out.println(element.getDomain());
                System.out.println("MBean: " + element.getCanonicalName());
                // ObjectInstance instance =
                // connection.getObjectInstance(element);
                processBeanInfo(connection.getMBeanInfo(element));
            }
        }

        // make a test call
        System.out.println("---------------Making test call--------------");
        sets = connection
                .queryNames(
                        new ObjectName(
                                "Mule*:name=Awips.Mule.Service.IngestSrv-ASCII,type=org.mule.Component"),
                        null);
        for (Iterator<ObjectName> iter = sets.iterator(); iter.hasNext();) {
            ObjectName element = (ObjectName) iter.next();
            System.out.println("MBean: " + element.getCanonicalName());
            MBeanInfo info = connection.getMBeanInfo(element);
            MBeanOperationInfo[] oper = info.getOperations();

            for (int i = 0; i < oper.length; i++) {
                if (oper[i].getName().contains("start")) {
                    // MBeanParameterInfo[] mbeanParm = oper[i].getSignature();
                    connection.invoke(element, oper[i].getName(),
                            new String[] {}, new String[] {});

                }
            }
        }

    }

    private static void processBeanInfo(MBeanInfo info) {
        System.out.println("\tMBean Class Name: " + info.getClassName());
        System.out.println("\t\tMBean Description: " + info.getDescription());
        MBeanOperationInfo[] ops = info.getOperations();

        for (int i = 0; i < ops.length; i++) {
            System.out.println("\t\t\t\tOperation #" + i);
            System.out.println("\t\t\t\tOperation name: " + ops[i].getName());
            System.out.println("\t\t\t\tOperation Impact: "
                    + ops[i].getImpact());
            System.out.println("\t\t\t\tOperation ret type: "
                    + ops[i].getReturnType());

            MBeanParameterInfo[] ard = ops[i].getSignature();

            for (int j = 0; j < ard.length; j++) {
                System.out.println("\t\t\t\t\tOperation params");
                System.out
                        .println("\t\t\t\t\tparam name = " + ard[j].getName());
                System.out
                        .println("\t\t\t\t\tparam type = " + ard[j].getType());
                System.out.println("\t\t\t\t\tparam desc = "
                        + ard[j].getDescription());
            }

        }
    }

    /**
     * Check for the availability of the MBean server for an application at the
     * given host.
     * 
     * @param application
     *            The application to query (Mule, ActiveMQ, etc.)
     * @param host
     *            The hostname of the box
     * @return True if the server is available
     * @throws Exception
     */
    public static boolean checkServer(String application, String host)
            throws Exception {
        JMXServiceURL url = null;
        JMXConnector connector = null;
        boolean retVal = false;

        try {
            // get url to connect to
            url = new JMXServiceURL("service:jmx:rmi://" + host
                    + "/jndi/rmi://" + host + ":1099/" + application);
        } catch (MalformedURLException e) {
            System.out.println("service:jmx:rmi://" + host + "/jndi/rmi://"
                    + host + ":1099/" + application + " is invalid");
        }

        try {
            // verify server status
            connector = JMXConnectorFactory.connect(url);
        } catch (IOException e) {
            // don't report we can't connect, return value is doing this
        } finally {
            if (connector != null) {
                connector.close();
                retVal = true;
            }
        }

        return retVal;

    }

    /**
     * Creates an RMI connection to the MBean server for the application on the
     * given host.
     * 
     * @param application
     *            The application to query (Mule, ActiveMQ, etc.)
     * @param host
     *            The hostname of the box
     * @return A connection to this MBean servers
     * @throws Exception
     */
    public static MBeanServerConnection createConnection(String application,
            String host) throws Exception {
        JMXServiceURL url = null;
        JMXConnector connector = null;
        MBeanServerConnection connection = null;
        String urlString = "service:jmx:rmi://" + host + "/jndi/rmi://" + host
                + ":1099/" + application;

        try {
            // get url to connect to
            url = new JMXServiceURL(urlString);

            connector = JMXConnectorFactory.connect(url);
        } catch (MalformedURLException e) {
            throw new Exception(urlString + " is invalid");
        } catch (IOException e) {
            throw new Exception("Unable to connect to: " + urlString);
        }

        if (connector != null) {
            connection = connector.getMBeanServerConnection();
        } else {
            throw new Exception("Unable to create connection to: " + urlString);
        }

        return connection;

    }

    /**
     * Retrieves the list of domain names available for this connection
     * 
     * @param connection
     *            A connection to a MBean server
     * @return A string array of domain names
     */
    public static String[] queryDomainNames(MBeanServerConnection connection) {
        String[] domainNames = null;

        try {
            domainNames = connection.getDomains();
        } catch (IOException e) {
            logger.error("Unable to retrieve domain names", e);
        }

        return domainNames;
    }

    /**
     * Retrieve all object names for a domain name. The query is based on the
     * following string: domainName:* , where all properties are selected
     * 
     * @param connection
     *            The connection to the MBean server
     * @param domainName
     *            The domain to query
     * @return A list of objects available for that domain
     */
    public static Set<ObjectName> queryObjectNames(
            MBeanServerConnection connection, String domainName) {
        Set<ObjectName> returnSet = null;

        try {
            returnSet = connection.queryNames(null, new ObjectName(domainName
                    + ":*"));
        } catch (Exception e) {
            logger.error("Unable to retrieve object names for domain");
        }

        return returnSet;
    }

}