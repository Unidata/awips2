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
package com.raytheon.edex.monitors;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.Set;

import javax.management.MBeanServerConnection;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;

import com.raytheon.edex.services.MonitorSrv;
import com.raytheon.edex.util.Util;

/**
 * Uses JMX to obtain latency information for an end-point. Reports queue size,
 * event backlog and average execution time for the end-point. The format of the
 * log message is
 * <P>
 *         {end-point name}: Queue Size = {count}, Event Backlog  = {count}, Average Execution Time = {count}
 * <P>
 * The monitoring defaults to the current Mule instance on the local computer.
 * There is no default for the end-point parameter.
 * <P>
 * This class is intended to be injected into an {@link MonitorSrv} instance by
 * Mule. Because of that, all constructor arguments are of type String. 
 * <P>
 *    
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07May2008    1113       MW Fegan    Initial creation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public class EdexLatencyMonitor extends AEdexMonitor {
    /* format string for creating the connection URL */
    private static final String URL_FORMAT = "service:jmx:rmi://%s/jndi/rmi://%s:%s/%s";
    /* format string for reporting statistics */
    private static final String REPORT_FORMAT = "%s: Queue Size = %d, Event Backlog  = %d, Average Execution Time = %d";
    /** host name for connection string */
    private String server = "localhost";
    /** port for connection string */
    private String port = "1099";
    /** application for connection string */
    private String application = "Mule";
    /** complete connection URL */
    private String strurl = String.format(URL_FORMAT, server,server,port,application);
    /** name of end-point to monitor */
    private String beanName = null;

    MBeanServerConnection connection = null;
    /**
     * Constructor. Creates a monitor using defaults.
     */
    public EdexLatencyMonitor() {
        // intentionally empty
    }
    /**
     * Constructor. Creates a monitor for the specified end-point. The monitor uses
     * the default connection URL.
     * 
     * @param beanName name of end-point to monitor
     */
    public EdexLatencyMonitor(String beanName) {
        this.beanName = beanName;
    }
    /**
     * Constructor. Creates a monitor for the specified Mule instance using the 
     * specified end-point.
     * 
     * @param server the server hosting the mule instance
     * @param port the port to which Mule is listening
     * @param application the Mule application identifier (usually "Mule")
     * @param beanName name of end-point to monitor
     */
    public EdexLatencyMonitor(String server, String port, String application, String beanName) {
        this.server = server;
        this.port = port;
        this.application = application;
        this.strurl = String.format(URL_FORMAT, server,server,port,application);
        this.beanName = beanName;
    }
    /**
     * Constructor. Creates a monitor for the specified connection URL using the
     * specified end-point. The format of the connection URL is
     * <P>
     *       service:jmx:rmi://{server}/jndi/rmi://{server}:{port}/{application}
     * 
     * @param strurl the connection URL
     * @param beanName  name of end-point to monitor
     */
    public EdexLatencyMonitor(String strurl, String beanName) {
        this.strurl = strurl;
        this.beanName = beanName;
    }
    /**
     * Initializes the JMX connection.
     */
    private void init() {
        if (this.connection != null) {
            return;
        }
        JMXConnector connector = null;
        try {
            JMXServiceURL url = new JMXServiceURL(strurl);
            connector = JMXConnectorFactory.connect(url);
        } catch (MalformedURLException e) {
            logger.warn(connection + "is invalid",e);
        } catch (IOException e) {
            logger.warn("Unable to connect to " + strurl,e);
        }
        if (connector != null) {
            try {
                this.connection = connector.getMBeanServerConnection();
            } catch (IOException e) {
                logger.warn("Unable to create connection to " + strurl,e);
            }
        }
    }

    /* (non-Javadoc)
     * @see com.raytheon.edex.monitors.AEdexMonitor#execute()
     */
    @Override
    public void execute() {
        init();
        if (this.connection == null) {
            logger.warn("connection to " + strurl + " is unavailable, unable to monitor");
            return;
        }
        try {
            String[] domainNames = this.connection.getDomains();
            String domainName = "";
            for (String name : domainNames) {
                if (name.startsWith("Mule")) {
                    domainName = name;
                }
            }
            String objStr = domainName+":type=org.mule.Component,name=\""+beanName+"\"";
            Set<ObjectName> sets = null;
            ObjectName obName = new ObjectName(objStr);
            sets = this.connection.queryNames(obName, null);
            if (sets.size() != 1) {
                logger.warn("unable to get access to " + objStr);
                return;
            }
            ObjectName theObject = (ObjectName)sets.toArray()[0];
            Integer queueSize = (Integer)connection.getAttribute(theObject, "QueueSize");
            Long queuedEvents = (Long)connection.getAttribute(theObject, "QueuedEvents");
            Long averageTime = (Long)connection.getAttribute(theObject, "AverageExecutionTime");
            logger.info(String.format(REPORT_FORMAT,Util.printString(beanName),
                    queueSize,queuedEvents,averageTime));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /* (non-Javadoc)
     * @see com.raytheon.edex.monitors.AEdexMonitor#setData(java.lang.String)
     */
    @Override
    public void setData(String data) {
        this.beanName = data;
    }

}
