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
package com.raytheon.uf.edex.esb.camel.context;

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

import org.apache.camel.CamelContext;
import org.apache.camel.Consumer;
import org.apache.camel.Route;
import org.apache.camel.impl.ServiceSupport;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterTask;

/**
 * Dynamically starts/stops a context and its associated routes so that only one
 * context in the cluster is running. This should mainly be used for reading
 * from topics so that only box is processing the topic data in the cluster for
 * singleton type events.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 10, 2010 5050       rjpeter     Initial creation
 * Jul 16, 2012 DR 15073   D. Friedman Stop consumers instead of whole context
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class ClusteredContextManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(ClusteredContextManager.class);
    private static final String taskName = "ClusteredContext";

    private String myName;

    private List<CamelContext> clusteredContextList = new ArrayList<CamelContext>();

    private int timeOutMillis;

    private static ClusteredContextManager instance = new ClusteredContextManager();

    public static ClusteredContextManager getInstance() {
        return instance;
    }

    private ClusteredContextManager() {
        myName = getHostName() + ":" + System.getProperty("edex.run.mode");
    }

    private static String getHostName() {
        String hostname = System.getenv("HOSTNAME");
        if (hostname == null) {
            hostname = System.getenv("COMPUTERNAME");
        }
        if (hostname == null) {
            hostname = System.getenv("HOST");
        }
        if (hostname == null) {
            try {
                Enumeration<NetworkInterface> nis = NetworkInterface
                        .getNetworkInterfaces();
                while (nis.hasMoreElements()) {
                    NetworkInterface ni = nis.nextElement();
                    ni.isVirtual();
                    ni.isUp();
                    Enumeration<InetAddress> addrs = ni.getInetAddresses();
                    while (addrs.hasMoreElements()) {
                        InetAddress addr = addrs.nextElement();
                        if (addr.isLinkLocalAddress() == false
                                && addr.isSiteLocalAddress() == false
                                && addr.isLoopbackAddress() == false) {
                            hostname = addr.getHostName();
                        }
                    }
                }
            } catch (SocketException e) {
                statusHandler.handle(Priority.ERROR,
                        "Failed to determine hostname", e);
            }
        }
        return hostname;
    }

    public void checkClusteredContexts() {
        for (CamelContext camelContext : clusteredContextList) {
            String contextName = camelContext.getName();
            ClusterTask lock = ClusterLockUtils.lock(taskName, contextName,
                    myName, timeOutMillis, false);
            boolean activateRoute = false;

            switch (lock.getLockState()) {
            case ALREADY_RUNNING:
                // check if we already have lock
                activateRoute = lock.getExtraInfo().equals(myName);
                break;
            case SUCCESSFUL:
                activateRoute = true;
                break;
            default:
            }

            try {
                if (activateRoute) {
                    ClusterLockUtils.updateLockTime(taskName, contextName,
                            System.currentTimeMillis());

                    if (! camelContext.getStatus().isStarted())
                        camelContext.start();
                    else
                        for (Route route: camelContext.getRoutes()) {
                            Consumer consumer = route.getConsumer();
                            /*
                             * It is safe to call Consumer.start/.stop
                             * unconditionally (assuming the component is
                             * written correctly), but in order to provide
                             * useful logging of these events, we must perform a
                             * status check.
                             */
                            if (consumer instanceof ServiceSupport) {
                                if (! ((ServiceSupport) consumer).getStatus().isStarted()) {
                                    statusHandler.handle(Priority.INFO,
                                            "Starting consumer for route " + route.getId());
                                    consumer.start();
                                }
                            } else
                                consumer.start();
                        }
                } else {
                    for (Route route: camelContext.getRoutes()) {
                        Consumer consumer = route.getConsumer();
                        if (consumer instanceof ServiceSupport) {
                            if (((ServiceSupport) consumer).getStatus().isStarted()) {
                                statusHandler.handle(Priority.INFO,
                                        "Stopping consumer for route " + route.getId());
                                consumer.stop();
                            }
                        } else
                            consumer.stop();
                    }
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.ERROR,
                        "Failed to start/stop route", e);
            }
        }
    }

    public ClusteredContextManager register(CamelContext context) {
        clusteredContextList.add(context);
        return this;
    }

    public int getTimeOutMillis() {
        return timeOutMillis;
    }

    public void setTimeOutMillis(int timeOutMillis) {
        this.timeOutMillis = timeOutMillis;
    }
}
