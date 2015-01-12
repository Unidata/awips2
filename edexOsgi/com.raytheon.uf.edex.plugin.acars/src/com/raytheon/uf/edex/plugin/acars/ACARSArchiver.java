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
package com.raytheon.uf.edex.plugin.acars;

import java.lang.management.ManagementFactory;
import java.util.concurrent.atomic.AtomicInteger;

import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.StandardMBean;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * ACARS Archiver.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer    Description
 * ------------  ---------- ----------- --------------------------
 * Mar 27, 2014  2811       skorolev    Updated logger.
 * 
 * </pre>
 * 
 * @author
 * @version 1.0
 */
public class ACARSArchiver implements ACARSArchiverMBean {

    private IUFStatusHandler logger = UFStatus.getHandler(ACARSDecoder.class);

    private static AtomicInteger serviceInstanceId = new AtomicInteger();

    private final String domain;

    private final String serviceName = "Archive.ACARS";

    private ObjectName serviceJmxId = null;

    private boolean serviceRegistered = false;

    /**
     * 
     * @param domain
     */
    public ACARSArchiver(String domain) {
        this.domain = domain;
        register("acarsArchive");
    }

    /**
     * @param headers
     */
    public void checkForArchive(Headers headers) {
        String traceId = serviceName;
        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }
        logger.info(traceId);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.plugin.acars.ACARSArchiverMBean#execute(java.lang
     * .String)
     */
    public void execute(String command) {

        logger.info("Archive command = " + command);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.plugin.acars.ACARSArchiverMBean#getServiceName()
     */
    @Override
    public String getServiceName() {
        return serviceName;
    }

    /**
     * Register this service with the JMX management.
     * 
     * @param name
     */
    protected void register(String name) {
        if (serviceRegistered) {
            return;
        }

        // Get the MBean server for the platform
        MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
        try {
            serviceJmxId = new ObjectName(domain + ":type=server,name=" + name
                    + "." + serviceInstanceId.incrementAndGet());
            StandardMBean smbean = new StandardMBean(this,
                    ACARSArchiverMBean.class);
            mbs.registerMBean(smbean, serviceJmxId);
            serviceRegistered = true;
        } catch (Exception e) {
            logger.error("Instance failed to register with JMX server", e);

            serviceRegistered = false;
        }
    }

    /**
     * Unregister this service from the JMX server. This should be called prior
     * to shutting down the service.
     * 
     * @param name
     */
    protected void unRegister(String name) {
        if (!serviceRegistered) {
            return;
        }
        // Get the MBean server for the platform
        MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
        try {
            if (mbs.isRegistered(serviceJmxId)) {
                mbs.unregisterMBean(serviceJmxId);
            }

            serviceRegistered = false;
            logger.info("JMX Monitoring for " + serviceName + " stopped");
        } catch (Exception e) {
            logger.error("register(2) failed to register with JMX server", e);
            serviceRegistered = false;
            // jmxModeOn = false;
        }
    }
}
