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
package com.raytheon.edex.plugin.gfe.isc;

import java.util.ArrayList;
import java.util.Collection;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.IContextStateProcessor;

/**
 * An {@code IContextStateProcessor} implementation for ISC beans. Ensures that
 * all ISC services are running on the same cluster node together.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2015  #4128     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class IscServiceProvider implements IContextStateProcessor {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(IscServiceProvider.class);

    private volatile boolean activeInstance;

    private final Collection<IISCServiceBean> iscBeans;

    private final ExecutorService jobThreads;

    private final IRTManager irtManager;

    /**
     * Default constructor.
     */
    public IscServiceProvider() {
        this.iscBeans = new ArrayList<>();
        this.activeInstance = false;
        this.jobThreads = Executors.newCachedThreadPool();
        this.irtManager = new IRTManager();
    }

    /**
     * Register a new {@code IISCServiceBean} instance with this class. Beans
     * can only be added and never removed.
     * 
     * @param iscServiceBean
     *            The bean to register.
     * @return The bean that was registered.
     */
    public IISCServiceBean addISCService(final IISCServiceBean iscServiceBean) {
        iscBeans.add(iscServiceBean);
        return iscServiceBean;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.core.IContextStateProcessor#preStart()
     */
    @Override
    public void preStart() {
        activeInstance = true;

        irtManager.preStart();
        for (IISCServiceBean iscBean : iscBeans) {
            iscBean.startup();
        }

        for (IFPServer ifpServer : IFPServer.getActiveServers()) {
            activateSite(ifpServer.getSiteId(), ifpServer.getConfig());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.core.IContextStateProcessor#postStart()
     */
    @Override
    public void postStart() {
        // no-op
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.core.IContextStateProcessor#preStop()
     */
    @Override
    public void preStop() {
        irtManager.preStop();
        for (IISCServiceBean iscBean : iscBeans) {
            iscBean.preShutdown();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.core.IContextStateProcessor#postStop()
     */
    @Override
    public void postStop() {
        irtManager.postStop();
        for (IISCServiceBean iscBean : iscBeans) {
            iscBean.postShutdown();
        }
    }

    /**
     * Dummy trigger method for the timer in the camel context this bean
     * monitors. Ensures this bean properly fails over between cluster members
     * as needed.
     */
    public void activateInstance() {
        activeInstance = true;
    }

    /**
     * Activate the ISC services for the given site. All registered beans will
     * have their {@code activateSite} method called if this instance is running
     * on the designated ISC cluster node.
     * 
     * @param siteID
     *            Site identifier to activate ISC services for.
     * @param config
     *            Configuration data for this site.
     */
    public void activateSite(final String siteID, final IFPServerConfig config) {
        statusHandler.info("Checking ISC configuration for site " + siteID);

        if (config.requestISC()) {
            if (activeInstance) {
                statusHandler.info("Enabling ISC for site " + siteID);

                irtManager.activateSite(siteID, config);

                for (final IISCServiceBean bean : iscBeans) {
                    Runnable activationJob = new Runnable() {

                        @Override
                        public void run() {
                            try {
                                bean.activateSite(siteID, config);
                            } catch (Throwable t) {
                                statusHandler.error(
                                        "Unhandled RuntimeException thrown while activating service "
                                                + bean.getClass()
                                                + " for site " + siteID, t);
                            }
                        }
                    };

                    jobThreads.submit(activationJob);
                }

            } else {
                statusHandler
                        .info("ISC Enabled but will use another EDEX instance");
            }
        } else {
            statusHandler.info("ISC is not enabled.");
        }
    }

    /**
     * Deactivates the ISC services for the given site. All registered beans
     * will have their {@code deactivateSite} method called if this instance is
     * running on the designated ISC cluster node.
     * 
     * @param siteID
     *            Site identifier to deactivate ISC services for.
     */
    public void deactivateSite(final String siteID) {
        if (activeInstance) {
            irtManager.deactivateSite(siteID);

            for (final IISCServiceBean bean : iscBeans) {
                Runnable deactivationJob = new Runnable() {

                    @Override
                    public void run() {
                        try {
                            bean.deactivateSite(siteID);
                        } catch (Throwable t) {
                            statusHandler.error(
                                    "Unhandled RuntimeException thrown while deactivating service "
                                            + bean.getClass() + " for site "
                                            + siteID, t);
                        }
                    }
                };

                jobThreads.submit(deactivationJob);
            }
        }
    }
}
