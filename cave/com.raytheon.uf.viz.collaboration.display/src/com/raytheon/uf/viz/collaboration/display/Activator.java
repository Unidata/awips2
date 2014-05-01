package com.raytheon.uf.viz.collaboration.display;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.jobs.StatsJob;

public class Activator implements BundleActivator {

    public static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(Activator.class);

    private static BundleContext context;

    private StatsJob statsJob = new StatsJob(
            "Collaboration Network Statistics",
            com.raytheon.uf.viz.collaboration.comm.Activator.getDefault()
                    .getNetworkStats());

    static BundleContext getContext() {
        return context;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext
     * )
     */
    public void start(BundleContext bundleContext) throws Exception {
        Activator.context = bundleContext;
        statsJob.schedule();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
     */
    public void stop(BundleContext bundleContext) throws Exception {
        Activator.context = null;
        statsJob.shutdown();
    }

}
