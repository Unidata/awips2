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
package com.raytheon.uf.viz.collaboration.comm;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

import com.raytheon.uf.common.comm.NetworkStatistics;

/**
 * Activator for Collaboration Communication
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class Activator implements BundleActivator {

    public static final String PEER_TO_PEER = "PeerToPeerMsg";

    public static final String VENUE = "VenueMsg";

    private static Activator plugin;

    private BundleContext context;

    private NetworkStatistics networkStats = new NetworkStatistics();

    public BundleContext getContext() {
        return context;
    }

    /**
     * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
     */
    public void start(BundleContext bundleContext) throws Exception {
        this.context = bundleContext;
        plugin = this;
    }

    /**
     * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
     */
    public void stop(BundleContext bundleContext) throws Exception {
        plugin = null;
        context = null;
    }

    public NetworkStatistics getNetworkStats() {
        return networkStats;
    }

    public static Activator getDefault() {
        return plugin;
    }

}
