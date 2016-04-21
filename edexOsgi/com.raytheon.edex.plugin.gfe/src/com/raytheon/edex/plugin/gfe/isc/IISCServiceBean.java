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

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;

/**
 * An interface for beans that provide ISC services for
 * {@code IscServiceProvider}. This bean, once registered with
 * {@code IscServiceProvider} will run on only one of the available EDEX cluster
 * nodes. The {@code IscServiceProvider} ensures that dependent services all run
 * together on the same node.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2015            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public interface IISCServiceBean {

    /**
     * Called upon activation of a new GFE site. Will only trigger on the
     * cluster node currently hosting ISC services and if
     * {@code gfeConfig.requestISC()} returns {@code true}.
     * 
     * @param siteID
     *            Site identifier to activate ISC services for.
     * @param gfeConfig
     *            Configuration data for this site.
     */
    void activateSite(final String siteID, final IFPServerConfig gfeConfig);

    /**
     * Called upon deactivation of a GFE site. Will only trigger on the cluster
     * node currently hosting ISC services.
     * 
     * @param siteID
     *            Site identifier to deactivate ISC services for.
     */
    void deactivateSite(final String siteID);

    /**
     * The startup method for this bean. Should be used to initialize heavy
     * objects that shouldn't be allowed to run on both cluster nodes to
     * conserve system resources.
     */
    void startup();

    /**
     * Called to begin the shutdown process for this bean. Recommend using this
     * method to cancel any asynchronous tasks this bean may be running.
     * <p>
     * Note that this method does not require that the startup method has
     * previously been called so ensure this code does not rely on any behaviors
     * of that method.
     */
    void preShutdown();

    /**
     * Called after {@code IscServiceProvider} has completed its shutdown. One
     * last chance to cleanup any resources in use by this bean.
     * <p>
     * Note that this method does not require that the startup method has
     * previously been called so ensure this code does not rely on any behaviors
     * of that method.
     */
    void postShutdown();
}
