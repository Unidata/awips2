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
package com.raytheon.uf.viz.thinclient.localization;

import java.util.Map;

import org.apache.commons.collections.map.DefaultedMap;
import org.eclipse.jface.preference.IPreferenceStore;

import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.localization.msgs.GetServersResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.VizServers;
import com.raytheon.uf.viz.core.comm.ConnectivityManager;
import com.raytheon.uf.viz.core.exception.VizCommunicationException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationInitializer;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.requests.ServerRequestException;
import com.raytheon.uf.viz.thinclient.Activator;
import com.raytheon.uf.viz.thinclient.ThinClientUriUtil;
import com.raytheon.uf.viz.thinclient.preferences.ThinClientPreferenceConstants;
import com.raytheon.uf.viz.thinclient.ui.ThinClientConnectivityDialog;

/**
 * Initializer that does work of checking localization settings and configuring
 * the servers to use based on the preferences.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer    Description
 * ------------- ---------- ----------- --------------------------
 * Nov 23, 2011             bsteffen    Initial creation
 * Dec 06, 2012  1396       njensen     Added setting VizServers
 * Jan 14, 2013  1469       bkowal      Removed setting the hdf5 data directory
 * Aug 02, 2013  2202       bsteffen    Add edex specific connectivity checking.
 * Aug 27, 2013  2295       bkowal      The entire jms connection string is now
 *                                      nprovided by EDEX.
 * Feb 04, 2014  2704       njensen     Single proxy address/preference
 * May 19, 2014  3164       bsteffen    Disable request compression if it
 *                                      doesn't work.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class ThinClientLocalizationInitializer extends LocalizationInitializer {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ThinClientLocalizationInitializer.class);

    /**
     * @param promptUI
     * @param checkAlertViz
     */
    public ThinClientLocalizationInitializer(boolean promptUI,
            boolean checkAlertViz) {
        super(promptUI, checkAlertViz);
    }

    @Override
    protected void setupServers() throws VizException {
        HttpClient.getInstance().setGzipResponseHandling(true);
        if (promptUI) {
            ThinClientConnectivityDialog dlg = new ThinClientConnectivityDialog(
                    checkAlertviz);
            if (dlg.open() == true) {
                System.exit(0);
            }
        }

        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        boolean disableJMS = store
                .getBoolean(ThinClientPreferenceConstants.P_DISABLE_JMS);

        if (store.getBoolean(ThinClientPreferenceConstants.P_USE_PROXIES)) {
            String proxyAddr = store
                    .getString(ThinClientPreferenceConstants.P_PROXY_ADDRESS);
            String servicesProxy = ThinClientUriUtil
                    .getServicesAddress(proxyAddr);

            boolean compressRequests = store
                    .getBoolean(ThinClientPreferenceConstants.P_ENABLE_REQUEST_COMPRESSION);
            if (compressRequests) {
                HttpClient.getInstance().setCompressRequests(compressRequests);
                try {
                    ConnectivityManager.checkLocalizationServer(servicesProxy,
                            true);
                } catch (ServerRequestException | VizCommunicationException e) {
                    HttpClient.getInstance().setCompressRequests(false);
                    statusHandler
                            .error("Server ("
                                    + servicesProxy
                                    + ") does not support compressed requests, this feature has been disabled and some operations may be slower.",
                                    e);
                }
            }
            LocalizationManager.getInstance().setCurrentServer(servicesProxy,
                    false);
            VizApp.setHttpServer(servicesProxy);

            if (!disableJMS) {
                GetServersResponse resp = ConnectivityManager
                        .checkLocalizationServer(servicesProxy, false);
                if (!disableJMS) {
                    VizApp.setJmsConnectionString(resp.getJmsConnectionString());
                }
            }

            String pypiesProxy = ThinClientUriUtil.getPypiesAddress(proxyAddr);
            VizApp.setPypiesServer(pypiesProxy);

            // use the proxy for all servers in VizServers
            @SuppressWarnings("unchecked")
            Map<String, String> serversMap = new DefaultedMap(servicesProxy);
            VizServers.getInstance().setServerLocations(serversMap);
        } else {
            processGetServers();
            if (disableJMS) {
                VizApp.setJmsConnectionString(null);
            }
        }
    }
}
