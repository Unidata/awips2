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

import org.eclipse.jface.preference.IPreferenceStore;

import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.localization.msgs.GetServersRequest;
import com.raytheon.uf.common.localization.msgs.GetServersResponse;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationInitializer;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.thinclient.Activator;
import com.raytheon.uf.viz.thinclient.preferences.ThinClientPreferenceConstants;
import com.raytheon.uf.viz.thinclient.ui.ThinClientConnectivityDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 23, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class ThinClientLocalizationInitializer extends LocalizationInitializer {

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
            LocalizationManager
                    .getInstance()
                    .setCurrentServer(
                            store.getString(ThinClientPreferenceConstants.P_SERVICES_PROXY));
            String dataDir = VizApp.getServerDataDir();
            if (dataDir == null || dataDir.isEmpty()) {
                dataDir = store
                        .getString(ThinClientPreferenceConstants.P_SERVER_DATA_DIR);
                VizApp.setServerDataDir(dataDir);
            }
            if (!disableJMS || dataDir == null || dataDir.isEmpty()) {
                GetServersRequest req = new GetServersRequest();
                GetServersResponse resp = (GetServersResponse) ThriftClient
                        .sendLocalizationRequest(req);
                VizApp.setServerDataDir(resp.getServerDataDir());
                if (!disableJMS) {
                    VizApp.setJmsServer(resp.getJmsServer());
                }
            }
            VizApp.setHttpServer(store
                    .getString(ThinClientPreferenceConstants.P_SERVICES_PROXY));
            VizApp.setPypiesServer(store
                    .getString(ThinClientPreferenceConstants.P_PYPIES_PROXY));
            boolean compressRequests = store
                    .getBoolean(ThinClientPreferenceConstants.P_ENABLE_REQUEST_COMPRESSION);
            HttpClient.getInstance().setCompressRequests(compressRequests);

        } else {
            GetServersRequest req = new GetServersRequest();
            GetServersResponse resp = (GetServersResponse) ThriftClient
                    .sendLocalizationRequest(req);
            VizApp.setHttpServer(resp.getHttpServer());
            VizApp.setPypiesServer(resp.getPypiesServer());
            VizApp.setServerDataDir(resp.getServerDataDir());
            if (!disableJMS) {
                VizApp.setJmsServer(resp.getJmsServer());
            }
        }
        store.setValue(ThinClientPreferenceConstants.P_SERVER_DATA_DIR,
                VizApp.getServerDataDir());

    }
}
