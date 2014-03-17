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
package com.raytheon.uf.viz.core.localization;

import java.io.File;

import com.raytheon.uf.common.localization.FileLocker;
import com.raytheon.uf.common.localization.FileLocker.Type;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.msgs.GetServersResponse;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.VizServers;
import com.raytheon.uf.viz.core.comm.ConnectivityManager;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Class that does work of checking localization server, popping up dialog if
 * unable to connect and gets the http/jms server from localization server,
 * PlatformUI.createDisplay should have already been called before this code
 * executes
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 05, 2009            mschenke    Initial creation
 * Sep 12, 2012 1167       djohnson    Add datadelivery servers.
 * Jan 14, 2013 1469       bkowal      Removed the hdf5 data directory.
 * Aug 02, 2013 2202       bsteffen    Add edex specific connectivity checking.
 * Aug 27, 2013 2295       bkowal      The entire jms connection string is now
 *                                     provided by EDEX.
 * Feb 04, 2014 2704       njensen     Pass connectivity dialog title
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class LocalizationInitializer {

    protected boolean checkAlertviz;

    protected boolean promptUI;

    public LocalizationInitializer(boolean promptUI, boolean checkAlertViz) {
        this.promptUI = promptUI;
        this.checkAlertviz = checkAlertViz;
    }

    public void run() throws Exception {
        setupServers();
        // Setup CAVE_CONFIG base files
        long t0 = System.currentTimeMillis();
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext baseContext = pm.getContext(
                LocalizationType.CAVE_CONFIG, LocalizationLevel.BASE);
        String filePath = "config.xml";
        BundleScanner scanner = new BundleScanner(filePath);
        for (String bundle : scanner.getContributingBundles()) {
            File copyFrom = scanner.searchInBundle(bundle, null);
            if (copyFrom != null) {
                String searchPath = bundle + IPathManager.SEPARATOR + filePath;
                File copyTo = pm.getFile(baseContext, searchPath);
                if (copyTo.exists() == false
                        || copyFrom.lastModified() != copyTo.lastModified()) {
                    FileLocker.lock(this, copyTo, Type.WRITE);
                    try {
                        if (copyTo.exists()) {
                            copyTo.setWritable(true);
                            copyTo.delete();
                        }
                        FileUtil.copyFile(copyFrom, copyTo);
                        copyTo.setLastModified(copyFrom.lastModified());
                        copyTo.setReadOnly();
                    } finally {
                        FileLocker.unlock(this, copyTo);
                    }
                }
            }
        }
        System.out.println("Time to setup CAVE_CONFIG = "
                + (System.currentTimeMillis() - t0));
    }

    protected void setupServers() throws VizException {
        if (promptUI) {
            ConnectivityPreferenceDialog dlg = new ConnectivityPreferenceDialog(
                    checkAlertviz, "Connectivity Preferences");
            if (dlg.open() == true) {
                System.exit(0);
            }
        }

        processGetServers();
    }

    /**
     * Sends a GetServersRequest and sets the references that hold the various
     * server addresses
     * 
     * @throws VizException
     */
    protected final void processGetServers() throws VizException {
        GetServersResponse resp = ConnectivityManager.checkLocalizationServer(
                LocalizationManager.getInstance().getLocalizationServer(),
                false);
        VizApp.setHttpServer(resp.getHttpServer());
        VizApp.setJmsConnectionString(resp.getJmsConnectionString());
        VizApp.setPypiesServer(resp.getPypiesServer());
        VizServers.getInstance().setServerLocations(resp.getServerLocations());
    }
}
