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
package com.raytheon.uf.viz.thinclient.ui;

import java.io.IOException;

import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;

import com.raytheon.uf.common.localization.msgs.GetServersResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.comm.ConnectivityManager;
import com.raytheon.uf.viz.core.comm.ConnectivityManager.ConnectivityResult;
import com.raytheon.uf.viz.core.comm.IConnectivityCallback;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.ConnectivityPreferenceDialog;
import com.raytheon.uf.viz.core.localization.LocalizationConstants;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.localization.ServerRemembrance;
import com.raytheon.uf.viz.thinclient.Activator;
import com.raytheon.uf.viz.thinclient.ThinClientUriUtil;
import com.raytheon.uf.viz.thinclient.preferences.ThinClientPreferenceConstants;

/**
 * Connectivity dialog for launching thinclient or thinalertviz. Contains extra
 * options not available when connecting with a normal CAVE.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 23, 2011           bsteffen    Initial creation
 * Aug 02, 2013  2202     bsteffen    Add edex specific connectivity checking.
 * Feb 04, 2014  2704     njensen     Refactored
 * Feb 17, 2014  2704     njensen     Added checks for alertviz connectivity
 * Feb 20, 2014  2704     njensen     Fix issues where settings are valid
 *                                    but dialog doesn't realize it
 * Jun 03, 2014  3217     bsteffen    Add option to always open startup dialog.
 * Jun 24, 2014  3236     njensen     Add ability to remember multiple servers
 * Oct 08, 2015  4891     njensen     Added tooltip to useProxyCheck
 * Feb 08, 2016  5281     tjensen     Reworked interface to simply options
 * Feb 15, 2016  5281     tjensen     Added check for null in validate method
 * Feb 18, 2016  5281     tjensen     Fix issue when no JMS available.
 * Feb 19, 2016  5281     tjensen     Fix validation when JMS not available.
 * Mar 01, 2016  5281     tjensen     Update dataRefreshMethod when automatically 
 *                                     enabling/disabling push
 * Mar 15, 2016  5281     tjensen     Fix validation prior to prompt
 * Apr 06, 2016  5281     tjensen     Fix validation of JMS when prompt disabled 
 *                                     and using poll method.
 * Jun 24, 2016           mjames      Simplify dialog for UCAR release.
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class ThinClientConnectivityDialog extends ConnectivityPreferenceDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ThinClientConnectivityDialog.class, "CAVE");

    private String dataRefreshMethod;

    private String proxyAddress;

    public ThinClientConnectivityDialog(boolean checkAlertViz) {
        super(checkAlertViz, "TC Connectivity Preferences");
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        dataRefreshMethod = store
                .getString(ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD);
        proxyAddress = store
                .getString(ThinClientPreferenceConstants.P_PROXY_ADDRESS);
    }

}
