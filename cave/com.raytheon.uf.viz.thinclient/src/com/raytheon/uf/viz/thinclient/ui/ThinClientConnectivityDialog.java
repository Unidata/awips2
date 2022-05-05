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
 * </pre>
 *
 * @author bsteffen
 * @version 1.0
 */

public class ThinClientConnectivityDialog extends ConnectivityPreferenceDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ThinClientConnectivityDialog.class, "CAVE");

    private class ServicesCallback implements IConnectivityCallback {

        @Override
        public void connectionChecked(ConnectivityResult results) {
            servicesGood = results.hasConnectivity;
            appendDetails(buildDetails(results));
            if (!results.hasConnectivity && status == null) {
                status = buildErrorMessage(results);
            }
        }
    }

    private class PypiesCallback implements IConnectivityCallback {

        @Override
        public void connectionChecked(ConnectivityResult results) {
            pypiesGood = results.hasConnectivity;
            appendDetails(buildDetails(results));
            if (!results.hasConnectivity && status == null) {
                status = buildErrorMessage(results);
            }
        }
    }

    private class JmsCallback implements IConnectivityCallback {

        @Override
        public void connectionChecked(ConnectivityResult results) {
            jmsGood = results.hasConnectivity;
            appendDetails(buildDetails(results));
            if (!results.hasConnectivity && status == null) {
                status = buildErrorMessage(results);
            }
        }

    }

    private boolean servicesGood = false;

    private IConnectivityCallback servicesCallback = new ServicesCallback();

    private boolean pypiesGood = false;

    private IConnectivityCallback pypiesCallback = new PypiesCallback();

    private boolean useProxy = true;

    private Button alwaysPromptCheck;

    private String dataRefreshMethod;

    private boolean jmsGood = false;

    private boolean alwaysPrompt;

    private IConnectivityCallback jmsCallback = new JmsCallback();

    private String proxyAddress;

    private Button autoPullBtn;

    private Button timedPollBtn;

    private static final String dataRefreshTooltip = "Automatic Push: Data pushed as soon as available\n"
            + "Timed Poll: Poll for new data at scheduled intervals";

    private static final String unableConnectJMS = "\n\nUnable to connect to JMS: Automatic Push disabled";

    private Group dataRefreshGroup;

    public ThinClientConnectivityDialog(boolean checkAlertViz) {
        super(checkAlertViz, "Thin Client Connectivity Preferences");
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        dataRefreshMethod = store
                .getString(ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD);
        proxyAddress = store
                .getString(ThinClientPreferenceConstants.P_PROXY_ADDRESS);
    }

    @Override
    protected String[] getServerOptions() {
        IPreferenceStore thinPrefs = Activator.getDefault()
                .getPreferenceStore();
        return ServerRemembrance.getServerOptions(thinPrefs,
                ThinClientPreferenceConstants.P_PROXY_SERVER_OPTIONS);
    }

    @Override
    protected void createTextBoxes(Composite textBoxComp) {
        super.createTextBoxes(textBoxComp);

        // Reuse the localization stuff for proxy
        localizationLabel.setText("Proxy Server:");
        localizationSrv.setText(proxyAddress == null ? "" : proxyAddress);

        new Label(textBoxComp, SWT.NONE);
        Composite drComp = new Composite(textBoxComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        drComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        drComp.setLayoutData(gd);

        dataRefreshGroup = new Group(drComp, SWT.BORDER_SOLID);
        dataRefreshGroup.setText("Data Refresh:");
        gl = new GridLayout(2, true);
        gd = new GridData(
                GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL);
        dataRefreshGroup.setLayout(gl);
        dataRefreshGroup.setLayoutData(gd);
        autoPullBtn = new Button(dataRefreshGroup, SWT.RADIO);
        autoPullBtn.setText("Automatic Push");
        if (ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD_PUSH
                .equals(dataRefreshMethod)) {
            autoPullBtn.setSelection(true);
        }
        timedPollBtn = new Button(dataRefreshGroup, SWT.RADIO);
        timedPollBtn.setText("Timed Poll");
        if (ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD_POLL
                .equals(dataRefreshMethod)) {
            timedPollBtn.setSelection(true);
        }
        timedPollBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (timedPollBtn.getSelection()) {
                    dataRefreshMethod = ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD_POLL;
                } else {
                    dataRefreshMethod = ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD_PUSH;
                }
                validate();
            }
        });
        dataRefreshGroup.setToolTipText(dataRefreshTooltip);
        new Label(textBoxComp, SWT.NONE);

        alwaysPrompt = LocalizationManager.getInstance().getLocalizationStore()
                .getBoolean(
                        LocalizationConstants.P_LOCALIZATION_PROMPT_ON_STARTUP);
        alwaysPromptCheck = new Button(textBoxComp, SWT.CHECK | SWT.LEFT);
        alwaysPromptCheck.setText("Prompt for settings on startup");
        alwaysPromptCheck.setSelection(alwaysPrompt);
        alwaysPromptCheck.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                alwaysPrompt = alwaysPromptCheck.getSelection();
            }

        });
    }

    @Override
    protected void applySettings() {
        IPersistentPreferenceStore thinStore = Activator.getDefault()
                .getPreferenceStore();
        thinStore.setValue(ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD,
                dataRefreshMethod);
        thinStore.setValue(ThinClientPreferenceConstants.P_USE_PROXIES,
                useProxy);

        IPersistentPreferenceStore localStore = LocalizationManager
                .getInstance().getLocalizationStore();
        localStore.setValue(
                LocalizationConstants.P_LOCALIZATION_PROMPT_ON_STARTUP,
                alwaysPrompt);

        thinStore.setValue(ThinClientPreferenceConstants.P_PROXY_ADDRESS,
                proxyAddress);
        String proxyServerOptions = ServerRemembrance.formatServerOptions(
                proxyAddress, thinStore,
                ThinClientPreferenceConstants.P_PROXY_SERVER_OPTIONS);
        thinStore.setValue(ThinClientPreferenceConstants.P_PROXY_SERVER_OPTIONS,
                proxyServerOptions);

        if (getAlertVizServer() != null) {
            localStore.setValue(LocalizationConstants.P_ALERT_SERVER,
                    getAlertVizServer());
        }
        LocalizationManager.getInstance().setCurrentSite(getSite());

        try {
            localStore.save();
        } catch (IOException e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Unable to persist localization preference store", e);
        }

        /*
         * Have to store the thin client preferences either way to remember the
         * JMS and proxy checkboxes correctly
         */
        try {
            thinStore.save();
        } catch (IOException e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Unable to persist thinclient localization preference store",
                    e);
        }

    }

    @Override
    public boolean validate() {

        status = null;
        details = null;

        // If we don't have a localizationSrv yet, try to use the stored
        // preference value.
        if (localizationSrv != null) {
            // validate proxy
            proxyAddress = localizationSrv.getText();
        }

        if (proxyAddress != null && proxyAddress.length() > 0) {
            validateServices();
            validatePypies();
        } else {
            servicesGood = false;
            pypiesGood = false;
            status = "Please enter a thin client proxy server address";
        }

        if (localizationSrv != null && !localizationSrv.widget.isDisposed()) {
            localizationSrv.widget
                    .setBackground(getTextColor(servicesGood && pypiesGood));
        }

        validateJms(servicesGood);

        // validate site
        if (siteText != null && !siteText.isDisposed()) {
            super.setSite(siteText.getText());
        }
        super.validateSite();
        if (siteText != null && !siteText.isDisposed()) {
            siteText.setBackground(getTextColor(isSiteGood()));
        }

        // validate alertviz
        // apparently alertvizserver == null means it's alertviz itself
        if (alertVizServer != null) {
            if (alertVizText != null && !alertVizText.isDisposed()) {
                setAlertVizServer(alertVizText.getText());
            }
            super.validateAlertviz();
            if (alertVizText != null && !alertVizText.isDisposed()) {
                alertVizText.setBackground(getTextColor(isAlertVizGood()));
            }
        }

        boolean everythingGood = servicesGood && pypiesGood && isSiteGood()
                && isAlertVizGood() && jmsGood;
        updateStatus(everythingGood, status, details);

        return everythingGood;
    }

    private void validateServices() {
        ConnectivityManager.checkLocalizationServer(
                ThinClientUriUtil.getServicesAddress(proxyAddress),
                servicesCallback);
    }

    private void validatePypies() {
        ConnectivityManager.checkHttpServer(
                ThinClientUriUtil.getPypiesAddress(proxyAddress),
                pypiesCallback);
    }

    /**
     * Validates that a connection to JMS works.
     *
     * @param hasEdexConnection
     *            if we've successfully connected to edex
     */
    private void validateJms(boolean hasEdexConnection) {
        if (hasEdexConnection) {
            try {
                String server = ThinClientUriUtil
                        .getServicesAddress(proxyAddress);
                GetServersResponse response = ConnectivityManager
                        .checkLocalizationServer(server, false);
                ConnectivityManager.checkJmsServer(
                        response.getJmsConnectionInfo(), jmsCallback);
            } catch (VizException e) {
                appendDetails(
                        buildDetails(new ConnectivityResult(false, null, e)));
                jmsGood = false;
            }
        } else {
            // JMS can't be good if we're not connected to edex cause
            // then we don't even know where to connect to
            jmsGood = false;
        }

        // If display items are not disposed, update them as needed.
        if (autoPullBtn != null && timedPollBtn != null
                && dataRefreshGroup != null && !autoPullBtn.isDisposed()
                && !timedPollBtn.isDisposed()
                && !dataRefreshGroup.isDisposed()) {
            if (jmsGood && !autoPullBtn.isEnabled()) {
                autoPullBtn.setEnabled(true);
                autoPullBtn.setSelection(true);
                timedPollBtn.setSelection(false);
                dataRefreshMethod = ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD_PUSH;
                dataRefreshGroup.setToolTipText(dataRefreshTooltip);
            } else if (!jmsGood && autoPullBtn.isEnabled()) {
                autoPullBtn.setEnabled(false);
                autoPullBtn.setSelection(false);
                timedPollBtn.setSelection(true);
                dataRefreshMethod = ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD_POLL;
                dataRefreshGroup
                        .setToolTipText(dataRefreshTooltip + unableConnectJMS);
            }
        }
        if (!jmsGood && dataRefreshMethod.equals(
                ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD_POLL)) {
            // Since JMS is now disabled, we're in a good state.
            jmsGood = true;
        }
    }

}
