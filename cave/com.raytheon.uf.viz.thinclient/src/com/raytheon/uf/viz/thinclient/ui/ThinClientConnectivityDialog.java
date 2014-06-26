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
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
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
import com.raytheon.uf.viz.core.localization.TextOrCombo;
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
 * 
 * 
 * 
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

    private Button useProxyCheck;

    private boolean useProxy = false;

    private Button disableJmsCheck;

    private Button alwaysPromptCheck;

    private boolean disableJms = false;

    private boolean jmsGood = false;

    private boolean alwaysPrompt;

    private Label jmsErrorLabel;

    private IConnectivityCallback jmsCallback = new JmsCallback();

    private TextOrCombo proxySrv;

    private String proxyAddress;

    public ThinClientConnectivityDialog(boolean checkAlertViz) {
        super(checkAlertViz, "Thin Client Connectivity Preferences");
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        useProxy = store
                .getBoolean(ThinClientPreferenceConstants.P_USE_PROXIES);
        disableJms = store
                .getBoolean(ThinClientPreferenceConstants.P_DISABLE_JMS);
        proxyAddress = store
                .getString(ThinClientPreferenceConstants.P_PROXY_ADDRESS);
    }

    @Override
    protected void createTextBoxes(Composite textBoxComp) {
        super.createTextBoxes(textBoxComp);

        Label label = new Label(textBoxComp, SWT.RIGHT);
        label.setText("Use Proxy Server:");
        GridData gd = new GridData(SWT.RIGHT, SWT.CENTER, false, true);
        gd.horizontalIndent = 20;
        label.setLayoutData(gd);

        Composite proxyComp = new Composite(textBoxComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        proxyComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        proxyComp.setLayoutData(gd);

        useProxyCheck = new Button(proxyComp, SWT.CHECK | SWT.LEFT);
        useProxyCheck.setSelection(useProxy);
        useProxyCheck.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateProxyEnabled();
            }
        });

        IPreferenceStore thinPrefs = Activator.getDefault()
                .getPreferenceStore();
        String[] proxyOptions = ServerRemembrance.getServerOptions(thinPrefs,
                ThinClientPreferenceConstants.P_PROXY_SERVER_OPTIONS);
        proxySrv = new TextOrCombo(proxyComp, SWT.BORDER, proxyOptions);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        proxySrv.widget.setLayoutData(gd);
        proxySrv.setText(proxyAddress == null ? "" : proxyAddress);
        proxySrv.widget.setBackground(getTextColor(servicesGood && pypiesGood));
        proxySrv.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                // user clicked an option
                validate();
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                // user hit Enter
                performOk();
            }
        });

        new Label(textBoxComp, SWT.NONE);

        Composite jmsComp = new Composite(textBoxComp, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        jmsComp.setLayout(gl);

        disableJmsCheck = new Button(jmsComp, SWT.CHECK | SWT.LEFT);
        disableJmsCheck.setSelection(disableJms);
        disableJmsCheck.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                disableJms = disableJmsCheck.getSelection();
                validate();
            }
        });
        disableJmsCheck.setText("Disable JMS");
        jmsErrorLabel = new Label(jmsComp, SWT.LEFT);
        jmsErrorLabel.setText("Error connecting to JMS");
        jmsErrorLabel.setForeground(display.getSystemColor(SWT.COLOR_RED));
        jmsErrorLabel.setVisible(true);
        new Label(textBoxComp, SWT.NONE);

        alwaysPrompt = LocalizationManager
                .getInstance()
                .getLocalizationStore()
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

        updateProxyEnabled();
    }

    @Override
    protected void applySettings() {
        IPersistentPreferenceStore thinStore = Activator.getDefault()
                .getPreferenceStore();
        thinStore.setValue(ThinClientPreferenceConstants.P_DISABLE_JMS,
                disableJms);
        thinStore.setValue(ThinClientPreferenceConstants.P_USE_PROXIES,
                useProxy);

        IPersistentPreferenceStore localStore = LocalizationManager
                .getInstance().getLocalizationStore();
        localStore.setValue(
                LocalizationConstants.P_LOCALIZATION_PROMPT_ON_STARTUP,
                alwaysPrompt);

        if (useProxy) {
            thinStore.setValue(ThinClientPreferenceConstants.P_PROXY_ADDRESS,
                    proxyAddress);
            String proxyServerOptions = ServerRemembrance.formatServerOptions(
                    proxyAddress, thinStore,
                    ThinClientPreferenceConstants.P_PROXY_SERVER_OPTIONS);
            thinStore.setValue(
                    ThinClientPreferenceConstants.P_PROXY_SERVER_OPTIONS,
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
        } else {
            super.applySettings();
        }

        /*
         * Have to store the thin client preferences either way to remember the
         * JMS and proxy checkboxes correctly
         */
        try {
            thinStore.save();
        } catch (IOException e) {
            statusHandler
                    .handle(Priority.SIGNIFICANT,
                            "Unable to persist thinclient localization preference store",
                            e);
        }

    }

    @Override
    public boolean validate() {
        if (!useProxy) {
            boolean superResult = super.validate();
            validateJms(superResult);
            return superResult && jmsGood;
        }

        status = null;
        details = null;

        // validate proxy
        if (proxySrv != null && !proxySrv.widget.isDisposed()
                && proxySrv.widget.isEnabled()) {
            proxyAddress = proxySrv.getText();
        }
        if (proxyAddress != null && proxyAddress.length() > 0) {
            validateServices();
            validatePypies();
        } else {
            status = "Please enter a thin client proxy server address";
        }
        if (proxySrv != null && !proxySrv.widget.isDisposed()) {
            proxySrv.widget.setBackground(getTextColor(servicesGood
                    && pypiesGood));
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

    private void updateProxyEnabled() {
        useProxy = useProxyCheck.getSelection();
        proxySrv.widget.setEnabled(useProxy);
        super.setLocalizationEnabled(!useProxy);
        if (useProxy) {
            if (localizationSrv != null && !localizationSrv.widget.isDisposed()) {
                localizationSrv.widget.setBackground(getTextColor(true));
            }
        } else {
            if (proxySrv != null && !proxySrv.widget.isDisposed()) {
                proxySrv.widget.setBackground(getTextColor(true));
            }
        }
        validate();
    }

    /**
     * Validates that a connection to JMS works.
     * 
     * @param hasEdexConnection
     *            if we've successfully connected to edex
     */
    private void validateJms(boolean hasEdexConnection) {
        // only check Jms if it's enabled and we can connect to the services
        if (!disableJms) {
            if (hasEdexConnection) {
                try {
                    String server = useProxy ? ThinClientUriUtil
                            .getServicesAddress(proxyAddress)
                            : getLocalization();
                    GetServersResponse response = ConnectivityManager
                            .checkLocalizationServer(server, false);
                    ConnectivityManager.checkJmsServer(
                            response.getJmsConnectionString(), jmsCallback);
                } catch (VizException e) {
                    if (status == null) {
                        status = "Error connecting to JMS";
                    }
                    appendDetails(buildDetails(new ConnectivityResult(false,
                            null, e)));
                    jmsGood = false;
                }
            } else {
                // JMS can't be good if we're not connected to edex cause
                // then we don't even know where to connect to
                jmsGood = false;
            }
        }
        jmsGood = (jmsGood || disableJms);
        if (jmsErrorLabel != null && !jmsErrorLabel.isDisposed()) {
            jmsErrorLabel.setVisible(!jmsGood);
        }

    }

}
