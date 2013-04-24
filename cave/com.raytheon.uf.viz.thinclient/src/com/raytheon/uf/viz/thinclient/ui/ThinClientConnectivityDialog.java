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
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.comm.ConnectivityManager;
import com.raytheon.uf.viz.core.comm.ConnectivityManager.ConnectivityResult;
import com.raytheon.uf.viz.core.comm.IConnectivityCallback;
import com.raytheon.uf.viz.core.localization.ConnectivityPreferenceDialog;
import com.raytheon.uf.viz.core.localization.LocalizationConstants;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.thinclient.Activator;
import com.raytheon.uf.viz.thinclient.preferences.ThinClientPreferenceConstants;

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

public class ThinClientConnectivityDialog extends ConnectivityPreferenceDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ThinClientConnectivityDialog.class, "CAVE");

    private class ServicesCallback implements IConnectivityCallback {

        @Override
        public void connectionChecked(ConnectivityResult results) {
            servicesGood = results.hasConnectivity;
        }

    }

    private class PypiesCallback implements IConnectivityCallback {

        @Override
        public void connectionChecked(ConnectivityResult results) {
            pypiesGood = results.hasConnectivity;
        }

    }

    private Label servicesLabel;

    private Text servicesText;

    private String services = "";

    private boolean servicesGood = false;

    private IConnectivityCallback servicesCallback = new ServicesCallback();

    private Label pypiesLabel;

    private Text pypiesText;

    private String pypies = "";

    private boolean pypiesGood = false;

    private IConnectivityCallback pypiesCallback = new PypiesCallback();

    private Button useProxyCheck;

    private boolean useProxy = false;

    public ThinClientConnectivityDialog(boolean checkAlertViz) {
        super(checkAlertViz);
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        useProxy = store
                .getBoolean(ThinClientPreferenceConstants.P_USE_PROXIES);
        services = store
                .getString(ThinClientPreferenceConstants.P_SERVICES_PROXY);
        pypies = store.getString(ThinClientPreferenceConstants.P_PYPIES_PROXY);

    }

    @Override
    protected void createTextBoxes(Composite textBoxComp) {
        super.createTextBoxes(textBoxComp);

        Label label = new Label(textBoxComp, SWT.RIGHT);
        label.setText("Use Proxy Servers:");
        GridData gd = new GridData(SWT.RIGHT, SWT.None, true, true);
        gd.widthHint = 150;
        label.setLayoutData(gd);

        useProxyCheck = new Button(textBoxComp, SWT.CHECK | SWT.LEFT);
        useProxyCheck.setSelection(useProxy);
        useProxyCheck.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                updateProxyEnabled();
            }

        });

        servicesLabel = new Label(textBoxComp, SWT.RIGHT);
        servicesLabel.setText("Services Proxy Address:");
        gd = new GridData(SWT.RIGHT, SWT.None, true, true);
        gd.widthHint = 150;
        servicesLabel.setLayoutData(gd);

        servicesText = new Text(textBoxComp, SWT.NONE);
        gd = new GridData(SWT.RIGHT, SWT.None, true, true);
        gd.widthHint = 300;
        servicesText.setLayoutData(gd);
        servicesText.setText(services);
        if (!servicesGood) {
            servicesText.setBackground(textBoxComp.getDisplay().getSystemColor(
                    SWT.COLOR_RED));
        }

        pypiesLabel = new Label(textBoxComp, SWT.RIGHT);
        pypiesLabel.setText("Pypies Proxy Address:");
        gd = new GridData(SWT.RIGHT, SWT.None, true, true);
        gd.widthHint = 150;
        pypiesLabel.setLayoutData(gd);

        pypiesText = new Text(textBoxComp, SWT.NONE);
        gd = new GridData(SWT.RIGHT, SWT.None, true, true);
        gd.widthHint = 300;
        pypiesText.setLayoutData(gd);
        pypiesText.setText(pypies);
        if (!pypiesGood) {
            pypiesText.setBackground(textBoxComp.getDisplay().getSystemColor(
                    SWT.COLOR_RED));
        }
        updateProxyEnabled();
    }

    @Override
    protected void applySettings() {
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        store.setValue(ThinClientPreferenceConstants.P_USE_PROXIES, useProxy);
        if (useProxy) {
            store.setValue(ThinClientPreferenceConstants.P_SERVICES_PROXY,
                    services);
            store.setValue(ThinClientPreferenceConstants.P_PYPIES_PROXY, pypies);
            // Set the site and alertViz server for super.
            LocalizationManager.getInstance().setCurrentSite(getSite());
            if (getAlertVizServer() != null) {
                LocalizationManager
                        .getInstance()
                        .getLocalizationStore()
                        .setValue(LocalizationConstants.P_ALERT_SERVER,
                                getAlertVizServer());
            }

            try {
                ((IPersistentPreferenceStore) store).save();
                ((IPersistentPreferenceStore) LocalizationManager.getInstance()
                        .getLocalizationStore()).save();
            } catch (IOException e) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Unable to persist localization preference store", e);
            }
        } else {
            super.applySettings();
        }

    }

    @Override
    public boolean validate() {
        boolean superValid = super.validate();
        if (!useProxy) {
            return superValid;
        }
        if (servicesText != null && !servicesText.isDisposed()
                && servicesText.isEnabled()) {
            String services = servicesText.getText().trim();
            if (!servicesGood || !this.services.equals(services)) {
                this.services = services;
                validateServices();
                servicesText.setBackground(getTextColor(servicesGood));
            }
        } else {
            validateServices();
        }
        if (pypiesText != null && !pypiesText.isDisposed()
                && pypiesText.isEnabled()) {
            String pypies = pypiesText.getText().trim();
            if (!pypiesGood || !this.pypies.equals(pypies)) {
                this.pypies = pypies;
                validatePypies();
                pypiesText.setBackground(getTextColor(pypiesGood));
            }
        } else {
            validatePypies();
        }
        return servicesGood && pypiesGood && isSiteGood() && isAlertVizGood();
    }

    private void validateServices() {
        ConnectivityManager.checkHttpServer(services, servicesCallback);
    }

    private void validatePypies() {
        ConnectivityManager.checkHttpServer(pypies, pypiesCallback);
    }

    private void updateProxyEnabled() {
        useProxy = useProxyCheck.getSelection();
        servicesLabel.setEnabled(useProxy);
        servicesText.setEnabled(useProxy);
        pypiesLabel.setEnabled(useProxy);
        pypiesText.setEnabled(useProxy);
        setLocalizationEnabled(!useProxy);
        validate();
    }

}
