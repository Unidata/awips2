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

import java.io.IOException;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.comm.ConnectivityManager;
import com.raytheon.uf.viz.core.comm.ConnectivityManager.ConnectivityResult;
import com.raytheon.uf.viz.core.comm.IConnectivityCallback;

/**
 * Dialog that pops up when unable to connect to localization server
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 5, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ConnectivityPreferenceDialog extends Dialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ConnectivityPreferenceDialog.class, "CAVE");

    private class LocalizationCallback implements IConnectivityCallback {

        @Override
        public void connectionChecked(ConnectivityResult results) {
            localizationGood = results.hasConnectivity;
        }

    }

    private class AlertVizCallback implements IConnectivityCallback {

        @Override
        public void connectionChecked(ConnectivityResult results) {
            alertVizGood = results.hasConnectivity;
        }

    }

    /**
     * Set time dialog shell
     */
    private Shell shell;

    /**
     * Display component
     */
    private Display display;

    private Label localizationLabel;

    private Text localizationText;

    private String localization = "";

    private boolean localizationGood = false;

    private Text alertVizText;

    private String alertVizServer = null;

    private boolean alertVizGood = true;

    private boolean siteGood = false;

    private String site = "";

    private Text siteText;

    private boolean canceled = false;

    private IConnectivityCallback localizationCallback = new LocalizationCallback();

    private IConnectivityCallback alertCallback = new AlertVizCallback();

    /**
     * Title of the dialog.
     */
    private static final String dialogTitle = "Connectivity Preferences";

    public ConnectivityPreferenceDialog(boolean checkAlertViz) {
        this(new Shell(Display.getDefault()), checkAlertViz);
    }

    public ConnectivityPreferenceDialog(Shell parentShell, boolean checkAlertViz) {
        super(parentShell);
        localization = LocalizationManager.getInstance()
                .getLocalizationServer();
        site = LocalizationManager.getInstance().getSite();
        if (checkAlertViz) {
            alertVizServer = LocalizationManager.getInstance()
                    .getLocalizationStore()
                    .getString(LocalizationConstants.P_ALERT_SERVER);
            alertVizGood = false;
        }
    }

    /**
     * Open the preference dialog
     * 
     * @return whether cancel was issued or not
     */
    public boolean open() {
        // Only open if current settings are not valid.
        if (!validate()) {
            Shell parent = getParent();
            display = parent.getDisplay();
            shell = new Shell(parent, SWT.DIALOG_TRIM);
            shell.setText(dialogTitle);

            // Create the main layout for the shell.
            GridLayout mainLayout = new GridLayout(1, true);
            shell.setLayout(mainLayout);

            initializeComponents();

            shell.pack();

            shell.open();
            while (!shell.isDisposed()) {
                if (!display.readAndDispatch()) {
                    display.sleep();
                }
            }
        }
        return canceled;
    }

    private void initializeComponents() {
        createErrorText();
        Composite textBoxComp = new Composite(shell, SWT.NONE);
        textBoxComp.setLayout(new GridLayout(2, false));
        createTextBoxes(textBoxComp);
        createBottomButtons();
    }

    private void createErrorText() {
        Label label = new Label(shell, SWT.CENTER);
        label.setText("Error: Unable to connect to localization server");
    }

    protected void createTextBoxes(Composite textBoxComp) {
        // Create localization text

        localizationLabel = new Label(textBoxComp, SWT.RIGHT);
        localizationLabel.setText("Localization Server:");
        GridData gd = new GridData(SWT.RIGHT, SWT.None, true, true);
        gd.widthHint = 150;
        localizationLabel.setLayoutData(gd);

        localizationText = new Text(textBoxComp, SWT.NONE);
        gd = new GridData(SWT.RIGHT, SWT.None, true, true);
        gd.widthHint = 300;
        localizationText.setLayoutData(gd);
        localizationText.setText(localization == null ? "" : localization);
        localizationText.setBackground(getTextColor(localizationGood));

        Label label = new Label(textBoxComp, SWT.RIGHT);
        label.setText("Site:");
        gd = new GridData(SWT.RIGHT, SWT.None, true, true);
        gd.widthHint = 150;
        label.setLayoutData(gd);

        siteText = new Text(textBoxComp, SWT.NONE);
        siteText.addVerifyListener(new VerifyListener() {
            public void verifyText(VerifyEvent e) {
                e.text = e.text.toUpperCase();
            }
        });
        gd = new GridData(SWT.RIGHT, SWT.None, true, true);
        gd.widthHint = 300;
        siteText.setLayoutData(gd);
        siteText.setText(site == null ? "" : site);
        siteText.setBackground(getTextColor(siteGood));

        if (alertVizServer != null) {
            label = new Label(textBoxComp, SWT.RIGHT);
            label.setText("Alert Server:");
            gd = new GridData(SWT.RIGHT, SWT.None, true, true);
            gd.widthHint = 150;
            label.setLayoutData(gd);

            alertVizText = new Text(textBoxComp, SWT.NONE);
            gd = new GridData(SWT.RIGHT, SWT.None, true, true);
            gd.widthHint = 300;
            alertVizText.setLayoutData(gd);
            alertVizText.setText(alertVizServer);
            alertVizText.setBackground(getTextColor(alertVizGood));
        }
    }

    private void createBottomButtons() {
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        centeredComp.setLayout(gl);
        GridData gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        centeredComp.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Button validateBtn = new Button(centeredComp, SWT.NONE);
        validateBtn.setText("Validate");
        validateBtn.setLayoutData(gd);
        validateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                validate();
            }
        });

        gd = new GridData(80, SWT.DEFAULT);
        Button okBtn = new Button(centeredComp, SWT.NONE);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                canceled = false;
                validateAndClose();
            }
        });

        gd = new GridData(80, SWT.DEFAULT);
        Button cancelBtn = new Button(centeredComp, SWT.NONE);
        cancelBtn.setText("Quit");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                canceled = true;
                dispose();
            }
        });

        shell.addListener(SWT.Close, new Listener() {
            @Override
            public void handleEvent(Event event) {
                event.doit = validateAndClose();
            }
        });
    }

    private boolean validateAndClose() {
        boolean valid = validate();
        if (valid) {
            shell.setVisible(false);
            dispose();
            applySettings();
        } else {
            shell.setVisible(false);
            MessageDialog
                    .openError(
                            null,
                            "Connectivity Error",
                            "Unable to validate localization preferences, please enter valid options or quit the application");
            shell.setVisible(true);
        }
        return valid;
    }

    protected void applySettings() {
        LocalizationManager.getInstance().setCurrentSite(site);
        LocalizationManager.getInstance().setCurrentServer(localization);
        if (alertVizServer != null) {
            LocalizationManager
                    .getInstance()
                    .getLocalizationStore()
                    .setValue(LocalizationConstants.P_ALERT_SERVER,
                            alertVizServer);
        }
        try {
            ((IPersistentPreferenceStore) LocalizationManager.getInstance()
                    .getLocalizationStore()).save();
        } catch (IOException e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Unable to persist localization preference store", e);
        }
    }

    public boolean validate() {
        if (localizationText != null && !localizationText.isDisposed()
                && localizationText.isEnabled()) {
            String localization = localizationText.getText().trim();
            if (!localizationGood || !this.localization.equals(localization)) {
                this.localization = localization;
                validateLocalization();
                localizationText.setBackground(getTextColor(localizationGood));
            }
        } else {
            validateLocalization();
        }
        if (alertVizServer == null) {
            alertVizGood = true;
        } else if (alertVizText != null && !alertVizText.isDisposed()) {
            String alertVizServer = alertVizText.getText().trim();
            if (!alertVizGood || !this.alertVizServer.equals(alertVizServer)) {
                this.alertVizServer = alertVizServer;
                validateAlertviz();
                alertVizText.setBackground(getTextColor(alertVizGood));
            }
        } else {
            validateAlertviz();
        }
        if (siteText != null && !siteText.isDisposed()) {
            String site = siteText.getText().trim();
            if (!siteGood || !this.site.equals(site)) {
                this.site = site;
                validateSite();
                siteText.setBackground(getTextColor(siteGood));
            }
        } else {
            validateSite();
        }
        return siteGood && localizationGood && alertVizGood;
    }

    private void validateLocalization() {
        ConnectivityManager.checkHttpServer(localization, localizationCallback);
    }

    private void validateAlertviz() {
        ConnectivityManager.checkJmsServer(alertVizServer, alertCallback);
    }

    private void validateSite() {
        if (site == null || site.trim().equals("")) {
            siteGood = false;
        } else {
            siteGood = true;
        }
    }

    protected Color getTextColor(boolean isGood) {
        if (isGood) {
            return display.getSystemColor(SWT.COLOR_WHITE);
        } else {
            return display.getSystemColor(SWT.COLOR_RED);
        }
    }

    public String getLocalization() {
        return localization;
    }

    public void setLocalization(String localization) {
        this.localization = localization;
        if (localizationText != null && !localizationText.isDisposed()) {
            localizationText.setText(localization);
        }
    }

    public boolean isLocalizationGood() {
        return localizationGood;
    }

    public void setAlertVizServer(String server) {
        this.alertVizServer = server;
    }

    public String getAlertVizServer() {
        return this.alertVizServer;
    }

    public boolean isAlertVizGood() {
        return this.alertVizGood;
    }

    public String getSite() {
        return site;
    }

    public void setSite(String site) {
        this.site = site;
    }

    public boolean isSiteGood() {
        return siteGood;
    }

    private void dispose() {
        shell.dispose();
    }

    protected void setLocalizationEnabled(boolean enabled) {
        if (localizationLabel != null && !localizationLabel.isDisposed()) {
            localizationLabel.setEnabled(enabled);
        }
        if (localizationText != null && !localizationText.isDisposed()) {
            localizationText.setEnabled(enabled);
        }
    }
}
