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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.net.UnknownHostException;
import java.util.regex.Pattern;

import org.apache.http.conn.HttpHostConnectException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
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

import com.raytheon.uf.common.comm.HttpServerException;
import com.raytheon.uf.common.comm.InvalidURIException;
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
 * Aug 05, 2009            mschenke    Initial creation
 * Aug 02, 2013 2202       bsteffen    Add edex specific connectivity checking.
 * Feb 04, 2014 2704       njensen     Shifted some private fields/methods to protected,
 *                                      Added status and details, better site validation
 * Feb 17, 2014 2704       njensen     Changed some alertviz fields to protected
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ConnectivityPreferenceDialog extends Dialog {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ConnectivityPreferenceDialog.class, "CAVE");

    protected static final Pattern VALID_SITENAME = Pattern
            .compile("^[A-Za-z0-9._-]+$");

    private class LocalizationCallback implements IConnectivityCallback {

        @Override
        public void connectionChecked(ConnectivityResult results) {
            localizationGood = results.hasConnectivity;
            appendDetails(buildDetails(results));
            if (!results.hasConnectivity && status == null) {
                status = buildErrorMessage(results);
            }
        }
    }

    private class AlertVizCallback implements IConnectivityCallback {

        @Override
        public void connectionChecked(ConnectivityResult results) {
            alertVizGood = results.hasConnectivity;
            appendDetails(buildDetails(results));
            if (!results.hasConnectivity && status == null) {
                status = buildErrorMessage(results);
            }
        }

    }

    private Shell shell;

    /**
     * Display component
     */
    protected Display display;

    private Label localizationLabel;

    protected Text localizationText;

    private String localization = "";

    private boolean localizationGood = false;

    protected Text alertVizText;

    protected String alertVizServer = null;

    private boolean alertVizGood = true;

    private boolean siteGood = false;

    private String site = "";

    protected Text siteText;

    private Label statusLabel;

    private boolean canceled = false;

    private Composite detailsComp;

    private StyledText detailsText;

    private IConnectivityCallback localizationCallback = new LocalizationCallback();

    private IConnectivityCallback alertCallback = new AlertVizCallback();

    /**
     * Title of the dialog.
     */
    private String title;

    protected String status;

    protected String details;

    public ConnectivityPreferenceDialog(boolean checkAlertViz, String title) {
        this(new Shell(Display.getDefault()), checkAlertViz, title);
    }

    public ConnectivityPreferenceDialog(Shell parentShell,
            boolean checkAlertViz, String title) {
        super(parentShell);
        this.title = title;
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
            shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.RESIZE);
            shell.setText(title);

            // Create the main layout for the shell.
            GridLayout mainLayout = new GridLayout(1, true);
            shell.setLayout(mainLayout);

            initializeComponents();

            shell.pack();
            shell.setMinimumSize(shell.getBounds().width,
                    shell.getBounds().height);
            updateStatus(false, status, details);

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
        Composite textBoxComp = new Composite(shell, SWT.NONE);
        textBoxComp.setLayout(new GridLayout(2, false));
        createTextBoxes(textBoxComp);
        createStatusText();
        createBottomButtons();
    }

    /**
     * Creates the status label, text, and details button
     */
    protected void createStatusText() {
        Composite comp = new Composite(shell, SWT.NONE);
        comp.setLayout(new GridLayout(3, false));
        comp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        Label lbl = new Label(comp, SWT.NONE);
        lbl.setText("Status:");

        GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        statusLabel = new Label(comp, SWT.BORDER);
        statusLabel.setLayoutData(gd);
        statusLabel.setText("");

        final Button detailsButton = new Button(comp, SWT.TOGGLE);
        detailsButton.setText("Details");
        detailsButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (detailsComp.isVisible()) {
                    ((GridData) detailsComp.getLayoutData()).exclude = true;
                    detailsComp.setVisible(false);
                    shell.pack();
                } else {
                    ((GridData) detailsComp.getLayoutData()).exclude = false;
                    ((GridData) detailsComp.getLayoutData()).widthHint = detailsComp
                            .getBounds().width;
                    detailsComp.setVisible(true);
                    shell.pack();
                }
            }
        });
        createDetailsText();
    }

    /**
     * Creates the expanding details text
     */
    protected void createDetailsText() {
        detailsComp = new Composite(shell, SWT.NONE);
        detailsComp.setLayout(new GridLayout(1, false));
        detailsComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 150;
        detailsText = new StyledText(detailsComp, SWT.BORDER | SWT.MULTI
                | SWT.H_SCROLL | SWT.V_SCROLL);
        detailsText.setText("");
        detailsText.setLayoutData(gd);

        /*
         * Hide the composite
         */
        ((GridData) detailsComp.getLayoutData()).exclude = true;
        detailsComp.setVisible(false);
    }

    protected void createTextBoxes(Composite textBoxComp) {
        // Create localization text

        localizationLabel = new Label(textBoxComp, SWT.RIGHT);
        localizationLabel.setText("Localization Server:");
        GridData gd = new GridData(SWT.RIGHT, SWT.CENTER, true, true);
        gd.widthHint = 150;
        localizationLabel.setLayoutData(gd);

        localizationText = new Text(textBoxComp, SWT.BORDER);
        gd = new GridData(SWT.LEFT, SWT.None, true, true);
        gd.widthHint = 300;
        localizationText.setLayoutData(gd);
        localizationText.setText(localization == null ? "" : localization);
        localizationText.setBackground(getTextColor(localizationGood));

        Label label = new Label(textBoxComp, SWT.RIGHT);
        label.setText("Site:");
        gd = new GridData(SWT.RIGHT, SWT.CENTER, true, true);
        gd.widthHint = 150;
        label.setLayoutData(gd);

        siteText = new Text(textBoxComp, SWT.BORDER);
        siteText.addVerifyListener(new VerifyListener() {
            @Override
            public void verifyText(VerifyEvent e) {
                e.text = e.text.toUpperCase();
            }
        });
        gd = new GridData(SWT.LEFT, SWT.None, true, true);
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
                            shell,
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
        status = null;
        details = null;
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

        boolean everythingGood = siteGood && localizationGood && alertVizGood;
        updateStatus(everythingGood, status, details);
        return everythingGood;
    }

    private void validateLocalization() {
        ConnectivityManager.checkLocalizationServer(localization,
                localizationCallback);
    }

    protected void validateAlertviz() {
        ConnectivityManager.checkAlertService(alertVizServer, alertCallback);
    }

    protected void validateSite() {
        if (site == null || site.trim().length() == 0
                || !VALID_SITENAME.matcher(site).find()) {
            siteGood = false;
            if (status == null) {
                status = "Invalid Site ID";
            }
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

    /**
     * Gets the color for the status label
     * 
     * @param isGood
     * @return
     */
    protected Color getForegroundColor(boolean isGood) {
        if (isGood) {
            return display.getSystemColor(SWT.COLOR_DARK_GREEN);
        } else {
            return display.getSystemColor(SWT.COLOR_DARK_RED);
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

    /**
     * Builds a details string based on a stacktrace of connectivity results. If
     * there is no exception with the results, this returns the empty string.
     * 
     * @param results
     * @return
     */
    protected String buildDetails(ConnectivityResult results) {
        StringBuilder sb = new StringBuilder();
        if (results.exception != null) {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            PrintStream ps = new PrintStream(baos);
            results.exception.printStackTrace(ps);
            String stack = baos.toString();
            ps.close();
            sb.append(stack);
        }
        return sb.toString();
    }

    /**
     * Adds new details to the details field without overwriting it
     * 
     * @param newDetails
     */
    protected void appendDetails(String newDetails) {
        if (details == null) {
            details = "";
        }
        if (details.length() > 0) {
            details += "\n\n\n";
        }
        details += newDetails;
    }

    /**
     * Creates an error message for the status label by attempting to find the
     * most relevant error message from the exception's stacktrace.
     * 
     * @param result
     * @return
     */
    protected String buildErrorMessage(ConnectivityResult result) {
        StringBuilder sb = new StringBuilder();
        Exception prettyErrExc = result.exception;
        /*
         * Loop through the Caused Bys and try to find one that is the most
         * useful for the label. This is totally arbitrary and corresponds to
         * what njensen predicted would be most useful.
         */
        while (prettyErrExc != null) {
            if (prettyErrExc instanceof HttpHostConnectException
                    || prettyErrExc instanceof InvalidURIException) {
                sb.append(prettyErrExc.getMessage());
                break;
            }
            if (prettyErrExc instanceof UnknownHostException) {
                sb.append("Unknown host: " + prettyErrExc.getMessage());
                break;
            } else if (prettyErrExc instanceof HttpServerException) {
                sb.append("Server returned Error ");
                String emsg = prettyErrExc.getMessage();
                int titleIndex = emsg.indexOf("<title>");
                if (titleIndex > -1) {
                    String httpMsg = emsg.substring(titleIndex + 7,
                            emsg.indexOf("</title>"));
                    sb.append(httpMsg);
                } else {
                    int statusCode = ((HttpServerException) prettyErrExc)
                            .getStatusCode();
                    sb.append(statusCode);
                    break;
                }
            }
            prettyErrExc = (Exception) prettyErrExc.getCause();
        }

        if (sb.length() == 0) {
            if (result.exception != null
                    && result.exception.getMessage() != null) {
                sb.append(result.exception.getMessage());
            } else {
                sb.append("Connectivity Error");
            }
        }

        return sb.toString();
    }

    /**
     * Updates the status label and details of the connectivity dialog
     * 
     * @param good
     * @param status
     * @param details
     */
    protected void updateStatus(boolean good, String status, String details) {
        if (statusLabel != null && !statusLabel.isDisposed()
                && detailsText != null && !detailsText.isDisposed()) {
            statusLabel.setForeground(getForegroundColor(good));
            detailsText.setText(details != null ? details : "");
            if (good) {
                statusLabel.setText("Successful connection");
            } else {
                if (status != null) {
                    statusLabel.setText(status);
                } else {
                    // shoudln't be able to reach this but just in case
                    statusLabel.setText("Connection error");
                }
            }
        }
    }
}
