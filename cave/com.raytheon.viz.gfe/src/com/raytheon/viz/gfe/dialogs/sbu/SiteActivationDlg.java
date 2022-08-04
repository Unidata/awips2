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
package com.raytheon.viz.gfe.dialogs.sbu;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.auth.req.CheckAuthorizationRequest;
import com.raytheon.uf.common.auth.util.PermissionUtils;
import com.raytheon.uf.common.jms.notification.INotificationObserver;
import com.raytheon.uf.common.jms.notification.NotificationException;
import com.raytheon.uf.common.jms.notification.NotificationMessage;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.site.notify.SiteActivationNotification;
import com.raytheon.uf.common.site.requests.ActivateSiteRequest;
import com.raytheon.uf.common.site.requests.DeactivateSiteRequest;
import com.raytheon.uf.common.site.requests.GetActiveSitesRequest;
import com.raytheon.uf.common.site.requests.ValidateConfigRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Site Activation Dialog
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 05, 2011           randerso  Initial creation
 * Oct 26, 2012  1287     rferrel   Code clean up for non-blocking dialog.
 * Aug 28, 2014  3563     randerso  Move to the new common INotificationObserer
 *                                  code
 * May 23, 2017  6217     randerso  Change to work with updated roles and
 *                                  permissions framework
 * May 23, 2017  6285     randerso  Migrate to new roles and permissions
 *                                  framework
 * May 04, 2020  8151     randerso  Only allow sites with SiteConfig.py files to
 *                                  be selected.
 *
 * </pre>
 *
 * @author randerso
 */

public class SiteActivationDlg extends CaveJFACEDialog
        implements INotificationObserver {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SiteActivationDlg.class);

    private static final String SITE_ACTIVATION_TOPIC = "edex.alerts.siteActivate";

    private static final String SITE_CONFIG_PATH = LocalizationUtil.join("gfe",
            "config", "siteConfig.py");

    private SimpleDateFormat dateFormat;

    private Combo siteId;

    private Text activeSites;

    private Text logRoll;

    /**
     * @param parentShell
     */
    public SiteActivationDlg(Shell parentShell) {
        super(parentShell);
        setShellStyle(SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL | SWT.RESIZE);
        this.dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        this.dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("GFE Site Activation");

        newShell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                NotificationManagerJob.removeObserver(SITE_ACTIVATION_TOPIC,
                        SiteActivationDlg.this);
            }
        });
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite top = (Composite) super.createDialogArea(parent);

        Composite comp = new Composite(top, SWT.NONE);
        GridLayout layout = new GridLayout(5, false);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        comp.setLayout(layout);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        comp.setLayoutData(layoutData);

        Label label = new Label(comp, SWT.NONE);
        label.setText("Site ID:");

        List<String> sites = getAvailableSites();

        siteId = new Combo(comp, SWT.DROP_DOWN | SWT.READ_ONLY);
        siteId.setToolTipText(
                "Only sites with existing siteConfig.py files can be selected");
        siteId.setItems(sites.toArray(new String[0]));
        if (!sites.isEmpty()) {
            siteId.select(0);
        }

        Button validateBtn = new Button(comp, SWT.PUSH);
        validateBtn.setText("Validate");
        validateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                doValidate();
            }
        });

        Button activateBtn = new Button(comp, SWT.PUSH);
        activateBtn.setText("Activate");
        activateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                doActivate();
            }
        });

        Button deactivateBtn = new Button(comp, SWT.PUSH);
        deactivateBtn.setText("Deactivate");
        deactivateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                doDeactivate();
            }
        });

        Composite activeComp = new Composite(top, SWT.NONE);
        layout = new GridLayout(2, false);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        activeComp.setLayout(layout);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        activeComp.setLayoutData(layoutData);

        Label label1 = new Label(activeComp, SWT.NONE);
        label1.setText("Active Sites:");

        activeSites = new Text(activeComp, SWT.BORDER | SWT.READ_ONLY);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        activeSites.setLayoutData(layoutData);

        Group group = new Group(top, SWT.BORDER);
        group.setText("Log Roll");
        layout = new GridLayout(1, false);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        group.setLayout(layout);
        layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        group.setLayoutData(layoutData);

        logRoll = new Text(group,
                SWT.MULTI | SWT.READ_ONLY | SWT.V_SCROLL | SWT.H_SCROLL);
        layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        layoutData.widthHint = this.convertWidthInCharsToPixels(130);
        layoutData.heightHint = this.convertHeightInCharsToPixels(24);
        logRoll.setLayoutData(layoutData);

        boolean authorized = false;
        CheckAuthorizationRequest request = new CheckAuthorizationRequest(
                PermissionUtils.buildPermissionString("gfe", "siteActivation"));
        try {
            authorized = (Boolean) ThriftClient.sendRequest(request);
        } catch (VizException e) {
            statusHandler.error("Unable to determine user authorization", e);
        }
        if (authorized) {
            NotificationManagerJob.addObserver(SITE_ACTIVATION_TOPIC, this);
            updateActiveSites();

        } else {
            validateBtn.setEnabled(false);
            activateBtn.setEnabled(false);
            deactivateBtn.setEnabled(false);
            siteId.setEnabled(false);
            logRoll.setEnabled(false);
            activeSites.setEnabled(false);
            logRoll.setText("User " + UserController.getUserObject().uniqueId()
                    + " is not authorized to activate and deactivate sites");
        }

        return top;

    }

    private List<String> getAvailableSites() {
        List<String> sites = new ArrayList<>();

        IPathManager pm = PathManagerFactory.getPathManager();
        String[] contexts = pm.getContextList(LocalizationLevel.SITE);
        LocalizationContext ctx = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);
        for (String contextName : contexts) {
            ctx.setContextName(contextName);
            ILocalizationFile lf = pm.getLocalizationFile(ctx,
                    SITE_CONFIG_PATH);
            if (lf != null && lf.exists()) {
                sites.add(contextName);
            }
        }
        Collections.sort(sites);

        List<String> primary = new ArrayList<>(CheckPrimary.getPrimarySites());
        Collections.sort(primary);

        // return sorted list with primary sites listed first
        sites.removeAll(primary);
        primary.addAll(sites);
        return primary;
    }

    @Override
    protected Control createButtonBar(Composite parent) {
        return null;
    }

    private void doValidate() {
        String siteID = this.siteId.getText();
        if (!siteID.isEmpty()) {
            ValidateConfigRequest request = new ValidateConfigRequest(siteID,
                    "gfe");
            try {
                String result = (String) ThriftClient.sendRequest(request);
                appendToLog(result);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error processing site activation request", e);
            }
        }
    }

    private void doActivate() {
        String siteID = this.siteId.getText();
        if (!siteID.isEmpty()) {
            ActivateSiteRequest request = new ActivateSiteRequest(siteID,
                    "gfe");
            try {
                ThriftClient.sendRequest(request);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error processing site activation request", e);

            }
        }
    }

    private void doDeactivate() {
        String siteID = this.siteId.getText();
        if (!siteID.isEmpty()) {
            DeactivateSiteRequest request = new DeactivateSiteRequest(siteID,
                    "gfe");
            try {
                ThriftClient.sendRequest(request);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error processing site deactivation request", e);

            }
        }
    }

    private void updateActiveSites() {
        final StringBuilder sb = new StringBuilder();
        String[] sites = getActiveSites();
        Arrays.sort(sites);
        for (String site : sites) {
            sb.append(' ').append(site);
        }

        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                activeSites.setText(sb.toString());
            }
        });
    }

    private String[] getActiveSites() {
        String[] activeSites = new String[0];
        GetActiveSitesRequest request = new GetActiveSitesRequest();
        try {
            Object obj = ThriftClient.sendRequest(request);
            if (obj instanceof String[]) {
                activeSites = (String[]) obj;
            } else {
                statusHandler.handle(Priority.PROBLEM,
                        "getActiveSites received " + obj);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error processing get active sites request", e);

        }
        return activeSites;
    }

    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        for (NotificationMessage msg : messages) {
            Object obj;
            try {
                obj = msg.getMessagePayload();
                String message = null;
                if (obj instanceof SiteActivationNotification) {
                    message = dateFormat.format(new Date()) + "  "
                            + obj.toString();
                    appendToLog(message);
                    updateActiveSites();
                }
            } catch (NotificationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to read incoming notification", e);
                continue;
            }
        }
    }

    private void appendToLog(final String message) {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                logRoll.setText(logRoll.getText() + message + "\n");
                logRoll.setSelection(logRoll.getText().length());
            }
        });
    }
}
