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

import java.io.FileNotFoundException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.program.Program;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;
import org.osgi.framework.Bundle;

import com.raytheon.uf.common.activetable.GetVtecAttributeRequest;
import com.raytheon.uf.common.activetable.GetVtecAttributeResponse;
import com.raytheon.uf.common.auth.req.CheckAuthorizationRequest;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.dataplugin.gfe.request.CleanupSvcBuRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.ExportConfRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.ExportDataToFailedSiteRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.ExportFailedSiteDataToCCRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.ExportGridsRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.ExportGridsRequest.ExportGridsMode;
import com.raytheon.uf.common.dataplugin.gfe.request.GetKnownSitesRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetServiceBackupJobStatusRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.ImportConfRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.ImportDigitalDataRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.svcbu.JobProgress;
import com.raytheon.uf.common.dataplugin.gfe.svcbu.ServiceBackupJobStatus;
import com.raytheon.uf.common.jms.notification.INotificationObserver;
import com.raytheon.uf.common.jms.notification.NotificationException;
import com.raytheon.uf.common.jms.notification.NotificationMessage;
import com.raytheon.uf.common.site.notify.ClusterActivationNotification;
import com.raytheon.uf.common.site.notify.SiteActivationNotification.ACTIVATIONSTATUS;
import com.raytheon.uf.common.site.notify.SiteActivationNotification.ACTIVATIONTYPE;
import com.raytheon.uf.common.site.requests.GetActiveSitesRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.dialogs.sbu.jobs.AbstractSbuTask;
import com.raytheon.viz.gfe.dialogs.sbu.jobs.AwaitNotificationSbuTask;
import com.raytheon.viz.gfe.dialogs.sbu.jobs.ServerRequestSbuTask;
import com.raytheon.viz.gfe.dialogs.sbu.jobs.SiteActivationSbuTask;
import com.raytheon.viz.gfe.dialogs.sbu.jobs.StartGfeSbuTask;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * GFE Service Backup main dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 04, 2011            randerso     Initial creation
 * Sep 19, 2011   10955    rferrel      Use RunProcess
 * Oct 25, 2012    1287    rferrel      Code clean up for non-blocking dialog.
 * Nov 15, 2012   15614    jdynina      Added check for national center
 * Mar 20, 2013    1447    dgilling     Port troubleshooting mode changes
 *                                      from A1 DR 21404, some code cleanup.
 * May 01, 2013    1762    dgilling     Remove national center check.
 * Jul 22, 2013    1762    dgilling     Fix running as primary check.
 * Apr 14, 2014    2984    njensen      Moved help files to viz.gfe plugin
 * Jun 10,2014   DR-17401  lshi
 * Feb 18, 2015  #4300     randerso     Completely rewritten to support multi-site backup
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ServiceBackupDlg extends CaveJFACEDialog implements
        INotificationObserver {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ServiceBackupDlg.class);

    private static final String SITE_ACTIVATION_TOPIC = "edex.alerts.siteActivate";

    private static final String BACKING_UP = "YOU ARE BACKING UP";

    private static final String NOT_IN_BACKUP = "YOU ARE NOT IN BACKUP MODE";

    private static final long IMPORT_TIMEOUT = 30 * TimeUtil.MILLIS_PER_MINUTE;

    private String site;

    private List<String> allSites;

    private List<String> primarySites;

    private Set<String> activatedBackupSites;

    private Label bannerLabel;

    private Label sitesLabel;

    private Button enterButton;

    private Combo enterCombo;

    private Button importGridsCheckBox;

    private Button troubleShootingCheckBox;

    private Button startGFECheckBox;

    private Button importGridsButton;

    private Combo importGridsCombo;

    private Button startGFEButton;

    private Combo startGFECombo;

    private Button exportFailedGridsButton;

    private Combo exportFailedGridsCombo;

    private Button exitButton;

    private Combo exitCombo;

    private Button exportLocalGridsButton;

    private Combo exportLocalGridsCombo;

    private Button exportConfigButton;

    private Combo exportConfigCombo;

    private ConcurrentMap<String, ServiceBackupStatusDlg> statusDialogs;

    /**
     * Constructor
     * 
     * @throws GFEException
     */
    public ServiceBackupDlg() throws GFEException {
        super(null);
        this.site = LocalizationManager.getInstance().getCurrentSite();
        this.statusDialogs = new ConcurrentHashMap<String, ServiceBackupStatusDlg>();

        Boolean isAuthorized = checkPermission("serviceBackup");
        if (!isAuthorized) {
            String msg = "You are not authorized to run GFE service backup.";
            statusHandler.error(msg);
            throw new GFEException(msg);
        }

        populateSites();
    }

    private boolean checkPermission(String permission) throws GFEException {
        String userId = System.getProperty("user.name");
        CheckAuthorizationRequest request = new CheckAuthorizationRequest(
                userId, permission, "GFE");

        try {
            return (Boolean) ThriftClient.sendRequest(request);
        } catch (VizException e) {
            throw new GFEException("Error checking permissions ", e);
        }
    }

    private void populateSites() {
        primarySites = new ArrayList<String>(CheckPermissions.getPrimarySites());
        Collections.sort(primarySites);

        this.activatedBackupSites = new TreeSet<>();

        List<String> backupPartners = new ArrayList<String>();
        try {
            GetVtecAttributeRequest vtecAttrReq = new GetVtecAttributeRequest();
            vtecAttrReq.setSiteId(this.site);
            vtecAttrReq.setAttribute("BackupDict");
            GetVtecAttributeResponse vtecAttrResp = (GetVtecAttributeResponse) ThriftClient
                    .sendRequest(vtecAttrReq);
            @SuppressWarnings("unchecked")
            Map<String, List<String>> backupDict = (Map<String, List<String>>) vtecAttrResp
                    .getValue();

            for (Entry<String, List<String>> entry : backupDict.entrySet()) {
                if (entry.getValue().contains(this.site)) {
                    backupPartners.add(entry.getKey());
                }
            }
        } catch (VizException e) {
            statusHandler.error("Error retreiving backup partner sites", e);
        }
        Collections.sort(backupPartners);

        GetKnownSitesRequest knownSitesReq = new GetKnownSitesRequest();
        knownSitesReq.setSiteID(this.site);
        List<String> knownSites = new ArrayList<String>();
        try {
            @SuppressWarnings("unchecked")
            ServerResponse<List<String>> sr = ((ServerResponse<List<String>>) ThriftClient
                    .sendRequest(knownSitesReq));
            if (sr.isOkay()) {
                knownSites = sr.getPayload();
            } else {
                statusHandler.error("Error retreiving known sites list: "
                        + sr.message());
            }
        } catch (VizException e) {
            statusHandler.error("Exception retrieving known sites list", e);
        }
        knownSites.removeAll(primarySites);
        knownSites.removeAll(backupPartners);
        Collections.sort(knownSites);

        allSites = new ArrayList<String>(knownSites.size()
                + backupPartners.size());
        allSites.addAll(backupPartners);
        allSites.addAll(knownSites);

    }

    private void initActiveBackupSites() {
        /*
         * We get the list of activated sites and then check that they have had
         * their configurations successfully imported and only add those that
         * have.
         * 
         * This keeps us from finding sites that were activated via the
         * SiteActivation dialog in CAVE instead of through the Service Backup
         * dialog.
         */
        try {
            String[] activatedSites = (String[]) ThriftClient
                    .sendRequest(new GetActiveSitesRequest());
            checkActivatedSites(activatedSites);
        } catch (Exception e) {
            statusHandler.error("Error getting activated sites", e);
            this.activatedBackupSites.clear();
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.jms.notification.INotificationObserver#
     * notificationArrived
     * (com.raytheon.uf.common.jms.notification.NotificationMessage[])
     */
    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        for (NotificationMessage msg : messages) {
            try {
                Object obj = msg.getMessagePayload();
                if (obj instanceof ClusterActivationNotification) {
                    ClusterActivationNotification notif = (ClusterActivationNotification) obj;
                    String site = notif.getModifiedSite();
                    if (notif.getStatus().equals(ACTIVATIONSTATUS.SUCCESS)) {
                        if (notif.getType().equals(ACTIVATIONTYPE.ACTIVATE)) {
                            checkActivatedSites(site);
                        } else {
                            this.activatedBackupSites.remove(site);
                        }
                    }
                }
            } catch (NotificationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to read incoming notification", e);
                continue;
            }
        }

        getShell().getDisplay().asyncExec(new Runnable() {
            @Override
            public void run() {
                updateDialog();
            }
        });
    }

    private void checkActivatedSites(String... sites) {
        if (sites.length == 0) {
            return;
        }

        try {
            GetServiceBackupJobStatusRequest request = new GetServiceBackupJobStatusRequest(
                    sites[0], sites);
            @SuppressWarnings("unchecked")
            ServerResponse<Map<String, Collection<ServiceBackupJobStatus>>> response = (ServerResponse<Map<String, Collection<ServiceBackupJobStatus>>>) ThriftClient
                    .sendRequest(request);
            if (response.isOkay()) {
                Map<String, Collection<ServiceBackupJobStatus>> backupStatus = response
                        .getPayload();

                for (Entry<String, Collection<ServiceBackupJobStatus>> entry : backupStatus
                        .entrySet()) {
                    String site = entry.getKey();
                    for (ServiceBackupJobStatus status : entry.getValue()) {
                        if (status.getJobName().equals("svcbuMode")
                                && status.getJobStatus().equals(
                                        JobProgress.SUCCESS)) {
                            this.activatedBackupSites.add(site);
                        }
                    }
                }
            } else {
                statusHandler.error("Error retrieving service backup status: "
                        + response.message());
            }
        } catch (Exception e) {
            statusHandler.error("Error retreiving active backup sites", e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("GFE Service Backup");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#getInitialLocation(org.eclipse.swt.graphics
     * .Point)
     */
    @Override
    protected Point getInitialLocation(Point initialSize) {
        Monitor primary = getShell().getDisplay().getPrimaryMonitor();
        Rectangle bounds = primary.getBounds();

        int x = (bounds.width - initialSize.x) / 4;
        int y = (bounds.height - initialSize.y) / 4;

        Point location = new Point(x, y);
        return location;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createButtonBar(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createButtonBar(Composite parent) {
        return null; // no button bar
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        Composite comp = (Composite) super.createDialogArea(parent);
        createMenuBar(comp);

        bannerLabel = new Label(comp, SWT.HORIZONTAL | SWT.CENTER);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        bannerLabel.setLayoutData(layoutData);
        bannerLabel.setText(NOT_IN_BACKUP);

        sitesLabel = new Label(comp, SWT.HORIZONTAL | SWT.CENTER);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        sitesLabel.setLayoutData(layoutData);

        Group startupActionsGroup = new Group(comp, SWT.SHADOW_NONE);
        layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        startupActionsGroup.setLayoutData(layoutData);
        startupActionsGroup.setLayout(new GridLayout(3, false));
        startupActionsGroup.setText("Startup Actions");

        enterButton = new Button(startupActionsGroup, SWT.PUSH);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        enterButton.setLayoutData(layoutData);
        enterButton.setText("Enter");
        Label label = new Label(startupActionsGroup, SWT.HORIZONTAL
                | SWT.CENTER);
        label.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        label.setText("Service Backup for");
        enterButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                enterBackup(enterCombo.getText());
            }
        });

        enterCombo = new Combo(startupActionsGroup, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        enterCombo.setLayoutData(layoutData);
        enterCombo
                .setToolTipText("Select a failed site you wish to start backing up");
        enterCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                enterButton.setToolTipText("Enter service backup for "
                        + enterCombo.getText());
                enterButton.setEnabled(true);
                importGridsCheckBox.setEnabled(true);
                troubleShootingCheckBox.setEnabled(true);
                startGFECheckBox.setEnabled(true);
            }

        });

        Composite startupOptionsComp = new Composite(startupActionsGroup,
                SWT.NONE);
        layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        layoutData.horizontalSpan = 3;
        layoutData.horizontalIndent = 15;
        startupOptionsComp.setLayoutData(layoutData);
        GridLayout layout = new GridLayout(1, false);
        layout.verticalSpacing = 0;
        layout.marginHeight = 0;
        layout.marginBottom = 5;
        startupOptionsComp.setLayout(layout);

        importGridsCheckBox = new Button(startupOptionsComp, SWT.CHECK);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        importGridsCheckBox.setLayoutData(layoutData);
        importGridsCheckBox.setText("Import Forecast Grids");
        importGridsCheckBox.setEnabled(false);

        troubleShootingCheckBox = new Button(startupOptionsComp, SWT.CHECK);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        troubleShootingCheckBox.setLayoutData(layoutData);
        troubleShootingCheckBox.setText("Troubleshooting Mode");
        troubleShootingCheckBox
                .setToolTipText("Disables ISC/VTEC AT sharing.\nYou should check this box when you are only testing and not actually entering service backup");
        troubleShootingCheckBox.setEnabled(false);

        startGFECheckBox = new Button(startupOptionsComp, SWT.CHECK);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        startGFECheckBox.setLayoutData(layoutData);
        startGFECheckBox.setText("Automatically Start GFE");
        startGFECheckBox.setEnabled(false);

        importGridsButton = new Button(startupActionsGroup, SWT.PUSH);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        importGridsButton.setLayoutData(layoutData);
        importGridsButton.setText("Import");
        label = new Label(startupActionsGroup, SWT.HORIZONTAL | SWT.CENTER);
        label.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        label.setText("Forecast Grids for");
        importGridsButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                importGrids(importGridsCombo.getText());
            }
        });

        importGridsCombo = new Combo(startupActionsGroup, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        importGridsCombo.setLayoutData(layoutData);
        importGridsCombo
                .setToolTipText("Select a failed site from which to import forecast grids");
        importGridsCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                importGridsButton.setToolTipText("Import forecast grids for "
                        + importGridsCombo.getText());
                importGridsButton.setEnabled(true);
            }

        });

        startGFEButton = new Button(startupActionsGroup, SWT.PUSH);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        startGFEButton.setLayoutData(layoutData);
        startGFEButton.setText("Start");
        startGFEButton
                .setToolTipText("Start GFE session for the selected site");
        label = new Label(startupActionsGroup, SWT.HORIZONTAL | SWT.CENTER);
        label.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        label.setText("GFE Session for");
        startGFEButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                startGFE(startGFECombo.getText());
            }
        });

        startGFECombo = new Combo(startupActionsGroup, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        startGFECombo.setLayoutData(layoutData);
        startGFECombo.setToolTipText("Select a site to start a GFE session");
        startGFECombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                startGFEButton.setToolTipText("Start GFE Sesson for "
                        + startGFECombo.getText());
                startGFEButton.setEnabled(true);
            }

        });

        Group exitActionsGroup = new Group(comp, SWT.SHADOW_NONE);
        layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        exitActionsGroup.setLayoutData(layoutData);
        exitActionsGroup.setLayout(new GridLayout(3, false));
        exitActionsGroup.setText("Exit/Cleanup Actions");

        exportFailedGridsButton = new Button(exitActionsGroup, SWT.PUSH);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        exportFailedGridsButton.setLayoutData(layoutData);
        exportFailedGridsButton.setText("Export");
        exportFailedGridsButton.setEnabled(false);
        label = new Label(exitActionsGroup, SWT.HORIZONTAL | SWT.CENTER);
        label.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        label.setText("Forecast Grids to");
        exportFailedGridsButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                exportGridsToFailedSite(exportFailedGridsCombo.getText());
            }
        });

        exportFailedGridsCombo = new Combo(exitActionsGroup, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        exportFailedGridsCombo.setLayoutData(layoutData);
        exportFailedGridsCombo
                .setToolTipText("Select a failed site to export forecast grids");
        exportFailedGridsCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                exportFailedGridsButton.setToolTipText("Export grids to  "
                        + exportFailedGridsCombo.getText());
                exportFailedGridsButton.setEnabled(true);
            }

        });

        exitButton = new Button(exitActionsGroup, SWT.PUSH);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        exitButton.setLayoutData(layoutData);
        exitButton.setText("Exit");
        label = new Label(exitActionsGroup, SWT.HORIZONTAL | SWT.CENTER);
        label.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        label.setText("Service Backup for");
        exitButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                exitBackup(exitCombo.getText());
            }
        });

        exitCombo = new Combo(exitActionsGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        exitCombo.setLayoutData(layoutData);
        exitCombo
                .setToolTipText("Select a failed site you wish to stop backing up");
        exitCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                exitButton.setToolTipText("Exit service backup for "
                        + exitCombo.getText());
                exitButton.setEnabled(true);
            }

        });

        Group localActionGroup = new Group(comp, SWT.SHADOW_NONE);
        layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        localActionGroup.setLayoutData(layoutData);
        localActionGroup.setLayout(new GridLayout(3, false));
        localActionGroup.setText("Local Site Actions");

        exportLocalGridsButton = new Button(localActionGroup, SWT.PUSH);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        exportLocalGridsButton.setLayoutData(layoutData);
        exportLocalGridsButton.setText("Export");
        exportLocalGridsButton.setEnabled(false);
        label = new Label(localActionGroup, SWT.HORIZONTAL | SWT.CENTER);
        label.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        label.setText("Forecast Grids for");
        exportLocalGridsButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                exportGridsToCentralServer(exportLocalGridsCombo.getText());
            }
        });

        exportLocalGridsCombo = new Combo(localActionGroup, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        exportLocalGridsCombo.setLayoutData(layoutData);
        exportLocalGridsCombo.setToolTipText("Select a local primary site");
        exportLocalGridsCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                exportLocalGridsButton.setToolTipText("Export grids for "
                        + exportLocalGridsCombo.getText()
                        + " to the central server");
                exportLocalGridsButton.setEnabled(true);
            }

        });

        exportLocalGridsCombo.setItems(primarySites
                .toArray(new String[primarySites.size()]));
        if (primarySites.size() == 1) {
            exportLocalGridsCombo.setText(primarySites.get(0));
            exportLocalGridsButton.setToolTipText("Export grids for "
                    + exportLocalGridsCombo.getText()
                    + " to the central server");
            exportLocalGridsButton.setEnabled(true);
        }

        boolean isAuthorized = false;
        try {
            isAuthorized = checkPermission("exportConfig");
        } catch (GFEException e) {
            statusHandler.error("Unable to check permissions for exportConfig",
                    e);
        }
        if (isAuthorized) {
            exportConfigButton = new Button(localActionGroup, SWT.PUSH);
            layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            exportConfigButton.setLayoutData(layoutData);
            exportConfigButton.setText("Export");
            exportConfigButton.setEnabled(false);
            label = new Label(localActionGroup, SWT.HORIZONTAL | SWT.CENTER);
            label.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
            label.setText("Configuration for");
            exportConfigButton.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    exportConfigToCentralServer(exportConfigCombo.getText());
                }
            });

            exportConfigCombo = new Combo(localActionGroup, SWT.DROP_DOWN
                    | SWT.READ_ONLY);
            layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            exportConfigCombo.setLayoutData(layoutData);
            exportConfigCombo.setToolTipText("Select a local primary site");
            exportConfigCombo.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    exportConfigButton
                            .setToolTipText("Export configuration for "
                                    + exportConfigCombo.getText()
                                    + " to the central server");
                    exportConfigButton.setEnabled(true);
                }

            });

            exportConfigCombo.setItems(primarySites
                    .toArray(new String[primarySites.size()]));
            if (primarySites.size() == 1) {
                exportConfigCombo.setText(primarySites.get(0));
                exportConfigButton.setToolTipText("Export grids for "
                        + exportConfigCombo.getText()
                        + " to the central server");
                exportConfigButton.setEnabled(true);
            }
        }

        getShell().addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                NotificationManagerJob.removeObserver(SITE_ACTIVATION_TOPIC,
                        ServiceBackupDlg.this);

            }
        });

        NotificationManagerJob.addObserver(SITE_ACTIVATION_TOPIC, this);
        initActiveBackupSites();
        updateDialog();

        return comp;
    }

    protected void createMenuBar(Composite parent) {
        Shell shell = parent.getShell();
        Menu menuBar = new Menu(shell, SWT.BAR);
        shell.setMenuBar(menuBar);
        shell.setSize(250, 300);

        // file menu item
        MenuItem file = new MenuItem(menuBar, SWT.CASCADE);
        file.setText("File");
        Menu fileMenu = new Menu(shell, SWT.DROP_DOWN);
        file.setMenu(fileMenu);

        MenuItem exitItem = new MenuItem(fileMenu, SWT.PUSH);
        exitItem.setText("Exit");
        exitItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });

        // Help menu item
        MenuItem help = new MenuItem(menuBar, SWT.CASCADE);
        help.setText("Help");
        Menu helpMenu = new Menu(shell, SWT.DROP_DOWN);
        help.setMenu(helpMenu);

        MenuItem instructionsItem = new MenuItem(helpMenu, SWT.PUSH);
        instructionsItem.setText("Instructions");
        instructionsItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                openHelp("help/GfeServiceBackup/svcbu_instructions.html");
            }
        });
    }

    private void updateDialog() {
        if (activatedBackupSites.isEmpty()) {
            bannerLabel.setText(NOT_IN_BACKUP);
        } else {
            bannerLabel.setText(BACKING_UP);
        }
        sitesLabel.setText(StringUtils.join(activatedBackupSites, ", "));

        String[] items = activatedBackupSites.isEmpty() ? new String[] { "WWW" }
                : activatedBackupSites.toArray(new String[activatedBackupSites
                        .size()]);
        Arrays.sort(items);
        if (!importGridsCombo.getItems().equals(items)) {
            String selected = importGridsCombo.getText();
            importGridsCombo.setItems(items);
            if (activatedBackupSites.contains(selected)) {
                importGridsCombo.setText(selected);
            } else {
                importGridsButton.setEnabled(false);
            }
            importGridsCombo.setEnabled(!activatedBackupSites.isEmpty());
        }

        if (!startGFECombo.getItems().equals(items)) {
            String selected = startGFECombo.getText();
            startGFECombo.setItems(items);
            if (activatedBackupSites.contains(selected)) {
                startGFECombo.setText(selected);
            } else {
                startGFEButton.setEnabled(false);
            }
            startGFECombo.setEnabled(!activatedBackupSites.isEmpty());
        }

        if (!exportFailedGridsCombo.getItems().equals(items)) {
            String selected = exportFailedGridsCombo.getText();
            exportFailedGridsCombo.setItems(items);
            if (activatedBackupSites.contains(selected)) {
                exportFailedGridsCombo.setText(selected);
            } else {
                exportFailedGridsButton.setEnabled(false);
            }
            exportFailedGridsCombo.setEnabled(!activatedBackupSites.isEmpty());
        }

        if (!exitCombo.getItems().equals(items)) {
            String selected = exitCombo.getText();
            exitCombo.setItems(items);
            if (activatedBackupSites.contains(selected)) {
                exitCombo.setText(selected);
            } else {
                exitButton.setEnabled(false);
            }
            exitCombo.setEnabled(!activatedBackupSites.isEmpty());
        }

        List<String> backupSites = getBackupSites();
        items = backupSites.toArray(new String[backupSites.size()]);
        if (!enterCombo.getItems().equals(items)) {
            String selected = enterCombo.getText();
            enterCombo.setItems(items);
            if (backupSites.contains(selected)) {
                enterCombo.setText(selected);
            } else {
                enterButton.setEnabled(false);
                importGridsCheckBox.setEnabled(false);
                troubleShootingCheckBox.setEnabled(false);
                startGFECheckBox.setEnabled(false);
            }
            enterCombo.setEnabled(!backupSites.isEmpty());
        }
    }

    private List<String> getBackupSites() {

        ArrayList<String> backupSites = new ArrayList<String>(allSites);
        backupSites.removeAll(activatedBackupSites);
        return backupSites;
    }

    private void enterBackup(final String failedSite) {
        List<AbstractSbuTask> tasks = new ArrayList<>();
        // Step 1: Deactivate failedSite
        tasks.add(new SiteActivationSbuTask(failedSite,
                ACTIVATIONTYPE.DEACTIVATE, "deactivatingServer",
                "Deactivating GFE server for %s"));

        // Step 2: Clean Up failedSite
        tasks.add(new ServerRequestSbuTask("cleanup",
                "Removing %s's config and data", new CleanupSvcBuRequest(
                        failedSite)));

        // Step 3: Import Configuration
        tasks.add(new ServerRequestSbuTask("importConfiguration",
                "Requesting %s's configuration", new ImportConfRequest(
                        failedSite, troubleShootingCheckBox.getSelection())));

        // Step 3a: Await receipt of configuration
        tasks.add(new AwaitNotificationSbuTask(failedSite,
                "importConfiguration", IMPORT_TIMEOUT, getShell(),
                "awaitConfiguration", "Waiting to receive %s's configuration"));

        // Step 4: Activate Site
        tasks.add(new SiteActivationSbuTask(failedSite,
                ACTIVATIONTYPE.ACTIVATE, "activatingServer",
                "Activating GFE server for %s"));

        if (importGridsCheckBox.getSelection()) {
            // Step 5: (optional) Import Forecast Grids
            tasks.add(new ServerRequestSbuTask("importGrids",
                    "Requesting %s's forecast grids",
                    new ImportDigitalDataRequest(failedSite)));

            // Step 5a: Await receipt of forecast grids
            tasks.add(new AwaitNotificationSbuTask(failedSite, "importGrids",
                    IMPORT_TIMEOUT, getShell(), "awaitGrids",
                    "Waiting to receive %s's forecast grids"));
        }

        // Step 6: Start GFE for failed site
        if (startGFECheckBox.getSelection()) {
            tasks.add(new StartGfeSbuTask(failedSite, "startingGfe",
                    "Starting GFE session for %s"));
        }

        startJob(failedSite, tasks);
    }

    /**
     * @param failedSite
     * @param tasks
     */
    private void startJob(String failedSite, List<AbstractSbuTask> tasks) {
        ServiceBackupStatusDlg dlg = statusDialogs.get(failedSite);
        if ((dlg == null) || dlg.isDisposed()) {
            dlg = new ServiceBackupStatusDlg(getShell(), failedSite);
            statusDialogs.put(failedSite, dlg);
            dlg.setBlockOnOpen(false);
            final ServiceBackupStatusDlg finalDlg = dlg;
            dlg.setTasks(tasks);
            dlg.open();
            dlg.getShell().addDisposeListener(new DisposeListener() {
                @Override
                public void widgetDisposed(DisposeEvent e) {
                    statusDialogs.remove(finalDlg.getSite());
                }
            });
        } else {
            dlg.setTasks(tasks);
        }
    }

    protected void importGrids(String failedSite) {
        List<AbstractSbuTask> tasks = new ArrayList<>();

        tasks.add(new ServerRequestSbuTask("importGrids",
                "Requesting %s's forecast grids", new ImportDigitalDataRequest(
                        failedSite)));

        // Await receipt of forecast grids
        tasks.add(new AwaitNotificationSbuTask(failedSite, "importGrids",
                IMPORT_TIMEOUT, getShell(), "awaitGrids",
                "Waiting to receive %s's forecast grids"));

        startJob(failedSite, tasks);
    }

    private void startGFE(String failedSite) {
        List<AbstractSbuTask> tasks = new ArrayList<>();

        // Step 1: Start GFE for failed site
        tasks.add(new StartGfeSbuTask(failedSite, "startingGfe",
                "Starting GFE session for %s"));

        startJob(failedSite, tasks);
    }

    private void exitBackup(final String failedSite) {
        /*
         * Prompt user to verify whether they have contacted the failed site to
         * confirm they have received their forecast grids
         */
        boolean confirmed = MessageDialog
                .openQuestion(
                        getShell(),
                        "Confirm Exit",
                        "Have you contacted "
                                + failedSite
                                + " and confirmed they have received their forecast grids?\nYou must do this prior to exiting backup or the grids will be lost!");
        if (!confirmed) {
            return;
        }

        List<AbstractSbuTask> tasks = new ArrayList<>();

        // Step 1: Deactivate failedSite
        tasks.add(new SiteActivationSbuTask(failedSite,
                ACTIVATIONTYPE.DEACTIVATE, "deactivatingServer",
                "Deactivating GFE server for %s"));

        // Step 2: Clean Up failedSite
        tasks.add(new ServerRequestSbuTask("cleanup",
                "Removing %s's config and data", new CleanupSvcBuRequest(
                        failedSite)));

        startJob(failedSite, tasks);
    }

    private void exportGridsToCentralServer(String localSite) {
        List<AbstractSbuTask> tasks = new ArrayList<>();

        /*
         * TODO: this request just puts the grids out for the rsync to pickup.
         * We probably should be pushing these out immediately as this request
         * is likely to be used immediately before going into service backup.
         */
        tasks.add(new ServerRequestSbuTask("exportGrids",
                "Export %s's forecast grids to central server",
                new ExportGridsRequest(localSite, ExportGridsMode.MANUAL)));

        startJob(localSite, tasks);
    }

    private void exportConfigToCentralServer(String localSite) {
        List<AbstractSbuTask> tasks = new ArrayList<>();

        IUser user = UserController.getUserObject();
        if (user != null) {
            tasks.add(new ServerRequestSbuTask("exportConfig",
                    "Export %s's configuration to central server",
                    new ExportConfRequest(user, localSite)));

            startJob(localSite, tasks);
        } else {
            statusHandler.handle(Priority.PROBLEM, "USER CANNOT BE NULL!");
        }
    }

    private void exportGridsToFailedSite(String failedSite) {
        List<AbstractSbuTask> tasks = new ArrayList<>();

        // export grids to failed site
        tasks.add(new ServerRequestSbuTask("exportBkSiteGrids",
                "Export forecast grids to %s",
                new ExportDataToFailedSiteRequest(failedSite)));

        // export grids to central server
        tasks.add(new ServerRequestSbuTask("exportGrids",
                "Export %s's forecast grids to central server",
                new ExportFailedSiteDataToCCRequest(failedSite)));

        startJob(failedSite, tasks);
    }

    private void openHelp(String helpPath) {
        try {
            Bundle bundle = Activator.getDefault().getBundle();
            URL url = bundle.getEntry(helpPath);
            if (url == null) {
                throw new FileNotFoundException(helpPath);
            }
            url = FileLocator.toFileURL(url);
            Program.launch(url.toString());
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Error loading help "
                    + helpPath, e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#close()
     */
    @Override
    public boolean close() {
        boolean canClose = true;
        for (ServiceBackupStatusDlg dlg : this.statusDialogs.values()) {
            if (!dlg.close()) {
                canClose = false;
            }
        }

        if (canClose) {
            return super.close();
        }

        return false;
    }
}
