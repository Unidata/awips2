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

import java.io.IOException;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.gfe.request.GetKnownSitesRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetSbLockFilesRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetServiceBackupServerRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.site.requests.GetActiveSitesRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.RunProcess;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.gfe.dialogs.sbu.jobs.ServiceBackupJobManager;
import com.raytheon.viz.gfe.dialogs.sbu.jobs.SvcbuActivateSiteJob;
import com.raytheon.viz.gfe.dialogs.sbu.jobs.SvcbuCleanupJob;
import com.raytheon.viz.gfe.dialogs.sbu.jobs.SvcbuDeactivateSiteJob;
import com.raytheon.viz.gfe.dialogs.sbu.jobs.SvcbuExitJob;
import com.raytheon.viz.gfe.dialogs.sbu.jobs.SvcbuExportConfJob;
import com.raytheon.viz.gfe.dialogs.sbu.jobs.SvcbuExportDigitalDataJob;
import com.raytheon.viz.gfe.dialogs.sbu.jobs.SvcbuExportDigitalDataToFailedSiteJob;
import com.raytheon.viz.gfe.dialogs.sbu.jobs.SvcbuExportFailedSiteDataJob;
import com.raytheon.viz.gfe.dialogs.sbu.jobs.SvcbuImportConfJob;
import com.raytheon.viz.gfe.dialogs.sbu.jobs.SvcbuImportDataJob;
import com.raytheon.viz.gfe.dialogs.sbu.jobs.SvcbuStartGfeJob;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Service Backup Dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 4, 2011            randerso     Initial creation
 * Sep 19,2011 10955      rferrel      Use RunProcess
 * Oct 25, 2012 1287       rferrel     Code clean up for non-blocking dialog.
 * Nov 15,2012 15614	  jdynina      Added check for national center	 
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ServiceBackupDlg extends CaveJFACEDialog {

    private enum SVCBU_OP {
        ghg, wait, imprtGrdMode, imprtMode, exprtbksiteGrdToCSMode, exprtMode, exprtbksiteGrdMode, svcbuMode, exprtGrdMode, no_backup
    }

    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ServiceBackupDlg.class);

    public static final String NOTIFY_TOPIC = "edex.alerts.gfe";

    public static final String ACTIVATION_TOPIC = "edex.alerts.siteActivate";

    private int REFRESH_ID = IDialogConstants.CLIENT_ID;

    private String site;

    private boolean runningAsPrimary;

    private String failedSite;

    private Button ghg;

    private Button doImCon;

    private Button doImGrids;

    private Button doExClean;

    private Button doExCon;

    private Button doExGrids;

    private Button doExCentral;

    private Button startGFEBtn;

    private Button doExit;

    private Button doClean;

    private Font bigFont;

    private Color orange;

    private Color black;

    private Color gray;

    private Label bannerLabel;

    private ServiceBackupJobManager jobManager;

    private ProgressDlg progress;

    private Job updateJob;

    private boolean authorized;
    
    private boolean nationalCenter;

    private SVCBU_OP currentOperation = SVCBU_OP.no_backup;

    /**
     * @param parentShell
     */
    public ServiceBackupDlg(Shell parentShell) {
        super(parentShell);
        authorized = CheckPermissions.getAuthorization();
        this.site = LocalizationManager.getInstance().getCurrentSite();
        this.nationalCenter = CheckPermissions.isNationalCenter();
        this.runningAsPrimary = CheckPermissions.runningAsPrimary();
        if (!ServiceBackupJobManager.getInstance().isRunning()) {
            ServiceBackupJobManager.getInstance().start();
        }
        jobManager = ServiceBackupJobManager.getInstance();
        progress = new ProgressDlg(getShell());
        progress.setBlockOnOpen(false);
        updateJob = new Job("SvcbuUpdateJob") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                VizApp.runAsync(new Runnable() {

                    @Override
                    public void run() {
                        doRefresh();
                    }
                });
                this.schedule(200);
                return Status.OK_STATUS;
            }

        };
        updateJob.setSystem(true);
        updateJob.schedule();
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
        newShell.setText("Service Backup");
        newShell.addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                if (bigFont != null) {
                    bigFont.dispose();
                }

                if (orange != null) {
                    orange.dispose();
                }
            }
        });
    }

    @Override
    public boolean close() {
        updateJob.cancel();
        return super.close();
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
        Composite top = (Composite) super.createDialogArea(parent);
        createMenuBar(parent);

        Font origFont = parent.getFont();
        FontData[] fontData = origFont.getFontData();
        for (FontData fd : fontData) {
            fd.setHeight(fd.getHeight() + 7);
            fd.setStyle(SWT.BOLD);
        }
        bigFont = new Font(parent.getDisplay(), fontData);

        orange = new Color(parent.getDisplay(), RGBColors.getRGBColor("Orange"));
        black = getShell().getDisplay().getSystemColor(SWT.COLOR_BLACK);
        gray = parent.getBackground();

        Label title = new Label(top, SWT.CENTER);
        title.setText("Service Backup");
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        title.setLayoutData(layoutData);
        title.setFont(bigFont);

        bannerLabel = new Label(top, SWT.CENTER | SWT.BORDER);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        bannerLabel.setLayoutData(layoutData);

        Group group = new Group(top, SWT.BORDER);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        group.setLayoutData(layoutData);
        GridLayout layout = new GridLayout(1, true);
        group.setLayout(layout);

        ghg = new Button(group, SWT.RADIO);
        ghg.setText("GHG Service Backup");

        doImCon = new Button(group, SWT.RADIO);
        doImCon.setText("Import Configuration Data");

        doImGrids = new Button(group, SWT.RADIO);

        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        new Label(group, SWT.SEPARATOR | SWT.HORIZONTAL)
                .setLayoutData(layoutData);

        doExClean = new Button(group, SWT.RADIO);

        doExCon = new Button(group, SWT.RADIO);
        doExCon.setText("Export " + site
                + "'s Configuration to the Central Server");

        doExGrids = new Button(group, SWT.RADIO);
        doExGrids.setText("Export " + site
                + "'s Digital Forecast to the Central Server");

        doExCentral = new Button(group, SWT.RADIO);

        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        new Label(group, SWT.SEPARATOR | SWT.HORIZONTAL)
                .setLayoutData(layoutData);

        startGFEBtn = new Button(group, SWT.RADIO);
        startGFEBtn.setText("Start GFE");

        doExit = new Button(group, SWT.RADIO);
        doExit.setText("Exit Service Backup");

        doClean = new Button(group, SWT.RADIO);
        doClean.setText("Clean Up Service Backup");

        doRefresh();

        return top;
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

        MenuItem helpItem = new MenuItem(helpMenu, SWT.PUSH);
        helpItem.setText("Help");
        helpItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                try {
                    // DR#10955
                    RunProcess
                            .getRunProcess()
                            .exec("/usr/bin/firefox http://"
                                    + getServiceBackupServer()
                                    + ":8080/uEngineWeb/GfeServiceBackup/help/svcbu_help.html");
                } catch (IOException e1) {
                    statusHandler.error("Unable to open Help page!", e1);
                }
            }
        });

        MenuItem instructionsItem = new MenuItem(helpMenu, SWT.PUSH);
        instructionsItem.setText("Instructions");
        instructionsItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                try {
                    // DR#10955
                    RunProcess
                            .getRunProcess()
                            .exec("/usr/bin/firefox http://"
                                    + getServiceBackupServer()
                                    + ":8080/uEngineWeb/GfeServiceBackup/help/svcbu_instructions.html");
                } catch (IOException e1) {
                    statusHandler.error("Unable to open Help page!", e1);
                }
            }
        });

        MenuItem faqItem = new MenuItem(helpMenu, SWT.PUSH);
        faqItem.setText("FAQ");
        faqItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                try {
                    // DR#10955
                    RunProcess
                            .getRunProcess()
                            .exec("/usr/bin/firefox http://"
                                    + getServiceBackupServer()
                                    + ":8080/uEngineWeb/GfeServiceBackup/help/svcbu_faq.html");
                } catch (IOException e1) {
                    statusHandler.error("Unable to open Help page!", e1);
                }
            }
        });

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        // create OK and Cancel buttons by default
        createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL,
                true);
        createButton(parent, REFRESH_ID, "REFRESH", false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void buttonPressed(int buttonId) {
        if (IDialogConstants.OK_ID == buttonId) {
            okPressed();
        } else if (REFRESH_ID == buttonId) {
            doRefresh();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveJFACEDialog#okPressed()
     */
    @Override
    protected void okPressed() {
        if (ghg.getSelection()) {
            doGhgSbu();
        } else if (doImCon.getSelection()) {
            doImportConfig();
        } else if (doImGrids.getSelection()) {
            doImportGrids();
        } else if (doExClean.getSelection()) {
            doExportClean();
        } else if (doExCon.getSelection()) {
            doExportConfig();
        } else if (doExGrids.getSelection()) {
            doExportGrids();
        } else if (startGFEBtn.getSelection()) {
            doStartGFE();
        } else if (doExit.getSelection()) {
            doExit();
        } else if (doClean.getSelection()) {
            doClean(true);
        } else if (doExCentral.getSelection()) {
            doExportFailedSiteGrids();
        }
        doRefresh();
    }

    private void doGhgSbu() {
        switch (currentOperation) {
        case svcbuMode:
            displayMessage("You are currently backing up "
                    + this.failedSite.toUpperCase()
                    + ".\nYou cannot start emergency GFE");
            break;
        case ghg:
            displayMessage("You are already running emergency GFE.\nYou cannot start another GFE session");
            break;
        case wait:
            displayMessage("Waiting for "
                    + this.failedSite.toUpperCase()
                    + "'s server to start.\nYou cannot start emergency GFE at this time");
            break;
        case imprtGrdMode:
            displayMessage("Importing "
                    + this.failedSite.toUpperCase()
                    + "'s digital data.\nYou cannot start emergency GFE at this time");
            break;
        case imprtMode:
            displayMessage("Importing a failed site's configuration.\nYou cannot start emergency GFE at this time");
            break;
        case exprtbksiteGrdMode:
            displayMessage("Exporting grids back to "
                    + this.failedSite.toUpperCase()
                    + ".\nYou cannot start emergency at this time");
            break;
        case exprtbksiteGrdToCSMode:
            displayMessage("Exporting "
                    + this.failedSite.toUpperCase()
                    + "'s grids to the central server.\nYou cannot start emergency GFE at this time");
            break;
        default:
            String failedSite = getFailedSite();
            if (failedSite != null) {
                this.failedSite = failedSite;
                jobManager.addJob(new SvcbuDeactivateSiteJob(failedSite,
                        this.site));
                jobManager.addJob(new SvcbuImportConfJob(site, failedSite,
                        progress));
                jobManager.addJob(new SvcbuActivateSiteJob(failedSite,
                        this.site));
                jobManager.addJob(new SvcbuStartGfeJob(failedSite, this.site));

            }
        }
    }

    private void doImportConfig() {

        switch (currentOperation) {
        case svcbuMode:
            displayMessage("" + this.failedSite.toUpperCase()
                    + "'s configuration has already been imported");
            break;
        case wait:
            displayMessage("Waiting for "
                    + this.failedSite.toUpperCase()
                    + "'s server to start.\nYou cannot import configuration at this time");
            break;
        case imprtGrdMode:
            displayMessage("Importing "
                    + this.failedSite.toUpperCase()
                    + "'s digital data.\nYou cannot import configuration at this time");
            break;
        case imprtMode:
            displayMessage("Importing a failed site's configuration.\nYou cannot start another import at this time");
            break;
        case exprtMode:
            displayMessage("Exporting your configuration to the central server.\nYou cannot import configuration at this time");
            break;
        case exprtbksiteGrdMode:
            displayMessage("Exporting grids back to "
                    + this.failedSite.toUpperCase()
                    + ".\nYou cannot import configuration at this time");
            break;
        case exprtbksiteGrdToCSMode:
            displayMessage("Exporting "
                    + this.failedSite.toUpperCase()
                    + "'s grids to the central server.\nYou cannot import configuration at this time");
            break;
        case ghg:
            displayMessage("Running emergency GFE.\nYou cannot import configuration at this time");
            break;
        default:
            QueryOptionsDlg dlg = new QueryOptionsDlg(getShell(), true);
            dlg.setBlockOnOpen(true);
            if (dlg.open() == Window.OK) {
                boolean importGrids = dlg.importGrids();
                boolean startGFE = dlg.startGFE();
                String failedSite = getFailedSite();
                jobManager.addJob(new SvcbuDeactivateSiteJob(failedSite,
                        this.site));
                jobManager.addJob(new SvcbuImportConfJob(site, failedSite,
                        progress));
                jobManager.addJob(new SvcbuActivateSiteJob(failedSite,
                        this.site));
                if (importGrids) {
                    jobManager.addJob(new SvcbuImportDataJob(site, failedSite,
                            progress));
                }
                if (startGFE) {
                    jobManager.addJob(new SvcbuStartGfeJob(failedSite,
                            this.site));
                }
            }
        }
    }

    private void doImportGrids() {
        switch (currentOperation) {
        case no_backup:
            displayMessage("You are not in Service Backup Mode.\nCannot import digital data.");
            break;
        case imprtMode:
            displayMessage("Importing a failed site's configuration.\nCannot import digital data.");
            break;
        case imprtGrdMode:
            displayMessage("Importing " + this.failedSite.toUpperCase()
                    + "'s digital data.\nCannot start another import process.");
            break;
        case wait:
            displayMessage("Waiting for " + this.failedSite.toUpperCase()
                    + "'s server to start.\nCannot import digital data");
            break;
        case exprtMode:
            displayMessage("Exporting your configuration to the central server.\nCannot import digital data.");
            break;
        case exprtbksiteGrdMode:
            displayMessage("Exporting grids back to "
                    + this.failedSite.toUpperCase()
                    + ".\nCannot import digital data.");
            break;
        case exprtbksiteGrdToCSMode:
            displayMessage("Exporting "
                    + this.failedSite.toUpperCase()
                    + "'s grids to the central server.\nCannot import digital data.");
            break;
        case ghg:
            displayMessage("Running emergency GFE.\nCannot import digital data.");
            break;
        default:
            QueryOptionsDlg dlg = new QueryOptionsDlg(getShell(), false);
            dlg.setBlockOnOpen(true);
            if (dlg.open() == Window.OK) {
                boolean startGFE = dlg.startGFE();

                jobManager.addJob(new SvcbuImportDataJob(site, this.failedSite,
                        progress));
                if (startGFE) {
                    jobManager.addJob(new SvcbuStartGfeJob(this.failedSite,
                            this.site));
                }
            }
        }
    }

    private void doExportClean() {
        switch (currentOperation) {

        case no_backup:
            displayMessage("You are not in Service Backup Mode.\nCannot export digital data.");
            break;
        case imprtMode:
            displayMessage("Importing a failed site's configuration.\nCannot export digital data.");
            break;
        case imprtGrdMode:
            displayMessage("Importing " + this.failedSite.toUpperCase()
                    + "'s digital data.\nCannot export digital data.");
            break;
        case wait:
            displayMessage("Waiting for " + this.failedSite.toUpperCase()
                    + "'s server to start.\nCannot export digital data");
            break;
        case exprtMode:
            displayMessage("Exporting your configuration to the central server.\nCannot export digital data");
            break;
        case exprtbksiteGrdMode:
            displayMessage("Exporting grids back to "
                    + this.failedSite.toUpperCase()
                    + ".\nCannot export digital data");
            break;
        case exprtbksiteGrdToCSMode:
            displayMessage("Exporting "
                    + this.failedSite.toUpperCase()
                    + "'s grids to the central server.\nCannot export digital data");
            break;
        case ghg:
            displayMessage("Running emergency GFE.\nCannot export digital data");
            break;
        default:
            MessageDialog.openInformation(
                    getShell(),
                    "notification",
                    "Exporting Digital Data Back To "
                            + this.failedSite.toUpperCase());
            jobManager.addJob(new SvcbuExportDigitalDataToFailedSiteJob(
                    this.site, this.failedSite));

        }

    }

    private void doExportConfig() {
        if (currentOperation.equals(SVCBU_OP.exprtMode)) {
            displayMessage("You are already exporting your configuration to the central server.\nCannot start another export at this time.");
        } else if (currentOperation.equals(SVCBU_OP.no_backup)) {
            jobManager.addJob(new SvcbuExportConfJob(this.site));
        }
    }

    private void doExportGrids() {
        switch (currentOperation) {
        case exprtGrdMode:
            displayMessage("You are already exporting your grids to\nthe central server.  You cannot start\nanother export at this time.");
            break;
        case exprtbksiteGrdToCSMode:
            displayMessage("You are already exporting "
                    + this.failedSite.toUpperCase()
                    + "'s grids to\nthe central server.  You cannot start\nanother export at this time.");
            break;
        }
        jobManager.addJob(new SvcbuExportDigitalDataJob(this.site));
    }

    private void doExportFailedSiteGrids() {
        switch (currentOperation) {
        case no_backup:
            displayMessage("You are not in Service Backup Mode.\nCannot export digital data to the central server.");
            break;
        case imprtMode:
            displayMessage("Importing a failed site's configuration.\nCannot export digital data to the central server.");
            break;
        case imprtGrdMode:
            displayMessage("Importing "
                    + this.failedSite.toUpperCase()
                    + "'s digital data.\nCannot export digital data to the central server.");
            break;
        case wait:
            displayMessage("Waiting for "
                    + this.failedSite.toUpperCase()
                    + "'s server to start.\nCannot export digital data to the central server");
            break;
        case exprtMode:
            displayMessage("Exporting your configuration to the central server.\nCannot export digital data to the central server");
            break;
        case exprtbksiteGrdMode:
            displayMessage("Exporting grids back to "
                    + this.failedSite.toUpperCase()
                    + ".\nCannot export digital data to the central server");
            break;
        case exprtbksiteGrdToCSMode:
            displayMessage("Already exporting "
                    + this.failedSite.toUpperCase()
                    + "'s grids to the central server.\nCannot start another export.");
            break;
        case ghg:
            displayMessage("Running emergency GFE.\nCannot export digital data to the central server");
            break;
        default:
            MessageDialog.openInformation(getShell(), "notification",
                    "Exporting " + this.failedSite.toUpperCase()
                            + "'s Digital Data To The Central Server");
            jobManager.addJob(new SvcbuExportFailedSiteDataJob(this.site,
                    this.failedSite));
        }

    }

    private void doStartGFE() {
        GetActiveSitesRequest request = new GetActiveSitesRequest();
        String[] activeSites = null;
        String selectedSite = null;
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
        if (activeSites.length == 1) {
            selectedSite = activeSites[0];
        } else {
            ChooseDomainDlg dlg = new ChooseDomainDlg(getShell());
            dlg.setBlockOnOpen(true);
            if (dlg.open() == Window.OK) {
                selectedSite = dlg.getSelectedSite();
            }
        }
        if (selectedSite != null) {
            jobManager.addJob(new SvcbuStartGfeJob(selectedSite, this.site));
        }
    }

    private void doExit() {
        switch (currentOperation) {
        case no_backup:
            displayMessage("You must be in Service Backup Mode to Exit Service Backup");
            break;
        case imprtMode:
            displayMessage("Importing a failed site's configuration.\nCannot exit service backup.");
            break;
        case imprtGrdMode:
            displayMessage("Importing " + this.failedSite.toUpperCase()
                    + "'s digital data.\nCannot exit service backup.");
            break;
        case wait:
            displayMessage("Waiting for " + this.failedSite.toUpperCase()
                    + "'s server to start.\nCannot exit service backup.");
            break;
        case exprtbksiteGrdMode:
            displayMessage("Exporting grids back to "
                    + this.failedSite.toUpperCase()
                    + ".\nCannot exit service backup.");
            break;
        case exprtbksiteGrdToCSMode:
            displayMessage("Exporting "
                    + this.failedSite.toUpperCase()
                    + "'s grids to the central server.\nCannot exit service backup.");
            break;
        default:
            doRefresh();
            if (confirmExit()) {
                doClean(false);
                jobManager.addJob(new SvcbuExitJob(this, this.site));
            }
        }

    }

    private void doClean(boolean showMessage) {
        switch (currentOperation) {
        case wait:
            displayMessage("Waiting for " + this.failedSite.toUpperCase()
                    + "'s server to start.\nCannot clean up");
            break;
        case imprtGrdMode:
            displayMessage("Importing " + this.failedSite.toUpperCase()
                    + "'s digital data.\nCannot clean up");
            break;
        case exprtbksiteGrdMode:
            displayMessage("Exporting grids back to "
                    + this.failedSite.toUpperCase() + ".\nCannot clean up");
            break;
        case exprtbksiteGrdToCSMode:
            displayMessage("Exporting " + this.failedSite.toUpperCase()
                    + "'s grids to the central server.\nCannot clean up.");
            break;
        case ghg:
            displayMessage("Running emergency GFE.\nCannot clean up.");
            break;
        default:
            doRefresh();
            String theFailedSite = this.failedSite;
            if (this.failedSite == null) {
                theFailedSite = getFailedSite();
            }
            if (theFailedSite != null) {
                if (showMessage) {
                    MessageDialog.openInformation(getShell(), "notification",
                            "Cleaning up " + theFailedSite.toUpperCase()
                                    + "'s Configuration");
                }
                jobManager.addJob(new SvcbuDeactivateSiteJob(theFailedSite,
                        this.site));
                jobManager
                        .addJob(new SvcbuCleanupJob(this.site, theFailedSite));
            }
        }

    }

    @SuppressWarnings("unchecked")
    private String getFailedSite() {
        try {
            GetKnownSitesRequest request = new GetKnownSitesRequest();
            request.setSiteID(this.site);
            List<String> knownSites = ((ServerResponse<List<String>>) ThriftClient
                    .sendRequest(request)).getPayload();
            FailedSiteDlg dlg = new FailedSiteDlg(getShell(), site, knownSites);
            dlg.setBlockOnOpen(true);
            String failedSite = null;
            if (dlg.open() == Window.OK) {
                failedSite = dlg.getFailedSite();
            }
            return failedSite;
        } catch (VizException e) {
            statusHandler.error("Error getting known sites!", e);
        }
        return null;

    }

    private boolean confirmExit() {
        MessageBox mb = new MessageBox(getShell(), SWT.YES | SWT.NO);
        mb.setText("Exit Service Backup");
        mb.setMessage("Are you sure you want to remove " + this.failedSite
                + "'s configuration and digital data from your system?");
        int close = mb.open();
        if (close == SWT.YES) {
            return true;
        }

        return false;
    }

    @SuppressWarnings("unchecked")
    private void doRefresh() {

        GetSbLockFilesRequest request = new GetSbLockFilesRequest();
        List<String> lockFiles = null;
        try {
            ServerResponse<List<String>> response = (ServerResponse<List<String>>) ThriftClient
                    .sendRequest(request);
            lockFiles = response.getPayload();
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error processing site activation request", e);

        }

        String theFailedSite = null;
        if (this.failedSite != null) {
            this.failedSite.toLowerCase();
        }
        boolean lock_file = lockFiles.contains(theFailedSite + "svcbuMode");
        if (!lock_file) {
            for (String file : lockFiles) {
                if (file.contains("svcbuMode")) {
                    this.failedSite = file.replace("svcbuMode", "")
                            .toUpperCase();
                    theFailedSite = this.failedSite.toLowerCase();
                    lock_file = true;
                    break;
                } else {
                    this.failedSite = null;
                }
            }
        }
        if (lockFiles.isEmpty()) {
            this.failedSite = null;
        }
        boolean ghg_lock_file = false;
        boolean wait_lock_file = lockFiles.contains(theFailedSite + "waitMode");
        boolean export_lock_file = lockFiles.contains(this.site.toLowerCase()
                + "exportGrids")
                || lockFiles.contains(theFailedSite + "exportGridsCron");
        boolean grd_lock_file = lockFiles.contains("importGrids")
                || lockFiles.contains("procGrids");
        boolean imp_lock_file = lockFiles.contains("importConfiguration");
        boolean cs_lock_file = lockFiles.contains(theFailedSite
                + "exportBksiteGridsCS");
        boolean excon_lock_file = lockFiles.contains(this.site.toLowerCase()
                + "exportConfig");
        boolean bksite_lock_file = lockFiles.contains(this.site.toLowerCase()
                + "exportBkSiteGrids");

        if (ghg_lock_file) {
            updateBanner("CURRENTLY RUNNING\nEMERGENCY GFE", bigFont, black,
                    orange);
            ghg.setEnabled(false);
            doImCon.setEnabled(false);
            doImCon.setText("Import Configuration Data");
            doImGrids.setEnabled(false);
            doImGrids.setText("Import Failed Site's Digital Data");
            doExClean.setEnabled(false);
            doExClean.setText("Export Grids to Failed Site");
            doExit.setEnabled(false);
            doExit.setText("Exit Service Backup");
            doExCon.setEnabled(false);
            doClean.setEnabled(false);
            doExCentral.setEnabled(false);
            doExCentral
                    .setText("Export a Failed Site's Digital Data to the Central Server");

            if (export_lock_file) {
                doExGrids.setEnabled(false);
                doExGrids.setText("Currently Exporting "
                        + this.site.toUpperCase() + "'s Digital Data");
            } else {
                doExGrids.setEnabled(true);
                doExGrids.setText("Export " + this.failedSite
                        + "'s Digital Forecast to the Central Server");
            }
            currentOperation = SVCBU_OP.ghg;
        } else if (wait_lock_file) {
            if (lock_file) {
                updateBanner("WAITING FOR " + this.failedSite
                        + "'S SERVER\nTO START", bigFont, black, orange);
            } else {
                updateBanner("WAITING FOR A FAILED SITE'S SERVER\nTO START",
                        bigFont, black, orange);
            }

            ghg.setEnabled(false);
            doImCon.setEnabled(false);
            doImCon.setText("Import Configuration Data");
            doImGrids.setEnabled(false);
            doImGrids.setText("Import Failed Site's Digital Data");
            doExClean.setEnabled(false);
            doExClean.setText("Export Grids to Failed Site");
            doExit.setEnabled(false);
            doExit.setText("Exit Service Backup");
            doExCon.setEnabled(false);
            doClean.setEnabled(false);
            doExCentral.setEnabled(false);
            doExCentral
                    .setText("Export a Failed Site's Digital Data to the Central Server");

            if (export_lock_file) {
                doExGrids.setEnabled(false);
                doExGrids.setText("Currently Exporting "
                        + this.site.toUpperCase() + "'s Digital Data");
            } else {
                doExGrids.setEnabled(true);
                doExGrids.setText("Export " + this.failedSite
                        + "'s Digital Forecast to the Central Server");
            }
            currentOperation = SVCBU_OP.wait;
        } else if (grd_lock_file) {
            if (lock_file) {
                updateBanner("YOU ARE CURRENTLY IMPORTING\n" + this.failedSite
                        + "'S DIGITAL DATA", bigFont, black, orange);
                ;
            } else {
                updateBanner(
                        "YOU ARE CURRENTLY IMPORTING A\nFAILED SITE'S DIGITAL DATA",
                        bigFont, black, orange);
            }
            ghg.setEnabled(false);
            doImCon.setEnabled(false);
            doImCon.setText("Import Configuration Data");
            doImGrids.setEnabled(false);
            doImGrids.setText("Import Failed Site's Digital Data");
            doExClean.setEnabled(false);
            doExClean.setText("Export Grids to Failed Site");
            doExit.setEnabled(false);
            doExit.setText("Exit Service Backup");
            doExCon.setEnabled(false);
            doClean.setEnabled(false);
            doExCentral.setEnabled(false);
            doExCentral.setText("Export " + this.failedSite
                    + "'s Digital Data to the Central Server");

            if (export_lock_file) {
                doExGrids.setEnabled(false);
                doExGrids.setText("Currently Exporting "
                        + this.site.toUpperCase() + "'s Digital Data");
            } else {
                doExGrids.setEnabled(true);
                doExGrids.setText("Export " + this.site.toUpperCase()
                        + "'s Digital Forecast to the Central Server");
            }
            currentOperation = SVCBU_OP.imprtGrdMode;
        } else if (imp_lock_file) {
            updateBanner(
                    "YOU ARE CURRENTLY IMPORTING A\nFAILED SITE'S CONFIGURATION",
                    bigFont, black, orange);
            ghg.setEnabled(false);
            doImCon.setEnabled(false);
            doImCon.setText("Import Configuration Data");
            doImGrids.setEnabled(false);
            doImGrids.setText("Import Failed Site's Digital Data");
            doExClean.setEnabled(false);
            doExClean.setText("Export Grids to Failed Site");
            doExit.setEnabled(false);
            doExit.setText("Exit Service Backup");
            doExCon.setEnabled(false);
            doClean.setEnabled(true);
            doExCentral.setEnabled(false);
            doExCentral
                    .setText("Export a Failed Site's Digital Data to the Central Server");
            if (export_lock_file) {
                doExGrids.setEnabled(false);
                doExGrids.setText("Currently Exporting "
                        + this.site.toUpperCase() + "'s Digital Data");
            } else {
                doExGrids.setEnabled(true);
                doExGrids.setText("Export " + this.site.toUpperCase()
                        + "'s Digital Forecast to the Central Server");
            }
            currentOperation = SVCBU_OP.imprtMode;
        } else if (cs_lock_file) {
            updateBanner("YOU ARE CURRENTLY EXPORTING " + this.failedSite
                    + "'S\nGRIDS TO THE CENTRAL SERVER", bigFont, black, orange);
            ghg.setEnabled(false);
            doImCon.setEnabled(false);
            doImGrids.setEnabled(false);
            doImGrids.setText("Import " + this.failedSite + "'s Digital Data");
            doExClean.setEnabled(false);
            doExClean.setText("Export Grids to " + this.failedSite + "");
            doExit.setEnabled(false);
            doExit.setText("Exit Service Backup");
            doExCon.setEnabled(false);
            doClean.setEnabled(false);
            doExCentral.setEnabled(false);
            doExCentral.setText("Export " + this.failedSite
                    + "'s Digital Data to the Central Server");
            if (export_lock_file) {
                doExGrids.setEnabled(false);
                doExGrids.setText("Currently Exporting " + this.site
                        + "'s Digital Data");
            } else {
                doExGrids.setEnabled(true);
                doExGrids.setText("Export " + this.site
                        + "'s Digital Forecast to the Central Server");
            }
            currentOperation = SVCBU_OP.exprtbksiteGrdToCSMode;
        } else if (excon_lock_file) {
            updateBanner(
                    "YOU ARE CURRENTLY EXPORTING YOUR\nCONFIGURATION TO THE CENTRAL SERVER",
                    bigFont, black, orange);
            ghg.setEnabled(true);
            doImCon.setEnabled(false);
            doImGrids.setEnabled(false);
            doImGrids.setText("Import Failed Site's Digital Data");
            doExClean.setEnabled(false);
            doExClean.setText("Export Grids to Failed Site");
            doExit.setEnabled(false);
            doExit.setText("Exit Service Backup");
            doExCon.setEnabled(false);
            doClean.setEnabled(true);
            doExCentral.setEnabled(false);
            doExCentral
                    .setText("Export a Failed Site's Digital Data to the Central Server");
            if (export_lock_file) {
                doExGrids.setEnabled(false);
                doExGrids.setText("Currently Exporting " + this.site
                        + "'s Digital Data");
            } else {
                doExGrids.setEnabled(true);
                doExGrids.setText("Export " + this.site
                        + "'s Digital Forecast to the Central Server");
            }
            currentOperation = SVCBU_OP.exprtMode;
        } else if (bksite_lock_file) {
            if (lock_file) {
                updateBanner("EXPORTING GRIDS BACK TO " + this.failedSite + "",
                        bigFont, black, orange);
            } else {
                updateBanner("EXPORTING GRIDS BACK TO A FAILED SITE", bigFont,
                        black, orange);
            }
            ghg.setEnabled(false);
            doImCon.setEnabled(false);
            doImGrids.setEnabled(false);
            doImGrids.setText("Import " + this.failedSite + "'s Digital Data");
            doExClean.setEnabled(false);
            doExClean.setText("Export Grids to " + this.failedSite + "");
            doExit.setEnabled(false);
            doExit.setText("Exit Service Backup for " + this.failedSite + "");
            doExCon.setEnabled(false);
            doClean.setEnabled(false);
            doExCentral.setEnabled(false);
            doExCentral.setText("Export " + this.failedSite
                    + "'s Digital Data to the Central Server");
            if (export_lock_file) {
                doExGrids.setEnabled(false);
                doExGrids.setText("Currently Exporting " + this.site
                        + "'s Digital Data");
            } else {
                doExGrids.setEnabled(true);
                doExGrids.setText("Export " + this.site
                        + "'s Digital Forecast to the Central Server");
            }
            currentOperation = SVCBU_OP.exprtbksiteGrdMode;
        } else if (lock_file) {
            doExCon.setEnabled(false);
            ghg.setEnabled(false);
            doImCon.setEnabled(true);
            doImGrids.setEnabled(true);
            doImGrids.setText("Import " + this.failedSite + "'s Digital Data");
            doExClean.setEnabled(true);
            doExClean.setText("Export Grids to " + this.failedSite + "");
            doExit.setEnabled(true);
            doExit.setText("Exit Service Backup for " + this.failedSite + "");
            doClean.setEnabled(true);
            doExCentral.setEnabled(true);
            doExCentral.setText("Export " + this.failedSite
                    + "'s Digital Data to the Central Server");
            if (export_lock_file) {
                doExGrids.setEnabled(false);
                doExGrids.setText("Currently Exporting " + this.site
                        + "'s Digital Data");
            } else {
                doExGrids.setEnabled(true);
                doExGrids.setText("Export " + this.site
                        + "'s Digital Forecast to the Central Server");
            }
            updateBanner("YOU ARE CURRENTLY BACKING UP\n" + this.failedSite,
                    bigFont, black, orange);
            currentOperation = SVCBU_OP.svcbuMode;
        } else if (export_lock_file) {
            updateBanner(
                    "YOU ARE CURRENTLY EXPORTING YOUR\nGRIDS TO THE CENTRAL SERVER",
                    bigFont, black, orange);
            ghg.setEnabled(true);
            doImCon.setEnabled(true);
            doImCon.setText("Import Configuration Data");
            doImGrids.setEnabled(false);
            doImGrids.setText("Import Failed Site's Digital Data");
            doExClean.setEnabled(false);
            doExClean.setText("Export Grids to Failed Site");
            doExit.setEnabled(false);
            doExit.setText("Exit Service Backup");
            doExCon.setEnabled(true);
            doClean.setEnabled(true);
            doExCentral.setEnabled(false);
            doExCentral
                    .setText("Export a Failed Site's Digital Data to the Central Server");
            doExGrids.setEnabled(false);
            doExGrids.setText("Currently Exporting " + this.site
                    + "'s Digital Data");
            currentOperation = SVCBU_OP.exprtGrdMode;
        } else {
            ghg.setEnabled(true);
            doImCon.setEnabled(true);
            doImCon.setText("Import Configuration Data");
            doImGrids.setEnabled(false);
            doImGrids.setText("Import Failed Site's Digital Data");
            doExClean.setEnabled(false);
            doExClean.setText("Export Grids to Failed Site");
            doExCon.setEnabled(true);
            doExit.setEnabled(false);
            doExit.setText("Exit Service Backup");
            doClean.setEnabled(true);
            doExCentral.setEnabled(false);
            doExCentral
                    .setText("Export a Failed Site's Digital Data to the Central Server");
            doExGrids.setEnabled(true);
            doExGrids.setText("Export " + this.site
                    + "'s Digital Forecast to the Central Server");
            updateBanner("YOU ARE NOT IN BACKUP MODE", getShell().getParent()
                    .getFont(), black, gray);
            currentOperation = SVCBU_OP.no_backup;
        }

        if (!authorized) {
            doExCon.setEnabled(false);
            doExCon.setText("Export " + this.site.toUpperCase()
                    + "'s Configuration not available for user "
                    + UserController.getUserObject().uniqueId());
        }

        if ((!runningAsPrimary) & (!nationalCenter)) {
            doExCon.setEnabled(false);
        }
    }

    private void updateBanner(String text, Font font, Color foregroundColor,
            Color backgroundColor) {
        bannerLabel.setFont(font);
        bannerLabel.setForeground(foregroundColor);
        bannerLabel.setBackground(backgroundColor);
        bannerLabel.setText(text);
        this.getShell().pack(true);
    }

    private String getServiceBackupServer() {
        GetServiceBackupServerRequest request = new GetServiceBackupServerRequest();
        try {
            String obj = (String) ThriftClient.sendRequest(request);
            return obj;
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error processing get service backup server request", e);
        }
        return "dx3";
    }

    private void displayMessage(String msg) {
        MessageDialog.openWarning(getShell(), "Warning", msg);
    }
}
