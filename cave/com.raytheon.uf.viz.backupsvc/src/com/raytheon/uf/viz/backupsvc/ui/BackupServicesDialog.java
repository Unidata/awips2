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
package com.raytheon.uf.viz.backupsvc.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.backupsvc.constants.VizBackupServicesConstants;
import com.raytheon.uf.viz.backupsvc.notification.BackupServiceNotificationSubscriber;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Backup Services Dialog
 *
 * This dialog allows a ITOs or Focal Points to control the localization files
 * that are sent to backup sites. It also allows backup sites to accept/reject
 * on the receiving end.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------
 * Mar 02, 2021  84466    Robert.Blum     Initial creation
 * Apr 12, 2021  84467    Robert.Blum     Implement outgoing/incoming tabs.
 * May 03, 2021  84639    Robert.Blum     Added the Help menu.
 * May 19, 2021  84466    Robert.Blum     Removed log menu as no immediate
 *                                        tickets will implement it.
 * May 19, 2021  84466    Robert.Blum     Set heightHint so dialog does not scale
 *                                        to the amount of data in the db.
 * May 25, 2021  84644    Gang Chen       Set up the initial default size
 * Jul 01, 2021  93517    Amanuel Challa  Made outgoingTab/incomingTab a global variable and
 *                                        added a new field that is an instance of AbstractBackupServicesDialogTab
 *                                        when BackupServicesFilterDialog is Initialized
 * Jun 28, 2021  84643    Gang Chen       Add Synchronization notification module
 * Jul 29, 2021  92922    Robert.Blum     Fix dialog sizing issues.
 * Aug 12, 2021  84654    Robert.Blum     Cleaned up constant files.
 * Sep 20, 2021  96170    Lisa.Singh      Made dialog independent of CAVE perspectives.
 *
 * </pre>
 *
 * @author Robert.Blum
 */

public class BackupServicesDialog extends CaveSWTDialog {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(this.getClass());

    BackupServicesHelpDialog hlpDialog;

    BackupServicesFilterDialog filterDialogOutgoing, filterDialogIncoming;

    AbstractBackupServicesDialogTab outgoingTab, incomingTab;

    BackupServicesSettingsDialog settingDialog;

    /**
     * Constructor
     *
     * @param parent
     */
    public BackupServicesDialog(Shell parent) {
        super(parent, SWT.SHELL_TRIM | SWT.RESIZE, CAVE.PERSPECTIVE_INDEPENDENT);
        setText("Backup Services");
    }

    @Override
    protected void initializeComponents(Shell shell) {
        shell.setLayout(new GridLayout(1, false));
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, false, false);
        shell.setLayoutData(gridData);

        createMenuBar(shell);

        Composite mainComp = new Composite(shell, SWT.NONE);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        mainComp.setLayoutData(gridData);
        mainComp.setLayout(new GridLayout(1, true));

        TabFolder tabFolder = new TabFolder(mainComp, SWT.BORDER);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        tabFolder.setLayoutData(gridData);

        outgoingTab = new OutgoingBackupServicesTab(tabFolder, SWT.NONE);
        incomingTab = new IncomingBackupServicesTab(tabFolder, SWT.NONE);

        // Initiate the instance for backup service notification subscriber
        BackupServiceNotificationSubscriber.getInstance(outgoingTab,
                incomingTab);

        /* Create buttons */
        Composite buttonComp = new Composite(mainComp, SWT.NONE);
        gridData = new GridData(SWT.FILL, SWT.CENTER, true, false);
        buttonComp.setLayoutData(gridData);
        buttonComp.setLayout(new GridLayout(1, true));

        Button closeButton = new Button(buttonComp, SWT.PUSH);
        gridData = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        closeButton.setLayoutData(gridData);
        closeButton.setText("Close");
        closeButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                BackupServiceNotificationSubscriber
                        .removeNotificationObserver();
                close();
            }
        });
    }

    /**
     * Creates the menu bar for the Backup Services dialog.
     *
     * @param shell
     *            Shell to be used as the parent
     */
    private void createMenuBar(Shell shell) {
        Menu menuBar = new Menu(shell, SWT.BAR);

        MenuItem settingsMenuHeader = new MenuItem(menuBar, SWT.CASCADE);
        settingsMenuHeader.setText("&Settings");

        Menu settingMenu = new Menu(shell, SWT.DROP_DOWN);
        settingsMenuHeader.setMenu(settingMenu);

        MenuItem settingMenuItem = new MenuItem(settingMenu, SWT.PUSH);
        settingMenuItem.setText("View Backup Services Config");

        settingMenuItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (settingDialog == null || settingDialog.isDisposed()) {
                    settingDialog = new BackupServicesSettingsDialog(
                            getShell());

                    settingDialog.open();
                } else {
                    settingDialog.bringToTop();
                }
            }
        });

        MenuItem filterMenuHeader = new MenuItem(menuBar, SWT.CASCADE);
        filterMenuHeader.setText("&Filter");

        Menu filterMenu = new Menu(shell, SWT.DROP_DOWN);
        filterMenuHeader.setMenu(filterMenu);

        MenuItem filterMenuItem1 = new MenuItem(filterMenu, SWT.PUSH);
        filterMenuItem1.setText(
                VizBackupServicesConstants.OUTGOING_FILTER_DIALOG_MENU_TEXT);
        filterMenuItem1.addSelectionListener(outgoingFilterDlgListener);

        MenuItem filterMenuItem2 = new MenuItem(filterMenu, SWT.PUSH);
        filterMenuItem2.setText(
                VizBackupServicesConstants.INCOMING_FILTER_DIALOG_MENU_TEXT);
        filterMenuItem2.addSelectionListener(incomingFilterDlgListener);

        MenuItem helpMenuHeader = new MenuItem(menuBar, SWT.CASCADE);
        helpMenuHeader.setText("&Help");

        Menu helpMenu = new Menu(shell, SWT.DROP_DOWN);
        helpMenuHeader.setMenu(helpMenu);

        MenuItem helpMenuItem = new MenuItem(helpMenu, SWT.PUSH);
        helpMenuItem.setText("About Backup Services");

        helpMenuItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (hlpDialog == null || hlpDialog.isDisposed()) {
                    hlpDialog = new BackupServicesHelpDialog(getShell());
                    hlpDialog.open();
                } else {
                    hlpDialog.bringToTop();
                }
            }
        });

        // TODO - Populate drop down menus
        shell.setMenuBar(menuBar);
    }

    /*
     * Filter dialog menu listeners
     */
    private SelectionAdapter outgoingFilterDlgListener = new SelectionAdapter() {
        @Override
        public void widgetSelected(SelectionEvent e) {
            if ((filterDialogIncoming != null)
                    && filterDialogIncoming.isOpen()) {
                filterDialogIncoming.close();
            }
            if (filterDialogOutgoing == null
                    || filterDialogOutgoing.isDisposed()) {
                filterDialogOutgoing = new BackupServicesFilterDialog(
                        getShell(),
                        VizBackupServicesConstants.OUTGOING_FILTER_DIALOG_MENU_TEXT,
                        true, outgoingTab);
                filterDialogOutgoing.open();
            } else {
                filterDialogOutgoing.bringToTop();
            }
        }
    };

    private SelectionAdapter incomingFilterDlgListener = new SelectionAdapter() {
        @Override
        public void widgetSelected(SelectionEvent e) {
            if ((filterDialogOutgoing != null)
                    && filterDialogOutgoing.isOpen()) {
                filterDialogOutgoing.close();
            }
            if (filterDialogIncoming == null
                    || filterDialogIncoming.isDisposed()) {
                filterDialogIncoming = new BackupServicesFilterDialog(
                        getShell(),
                        VizBackupServicesConstants.INCOMING_FILTER_DIALOG_MENU_TEXT,
                        false, incomingTab);
                filterDialogIncoming.open();
            } else {
                filterDialogIncoming.bringToTop();
            }
        }
    };
}
