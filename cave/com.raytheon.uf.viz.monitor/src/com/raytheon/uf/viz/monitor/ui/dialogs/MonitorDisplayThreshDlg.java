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
package com.raytheon.uf.viz.monitor.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.LoadSaveDeleteSelectDlg.DialogType;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Abstract dialog class that is the foundation for the Monitor and Display
 * threshold dialogs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 6, 2009             lvenable     Initial creation
 * Jun 17, 2010 5551,5548  skorolev     Add "Open" and "Help" menu itemsec
 * Dec 6, 2012 #1351       skorolev     Changes for non-blocking dialogs.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public abstract class MonitorDisplayThreshDlg extends CaveSWTDialog {

    /**
     * Tab folder.
     */
    protected TabFolder tabFolder;

    /**
     * Application name.
     */
    protected AppName appName;

    /**
     * Display type (Monitor/Display).
     */
    protected DataUsageKey displayType;

    /** File menu Open **/
    private LoadSaveDeleteSelectDlg openDlg;

    /** File menu Save **/
    private LoadSaveDeleteSelectDlg saveDlg;

    /** File menu Save As **/
    private LoadSaveDeleteSelectDlg saveAsDlg;

    /** File menu Select **/
    private LoadSaveDeleteSelectDlg selectDlg;

    /** File menu Delete **/
    private LoadSaveDeleteSelectDlg deleteDlg;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param appName
     *            Application name.
     * @param displayType
     *            Display type.
     */
    public MonitorDisplayThreshDlg(Shell parent, AppName appName,
            DataUsageKey displayType) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK
                | CAVE.INDEPENDENT_SHELL);
        // Set the dialog title
        StringBuilder sb = new StringBuilder();
        sb.append(appName.name()).append(": ");
        if (displayType == DataUsageKey.MONITOR) {
            sb.append("Configure Monitor Thresholds");
        } else {
            sb.append("Configure Display Thresholds");
        }
        setText(sb.toString());
        this.appName = appName;
        this.displayType = displayType;
    }

    // Create the main layout for the shell.
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayoutData()
     */
    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.DEFAULT, true, false);
    }

    /**
     * Initialize the components on the display.
     */
    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        createMenu();
        createTabFolder();
        createTabItems();
        createCommitChangesButton();
    }

    /**
     * Create the menu.
     */
    private void createMenu() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createFileMenu(menuBar);
        createHelpMenu(menuBar);
        shell.setMenuBar(menuBar);
    }

    /**
     * Create the File menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createFileMenu(Menu menuBar) {
        // -------------------------------------
        // Create the file menu
        // -------------------------------------
        MenuItem fileMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuItem.setText("&File");

        // Create the File menu item with a File "dropdown" menu
        Menu fileMenu = new Menu(menuBar);
        fileMenuItem.setMenu(fileMenu);

        // -------------------------------------------------
        // If this is the Display dialog then create the
        // menu items for the display menu
        // -------------------------------------------------
        if (displayType == DataUsageKey.DISPLAY) {
            createDisplayFileMenuItems(fileMenu);
        } else if (displayType == DataUsageKey.MONITOR) {
            createMonitorFileMenuItems(fileMenu);
        }

        // -------------------------------------------------
        // Create the Exit menu item the File dropdown menu
        // -------------------------------------------------
        MenuItem exitMI = new MenuItem(fileMenu, SWT.NONE);
        exitMI.setText("Exit");
        exitMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Create the Help menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createHelpMenu(Menu menuBar) {
        MenuItem helpMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        helpMenuItem.setText("&Help");
        // TODO Should be enhanced in the future versions.
    }

    /**
     * Create the menu items for the Display thresholds.
     * 
     * @param fileMenu
     *            File menu.
     */
    private void createDisplayFileMenuItems(Menu fileMenu) {
        /*
         * Create the Display menu items under the File menu
         */
        MenuItem open = new MenuItem(fileMenu, SWT.NONE);
        open.setText("Open");
        open.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                menuOpenAction();
            }
        });

        MenuItem saveAsMI = new MenuItem(fileMenu, SWT.NONE);
        saveAsMI.setText("Save As...");
        saveAsMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                menuSaveAsAction();
            }
        });

        MenuItem selectAsDefaultMI = new MenuItem(fileMenu, SWT.NONE);
        selectAsDefaultMI.setText("Select As Default...");
        selectAsDefaultMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                menuSelectAsDefaultAction();
            }
        });

        MenuItem loadDefaultMI = new MenuItem(fileMenu, SWT.NONE);
        loadDefaultMI.setText("Load Default");
        loadDefaultMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                menuLoadDefaultAction();
            }
        });

        MenuItem deleteMI = new MenuItem(fileMenu, SWT.NONE);
        deleteMI.setText("Delete");
        deleteMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                menuDeleteAction();
            }
        });
        new MenuItem(fileMenu, SWT.SEPARATOR);
    }

    /**
     * Creates the Monitor menu items under the File menu
     * 
     * @param fileMenu
     */
    private void createMonitorFileMenuItems(Menu fileMenu) {
        MenuItem saveMI = new MenuItem(fileMenu, SWT.NONE);
        saveMI.setText("Save");
        saveMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                menuSaveAction();
            }
        });
    }

    /**
     * Creates the tab folder for the threshold tabs.
     */
    private void createTabFolder() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        tabFolder = new TabFolder(shell, SWT.NONE);
        tabFolder.setLayoutData(gd);
    }

    /**
     * Create the commit changes button.
     */
    private void createCommitChangesButton() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(130, SWT.DEFAULT);
        Button commitBtn = new Button(buttonComp, SWT.PUSH);
        commitBtn.setText("Commit Changes");
        commitBtn.setLayoutData(gd);
        commitBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                commitChangeAction();
                close();
            }
        });
    }

    /**
     * Menu Delete Action.
     */
    protected void menuDeleteAction() {

        if (deleteDlg == null) {
            AbstractThresholdMgr atm = getThresholdMgr();
            deleteDlg = new LoadSaveDeleteSelectDlg(shell, DialogType.DELETE,
                    atm.getDisplayThresholdPath(),
                    atm.getDefaultFileName(DataUsageKey.DISPLAY));
            deleteDlg.setCloseCallback(new ICloseCallback() {
                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof LocalizationFile) {
                        LocalizationFile fileName = (LocalizationFile) returnValue;
                        doDelete(fileName);
                    }
                }
            });

        }
        deleteDlg.open();
    }

    /**
     * Deletes file.
     * 
     * @param fileName
     */
    protected void doDelete(LocalizationFile fileName) {
        AbstractThresholdMgr atm = getThresholdMgr();
        boolean deletedUserSelected = atm.deleteFile(fileName);
        if (deletedUserSelected == true) {
            reloadThresholds();
        }
    }

    /**
     * Method called for loading the default display thresholds.
     */
    protected void menuLoadDefaultAction() {
        AbstractThresholdMgr atm = getThresholdMgr();
        atm.loadDefaultDisplayThreshold();
        reloadThresholds();
    }

    /**
     * Menu SaveAs Action.
     */
    protected void menuSaveAsAction() {
        if (saveAsDlg == null) {
            AbstractThresholdMgr atm = getThresholdMgr();
            saveAsDlg = new LoadSaveDeleteSelectDlg(shell, DialogType.SAVE_AS,
                    atm.getDisplayThresholdPath(),
                    atm.getDefaultFileName(DataUsageKey.DISPLAY));
            saveAsDlg.setCloseCallback(new ICloseCallback() {
                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof LocalizationFile) {
                        LocalizationFile fileName = (LocalizationFile) returnValue;
                        doSaveAs(fileName);
                    }
                }
            });
        }
        saveAsDlg.open();
    }

    /**
     * Saves file as ...
     * 
     * @param fileName
     */
    protected void doSaveAs(LocalizationFile fileName) {
        AbstractThresholdMgr atm = getThresholdMgr();
        atm.saveAsDisplayThresholds(fileName.getFile().getName());
    }

    /**
     * Method for selecting the default threshold file to be used for the
     * Display thresholds.
     */
    protected void menuSelectAsDefaultAction() {
        if (selectDlg == null) {
            AbstractThresholdMgr atm = getThresholdMgr();
            selectDlg = new LoadSaveDeleteSelectDlg(shell,
                    DialogType.SELECT_DEFAULT, atm.getDisplayThresholdPath(),
                    atm.getDefaultFileName(DataUsageKey.DISPLAY));
            selectDlg.setCloseCallback(new ICloseCallback() {
                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof LocalizationFile) {
                        LocalizationFile fileName = (LocalizationFile) returnValue;
                        doSelect(fileName);
                    }
                }
            });
        }
        selectDlg.open();
    }

    /**
     * Selects default file.
     * 
     * @param fileName
     */
    protected void doSelect(LocalizationFile fileName) {
        AbstractThresholdMgr atm = getThresholdMgr();
        atm.setDefaultDisplayFileName(fileName.getFile().getName());
    }

    /**
     * Method used for saving monitor thresholds.
     */
    protected void menuSaveAction() {
        AbstractThresholdMgr atm = getThresholdMgr();
        atm.saveMonitorThresholds();
    }

    /**
     * Method used for opening threshold file to be used for the Display
     * thresholds.
     */
    protected void menuOpenAction() {
        if (openDlg == null) {
            AbstractThresholdMgr atm = getThresholdMgr();
            openDlg = new LoadSaveDeleteSelectDlg(shell, DialogType.OPEN,
                    atm.getDisplayThresholdPath(), null);
            openDlg.setCloseCallback(new ICloseCallback() {
                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof LocalizationFile) {
                        LocalizationFile fileName = (LocalizationFile) returnValue;
                        doOpen(fileName);
                    }
                }
            });
        }
        openDlg.open();
    }

    /**
     * Opens file.
     * 
     * @param fileName
     */
    protected void doOpen(LocalizationFile fileName) {
        AbstractThresholdMgr atm = getThresholdMgr();
        atm.loadDisplayThreashold(fileName.getFile().getName());
        reloadThresholds();
    }

    // Abstract method for creating the tab items in the tab folder.

    /**
     * Creates Table Items.
     */
    protected abstract void createTabItems();

    /**
     * Commit Change Action.
     */
    protected abstract void commitChangeAction();

    /**
     * Reloads Thresholds.
     */
    protected abstract void reloadThresholds();

    /**
     * Gets Threshold Manager.
     * 
     * @return manager
     */
    protected abstract AbstractThresholdMgr getThresholdMgr();

}
