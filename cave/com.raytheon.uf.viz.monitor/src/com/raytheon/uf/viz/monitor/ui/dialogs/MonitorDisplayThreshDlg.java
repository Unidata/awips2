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
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.LoadSaveDeleteSelectDlg.DialogType;

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
 * Jun 17, 2010 5551,5548  skorolev     Add "Open" and "Help" menu items
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public abstract class MonitorDisplayThreshDlg extends Dialog {
    /**
     * Dialog shell.
     */
    protected Shell shell;

    /**
     * The display control.
     */
    protected Display display;

    /**
     * Tab folder.
     */
    protected TabFolder tabFolder;

    /**
     * Return value for the open method.
     */
    private Boolean returnValue = false;

    /**
     * Application name.
     */
    protected CommonConfig.AppName appName;

    /**
     * Display type (Monitor/Display).
     */
    protected DataUsageKey displayType;

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
    public MonitorDisplayThreshDlg(Shell parent, CommonConfig.AppName appName,
            DataUsageKey displayType) {
        super(parent, 0);

        this.appName = appName;
        this.displayType = displayType;
    }

    /**
     * Open method used to display the dialog.
     * 
     * @return True/False.
     */
    public Object open() {
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM);

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        shell.setLayout(mainLayout);
        shell.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        // Set the dialog title

        StringBuilder sb = new StringBuilder();
        sb.append(appName.name()).append(": ");

        if (displayType == DataUsageKey.MONITOR) {
            sb.append("Configure Monitor Thresholds");
        } else {
            sb.append("Configure Display Thresholds");
        }

        shell.setText(sb.toString());

        // Initialize all of the controls and layouts
        initializeComponents();

        shell.pack();

        shell.open();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        return returnValue;
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
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
                shell.dispose();
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

    private void createMonitorFileMenuItems(Menu fileMenu) {
        /*
         * Create the Display menu items under the File menu
         */
        MenuItem saveMI = new MenuItem(fileMenu, SWT.NONE);
        saveMI.setText("Save");
        saveMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            }
        });
    }

    /**
     * Create the tab folder for the threshold tabs.
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
                shell.dispose();
            }
        });
    }

    protected void menuDeleteAction() {
        AbstractThresholdMgr atm = getThresholdMgr();

        LoadSaveDeleteSelectDlg lsDlg = new LoadSaveDeleteSelectDlg(shell,
                DialogType.DELETE, atm.getDisplayThresholdPath(), atm
                        .getDefaultFileName(DataUsageKey.DISPLAY));
        LocalizationFile fileName = (LocalizationFile) lsDlg.open();

        if (fileName == null) {
            System.out.println("FileName is null...");
            return;
        }

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

    protected void menuSaveAsAction() {
        AbstractThresholdMgr atm = getThresholdMgr();

        LoadSaveDeleteSelectDlg lsDlg = new LoadSaveDeleteSelectDlg(shell,
                DialogType.SAVE_AS, atm.getDisplayThresholdPath(), atm
                        .getDefaultFileName(DataUsageKey.DISPLAY));
        LocalizationFile fileName = (LocalizationFile) lsDlg.open();

        if (fileName == null) {
            System.out.println("FileName is null...");
            return;
        }

        System.out.println("Selected file absolute path= "
                + fileName.getFile().getAbsolutePath());
        System.out.println("Selected file name = "
                + fileName.getFile().getName());

        atm.saveAsDisplayThresholds(fileName.getFile().getName());
    }

    /**
     * Method for selecting the default threshold file to be used for the
     * Display thresholds.
     */
    protected void menuSelectAsDefaultAction() {
        AbstractThresholdMgr atm = getThresholdMgr();

        LoadSaveDeleteSelectDlg lsDlg = new LoadSaveDeleteSelectDlg(shell,
                DialogType.SELECT_DEFAULT, atm.getDisplayThresholdPath(), atm
                        .getDefaultFileName(DataUsageKey.DISPLAY));
        LocalizationFile fileName = (LocalizationFile) lsDlg.open();

        if (fileName == null) {
            System.out.println("FileName is null...");
            return;
        }

        System.out.println("Selected file absolute path= "
                + fileName.getFile().getAbsolutePath());
        System.out.println("Selected file name = "
                + fileName.getFile().getName());

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
        AbstractThresholdMgr atm = getThresholdMgr();

        LoadSaveDeleteSelectDlg lsDlg = new LoadSaveDeleteSelectDlg(shell,
                DialogType.OPEN, atm.getDisplayThresholdPath(), null);
        LocalizationFile fileName = (LocalizationFile) lsDlg.open();
        if (fileName == null) {
            System.out.println("FileName is null...");
            return;
        }
        atm.loadDisplayThreashold(fileName.getFile().getName());
        reloadThresholds();
        System.out.println("Opened file absolute path= "
                + fileName.getFile().getAbsolutePath());
        System.out
                .println("Opened file name = " + fileName.getFile().getName());

    }
    /**
     * Abstract method for creating the tab items in the tab folder.
     */
    protected abstract void createTabItems();

    protected abstract void commitChangeAction();

    protected abstract void reloadThresholds();

    protected abstract AbstractThresholdMgr getThresholdMgr();
    
}
