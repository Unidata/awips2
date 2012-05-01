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
package com.raytheon.viz.aviation;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory;

import com.raytheon.viz.aviation.climatedata.ClimateDataMenuDlg;
import com.raytheon.viz.avnconfig.HelpUsageDlg;
import com.raytheon.viz.avnconfig.MessageStatusComp;
import com.raytheon.viz.avnconfig.MonitoringCriteriaDlg;
import com.raytheon.viz.avnconfig.TafProductConfigDlg;
import com.raytheon.viz.avnconfig.TafSiteInfoEditorDlg;
import com.raytheon.viz.avnconfig.TextEditorSetupDlg;

/**
 * This class is the main AvnFPS configuration dialog. It contains the controls
 * to launch the Text Editor, Monitoring Rules, TAF Site Info, TAF Products, or
 * Climate Data dialogs.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 22 MAY 2008  1119       lvenable    Initial creation
 * 01 OCT 2010  4345       rferrel     Bring existing dialog to the front.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class AvnconfigDlg extends Dialog {

    /**
     * Dialog shell.
     */
    private Shell shell;

    /**
     * The display control.
     */
    private Display display;

    /**
     * Composite containing message status controls.
     */
    @SuppressWarnings("unused")
    private MessageStatusComp msgStatusComp;

    /**
     * Text Editor dialog.
     */
    private TextEditorSetupDlg editorDlg;

    /**
     * Monitoring Rules Dialog,
     */
    private MonitoringCriteriaDlg monCriteriaDlg;

    /**
     * TAF Site Information dialog.
     */
    private TafSiteInfoEditorDlg siteInfoDlg;

    /**
     * TAF Product dialog.
     */
    private TafProductConfigDlg productsDlg;

    /**
     * Climate Data dialog.
     */
    private ClimateDataMenuDlg climateDataDlg;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public AvnconfigDlg(Shell parent) {
        super(parent, 0);
    }

    /**
     * Open method used set up components and display the dialog.
     * 
     * @return Null.
     */
    public Object open() {
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM);
        shell.setText("AvnFPS Setup");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        mainLayout.verticalSpacing = 5;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();

        shell.pack();

        shell.open();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        return null;
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        // ---------------------------------------------
        // Create the menus at the top of the dialog.
        // ---------------------------------------------
        createMenus();

        createMainControls();

        createBottomMessageControls();
    }

    /**
     * Create the menus at the top of the display.
     */
    private void createMenus() {
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
        // Create all the items in the File dropdown menu
        // -------------------------------------------------

        // Close menu item
        MenuItem quitMI = new MenuItem(fileMenu, SWT.NONE);
        quitMI.setText("Q&uit");
        quitMI.addSelectionListener(new SelectionAdapter() {
            @Override
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
        // -------------------------------------
        // Create the Help menu
        // -------------------------------------
        MenuItem helpMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        helpMenuItem.setText("&Help");

        // Create the File menu item with a File "dropdown" menu
        Menu helpMenu = new Menu(menuBar);
        helpMenuItem.setMenu(helpMenu);

        // -------------------------------------------------
        // Create all the items in the Help dropdown menu
        // -------------------------------------------------

        // About menu item
        MenuItem aboutMI = new MenuItem(helpMenu, SWT.NONE);
        aboutMI.setText("&About...");
        aboutMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ActionFactory.ABOUT.create(
                        PlatformUI.getWorkbench().getActiveWorkbenchWindow())
                        .run();
            }
        });

        // Usage menu item
        MenuItem usageMI = new MenuItem(helpMenu, SWT.NONE);
        usageMI.setText("&Usage...");
        usageMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                String description = "AvnFPS Setup Help";
                String helpText = "This application is used to configure AvnFPS.\n\nButton description:\n\nText Editor:    use to modify forecaster list and default resource\nfile.\n\nMonitoring rules: use to edit watch rules for TAF monitoring.\n\nTAF Site Info:  use to create TAF definition files\n\nTAF Products:   use to create lists of TAFs to load into forecast\neditor\n\nTriggers:       use to create and install Postgres trigger file.\n\nClimate Data:   use to create and update HDF5 climate files.";
                HelpUsageDlg usageDlg = new HelpUsageDlg(shell, description,
                        helpText);
                usageDlg.open();
            }
        });
    }

    /**
     * Create the main controls.
     */
    private void createMainControls() {
        // ------------------------------------------
        // Create the composite for the controls.
        // ------------------------------------------
        Composite controlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginWidth = 20;
        controlComp.setLayout(gl);

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        controlComp.setLayoutData(gd);

        // Create the setup label
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label avnfpsLbl = new Label(controlComp, SWT.CENTER);
        avnfpsLbl.setText("AvnFPS Setup");
        avnfpsLbl.setLayoutData(gd);

        // -------------------------------------------
        // Create a button composite for the buttons
        // -------------------------------------------
        Composite buttonComp = new Composite(controlComp, SWT.BORDER);
        gl = new GridLayout(1, false);
        gl.verticalSpacing = 15;
        buttonComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        buttonComp.setLayoutData(gd);

        int buttonWidth = 170;

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button textEditorBtn = new Button(buttonComp, SWT.PUSH);
        textEditorBtn.setText("Text Editor");
        textEditorBtn.setLayoutData(gd);
        textEditorBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (editorDlg == null || editorDlg.getParent().isDisposed()) {
                    editorDlg = new TextEditorSetupDlg(shell);
                    editorDlg.open();
                    editorDlg = null;
                } else {
                    editorDlg.getParent().forceActive();
                    editorDlg.getParent().forceFocus();
                }
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button monitorRulesBtn = new Button(buttonComp, SWT.PUSH);
        monitorRulesBtn.setText("Monitoring Rules");
        monitorRulesBtn.setLayoutData(gd);
        monitorRulesBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (monCriteriaDlg == null
                        || monCriteriaDlg.getParent().isDisposed()) {
                    monCriteriaDlg = new MonitoringCriteriaDlg(shell);
                    monCriteriaDlg.open();
                    monCriteriaDlg = null;
                } else {
                    monCriteriaDlg.getParent().forceActive();
                    monCriteriaDlg.getParent().forceFocus();
                }
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button tafSiteInfoBtn = new Button(buttonComp, SWT.PUSH);
        tafSiteInfoBtn.setText("TAF Site Info");
        tafSiteInfoBtn.setLayoutData(gd);
        tafSiteInfoBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (siteInfoDlg == null || siteInfoDlg.getParent().isDisposed()) {
                    siteInfoDlg = new TafSiteInfoEditorDlg(shell);
                    siteInfoDlg.open();
                    siteInfoDlg = null;
                } else {
                    siteInfoDlg.getParent().forceActive();
                    siteInfoDlg.getParent().forceFocus();
                }
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button tafProductsBtn = new Button(buttonComp, SWT.PUSH);
        tafProductsBtn.setText("TAF Products");
        tafProductsBtn.setLayoutData(gd);
        tafProductsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (productsDlg == null || productsDlg.getParent().isDisposed()) {
                    productsDlg = new TafProductConfigDlg(shell);
                    productsDlg.open();
                    productsDlg = null;
                } else {
                    productsDlg.getParent().forceActive();
                    productsDlg.getParent().forceFocus();
                }
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button climateBtn = new Button(buttonComp, SWT.PUSH);
        climateBtn.setText("Climate Data");
        climateBtn.setLayoutData(gd);
        climateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (climateDataDlg == null) {
                    climateDataDlg = new ClimateDataMenuDlg(shell);
                    climateDataDlg.open();
                    climateDataDlg = null;
                }
            }
        });
    }

    /**
     * Create the message status composite.
     */
    private void createBottomMessageControls() {
        msgStatusComp = new MessageStatusComp(shell, null, null);
    }

    public void setFocus() {
        shell.setFocus();
    }

    public boolean isDisposed() {
        return shell.isDisposed();
    }
}