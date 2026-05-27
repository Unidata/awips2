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
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory;

import com.raytheon.viz.aviation.climatedata.ClimateDataMenuDlg;
import com.raytheon.viz.avnconfig.HelpUsageDlg;
import com.raytheon.viz.avnconfig.MonitoringCriteriaDlg;
import com.raytheon.viz.avnconfig.TafProductConfigDlg;
import com.raytheon.viz.avnconfig.TafSiteInfoEditorDlg;
import com.raytheon.viz.avnconfig.TextEditorSetupDlg;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class is the main AvnFPS configuration dialog. It contains the controls
 * to launch the Text Editor, Monitoring Rules, TAF Site Info, TAF Products, or
 * Climate Data dialogs.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 22, 2008  1119     lvenable  Initial creation
 * Oct 01, 2010  4345     rferrel   Bring existing dialog to the front.
 * Oct 04, 2012  1229     rferrel   Work with non-blocking ClimateDataMenuDlg.
 * Oct 08, 2012  1229     rferrel   Make sub-class of CaveSWTDialog and make
 *                                  non-blocking.
 * Oct 11, 2012  1229     rferrel   Changes for non-blocking
 *                                  MonitoringCriteriaDlg.
 * Oct 12, 2012  1229     rferrel   Changes for non-blocking
 *                                  TafProductConfigDlg.
 * Oct 12, 2012  1229     rferrel   Changes for non-blocking
 *                                  TafSiteInfoEditorDlg.
 * Oct 15, 2012  1229     rferrel   Changes for non-blocking TextEditorSetupDlg.
 * Oct 15, 2012  1229     rferrel   Changed for non-blocking HelpUsageDlg.
 * Feb 13, 2013  1549     rferrel   Minor code cleanup.
 * Jan 26, 2016  5054     randerso  Change top level dialog to be parented to
 *                                  the display
 * Mar 15, 2016  5481     randerso  Fix GUI sizing problems
 * Mar 23, 2018  7111     njensen   Remove obsolete help text
 * Jul 28, 2020  8199     randerso  Fix MenuItem with style set to SWT.NONE
 *                                  which appeared to be causing issues with
 *                                  size computation. Remove a bunch of
 *                                  unnecessary cruft. Code cleanup.
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class AvnconfigDlg extends CaveSWTDialog {

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

    private HelpUsageDlg usageDlg;

    /**
     * Constructor.
     *
     * @param display
     */
    public AvnconfigDlg(Display display) {
        super(display, SWT.DIALOG_TRIM, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.INDEPENDENT_SHELL | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS Setup");
    }

    @Override
    protected void initializeComponents(Shell shell) {
        createMenus();
        createMainControls();

        // Set width to try to make dialog width similar to before
        GC gc = new GC(shell);
        int width = gc.getFontMetrics().getAverageCharWidth() * 30;
        gc.dispose();

        shell.pack();
        Point size = shell.getSize();
        shell.setMinimumSize(width, size.y);
    }

    /**
     * Create the menus at the top of the display.
     */
    private void createMenus() {
        // ---------------------------------------------
        // Create the menus at the top of the dialog.
        // ---------------------------------------------
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
        MenuItem quitMI = new MenuItem(fileMenu, SWT.PUSH);
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
        MenuItem aboutMI = new MenuItem(helpMenu, SWT.PUSH);
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
        MenuItem usageMI = new MenuItem(helpMenu, SWT.PUSH);
        usageMI.setText("&Usage...");
        usageMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(usageDlg)) {
                    String description = "AvnFPS Setup Help";

                    String helpText = "This application is used to configure AvnFPS.\n\nButton description:\n\nText Editor:    use to modify forecaster list and default resource\nfile.\n\nMonitoring rules: use to edit watch rules for TAF monitoring.\n\nTAF Site Info:  use to create TAF definition files\n\nTAF Products:   use to create lists of TAFs to load into forecast\neditor\n\nClimate Data:   use to create and update HDF5 climate files.";
                    usageDlg = new HelpUsageDlg(shell, description, helpText);
                    usageDlg.open();
                } else {
                    usageDlg.bringToTop();
                }
            }
        });
    }

    /**
     * Create the main controls.
     */
    private void createMainControls() {

        GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Button textEditorBtn = new Button(shell, SWT.PUSH);
        textEditorBtn.setText("Text Editor");
        textEditorBtn.setLayoutData(gd);
        textEditorBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(editorDlg)) {
                    editorDlg = new TextEditorSetupDlg(shell);
                    editorDlg.open();
                } else {
                    editorDlg.bringToTop();
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Button monitorRulesBtn = new Button(shell, SWT.PUSH);
        monitorRulesBtn.setText("Monitoring Rules");
        monitorRulesBtn.setLayoutData(gd);
        monitorRulesBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(monCriteriaDlg)) {
                    monCriteriaDlg = new MonitoringCriteriaDlg(shell);
                    monCriteriaDlg.open();
                } else {
                    monCriteriaDlg.bringToTop();
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Button tafSiteInfoBtn = new Button(shell, SWT.PUSH);
        tafSiteInfoBtn.setText("TAF Site Info");
        tafSiteInfoBtn.setLayoutData(gd);
        tafSiteInfoBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(siteInfoDlg)) {
                    siteInfoDlg = new TafSiteInfoEditorDlg(shell);
                    siteInfoDlg.open();
                } else {
                    siteInfoDlg.bringToTop();
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Button tafProductsBtn = new Button(shell, SWT.PUSH);
        tafProductsBtn.setText("TAF Products");
        tafProductsBtn.setLayoutData(gd);
        tafProductsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(productsDlg)) {
                    productsDlg = new TafProductConfigDlg(shell);
                    productsDlg.open();
                } else {
                    productsDlg.bringToTop();
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Button climateBtn = new Button(shell, SWT.PUSH);
        climateBtn.setText("Climate Data");
        climateBtn.setLayoutData(gd);
        climateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(climateDataDlg)) {
                    climateDataDlg = new ClimateDataMenuDlg(shell);
                    climateDataDlg.open();
                } else {
                    climateDataDlg.bringToTop();
                }
            }
        });
    }
}
