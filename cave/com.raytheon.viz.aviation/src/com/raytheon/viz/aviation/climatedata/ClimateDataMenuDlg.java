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
package com.raytheon.viz.aviation.climatedata;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.apache.commons.configuration.ConfigurationException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory;

import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.aviation.climatology.CigVisDistributionDlg;
import com.raytheon.viz.aviation.climatology.CigVisTrendDlg;
import com.raytheon.viz.aviation.climatology.MetarDisplayDialog;
import com.raytheon.viz.aviation.climatology.WindRosePlotDlg;
import com.raytheon.viz.avncommon.AvnMessageMgr.StatusMessageType;
import com.raytheon.viz.avnconfig.HelpUsageDlg;
import com.raytheon.viz.avnconfig.TafSiteConfigFactory;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * This class displays the main Climate Data dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 16, 2009 #3438      lvenable     Initial creation
 * Jul  9, 2010 #5078      rferrel      Add catch for FileNotFound
 *                                      in populateIdentList.
 * Jan 20, 2011 #4864      rferrel      Modifications to work like A1.
 * Feb 16, 2011 #7878      rferrel      Modifications for create ident/site.
 * Mar  2, 2011 #4549      rferrel      Added checks for missing climate data.
 * Mar 18, 2011 #8681      rferrel      Corrected checkSite to prevent exception.
 * May 24, 2011 #9075      rferrel      Changed getObsHistoryFromInv() to scan
 *                                      ish-inventory.txt only one time.
 * Oct 04, 2012 #1229      rferrel      Made non-blocking.
 * Oct 04, 2012 #1229      rferrel      Changes for non-blocking ClimateHistoryDlg.
 * Oct 08, 2012 #1229      rferrel      Changes for non-blocking GenScriptsDlg.
 * Oct 08, 2012 #1229      rferrel      Changes for non-blocking NCDCInvHistDlg.
 * Oct 08, 2012 #1229      rferrel      Changes for non-blocking CigVisDistributionDlg.
 * Oct 08, 2012 #1229      rferrel      Changes for non-blocking WindRosePlotDlg.
 * Oct 09, 2012 #1229      rferrel      Changes for non-blocking MetarDisplayDialog.
 * Oct 09, 2012 #1229      rferrel      Changes for non-blocking CigVisTrendDlg.
 * Oct 15, 2012 #1229      rferrel      Changes for non-blocking HelpUsageDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ClimateDataMenuDlg extends CaveSWTDialog {

    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ClimateDataMenuDlg.class);

    private final Pattern SP_PAT = Pattern.compile("\\s+");

    /**
     * Text font.
     */
    private Font textFont;

    /**
     * Append radio button.
     */
    private Button appendRdo;

    /**
     * Create radio button.
     */
    private Button createRdo;

    /**
     * Site text control
     */
    private Text siteTF;

    /**
     * METAR AFOS Id text control.
     */
    private Text metarAfosIdTF;

    /**
     * Write site data button.
     */
    private Button writeSiteBtn;

    /**
     * Dark blue background color for styled text controls.
     */
    private Color darkBlueBgColor;

    /**
     * Idents list.
     */
    private List identList;

    /**
     * Site information list.
     */
    private List siteInfoList;

    /**
     * File assessment styled text.
     */
    private StyledText fileAssessST;

    /**
     * Assess data button.
     */
    private Button assessDataBtn;

    /**
     * Generate scripts button.
     */
    private Button genScriptsBtn;

    /**
     * Process data button.
     */
    private Button processDataBtn;

    /**
     * Assess data button.
     */
    private Button validateBtn;

    /**
     * Commit button.
     */
    private Button commitBtn;

    /**
     * Reject button.
     */
    private Button rejectBtn;

    /**
     * Save log button.
     */
    private Button saveLogBtn;

    private StyledText headerST;

    /**
     * Status text control.
     */
    private Text statusTF;

    /**
     * NCDC Inventory History dialog.
     */
    private NCDCInvHistDlg invHistoryDlg;

    /**
     * Climate History dialog.
     */
    private ClimateHistoryDlg climateHistoryDlg;

    /**
     * Generate scripts dialog.
     */
    private GenScriptsDlg generateScriptsDlg;

    private MetarDisplayDialog metarDlg;

    private WindRosePlotDlg windRose;

    private CigVisDistributionDlg cigVisDist;

    private CigVisTrendDlg cigVisTrend;

    private java.util.List<String> siteList;

    private int waitCnt = 0;

    private HelpUsageDlg usageDlg;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public ClimateDataMenuDlg(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS Climate Data Menu");
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout gl = new GridLayout(1, false);
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        return gl;
    }

    @Override
    protected void disposed() {
        textFont.dispose();
        darkBlueBgColor.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        // Initialize all of the data, controls, and layouts
        textFont = new Font(getDisplay(), "Courier", 10, SWT.BOLD);
        darkBlueBgColor = new Color(getDisplay(), 82, 107, 129);

        // Initialize controls and layouts
        createMenus();

        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(new GridLayout(3, false));
        mainComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        createLeftSideControls(mainComp);
        createCenterControls(mainComp);
        createRightSideControls(mainComp);
    }

    /**
     * Create the menus.
     */
    private void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        ClimateDataManager.getInstance().reset();
        createFileMenu(menuBar);
        createCommandsMenu(menuBar);
        createToolsMenu(menuBar);
        createHelpMenu(menuBar);

        shell.setMenuBar(menuBar);
    }

    /**
     * Create the File menu.
     * 
     * @param menuBar
     *            The menu bar.
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

        // Quit menu item
        MenuItem quitMI = new MenuItem(fileMenu, SWT.NONE);
        quitMI.setText("&Quit");
        quitMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Create the Commands menu.
     * 
     * @param menuBar
     *            The menu bar.
     */
    private void createCommandsMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Commands menu
        // -------------------------------------
        MenuItem commandsMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        commandsMenuItem.setText("&Commands");

        // Create the Commands menu item with a Commands "dropdown" menu
        Menu commandsMenu = new Menu(menuBar);
        commandsMenuItem.setMenu(commandsMenu);

        // -------------------------------------------------
        // Create all the items in the Commands dropdown menu
        // -------------------------------------------------

        // Show Observations History menu item
        MenuItem showObsHistMI = new MenuItem(commandsMenu, SWT.NONE);
        showObsHistMI.setText("&Show Observations History...");
        showObsHistMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayClimateHistoryDialog();
            }
        });

        // Update NCDC menu item
        MenuItem updateNcdcMI = new MenuItem(commandsMenu, SWT.NONE);
        updateNcdcMI.setText("&Update NCDC \"ish\" Files...");
        updateNcdcMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayNCDCInventoryHistoryDialog();
            }
        });
    }

    /**
     * Create the Tools menu.
     * 
     * @param menuBar
     *            The menu bar.
     */
    private void createToolsMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Tools menu
        // -------------------------------------
        MenuItem toolsMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        toolsMenuItem.setText("&Tools");

        // Create the Tools menu item with a Tools "dropdown" menu
        Menu toolsMenu = new Menu(menuBar);
        toolsMenuItem.setMenu(toolsMenu);

        // -------------------------------------------------
        // Create all the items in the Tools dropdown menu
        // -------------------------------------------------

        // METARs menu item
        MenuItem metarsMI = new MenuItem(toolsMenu, SWT.NONE);
        metarsMI.setText("&METARs...");
        metarsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (metarDlg == null || metarDlg.getShell() == null
                        || metarDlg.isDisposed()) {
                    metarDlg = new MetarDisplayDialog(shell, siteList,
                            StatusMessageType.Metar, null);
                    metarDlg.open();
                } else {
                    metarDlg.bringToTop();
                }
            }
        });

        // Wind Rose menu item
        MenuItem windRoseMI = new MenuItem(toolsMenu, SWT.NONE);
        windRoseMI.setText("&Wind Rose...");
        windRoseMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (windRose == null || windRose.getShell() == null
                        || windRose.isDisposed()) {
                    windRose = new WindRosePlotDlg(shell, siteList,
                            StatusMessageType.WindRose, null);
                    windRose.open();
                } else {
                    windRose.bringToTop();
                }
            }
        });

        // CigVis Dist menu item
        MenuItem cigVisDistMI = new MenuItem(toolsMenu, SWT.NONE);
        cigVisDistMI.setText("&CigVis Dist...");
        cigVisDistMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (cigVisDist == null || cigVisDist.getShell() == null
                        || cigVisDist.isDisposed()) {
                    cigVisDist = new CigVisDistributionDlg(shell, siteList,
                            StatusMessageType.CigVis, null);
                    cigVisDist.open();
                } else {
                    cigVisDist.bringToTop();
                }
            }
        });

        // CigVis Trend menu item
        MenuItem cigVisTrendMI = new MenuItem(toolsMenu, SWT.NONE);
        cigVisTrendMI.setText("C&igVis Trend...");
        cigVisTrendMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (cigVisTrend == null || cigVisTrend.getShell() == null
                        || cigVisTrend.isDisposed()) {
                    cigVisTrend = new CigVisTrendDlg(shell, siteList,
                            StatusMessageType.CigVisTrend, null);
                    cigVisTrend.open();
                } else {
                    cigVisTrend.bringToTop();
                }
            }
        });
    }

    /**
     * Create the Help menu.
     * 
     * @param menuBar
     *            The menu bar.
     */
    private void createHelpMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Help menu
        // -------------------------------------
        MenuItem helpMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        helpMenuItem.setText("&Help");

        // Create the Help menu item with a Help "dropdown" menu
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
                if (mustCreate(usageDlg)) {
                    String description = "CLimate Data Update Dialog Help";

                    String helpText = "This dialog is used to create and append climatology data files.\n\nMenu Bar\nFile:\n\tQuit:   Close the dialog immediately\n\nCommands:\n\tShow observations history:  Opens a graphical display showing the current\n\t\t\t\t\tinventory of climate data available.\n\tUpdate NCDC \"ish\" files:    Opens a dialog allowing the user to generate\n\t\t\t\t\tautomated scripts for downloading the Integrated\n\t\t\t\t\tSurface Hourly (ISH) Database.\n\nTools:\n\tPulldown menu consist of the AvnFPS Climate Tools that read the HDF5 climate\n\t\tfiles.\n\nOptions:\n\tAppend: Add new data to existing climate data file\n\tCreate: Generate new climate data files, regardless of whether a file for\n\t\t\tthat site already exists\n\nFields:\n\tSITE ID:    Site ID of the site currently selected, or \n\t\t\ta user entered site ID if creating a new data file\n\tMETAR AFOS ID:  The AFOS ID used to retrieve location's METAR product for\n\t\t\tuse in AvnFPS's climate Cig/Vis Trend tool.\n\nIdents:\n\tList of current site IDs.\n\nSite info list:\n\tList of IDs and years of data available for each selected site\n\nMonitor area:\n\tArea where all informative messages are displayed.\n\nButtons:\n\n\tAssess Data:\n\tAfter sites are selected, click this to start the creation\n\t\tor append process\n\n\tGenerate Scripts:\n\tGenerate download scripts to retrieve data files from NCDC\n\n\tProcess Data:\n\tIncorporate NCDC data into HDF5 file(s).\n\n\tValidate Data:\n\tTemporarily move newly changed/created files to a location\n\t\tso that AvnFPS climate tools can examine the new climate\n\t\tfile.\n\n\tCommit:\n\tMove newly changed/created files to its permanent location. \n\tClicking this will also generate new station climate qc \n\t\tfiles (files that end in .nc in the data/climate directory)\n\n\tReject:\n\tReject the newly created files in favor of the original file(s),\n\t\tif available. This action deletes newly created files.\n\n\tSave Log:\n\tSave all output in the Monitor area to a file";
                    usageDlg = new HelpUsageDlg(shell, description, helpText);
                    usageDlg.open();
                } else {
                    usageDlg.bringToTop();
                }
            }
        });
    }

    /**
     * Create the controls on the left side of the display.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createLeftSideControls(Composite parentComp) {
        Composite leftComp = new Composite(parentComp, SWT.NONE);
        leftComp.setLayout(new GridLayout(1, false));
        leftComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        appendRdo = new Button(leftComp, SWT.RADIO);
        appendRdo.setText("Append");
        appendRdo.setSelection(true);
        appendRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleAppendRadioAction();
            }
        });

        createRdo = new Button(leftComp, SWT.RADIO);
        createRdo.setText("Create");
        createRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleCreateRadioAction();
            }
        });

        /*
         * Site/METAR/Write controls
         */
        Composite siteMetarCtrlComp = new Composite(leftComp, SWT.BORDER);
        siteMetarCtrlComp.setLayout(new GridLayout(1, false));
        siteMetarCtrlComp.setLayoutData(new GridData()); // TODO : may not need
        // this

        int textWidth = 120;

        Label siteIdLbl = new Label(siteMetarCtrlComp, SWT.NONE);
        siteIdLbl.setText("SITE ID:");

        GridData gd = new GridData(textWidth, SWT.DEFAULT);
        siteTF = new Text(siteMetarCtrlComp, SWT.BORDER);
        siteTF.setLayoutData(gd);

        Label metarLbl = new Label(siteMetarCtrlComp, SWT.NONE);
        metarLbl.setText("METAR AFOS ID:");

        gd = new GridData(textWidth, SWT.DEFAULT);
        metarAfosIdTF = new Text(siteMetarCtrlComp, SWT.BORDER);
        metarAfosIdTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        writeSiteBtn = new Button(siteMetarCtrlComp, SWT.PUSH);
        writeSiteBtn.setText("Write Site Data");
        writeSiteBtn.setToolTipText("Add new site to config/ids.cfg file");
        writeSiteBtn.setLayoutData(gd);
        writeSiteBtn.setEnabled(false);
        writeSiteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleWriteSiteData();
            }
        });

        handleAppendRadioAction();
    }

    /**
     * Create the controls on the center of the display.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createCenterControls(Composite parentComp) {
        Composite centerComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 0;
        centerComp.setLayout(gl);
        centerComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));

        /*
         * Idents composite
         */
        Composite identComp = new Composite(centerComp, SWT.NONE);
        identComp.setLayout(new GridLayout(1, false));
        identComp
                .setLayoutData(new GridData(SWT.DEFAULT, SWT.FILL, false, true));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Label identLbl = new Label(identComp, SWT.CENTER);
        identLbl.setText("Idents");
        identLbl.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        gd.widthHint = 60;
        identList = new List(identComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        identList.setLayoutData(gd);
        identList.setFont(textFont);
        identList.setLayoutData(gd);
        identList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleIdentsSelection();
            }
        });

        populateIdentList();

        /*
         * Information header and list.
         */
        Composite infoComp = new Composite(centerComp, SWT.NONE);
        infoComp.setLayout(new GridLayout(1, false));
        infoComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = 400;
        gd.heightHint = 35;
        headerST = new StyledText(infoComp, SWT.BORDER | SWT.MULTI);
        headerST.setLayoutData(gd);
        headerST.setBackground(darkBlueBgColor);
        headerST.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        headerST.setFont(textFont);
        headerST.setEditable(false);
        setHeaderInformation(headerST);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 400;
        gd.heightHint = 150;
        siteInfoList = new List(infoComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        siteInfoList.setLayoutData(gd);
        siteInfoList.setFont(textFont);

        /*
         * File assessment and status text controls.
         */
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 200;
        gd.horizontalSpan = 2;
        fileAssessST = new StyledText(centerComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL | SWT.H_SCROLL);
        fileAssessST.setBackground(darkBlueBgColor);
        fileAssessST
                .setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        fileAssessST.setFont(textFont);
        fileAssessST.setEditable(false);
        fileAssessST.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        statusTF = new Text(centerComp, SWT.BORDER | SWT.READ_ONLY);
        statusTF.setLayoutData(gd);
    }

    /**
     * Create the button controls on the right side of the display.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createRightSideControls(Composite parentComp) {
        Composite buttonComp = new Composite(parentComp, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp
                .setLayoutData(new GridData(SWT.DEFAULT, SWT.TOP, false, true));

        int buttonWidth = 120;

        GridData gd = new GridData(buttonWidth, SWT.DEFAULT);
        assessDataBtn = new Button(buttonComp, SWT.PUSH);
        assessDataBtn.setText("Assess Data");
        assessDataBtn.setLayoutData(gd);
        assessDataBtn.setEnabled(false);
        assessDataBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                assessData();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        genScriptsBtn = new Button(buttonComp, SWT.PUSH);
        genScriptsBtn.setText("Generate Scripts");
        genScriptsBtn.setLayoutData(gd);
        genScriptsBtn.setEnabled(false);
        genScriptsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                displayGenerateScriptDialog();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        processDataBtn = new Button(buttonComp, SWT.PUSH);
        processDataBtn.setText("Process Data");
        processDataBtn.setLayoutData(gd);
        processDataBtn.setEnabled(false);
        processDataBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                processData();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        validateBtn = new Button(buttonComp, SWT.PUSH);
        validateBtn.setText("Validate");
        validateBtn.setLayoutData(gd);
        validateBtn.setEnabled(false);
        validateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                validateData();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        commitBtn = new Button(buttonComp, SWT.PUSH);
        commitBtn.setText("Commit");
        commitBtn.setLayoutData(gd);
        commitBtn.setEnabled(false);
        commitBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                commitData();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        rejectBtn = new Button(buttonComp, SWT.PUSH);
        rejectBtn.setText("Reject");
        rejectBtn.setLayoutData(gd);
        rejectBtn.setEnabled(false);
        rejectBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                rejectData();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        saveLogBtn = new Button(buttonComp, SWT.PUSH);
        saveLogBtn.setText("Save Log");
        saveLogBtn.setLayoutData(gd);
        saveLogBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                saveLog();
            }
        });
    }

    private void handleIdentsSelection() {
        if (identList.getSelectionCount() == 0) {
            siteInfoList.removeAll();
            assessDataBtn.setEnabled(false);
            return;
        } else if (createRdo.getSelection()) {
            String site = identList.getSelection()[0];
            String pil = getSitePil(site);
            siteTF.setText(site);
            metarAfosIdTF.setText(pil);
            writeSiteBtn.setEnabled(false);
        }

        int timeout = identList.getSelectionCount() * 20;
        for (int index = 0; index < identList.getItemCount(); ++index) {
            String item = identList.getItem(index);
            if (identList.isSelected(index)) {
                if (!siteInSiteInfoList(item)) {
                    getSiteInfoList(item, timeout);
                }
            } else {
                removeFromSiteInfoList(item);
            }
        }
    }

    /**
     * Action performed when the Append radio button is selected.
     */
    private void handleAppendRadioAction() {
        siteTF.setEnabled(false);
        metarAfosIdTF.setEnabled(false);
        writeSiteBtn.setEnabled(false);
    }

    /**
     * Action performed when the Create radio button is selected.
     */
    private void handleCreateRadioAction() {
        siteTF.setEnabled(true);
        metarAfosIdTF.setEnabled(true);
        siteTF.setText("");
        metarAfosIdTF.setText("");
        writeSiteBtn.setEnabled(identList.getSelectionCount() == 0);
    }

    /**
     * Aciton performed for the Write Site Data button.
     */
    private void handleWriteSiteData() {
        // TODO check metarAfosIdTF for valid value.
        String site = siteTF.getText().trim().toUpperCase();
        siteTF.setText(site);
        String pil = metarAfosIdTF.getText().trim().toUpperCase();
        metarAfosIdTF.setText(pil);

        setIdsSite(site, pil);
        populateIdentList();
        identList.select(identList.indexOf(site));
        handleIdentsSelection();
    }

    /**
     * Set the header text.
     * 
     * @param headerST
     *            Header styled text.
     */
    private void setHeaderInformation(StyledText headerST) {
        String topFmt = "%42s";
        String bottomFmt = "%4s    %9S      %4S        %s";

        StringBuilder sb = new StringBuilder();
        sb.append(String.format(topFmt, "Years in Archive:"));
        sb.append("\n");
        sb.append(String
                .format(bottomFmt, "Site", "USAF-WBAN", "NCDC", "Local"));

        headerST.setText(sb.toString());
    }

    /**
     * Display the climate history dialog.
     */
    private void displayClimateHistoryDialog() {
        setWait(true);
        ClimateHistoryData data = null;
        try {
            data = getObsHistoryFromInv();
        } finally {
            setWait(false);
        }

        if (data.getStationNames().size() == 0) {
            MessageBox confirmDeleteMB = new MessageBox(shell,
                    SWT.ICON_INFORMATION | SWT.OK);
            confirmDeleteMB
                    .setMessage("No Idents with climate\nhistory selected.");
            confirmDeleteMB.setText("No Climate History");
            confirmDeleteMB.open();
            return;
        }

        if (climateHistoryDlg == null || climateHistoryDlg.getShell() == null
                || climateHistoryDlg.isDisposed()) {
            climateHistoryDlg = new ClimateHistoryDlg(shell, data);
            climateHistoryDlg.open();
        } else {
            climateHistoryDlg.bringToTop();
        }
    }

    /**
     * Display the NCDC inventory/history dialog.
     */
    private void displayNCDCInventoryHistoryDialog() {
        if (invHistoryDlg == null || invHistoryDlg.getShell() == null
                || invHistoryDlg.isDisposed()) {
            invHistoryDlg = new NCDCInvHistDlg(shell);
            invHistoryDlg.open();
        } else {
            invHistoryDlg.bringToTop();
        }
    }

    /**
     * Display the generate scripts dialog.
     */
    private void displayGenerateScriptDialog() {
        if (generateScriptsDlg == null || generateScriptsDlg.getShell() == null
                || generateScriptsDlg.isDisposed()) {
            ClimateDataManager.getInstance().assessStationsMap(this);
            generateScriptsDlg = new GenScriptsDlg(shell, "data");
            generateScriptsDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    genScriptsBtn.setEnabled((Boolean) returnValue);
                }
            });
            generateScriptsDlg.open();
        } else {
            generateScriptsDlg.bringToTop();
        }
    }

    private void populateIdentList() {
        try {
            identList.removeAll();
            siteList = TafSiteConfigFactory.getInstance().getIdsSiteList();
            for (int i = 0; i < siteList.size(); i++) {
                identList.add(siteList.get(i));
            }
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, e.getMessage());
        } catch (ConfigurationException e) {
            statusHandler.handle(Priority.PROBLEM, e.toString());
        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.PROBLEM, e.getMessage());
        }
    }

    private void setIdsSite(String site, String pil) {
        try {
            TafSiteConfigFactory.getInstance().setIdsSite(site, pil);
        } catch (ConfigurationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);

        } catch (LocalizationOpFailedException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);

        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);

        } catch (IOException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);

        }

    }

    private void getSiteInfoList(String ident, int timeout) {
        ClimateDataManager dataMgr = ClimateDataManager.getInstance();
        setWait(true);
        dataMgr.getIdnum(ident, timeout, this);
    }

    private void removeFromSiteInfoList(String ident) {
        for (int index = siteInfoList.getItemCount() - 1; index >= 0; --index) {
            if (siteInfoList.getItem(index).startsWith(ident)) {
                siteInfoList.remove(index);
            }
        }
    }

    private boolean siteInSiteInfoList(String ident) {
        for (String item : siteInfoList.getItems()) {
            if (item.startsWith(ident)) {
                return true;
            }
        }
        return false;
    }

    public void populateSiteInfoList(final String ident,
            final java.util.List<java.util.List<String>> list) {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                if (isDisposed()) {
                    return;
                }
                if (list != null) {
                    for (java.util.List<String> idList : list) {
                        StringBuilder sb = new StringBuilder();
                        sb.append(ident);
                        sb.append(String.format(
                                "%15s",
                                String.format("%s-%s", idList.get(0),
                                        idList.get(1))));
                        sb.append(String.format(
                                "%12s",
                                String.format("%s-%s", idList.get(2),
                                        idList.get(3))));
                        sb.append(String.format(
                                "%12s",
                                String.format("%s-%s", idList.get(4),
                                        idList.get(5))));
                        String item = sb.toString();

                        // Keep site info list in order.
                        int index = 0;
                        int diff = -1;
                        while (index < siteInfoList.getItemCount()) {
                            diff = item.compareTo(siteInfoList.getItem(index));
                            if (diff <= 0) {
                                break;
                            }
                            ++index;
                        }

                        // Do not include duplicates
                        if (diff != 0) {
                            siteInfoList.add(item, index);
                            siteInfoList.select(index);
                        }
                    }
                } else {
                    StringBuilder sb = new StringBuilder();
                    sb.append(String.format("%s   -- NO INFO AVAILABLE --",
                            ident));
                    siteInfoList.add(sb.toString());
                    assessDataBtn.setEnabled(false);
                }

            }

        });
    }

    private void assessData() {
        ClimateDataManager dataMgr = ClimateDataManager.getInstance();
        String[] items = siteInfoList.getItems();
        java.util.List<String> itemList = new ArrayList<String>(
                Arrays.asList(items));
        setWait(true);
        scriptsBtn(false);
        processBtn(false);
        validateBtn(false);
        commitBtn(false);
        rejectBtn(false);
        dataMgr.assessData(appendRdo.getSelection(), itemList, this);
        assessBtn(false);
    }

    private synchronized void setWait(final boolean state) {
        Shell shell = getShell();
        if (!shell.isDisposed()) {
            Cursor cursor = null;
            if (state) {
                cursor = Display.getDefault().getSystemCursor(SWT.CURSOR_WAIT);
                waitCnt++;
            } else {
                --waitCnt;
                if (waitCnt > 0) {
                    return;
                }
                waitCnt = 0;
            }
            if (Display.findDisplay(Thread.currentThread()) != null) {
                shell.setCursor(cursor);
                headerST.setCursor(cursor);
                fileAssessST.setCursor(cursor);
                statusTF.setCursor(cursor);
            } else {
                final Cursor c = cursor;
                Display.getDefault().syncExec(new Runnable() {

                    @Override
                    public void run() {
                        if (!getShell().isDisposed()) {
                            getShell().setCursor(c);
                            headerST.setCursor(c);
                            fileAssessST.setCursor(c);
                            statusTF.setCursor(c);
                        }
                    }
                });
            }
        }
    }

    @SuppressWarnings("unchecked")
    private void processData() {
        ClimateDataManager dataMgr = ClimateDataManager.getInstance();
        String[] items = siteInfoList.getItems();
        java.util.List<String> itemList = new ArrayList<String>(
                Arrays.asList(items));
        setWait(true);
        dataMgr.processData(appendRdo.getSelection(), this);
        scriptsBtn(false);
        processBtn(false);
    }

    private void validateData() {
        String site = "";
        if (identList.getSelectionIndex() >= 0) {
            site = identList.getItem(identList.getSelectionIndex());
        }
        ClimateDataManager dataMgr = ClimateDataManager.getInstance();
        setWait(true);
        dataMgr.validateData(site, this);
    }

    private void commitData() {
        ClimateDataManager dataMgr = ClimateDataManager.getInstance();
        setWait(true);
        dataMgr.commitData(this);
    }

    private void rejectData() {
        ClimateDataManager dataMgr = ClimateDataManager.getInstance();
        setWait(true);
        dataMgr.rejectData(this);
    }

    private void saveLog() {
        Calendar c = Calendar.getInstance();
        c.setTimeInMillis(System.currentTimeMillis());
        String t = String.format("%4d%02d%02d%02d%02d", c.get(Calendar.YEAR),
                c.get(Calendar.MONTH) + 1, c.get(Calendar.DATE),
                c.get(Calendar.HOUR_OF_DAY), c.get(Calendar.MINUTE));

        FileDialog dlg = new FileDialog(shell, SWT.SAVE);

        try {
            dlg.setFilterPath(ClimateDataPython.getIshFilePath() + "/tmp/");
        } catch (VizException e) {
        }

        dlg.setFileName("climatedata_" + t + ".log");
        String filename = dlg.open();
        if (filename == null) {
            return;
        }

        File file = new File(filename);
        FileWriter writer;
        try {
            setWait(true);
            writer = new FileWriter(file);
            BufferedWriter buf = new BufferedWriter(writer);

            buf.write(fileAssessST.getText());

            buf.close();
            writer.close();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } finally {
            setWait(false);
        }
    }

    public java.util.List<String> getSites() {
        java.util.List<String> list = new ArrayList<String>();
        String[] sites = siteInfoList.getItems();

        for (String site : sites) {
            list.add(site);
        }

        return list;
    }

    private String getSitePil(String site) {
        try {
            return TafSiteConfigFactory.getInstance().getIdsPil(site);
        } catch (ConfigurationException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);

        } catch (LocalizationOpFailedException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);

        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);

        } catch (IOException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);

        }
        return "";
    }

    public void updateMonitor(final String msg) {
        getDisplay().asyncExec(new Runnable() {
            public void run() {
                fileAssessST
                        .setText(fileAssessST.getText().trim() + "\n" + msg);
                // Scroll the visible area if necessary
                int lineHeight = fileAssessST.getLineHeight();
                int numLines = Math.round(fileAssessST.getBounds().height
                        / lineHeight);
                int lineCount = fileAssessST.getLineCount();

                if (lineCount > numLines) {
                    fileAssessST.setTopIndex(lineCount - numLines + 1);
                }
            }
        });
    }

    public void overwriteMonitor(final String msg) {
        getDisplay().asyncExec(new Runnable() {
            public void run() {
                statusTF.setText(msg);
            }
        });
    }

    public void assessBtn(final boolean enabled) {
        if (!isDisposed()) {
            getDisplay().asyncExec(new Runnable() {
                public void run() {
                    assessDataBtn.setEnabled(enabled);
                }
            });
        }
    }

    public void scriptsBtn(final boolean enabled) {
        getDisplay().asyncExec(new Runnable() {
            public void run() {
                genScriptsBtn.setEnabled(enabled);
            }
        });
    }

    public void processBtn(final boolean enabled) {
        getDisplay().asyncExec(new Runnable() {
            public void run() {
                processDataBtn.setEnabled(enabled);
            }
        });
    }

    public void validateBtn(final boolean enabled) {
        if (!isDisposed()) {
            getDisplay().asyncExec(new Runnable() {
                public void run() {
                    validateBtn.setEnabled(enabled);
                }
            });
        }
    }

    public void commitBtn(final boolean enabled) {
        if (!isDisposed()) {
            getDisplay().asyncExec(new Runnable() {
                public void run() {
                    commitBtn.setEnabled(enabled);
                }
            });
        }
    }

    public void rejectBtn(final boolean enabled) {
        if (!isDisposed()) {
            getDisplay().asyncExec(new Runnable() {
                public void run() {
                    rejectBtn.setEnabled(enabled);
                }
            });
        }
    }

    public void saveLogBtn(final boolean enabled) {
        if (!isDisposed()) {
            getDisplay().asyncExec(new Runnable() {
                public void run() {
                    saveLogBtn.setEnabled(enabled);
                }
            });
        }
    }

    private ClimateHistoryData getObsHistoryFromInv() {
        Map<String, java.util.List<StationData>> dataMap = new LinkedHashMap<String, java.util.List<StationData>>();
        ClimateHistoryData data = new ClimateHistoryData();

        try {
            if (siteInfoList.getItemCount() == 0) {
                return data;
            }

            StringBuilder expr = null;
            for (String str : siteInfoList.getItems()) {
                if (str.contains("NO INFO AVAILABLE")) {
                    continue;
                }
                if (expr == null) {
                    expr = new StringBuilder("^((");
                } else {
                    expr.append("|(");
                }
                String[] usafWban = SP_PAT.split(str)[1].split("-");
                expr.append(usafWban[0]).append("\\s").append(usafWban[1])
                        .append(")");
            }

            if (expr == null) {
                return data;
            }

            expr.append(")\\s.*$");

            Pattern usaf_wbanPat = Pattern.compile(expr.toString());

            // Parse the ish file and get the lines of interest.
            java.util.List<String> lines = new ArrayList<String>();
            File invFile = new File(ClimateDataPython.getIshFilePath()
                    + "/ish-inventory.txt");
            BufferedReader input = new BufferedReader(new FileReader(invFile));
            try {
                String line = null;
                while ((line = input.readLine()) != null) {
                    if (usaf_wbanPat.matcher(line).matches()) {
                        lines.add(line);
                    }
                }
            } finally {
                input.close();
                input = null;
            }

            // Fully parse the lines by usaf and wban.
            for (String str : siteInfoList.getItems()) {
                if (str.contains("NO INFO AVAILABLE")) {
                    continue;
                }
                String[] info = SP_PAT.split(str);
                String stationName = info[0];
                String usaf_wban = info[1].replace('-', ' ');
                Pattern startsWithPat = Pattern.compile("^" + usaf_wban
                        + "\\s.*$");
                StationData stationData = new StationData(usaf_wban);

                int nextYear = 9999;
                for (String line : lines) {
                    if (startsWithPat.matcher(line).matches()) {
                        String[] bits = SP_PAT.split(line);
                        int year = Integer.parseInt(bits[2]);

                        while (year > nextYear) {
                            ObsGraphData yearData = new ObsGraphData(nextYear,
                                    0);
                            stationData.addObsGraphData(yearData);
                            nextYear++;
                        }

                        nextYear = year + 1;
                        int obsCount = 0;

                        for (int i = 3; i < bits.length; i++) {
                            obsCount += Integer.parseInt(bits[i]);
                        }

                        ObsGraphData yearData = new ObsGraphData(year, obsCount);
                        stationData.addObsGraphData(yearData);
                    }
                }

                java.util.List<StationData> stationDataArray = dataMap
                        .get(stationName);

                if (stationDataArray == null) {
                    stationDataArray = new ArrayList<StationData>();
                }

                stationDataArray.add(stationData);
                dataMap.put(stationName, stationDataArray);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
        } catch (FileNotFoundException e) {
            statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
        }

        Set<String> keys = dataMap.keySet();

        for (String key : keys) {
            data.addStationData(key, dataMap.get(key));
        }

        return data;
    }

    private void removeIdsSite(String site) {
        try {
            TafSiteConfigFactory.getInstance().removeIdsSite(site);
        } catch (ConfigurationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);

        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);

        } catch (FileNotFoundException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);

        }
    }

    public void checkSite() {
        if (isDisposed()) {
            return;
        }
        getDisplay().asyncExec(new Runnable() {

            @Override
            public void run() {
                if (identList.getSelectionCount() > 0
                        && siteInfoList.getItemCount() > 0
                        && siteInfoList.getItem(0)
                                .contains("NO INFO AVAILABLE")) {
                    String site = identList.getSelection()[0];
                    MessageBox confirmDeleteMB = new MessageBox(shell,
                            SWT.ICON_QUESTION | SWT.YES | SWT.NO);
                    confirmDeleteMB.setText("Remove Site");
                    confirmDeleteMB.setMessage("No informaton for site " + site
                            + "\nRemove from the list?");
                    int result = confirmDeleteMB.open();

                    if (result == SWT.YES) {
                        siteInfoList.removeAll();
                        removeIdsSite(site);
                        populateIdentList();
                    }
                }
            }
        });
    }

    public void executeDone() {
        setWait(false);
    }
}
