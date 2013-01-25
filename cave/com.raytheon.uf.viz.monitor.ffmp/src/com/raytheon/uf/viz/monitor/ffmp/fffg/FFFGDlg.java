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
package com.raytheon.uf.viz.monitor.ffmp.fffg;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.ScrollBar;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinMetaData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPCounties;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPCounty;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.monitor.config.FFFGConfig.GuidSectType;
import com.raytheon.uf.common.monitor.config.FFFGConfig.UpdateType;
import com.raytheon.uf.common.monitor.config.FFFGDataMgr;
import com.raytheon.uf.common.monitor.config.SourceCompData;
import com.raytheon.uf.common.monitor.config.ValueNameIdData;
import com.raytheon.uf.common.monitor.xml.FFFGBasinIdXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.monitor.ffmp.FFMPMonitor;
import com.raytheon.uf.viz.monitor.ffmp.fffg.RetrieveMergeDlg.RetrieveMergeAction;
import com.raytheon.uf.viz.monitor.ui.dialogs.LoadSaveDeleteSelectDlg;
import com.raytheon.uf.viz.monitor.ui.dialogs.LoadSaveDeleteSelectDlg.DialogType;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * 
 * This is the main FFFG dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2010 #4517      lvenable     Initial creation
 * Dec 12, 2011 #11225     gzhang		Large font for FFG value,expiration time
 * Nov 29, 2012 #1353      rferrel      Make dialog non-blocking.
 *                                       Changes for non-blocking AboutDlg.
 *                                       Changes for non-blocking AcknowledgmentsDlg.
 *                                       Changes for non-blocking HelpDlg.
 *                                       Changes for non-blocking RetrieveMergeDlg.
 *                                       Changes for non-blocking LoadSaveDeleteSelectDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class FFFGDlg extends CaveSWTDialog implements ISourceCompAction,
        IFFFGData {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFFGDlg.class);

    private final String MODIFIED = "Modified";

    /**
     * File name composite.
     */
    private Composite fileNameComp;

    /**
     * File name label.
     */
    private Label fileNameLbl;

    /**
     * File name status label.
     */
    private Label fileNameStatusLbl;

    /**
     * Composite for the main controls.
     */
    private Composite mainControlComp;

    /**
     * FFG value text control.
     */
    private Text ffgValueTF;

    /**
     * Expire time text control.
     */
    private Spinner expireTimeSpnr;

    /**
     * Label font.
     */
    private Font labelFont;

    /**
     * List control font.
     */
    private Font listFont;

    /**
     * Status font.
     */
    private Font statusFont;

    /**
     * Sort by county name radio button.
     */
    private Button sortCountyNameRdo;

    /**
     * Sort by county ID radio button.
     */
    private Button sortCountyIdRdo;

    /**
     * County name list control.
     */
    private List countyNameList;

    /**
     * County ID list control.
     */
    private List countyIdList;

    /**
     * Add/Update County button.
     */
    private Button addUpdateCountyBtn;

    /**
     * Show basins for selected counties.
     */
    private Button showBasinsFromCountyChk;

    /**
     * Basin count (number of basins) for the selected counties label.
     */
    private Label basinCountLbl;

    /**
     * Sort basins by name radio button.
     */
    private Button sortBasinNameRdo;

    /**
     * Sort basins by ID radio button.
     */
    private Button sortBasinIdRdo;

    /**
     * Basin name list control.
     */
    private List basinNameList;

    /**
     * Basin ID list control.
     */
    private List basinIdList;

    /**
     * Add/Update basin button.
     */
    private Button addUpdateBasinBtn;

    /**
     * Array of source composites on the display.
     */
    private java.util.List<SourceComp> sourceCompArray;

    /**
     * Maximum number of source columns that can be displayed.
     */
    private final int maxSources = 4;

    /**
     * The currently selected source.
     */
    private SourceComp selectedSrc;

    /**
     * Dialog prompting the user for retrieving options.
     */
    private RetrieveMergeDlg retMergeDlg;

    /**
     * Dialog prompting the user for save file.
     */
    private LoadSaveDeleteSelectDlg saveDlg;

    /**
     * Dialog prompting the user for delete file.
     */
    private LoadSaveDeleteSelectDlg deleteDlg;

    /**
     * Dialog for getting file to retrieve.
     */
    private LoadSaveDeleteSelectDlg loadDlg;

    /**
     * Popup for Help menu item
     */
    private HelpDlg helpDlg;

    /**
     * Popup for About menu item
     */
    private AboutDlg aboutDlg;

    /**
     * All of the available counties.
     */
    private AcknowledgmentsDlg acknowledgmentsDlg;

    /**
     * /** All of the available counties.
     */
    private FFMPCounties counties;

    /**
     * The FFMP Templates.
     */
    private FFMPTemplates templates;

    /**
     * All of the basins that are used to populate the basin name and ID lists.
     */
    private FFMPBasins basinListData;

    /**
     * Flag indicating that all basins are loaded into the lists.
     */
    private boolean allBasinsAreLoaded = false;

    private Label statusLbl = null;

    private Composite leftControlComp;

    private Composite statusComp;

    private SimpleDateFormat statusFormat;

    private Thread dataRetrieveThread = null;

    private LinkedHashMap<String, SrcDisplayDurationData> guidances;

    /**
     * large Text Font
     */
    private Font largeTextFont;

    /**
     * large Spinner Font
     */
    private Font largeSipnnerFont;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public FFFGDlg(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.INDEPENDENT_SHELL
                | CAVE.DO_NOT_BLOCK);
        this.getParent().setCursor(
                this.getParent().getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        setText("Forced Flash Flood Guidance");

        sourceCompArray = new ArrayList<SourceComp>();
        labelFont = new Font(getDisplay(), "Sans", 9, SWT.BOLD);
        listFont = new Font(getDisplay(), "Monospace", 9, SWT.NORMAL);
        statusFont = new Font(getDisplay(), "Arial", 9, SWT.BOLD);
        statusFormat = new SimpleDateFormat("EEE MMM dd HH:mm zzz");
    }

    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    @Override
    protected void disposed() {
        this.getShell().setCursor(
                this.getShell().getDisplay().getSystemCursor(SWT.CURSOR_ARROW));
        labelFont.dispose();
        listFont.dispose();
        statusFont.dispose();

        if (largeSipnnerFont != null)
            largeSipnnerFont.dispose();

        if (largeTextFont != null)
            largeTextFont.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(final Shell shell) {
        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent event) {
                if (confirmClose()) {
                    shell.dispose();
                } else {
                    event.doit = false;
                }
            }
        });
        FFFGData dlgData = new FFFGData();
        dlgData.setCallback(this);
        dataRetrieveThread = new Thread(dlgData);
        dataRetrieveThread.start();

        createMenus();
        createCurrentFileLabel();
        createMainFFFGComposite();
        createBottomActionButtons();
        createStatusBar();
        mainControlComp.pack();

        statusLbl.setText("Retrieving Data...");
    }

    /**
     * Create the menu bar and menu items.
     */
    public void createMenus() {
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
        // Create the File menu
        // -------------------------------------
        MenuItem fileMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuItem.setText("&File");

        // Create the File menu item with a File "dropdown" menu
        Menu fileMenu = new Menu(menuBar);
        fileMenuItem.setMenu(fileMenu);

        // -------------------------------------------------
        // Create all the items in the File dropdown menu
        // -------------------------------------------------

        MenuItem retrieveMI = new MenuItem(fileMenu, SWT.NONE);
        retrieveMI.setText("Retrieve...\tAlt+M");
        retrieveMI.setAccelerator(SWT.ALT + 'M');
        retrieveMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                retrieveSavedData();
            }
        });

        MenuItem saveMI = new MenuItem(fileMenu, SWT.NONE);
        saveMI.setText("Save\tAlt+S");
        saveMI.setAccelerator(SWT.ALT + 'S');
        saveMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveFile();
            }
        });

        MenuItem saveAsMI = new MenuItem(fileMenu, SWT.NONE);
        saveAsMI.setText("Save As...\tAlt+A");
        saveAsMI.setAccelerator(SWT.ALT + 'A');
        saveAsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveFileAs();
            }
        });

        MenuItem deleteMI = new MenuItem(fileMenu, SWT.NONE);
        deleteMI.setText("Delete...\tAlt+D");
        deleteMI.setAccelerator(SWT.ALT + 'D');
        deleteMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteFile();
            }
        });
    }

    /**
     * Create the help menu.
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

        MenuItem helpMI = new MenuItem(helpMenu, SWT.NONE);
        helpMI.setText("Help");
        helpMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                callHelpDlg();

            }
        });

        MenuItem aboutMI = new MenuItem(helpMenu, SWT.NONE);
        aboutMI.setText("About");
        aboutMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                callAboutDlg();
            }
        });

        MenuItem acknowledgmentsMI = new MenuItem(helpMenu, SWT.NONE);
        acknowledgmentsMI.setText("Acknowledgments");
        acknowledgmentsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                callAcknowledgmentsDlg();
            }
        });
    }

    /**
     * Create the label to display the current file.
     */
    private void createCurrentFileLabel() {
        fileNameComp = new Composite(shell, SWT.NONE);

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(3, false);
        fileNameComp.setLayout(gl);
        fileNameComp.setLayoutData(gd);

        Label fnLbl = new Label(fileNameComp, SWT.NONE);
        fnLbl.setText("File Name:");

        fileNameLbl = new Label(fileNameComp, SWT.NONE);

        fileNameStatusLbl = new Label(fileNameComp, SWT.NONE);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Create the main FFG composite that will contain the main controls plus
     * the source composite(s).
     */
    private void createMainFFFGComposite() {
        mainControlComp = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(5, false);
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        gl.horizontalSpacing = 0;
        mainControlComp.setLayout(gl);
        mainControlComp.setLayoutData(gd);

        createLeftSideControls();

        SourceComp sc = new SourceComp(mainControlComp, true, this, labelFont);
        sc.setSelectedBackground(true);
        sc.setGuidanceData(guidances);
        selectedSrc = sc;

        sourceCompArray.add(sc);
    }

    /**
     * Create the FFG controls on the left side of the display.
     */
    private void createLeftSideControls() {
        int listWidth = 225;
        int listHeight = 125;

        leftControlComp = new Composite(mainControlComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        leftControlComp.setLayout(gl);

        /*
         * FFG value and Expiration time controls.
         */
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.heightHint = 170;
        Composite ffgExpireComp = new Composite(leftControlComp, SWT.NONE);
        ffgExpireComp.setLayout(new GridLayout(2, false));
        ffgExpireComp.setLayoutData(gd);

        Label ffgLbl = new Label(ffgExpireComp, SWT.NONE);
        ffgLbl.setText("Enter FFG value (inch): ");

        gd = new GridData(60, SWT.DEFAULT);
        ffgValueTF = new Text(ffgExpireComp, SWT.BORDER);
        ffgValueTF.setLayoutData(gd);
        ffgValueTF.setText("1.0");
        largeTextFont = getLargeFont(ffgValueTF);
        ffgValueTF.setFont(largeTextFont);

        Label expireLbl = new Label(ffgExpireComp, SWT.NONE);
        expireLbl.setText("Enter expiration time (hour): ");

        gd = new GridData(60, SWT.DEFAULT);
        expireTimeSpnr = new Spinner(ffgExpireComp, SWT.BORDER);
        expireTimeSpnr.setLayoutData(gd);
        expireTimeSpnr.setMinimum(0);
        expireTimeSpnr.setMaximum(100000);
        expireTimeSpnr.setSelection(12);
        largeSipnnerFont = getLargeFont(expireTimeSpnr);
        expireTimeSpnr.setFont(largeSipnnerFont);

        /*
         * CWA-Wide header
         */
        createHeaderLabel("Provide CWA-wide FFG Values:", leftControlComp);

        /*
         * CWA-wide FFG Values controls
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = 40;
        Composite ffgValuesComp = new Composite(leftControlComp, SWT.NONE);
        ffgValuesComp.setLayout(new GridLayout(3, false));
        ffgValuesComp.setLayoutData(gd);
        ffgValuesComp.setLayoutData(gd);

        gd = new GridData(SWT.RIGHT, SWT.CENTER, true, true);
        Label areaFFGLbl = new Label(ffgValuesComp, SWT.NONE);
        areaFFGLbl.setText("areaFFG value: ");
        areaFFGLbl.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        Button areaFfgSetBtn = new Button(ffgValuesComp, SWT.PUSH);
        areaFfgSetBtn.setText(" Set ");
        areaFfgSetBtn.setLayoutData(gd);
        areaFfgSetBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (validateFFGValue() == false) {
                    return;
                }
                selectedSrc.setAreaFFG(ffgValueTF.getText().trim());
                updateFileStatusLabel(true);
                setStatusMsg(0, "Inserted {" + ffgValueTF.getText().trim()
                        + " areaFFG} successfully.");
            }
        });

        gd = new GridData(60, SWT.DEFAULT);
        Button clearBtn = new Button(ffgValuesComp, SWT.PUSH);
        clearBtn.setText("Clear");
        clearBtn.setLayoutData(gd);
        clearBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                selectedSrc.setAreaFFG("clear");
            }
        });

        /*
         * Country FFG Values header
         */
        createHeaderLabel("Provide County FFG Values:", leftControlComp);

        /*
         * County radio and list controls
         */
        Composite countyComp = new Composite(leftControlComp, SWT.NONE);
        countyComp.setLayout(new GridLayout(2, false));

        sortCountyNameRdo = new Button(countyComp, SWT.RADIO);
        sortCountyNameRdo.setText("Sort by County Name");
        sortCountyNameRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                counties.sortBy(FFMPCounties.CountySort.DISPLAY_NAME);
                populateCountyLists();
            }
        });

        sortCountyIdRdo = new Button(countyComp, SWT.RADIO);
        sortCountyIdRdo.setText("Sort by County ID");
        sortCountyIdRdo.setSelection(true);
        sortCountyIdRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                counties.sortBy(FFMPCounties.CountySort.ID);
                populateCountyLists();
            }
        });

        gd = new GridData(listWidth, listHeight);
        countyNameList = new List(countyComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL);
        countyNameList.setLayoutData(gd);
        countyNameList.setFont(listFont);

        gd = new GridData(listWidth, listHeight);
        countyIdList = new List(countyComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL);
        countyIdList.setLayoutData(gd);
        countyIdList.setFont(listFont);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        addUpdateCountyBtn = new Button(countyComp, SWT.PUSH);
        addUpdateCountyBtn.setText(" Add/Update Selected County(ies) ");
        addUpdateCountyBtn.setLayoutData(gd);
        addUpdateCountyBtn.setEnabled(false);
        addUpdateCountyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateFileStatusLabel(true);
                addUpdateSourceData(countyNameList, countyIdList,
                        GuidSectType.COUNTY);
            }
        });

        gd = new GridData();
        gd.horizontalSpan = 2;
        showBasinsFromCountyChk = new Button(countyComp, SWT.CHECK);
        showBasinsFromCountyChk.setText("Show basins from selected county");
        showBasinsFromCountyChk.setLayoutData(gd);
        showBasinsFromCountyChk.setSelection(true);
        showBasinsFromCountyChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                populateBasinLists(false);
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        basinCountLbl = new Label(leftControlComp, SWT.NONE);
        basinCountLbl.setText(" NEED TO ADD INFO");
        basinCountLbl.setFont(labelFont);
        basinCountLbl.setBackground(getDisplay()
                .getSystemColor(SWT.COLOR_GREEN));
        basinCountLbl.setLayoutData(gd);

        /*
         * Basin FFG Values header
         */
        createHeaderLabel("Provide Basin FFG Values:", leftControlComp);

        /*
         * Basin radio and list controls
         */
        Composite basinComp = new Composite(leftControlComp, SWT.NONE);
        basinComp.setLayout(new GridLayout(2, false));

        sortBasinNameRdo = new Button(basinComp, SWT.RADIO);
        sortBasinNameRdo.setText("Sort by Basin Name");
        sortBasinNameRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (basinListData != null) {
                    basinListData.sortBy(FFMPBasins.BasinSort.NAME);
                    populateBasinLists(true);
                }
            }
        });

        sortBasinIdRdo = new Button(basinComp, SWT.RADIO);
        sortBasinIdRdo.setText("Sort by Basin ID");
        sortBasinIdRdo.setSelection(true);
        sortBasinIdRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (basinListData != null) {
                    basinListData.sortBy(FFMPBasins.BasinSort.ID);
                    populateBasinLists(true);
                }
            }
        });

        gd = new GridData(listWidth, listHeight);
        basinNameList = new List(basinComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL);
        basinNameList.setLayoutData(gd);
        basinNameList.setFont(listFont);

        gd = new GridData(listWidth, listHeight);
        basinIdList = new List(basinComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        basinIdList.setLayoutData(gd);
        basinIdList.setFont(listFont);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        addUpdateBasinBtn = new Button(basinComp, SWT.PUSH);
        addUpdateBasinBtn.setText(" Add/Update Selected Basin(s) ");
        addUpdateBasinBtn.setEnabled(false);
        addUpdateBasinBtn.setLayoutData(gd);
        addUpdateBasinBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                addUpdateSourceData(basinNameList, basinIdList,
                        GuidSectType.BASIN);
                updateFileStatusLabel(true);
            }
        });

        scrollListsTogether(countyNameList, countyIdList);
        selectCountyListsTogether(countyNameList, countyIdList,
                addUpdateCountyBtn);

        scrollListsTogether(basinNameList, basinIdList);
        selectBasinListsTogether(basinNameList, basinIdList, addUpdateBasinBtn);
    }

    /**
     * Create the action buttons at the bottom of the dialog.
     */
    private void createBottomActionButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, false));
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        buttonComp.setLayoutData(gd);

        int buttonWidth = 120;

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button applyBtn = new Button(buttonComp, SWT.PUSH);
        applyBtn.setText("Apply All");
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                getParent().setCursor(
                        getParent().getDisplay().getSystemCursor(
                                SWT.CURSOR_WAIT));
                shell.setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
                applyDataAction();
                updateFileStatusLabel(false);
                getParent().setCursor(
                        getParent().getDisplay().getSystemCursor(
                                SWT.CURSOR_ARROW));
                shell.setCursor(getDisplay().getSystemCursor(SWT.CURSOR_ARROW));

                saveFileAs();
            }
        });

        Button clearAllBtn = new Button(buttonComp, SWT.PUSH);
        clearAllBtn.setText("Clear All");
        clearAllBtn.setLayoutData(new GridData(buttonWidth, SWT.DEFAULT));
        clearAllBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                clearAllSourceData();
                updateFileStatusLabel(false);
                fileNameLbl.setText("");
                packLabels();
                setStatusMsg("Removed all Forcings");
            }
        });

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button exitBtn = new Button(buttonComp, SWT.PUSH);
        exitBtn.setText("Exit");
        exitBtn.setLayoutData(gd);
        exitBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (confirmClose()) {
                    shell.dispose();
                }
            }
        });
    }

    private void createStatusBar() {
        statusComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        statusComp.setLayout(gl);
        statusComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        Label attrLbl = new Label(statusComp, SWT.NONE);
        attrLbl.setText("Status Messages:");
        attrLbl.setFont(statusFont);
        attrLbl.setBackground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
        attrLbl.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        // attrLbl.setLayoutData(gd);

        GridData gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        statusLbl = new Label(statusComp, SWT.NONE);
        statusLbl.setFont(statusFont);
        statusLbl.setBackground(getDisplay().getSystemColor(SWT.COLOR_YELLOW));
        statusLbl.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
        statusLbl.setLayoutData(gridData);
        statusLbl.setText("");
    }

    public void setStatusMsg(String message) {
        this.setStatusMsg(0, message);
    }

    /**
     * Set the status message
     * 
     * @param status
     *            -1 = error, 0 = information message
     * @param message
     */
    public void setStatusMsg(int status, String message) {
        if (message == null) {
            statusLbl.setText("");
        } else {
            Calendar now = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            Color bg = null;
            if (status == 0) {
                bg = getDisplay().getSystemColor(SWT.COLOR_YELLOW);
            } else {
                // -1 value
                bg = getDisplay().getSystemColor(SWT.COLOR_RED);
            }

            statusLbl.setText(statusFormat.format(now.getTime()) + ": "
                    + message);
            Color white = getDisplay().getSystemColor(SWT.COLOR_WHITE);
            Color black = getDisplay().getSystemColor(SWT.COLOR_BLACK);
            // "Blink" the status bar
            for (int i = 0; i < 2; i++) {
                statusLbl.setBackground(black);
                statusLbl.setForeground(white);
                statusLbl.update();
                try {
                    Thread.sleep(250);
                } catch (InterruptedException e) {
                }
                statusLbl.setBackground(bg);
                statusLbl.setForeground(black);
                statusLbl.update();
                try {
                    Thread.sleep(250);
                } catch (InterruptedException e) {
                }
            }
        }
    }

    /**
     * Create a "header" label with the provided text.
     * 
     * @param text
     *            Label text.
     * @param parentComp
     *            Parent composite.
     */
    private void createHeaderLabel(String text, Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label attrLbl = new Label(parentComp, SWT.NONE);
        attrLbl.setText(" " + text);
        attrLbl.setFont(labelFont);
        attrLbl.setBackground(getDisplay().getSystemColor(SWT.COLOR_DARK_GRAY));
        attrLbl.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        attrLbl.setLayoutData(gd);
    }

    /**
     * Add listeners to the provided county list controls so selecting items
     * from one list will automatically select items from the other list. Also,
     * each selection will trigger updates to the basin lists.
     * 
     * @param list1
     *            List control.
     * @param list2
     *            List control.
     */
    private void selectCountyListsTogether(final List list1, final List list2,
            final Button removeBtn) {
        list1.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean enabled = true;
                list2.deselectAll();
                list2.select(list1.getSelectionIndices());
                if (list1.getSelectionCount() == 0) {
                    enabled = false;
                }

                removeBtn.setEnabled(enabled);
                populateBasinLists(false);
            }
        });

        list2.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean enabled = true;
                list1.deselectAll();
                list1.select(list2.getSelectionIndices());
                if (list2.getSelectionCount() == 0) {
                    enabled = false;
                }

                removeBtn.setEnabled(enabled);
                populateBasinLists(false);
            }
        });
    }

    /**
     * Add listeners to the provided basin list controls so selecting items from
     * one list will automatically select items from the other list.
     * 
     * @param list1
     *            List control.
     * @param list2
     *            List control.
     */
    private void selectBasinListsTogether(final List list1, final List list2,
            final Button removeBtn) {
        list1.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                list2.deselectAll();
                list2.select(list1.getSelectionIndices());
                if (list1.getSelectionCount() == 0) {
                    removeBtn.setEnabled(false);
                    return;
                }

                removeBtn.setEnabled(true);
            }
        });

        list2.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                list1.deselectAll();
                list1.select(list2.getSelectionIndices());
                if (list2.getSelectionCount() == 0) {
                    removeBtn.setEnabled(false);
                    return;
                }

                removeBtn.setEnabled(true);
            }
        });
    }

    /**
     * Add listeners to the provided list controls so scrolling the scroll bar
     * will scroll both lists. The first list will have its vertical scroll bar
     * hidden.
     * 
     * @param list1
     *            List control.
     * @param list2
     *            List control.
     */
    private void scrollListsTogether(final List list1, final List list2) {
        final ScrollBar vBar1 = list1.getVerticalBar();
        final ScrollBar vBar2 = list2.getVerticalBar();
        SelectionListener listener1 = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                vBar2.setSelection(vBar1.getSelection());
            }
        };
        SelectionListener listener2 = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                vBar1.setSelection(vBar2.getSelection());
            }
        };
        vBar1.addSelectionListener(listener1);
        vBar2.addSelectionListener(listener2);

        vBar1.setVisible(false);
    }

    /**
     * Un-select all of the source components.
     */
    private void unselectAllSources() {
        for (SourceComp sc : sourceCompArray) {
            sc.setSelectedBackground(false);
        }
    }

    /**
     * Set the button state (enabled/disabled) of all the source Add ('+')
     * buttons to the value passed in.
     * 
     * @param flag
     *            True/False enabled state.
     */
    private void setAddButtonState(boolean flag) {
        for (SourceComp sc : sourceCompArray) {
            sc.enableAddButton(flag);
        }
    }

    /**
     * Pack all of the source components.
     */
    private void packAllSources() {
        for (SourceComp sc : sourceCompArray) {
            sc.pack();
        }

        mainControlComp.pack();

        int compWidth = mainControlComp.getBounds().width;

        GridData gd = (GridData) statusComp.getLayoutData();
        gd.widthHint = compWidth;
    }

    /**
     * Populate the templates data.
     */
    /**
     * This method will apply the data from the source components to the master
     * data.
     */
    private void applyDataAction() {
        if (validateSources() == false) {
            return;
        }

        if (expireTimeSpnr.getSelection() == 0) {
            StringBuilder sb = new StringBuilder();
            sb.append("Setting the expiration time to zero will cause the forced ");
            sb.append("values to NEVER expire.\n\nDo you wish to continue?");
            int choice = displayConfirmMessageBox("Expire Time", sb.toString(),
                    SWT.ICON_WARNING);

            if (choice == SWT.NO) {
                return;
            }
        }

        FFFGDataMgr fdm = FFFGDataMgr.getInstance();
        ArrayList<SourceCompData> srcCompData = getSourceCompData();
        ArrayList<ArrayList<FFFGBasinIdXML>> basinList = new ArrayList<ArrayList<FFFGBasinIdXML>>();
        for (SourceCompData scd : srcCompData) {
            ArrayList<FFFGBasinIdXML> list = new ArrayList<FFFGBasinIdXML>();
            ArrayList<ValueNameIdData> dataList = scd.getCountyBasinData();
            for (ValueNameIdData vni : dataList) {
                // Get the basins for each county
                ArrayList<Long> pfafs = templates.getAllAggregatePfafs(
                        vni.getId(), "COUNTY");
                for (long pfaf : pfafs) {
                    list.add(new FFFGBasinIdXML(pfaf, vni.getValue()));
                }
            }
            basinList.add(list);
        }

        fdm.updateMasterXML(calculateExpTimeInMillis(), srcCompData, basinList);
        FFMPMonitor.getInstance().updateFFFG();

        setStatusMsg(" Applied all FFG values above to CWA. ");
    }

    /**
     * Calculate the expiration time in milliseconds. Current time plus the
     * offset time.
     * 
     * @return Expiration time in milliseconds.
     */
    private long calculateExpTimeInMillis() {
        int hours = expireTimeSpnr.getSelection();

        long expHoursInMillis = hours * 60 * 60 * 1000;
        if (expHoursInMillis == 0) {
            return expHoursInMillis;
        }

        long expTimeInMillis = SimulatedTime.getSystemTime().getTime()
                .getTime()
                + expHoursInMillis;

        return expTimeInMillis;
    }

    /**
     * Get an array of the source data from each source component.
     * 
     * @return Array of source data.
     */
    private ArrayList<SourceCompData> getSourceCompData() {
        ArrayList<SourceCompData> srcCompData = new ArrayList<SourceCompData>();

        for (SourceComp sc : sourceCompArray) {
            if (sc.hasData() == true) {
                srcCompData.add(sc.getSourceData());
            }
        }

        return srcCompData;
    }

    /**
     * Add/Update the source with the data in the list controls passed in. If
     * there are items in the source list that are in the list passed in the
     * user will be prompted to overwrite the list entries.
     * 
     * @param nameList
     *            Name list control.
     * @param idList
     *            ID list control.
     * @param type
     *            Guidance section type.
     */
    private void addUpdateSourceData(List nameList, List idList,
            GuidSectType type) {
        if (validateFFGValue() == false) {
            return;
        }

        int[] selIndexes = idList.getSelectionIndices();

        if (selIndexes.length == 0) {
            return;
        }

        Long id;
        UpdateType updateType = UpdateType.UPDATE;

        for (int i = 0; i < selIndexes.length; i++) {
            if (type == GuidSectType.COUNTY) {
                id = counties.getCounties().get(selIndexes[i]).getGid();
            } else {
                id = basinListData.getBasins().get(selIndexes[i]).getPfaf();
            }

            if (selectedSrc.containsID(id, type) == true) {
                int result = promptForOverwrite();

                // If the user canceled then return. No action needed.
                if (result == SWT.CANCEL) {
                    return;
                }

                if (result == SWT.YES) {
                    updateType = UpdateType.UPDATE;
                } else {
                    updateType = UpdateType.NO_UPDATE;
                }

                break;
            }
        }

        ArrayList<ValueNameIdData> newDataArray = new ArrayList<ValueNameIdData>();
        ValueNameIdData valNameIdData;

        String srcMsg = "";
        for (int i = 0; i < selIndexes.length; i++) {
            if (type == GuidSectType.COUNTY) {
                // The ID for county needs to be the GID. The ID list control
                // is a hybrid so we need the GID instead.
                valNameIdData = new ValueNameIdData(
                        ffgValueTF.getText().trim(),
                        nameList.getItem(selIndexes[i]), counties.getCounties()
                                .get(selIndexes[i]).getGid(), type);
                srcMsg = srcMsg + " {" + ffgValueTF.getText().trim() + " "
                        + nameList.getItem(selIndexes[i]) + " "
                        + counties.getCounties().get(selIndexes[i]).getGid()
                        + "} ";
            } else {
                // For the basins, the read ID is in the ID list control.
                valNameIdData = new ValueNameIdData(
                        ffgValueTF.getText().trim(),
                        nameList.getItem(selIndexes[i]), basinListData
                                .getBasins().get(selIndexes[i]).getPfaf(), type);
                srcMsg = srcMsg
                        + " {"
                        + ffgValueTF.getText().trim()
                        + " {"
                        + nameList.getItem(selIndexes[i])
                        + "} "
                        + basinListData.getBasins().get(selIndexes[i])
                                .getPfaf() + "}";
            }
            newDataArray.add(valNameIdData);
        }

        selectedSrc.addUpdateNewData(newDataArray, updateType, type);

        if (selIndexes.length <= 2) {
            setStatusMsg("Inserted " + srcMsg + " successfully.");
        } else {
            setStatusMsg("Inserted selected items successfully. ");
        }
    }

    /**
     * Update the basin count label to display how many available basins are in
     * the basin lists.
     */
    private void updateBasinCountLabel() {
        if (showBasinsFromCountyChk.getSelection() == false) {
            int basins = basinNameList.getItemCount();
            basinCountLbl.setText(basins + " total basins in all counties");
            return;
        }

        int selectedCounties = countyNameList.getSelectionCount();
        if (selectedCounties == 0) {
            basinCountLbl.setText("No county(ies) selected.");
            return;
        } else if (selectedCounties == 1) {
            int countyIndex = countyNameList.getSelectionIndex();
            int basins = basinNameList.getItemCount();
            basinCountLbl.setText(basins + " basin(s) in "
                    + countyNameList.getItem(countyIndex) + ", "
                    + countyIdList.getItem(countyIndex));
        } else {
            int basins = basinNameList.getItemCount();
            basinCountLbl.setText(basins + " basin(s) in " + selectedCounties
                    + " counties");
        }
    }

    /**
     * Clear the data in all of the source composites.
     */
    private void clearAllSourceData() {
        MessageBox mb = new MessageBox(shell, SWT.YES | SWT.NO);
        mb.setText("Clear Data");
        mb.setMessage("Do you wish to clear data for all of the sources?");
        int rv = mb.open();

        if (rv == SWT.NO) {
            return;
        }

        for (SourceComp sc : sourceCompArray) {
            sc.clearAllData();
        }
    }

    /**
     * Prompt the user to keep/overwrite the data.
     * 
     * @return SWT value for yes, no, or cancel.
     */
    private int promptForOverwrite() {
        MessageBox mb = new MessageBox(shell, SWT.YES | SWT.NO | SWT.CANCEL);
        mb.setText("Repeated Items");
        mb.setMessage("Some selected items have already been assign FFG.  Do "
                + "you wish to update their FFG values?");
        int rv = mb.open();

        return rv;
    }

    /**
     * Validate the entry in the FFG Value text control.
     * 
     * @return True if the FFG value is valid.
     */
    private boolean validateFFGValue() {
        ffgValueTF.setText(ffgValueTF.getText().trim());
        String ffgText = ffgValueTF.getText().trim();

        if (ffgText.length() == 0) {
            displayMessageBox("Warning", "You need to enter a FFG value.",
                    SWT.ICON_WARNING);
            return false;
        }

        if (ffgText.matches("^[+-]?\\d*\\.?\\d*") == false) {
            StringBuilder msg = new StringBuilder();
            msg.append("An incorrect FFG value was entered.\n");
            msg.append("The FFG value must follow these rules:\n\n");
            msg.append("* Only numbers (including decimals) can be entered in.\n");
            msg.append("* A '+' in front the number will add the number to the FFG value.\n");
            msg.append("* A '-' in front the number will subtract the number to the FFG value.\n");
            msg.append("* Just a number will override the existing FFG value.\n\n");
            msg.append("Valid Examples: +12.1, -4.5, 9, 9.3");
            displayMessageBox("Warning", msg.toString(), SWT.ICON_WARNING);
            return false;
        }

        double ffgVal = Double.valueOf(ffgText);

        /*
         * This check is following the legacy code. A '+' in front of the number
         * will add the value to the existing FFG value. A '-' in front of the
         * number will subtract the value to the existing FFG value. A regular
         * number (no '+' or '-') will replace the FFG value.
         */
        if ((ffgVal < -9.0) || (ffgVal == 0.0) || (ffgVal > 9.0)) {
            StringBuilder msg = new StringBuilder();
            msg.append("The FFG value must be between -9.0 and 9.0 and cannot be zero.\n");
            displayMessageBox("Warning", msg.toString(), SWT.ICON_WARNING);
            return false;
        }

        return true;
    }

    /**
     * Validate the source components. This will identify duplicate source names
     * (need to be fixed) or source components that do not contain any data
     * (will be skipped).
     * 
     * @return True if all sources are valid, false if there are conflicts.
     */
    private boolean validateSources() {
        /*
         * Loop and check if there are duplicate source names.
         */
        HashMap<String, Object> srcNameMap = new HashMap<String, Object>();
        for (SourceComp sc : sourceCompArray) {
            if (srcNameMap.containsKey(sc.getSourceName()) == false) {
                srcNameMap.put(sc.getSourceName(), null);
            } else {
                String msg = "There are multiple sources that have the same source name.";
                displayMessageBox("Warning", msg, SWT.ICON_WARNING);
                return false;
            }
        }

        /*
         * Loop and check if there are sources that do not have any data
         */
        StringBuilder srcNameSB = null;
        for (SourceComp sc : sourceCompArray) {
            if (sc.hasData() == false) {
                if (srcNameSB == null) {
                    srcNameSB = new StringBuilder();
                }
                srcNameSB.append(sc.getSourceName()).append("\n");
            }
        }

        // Commented out per ticket 9825
        // if (srcNameSB != null) {
        // StringBuilder msg = new StringBuilder();
        // msg.append("The following 'sources' do not contain data:\n\n");
        // msg.append(srcNameSB.toString()).append("\n");
        // msg.append("They will be ignored.");
        // displayMessageBox("No Data", msg.toString(), SWT.ICON_INFORMATION);
        // }

        return true;
    }

    /**
     * Display an "OK" message dialog with the provided title, message, and SWT
     * icon.
     * 
     * @param title
     *            Dialog title.
     * @param message
     *            Dialog message.
     * @param swtIcon
     *            SWT icon.
     */
    private void displayMessageBox(String title, String message, int swtIcon) {
        MessageBox mb = new MessageBox(shell, SWT.OK | swtIcon);
        mb.setText(title);
        mb.setMessage(message);
        mb.open();
    }

    /**
     * Display a Yes/No confirmation dialog with the provided title, message,
     * and SWT icon.
     * 
     * @param title
     *            Dialog title.
     * @param message
     *            Dialog message.
     * @param swtIcon
     *            SWT icon.
     * @return The user choice (SWT.YES or SWT.NO).
     */
    private int displayConfirmMessageBox(String title, String message,
            int swtIcon) {
        MessageBox mb = new MessageBox(shell, SWT.YES | SWT.NO | swtIcon);
        mb.setText(title);
        mb.setMessage(message);
        int choice = mb.open();

        return choice;
    }

    /**
     * Populate the county name and ID list control.
     */
    private void populateCountyLists() {
        countyNameList.removeAll();
        countyIdList.removeAll();

        ArrayList<FFMPCounty> countyArray = counties.getCounties();

        for (FFMPCounty county : countyArray) {
            countyNameList.add(county.getDisplayCountyName());
            countyIdList.add(county.getDisplayFips());
        }

        basinNameList.removeAll();
        basinIdList.removeAll();
        populateBasinLists(false);
    }

    /**
     * Populate the basin name and ID list controls.
     */
    private void populateBasinLists(boolean sorted) {
        if (showBasinsFromCountyChk.getSelection() == false) {
            if ((allBasinsAreLoaded == false)
                    || (basinIdList.getItemCount() == 0) || (sorted == true)) {
                ArrayList<FFMPBasinNameIdData> basinArray = new ArrayList<FFMPBasinNameIdData>();

                basinNameList.removeAll();
                basinIdList.removeAll();

                ArrayList<FFMPCounty> countyArray = counties.getCounties();
                ArrayList<Long> pfafArray;
                FFMPBasinNameIdData basinData;
                FFMPBasinMetaData basinMetaData;

                for (FFMPCounty county : countyArray) {
                    pfafArray = templates.getAllAggregatePfafs(county.getGid(),
                            "COUNTY");

                    for (Long pfaf : pfafArray) {
                        basinMetaData = templates.getBasin(pfaf);
                        basinData = new FFMPBasinNameIdData(
                                basinMetaData.getPfaf(),
                                basinMetaData.getStreamName());
                        basinArray.add(basinData);
                    }
                }

                basinListData = null;
                basinListData = new FFMPBasins(basinArray, getBasinSort());

                for (FFMPBasinNameIdData bd : basinArray) {
                    basinNameList.add(bd.getBasinName());
                    basinIdList.add(String.valueOf(bd.getPfaf()));
                }

                if (basinNameList.getItemCount() > 0) {
                    basinNameList.setTopIndex(0);
                    basinIdList.setTopIndex(0);
                }

                allBasinsAreLoaded = true;
            }
        } else {
            basinNameList.removeAll();
            basinIdList.removeAll();

            int[] indexes = countyNameList.getSelectionIndices();

            if (indexes.length == 0) {
                updateBasinCountLabel();
                return;
            }

            ArrayList<FFMPBasinNameIdData> basinArray = new ArrayList<FFMPBasinNameIdData>();
            FFMPBasinNameIdData basinData;
            FFMPBasinMetaData basinMetaData;
            ArrayList<Long> pfafArray;
            ArrayList<FFMPCounty> countyArray = counties.getCounties();

            for (int i = 0; i < indexes.length; i++) {
                pfafArray = templates.getAllAggregatePfafs(
                        countyArray.get(indexes[i]).getGid(), "COUNTY");

                for (Long pfaf : pfafArray) {
                    basinMetaData = templates.getBasin(pfaf);

                    if (basinMetaData == null) {
                        System.out.println("Meta Data is null!");
                    }

                    basinData = new FFMPBasinNameIdData(
                            basinMetaData.getPfaf(),
                            basinMetaData.getStreamName());
                    basinArray.add(basinData);
                }
            }

            basinListData = new FFMPBasins(basinArray, getBasinSort());

            for (FFMPBasinNameIdData bd : basinArray) {
                basinNameList.add(bd.getBasinName());
                basinIdList.add(String.valueOf(bd.getPfaf()));
            }

            if (basinNameList.getItemCount() > 0) {
                basinNameList.setTopIndex(0);
                basinIdList.setTopIndex(0);
            }

            allBasinsAreLoaded = false;
        }

        updateBasinCountLabel();
    }

    /**
     * Get the basin sort by type that determines how the basin is sorted.
     * 
     * @return The basin sort indicator.
     */
    private FFMPBasins.BasinSort getBasinSort() {
        if (sortBasinIdRdo.getSelection() == true) {
            return FFMPBasins.BasinSort.ID;
        }

        return FFMPBasins.BasinSort.NAME;
    }

    /**
     * Retrieve saved data.
     */
    private void retrieveSavedData() {
        if (loadDlg != null) {
            // The RetriveMergeDlg has done its work but still waiting for the
            // user to pick file to retrive.
            loadDlg.open();
            return;
        }

        if (isDialogClear() && (fileNameLbl.getText().trim().length() == 0)) {
            return;
        } else {
            if (retMergeDlg == null) {
                retMergeDlg = new RetrieveMergeDlg(shell);
                retMergeDlg.setCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        if (returnValue instanceof RetrieveMergeAction) {
                            RetrieveMergeAction action = (RetrieveMergeAction) returnValue;
                            getRetrieveFilename(action);
                        }
                        retMergeDlg = null;
                    }
                });
            }
            retMergeDlg.open();
        }
    }

    /**
     * Get file to retrieve and then perform the action.
     * 
     * @param action
     */
    private void getRetrieveFilename(final RetrieveMergeAction action) {

        FFFGDataMgr fdm = FFFGDataMgr.getInstance();

        loadDlg = new LoadSaveDeleteSelectDlg(shell, DialogType.OPEN,
                fdm.getFFFGDataFilePath(), fdm.getFFFGMasterFileName());

        loadDlg.setCloseCallback(new ICloseCallback() {

            @Override
            public void dialogClosed(Object returnValue) {
                if (returnValue instanceof LocalizationFile) {
                    LocalizationFile fileName = (LocalizationFile) returnValue;
                    doRetrieveSavedData(action, fileName);
                }
                loadDlg = null;
            }
        });
        loadDlg.open();
    }

    /**
     * Perform the desired and and update the display.
     * 
     * @param action
     * @param fileName
     */
    private void doRetrieveSavedData(RetrieveMergeAction action,
            LocalizationFile fileName) {

        if (fileName == null) {
            return;
        }
        FFFGDataMgr fdm = FFFGDataMgr.getInstance();

        fdm.loadUserFFFGData(fileName.getFile().getName());

        /*
         * Get an arraylist of SourceCompData from the XML data manager. Leave
         * the expiration time as is, only update the source components.
         */
        ArrayList<SourceCompData> tmpScdArray = fdm.getUserXMLData();
        ArrayList<SourceCompData> scdArray;

        if (tmpScdArray.size() == 0) {
            StringBuilder sb = new StringBuilder();
            sb.append("No data was in the file loaded.      \n");
            sb.append(action.getActionName() + " action aborted.     ");

            displayMessageBox("No Data", sb.toString(), SWT.ICON_WARNING);
            return;
        }

        /*
         * TODO -
         * 
         * IF MERGE or MERGE_OVERWRITE IS SELECTED...
         * 
         * We need to grab the array of source comp data from the sourcesComps
         * and then do the merge/merge_overwrite with the data retrieved.
         * 
         * retrieve: Will clear all data currently in the GUI and replace it
         * with the data read from the chosen file. merge overwrite: Will retain
         * the data currently in the GUI and load the data from the selected
         * file. The data coming in from the selected file will overwrite the
         * data retrieve currently in the GUI only when the two conflict. merge:
         * Will retain the data currently in the GUI and load the data from the
         * selected file. The data coming in from the selected file will not
         * overwrite the data currently in the GUI when the two conflict.
         */

        // TODO : merge the data here and then treat it like a retrieve
        if (action == RetrieveMergeAction.MERGE) {
            /*
             * TODO - merge the data
             */
            scdArray = mergeData(tmpScdArray, getSourceCompData());
        } else if (action == RetrieveMergeAction.MERGE_OVERWRITE) {
            /*
             * TODO - merge and overwrite the data
             */
            scdArray = mergeOverwriteData(tmpScdArray, getSourceCompData());
        } else {
            scdArray = tmpScdArray;
        }

        /*
         * TODO - should probably display a message box if the number of source
         * column is greater than the maximum number of sources (4).
         */

        // Create the number of sources to match the number of elements in the
        // array of Source Comp Data
        createSourcesForExistingData(scdArray);

        if (scdArray.size() != sourceCompArray.size()) {
            StringBuilder sb = new StringBuilder();
            sb.append("There was an error creating new Source controls for the\n");
            sb.append("data being read in.");

            displayMessageBox("Retrieve Error", sb.toString(), SWT.ICON_ERROR);
            return;
        }

        SourceCompData scd;
        SourceComp sc;

        for (int i = 0; i < scdArray.size(); i++) {
            scd = scdArray.get(i);
            sc = sourceCompArray.get(i);

            if (sc.isValidSourceName(scd.getSourceName()) == true) {
                sc.setSourceName(scd.getSourceName());
                sc.setAreaFFG(scd.getAreaFFGValue());
                sc.addCountyBasinData(scd.getCountyBasinData());
                sc.setSourceCompData(scd);
                sc.populateSourceComp();
            } else {
                StringBuilder sb = new StringBuilder();
                sb.append("The source name '").append(scd.getSourceName());
                sb.append("' is an invalid source name.  This source name will be skipped.");
                displayMessageBox("Invalid Source Name", sb.toString(),
                        SWT.ICON_WARNING);
            }
        }

        if (fileName.getFile().getName().compareTo(fdm.getFFFGMasterFileName()) == 0) {
            fileNameLbl.setText(fileName.getFile().getName() + "  (read-only)");
        } else {
            fileNameLbl.setText(fileName.getFile().getName());
        }

        updateFileStatusLabel(false);
        packLabels();
        setStatusMsg("Retrieved " + fileName.getFile().getName()
                + " successfully.");
    }

    /**
     * Help menu popup.
     */
    private void callHelpDlg() {
        if (helpDlg == null) {
            helpDlg = new HelpDlg(shell);
        }
        helpDlg.open();
    }

    /*
     * About menu popup
     */
    private void callAboutDlg() {
        if (aboutDlg == null) {
            aboutDlg = new AboutDlg(shell);
        }
        aboutDlg.open();
    }

    /*
     * Acknowledgments menu popup
     */
    private void callAcknowledgmentsDlg() {
        if (acknowledgmentsDlg == null) {
            acknowledgmentsDlg = new AcknowledgmentsDlg(shell);
        }
        acknowledgmentsDlg.open();
    }

    /**
     * Create the total number of sources that will be needed.
     * 
     * @param numSources
     *            Number of sources.
     */
    private void createSourcesForExistingData(ArrayList<SourceCompData> scdArray) {
        // Reinitialize all of the source components.
        reinitializeSourceComps();

        /*
         * Start the array at one since we never delete the first source
         * component. Add the correct number of sources.
         */
        for (int i = 1; i < scdArray.size(); i++) {
            addNewSource(scdArray.get(i));
        }

        selectedSrc = sourceCompArray.get(0);
        selectSource(selectedSrc);
    }

    /**
     * Save the source data to the user file.
     */
    private void saveFile() {
        FFFGDataMgr fdm = FFFGDataMgr.getInstance();
        if (fdm.getUserFileName() == null) {
            saveFileAs();
            return;
        }

        if (fdm.getUserFileName().compareTo(fdm.getFFFGMasterFileName()) == 0) {
            saveFileAs();
            return;
        }

        ArrayList<SourceCompData> srcCompData = getSourceCompData();

        if (srcCompData.size() == 0) {
            StringBuilder sb = new StringBuilder();
            sb.append("The source(s) do not contain data.  Saving operation\n");
            sb.append("will be canceled.\n");

            displayMessageBox("No Data", sb.toString(), SWT.ICON_WARNING);
            return;
        }

        ArrayList<ArrayList<FFFGBasinIdXML>> basinList = new ArrayList<ArrayList<FFFGBasinIdXML>>();
        for (SourceCompData scd : srcCompData) {
            ArrayList<FFFGBasinIdXML> list = new ArrayList<FFFGBasinIdXML>();
            ArrayList<ValueNameIdData> dataList = scd.getCountyBasinData();
            for (ValueNameIdData vni : dataList) {
                // Get the basins for each county
                ArrayList<Long> pfafs = templates.getAllAggregatePfafs(
                        vni.getId(), "COUNTY");
                for (long pfaf : pfafs) {
                    list.add(new FFFGBasinIdXML(pfaf, vni.getValue()));
                }
            }
            basinList.add(list);
        }
        fdm.saveUpdateUserXML(calculateExpTimeInMillis(), srcCompData,
                basinList);

        // Update the File name label
        fileNameLbl.setText(fdm.getUserFileName());
        updateFileStatusLabel(false);
        packLabels();

        this.setStatusMsg("Saved " + fdm.getUserFileName() + " successfully. ");
    }

    /**
     * Save the file to a user selected file name.
     */
    private void saveFileAs() {
        if (saveDlg == null) {
            FFFGDataMgr fdm = FFFGDataMgr.getInstance();

            saveDlg = new LoadSaveDeleteSelectDlg(shell, DialogType.SAVE_AS,
                    fdm.getFFFGDataFilePath(), fdm.getFFFGMasterFileName());
            saveDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof LocalizationFile) {
                        LocalizationFile fileName = (LocalizationFile) returnValue;
                        doSaveFileAs(fileName);
                    }
                    saveDlg = null;
                }
            });
        }
        saveDlg.open();
    }

    private void doSaveFileAs(LocalizationFile fileName) {
        FFFGDataMgr fdm = FFFGDataMgr.getInstance();

        ArrayList<SourceCompData> srcCompData = getSourceCompData();

        if (srcCompData.size() == 0) {
            StringBuilder sb = new StringBuilder();
            sb.append("The source(s) do not contain data.  Saving operation\n");
            sb.append("will be canceled.\n");

            displayMessageBox("No Data", sb.toString(), SWT.ICON_WARNING);
            return;
        }

        ArrayList<ArrayList<FFFGBasinIdXML>> basinList = new ArrayList<ArrayList<FFFGBasinIdXML>>();
        for (SourceCompData scd : srcCompData) {
            ArrayList<FFFGBasinIdXML> list = new ArrayList<FFFGBasinIdXML>();
            ArrayList<ValueNameIdData> dataList = scd.getCountyBasinData();
            for (ValueNameIdData vni : dataList) {
                // Get the basins for each county
                ArrayList<Long> pfafs = templates.getAllAggregatePfafs(
                        vni.getId(), "COUNTY");
                for (long pfaf : pfafs) {
                    list.add(new FFFGBasinIdXML(pfaf, vni.getValue()));
                }
            }
            basinList.add(list);
        }
        fdm.saveAsUpdateUserXML(fileName.getFile().getName(),
                calculateExpTimeInMillis(), srcCompData, basinList);

        // Update the File name label
        fileNameLbl.setText(fdm.getUserFileName());
        updateFileStatusLabel(false);
        packLabels();

        this.setStatusMsg("Saved as " + fdm.getUserFileName()
                + " successfully. ");
    }

    /**
     * Delete the user selected file.
     */
    private void deleteFile() {
        if (deleteDlg == null) {
            FFFGDataMgr fdm = FFFGDataMgr.getInstance();

            deleteDlg = new LoadSaveDeleteSelectDlg(shell, DialogType.DELETE,
                    fdm.getFFFGDataFilePath(), fdm.getFFFGMasterFileName());
            deleteDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof LocalizationFile) {
                        LocalizationFile fileName = (LocalizationFile) returnValue;
                        doDeleteFile(fileName);
                    }
                    deleteDlg = null;
                };
            });
        }
        deleteDlg.open();
    }

    private void doDeleteFile(LocalizationFile fileName) {
        FFFGDataMgr fdm = FFFGDataMgr.getInstance();
        String name = fileName.getFile().getName();

        try {
            if (fileName.getFile().getName().compareTo(fdm.getUserFileName()) == 0) {
                fdm.deleteUserXMLData(fileName.getFile().getName());
                fileNameLbl.setText("");
                updateFileStatusLabel(false);
            }
            fileName.delete();
            setStatusMsg("Deleted " + name + " successfully. ");
            fileName = null;
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to delete file \""
                    + fileName.toString() + "\"", e);
            setStatusMsg("Problem deleting file: " + name);
        }
    }

    /**
     * Remove all of the source components except the first one - only clear its
     * data.
     */
    private void reinitializeSourceComps() {

        if (sourceCompArray.size() == 1) {
            sourceCompArray.get(0).clearAllData();
            return;
        }

        unselectAllSources();

        for (int i = 0; i < sourceCompArray.size(); i++) {
            sourceCompArray.get(i).dispose();
        }

        sourceCompArray.clear();

        SourceComp sc = new SourceComp(mainControlComp, true, this, labelFont);
        sc.setSelectedBackground(true);
        sc.setGuidanceData(guidances);

        sourceCompArray.add(sc);

        selectedSrc = sc;

        packAllSources();
        shell.pack();
    }

    private ArrayList<SourceCompData> mergeData(
            ArrayList<SourceCompData> retrievedDataArray,
            ArrayList<SourceCompData> displayDataArray) {
        ArrayList<SourceCompData> newDataArray = new ArrayList<SourceCompData>(
                displayDataArray);

        /*
         * Loop over the arrays and compare the content. Do not overwrite any
         * entries that may be the same.
         */

        boolean foundMatchingScd = false;

        for (SourceCompData retrievedScd : retrievedDataArray) {
            foundMatchingScd = false;

            for (int i = 0; i < newDataArray.size(); i++) {
                if (newDataArray.get(i).getSourceName()
                        .compareTo(retrievedScd.getSourceName()) == 0) {
                    foundMatchingScd = true;
                    newDataArray.get(i).mergeCountyBasinData(
                            retrievedScd.getCountyBasinData());

                    newDataArray.get(i).mergeAreaFFG(
                            retrievedScd.getAreaFFGValue());

                    break;
                }
            }

            if (foundMatchingScd == false) {
                newDataArray.add(retrievedScd);
            }
        }

        return newDataArray;
    }

    private ArrayList<SourceCompData> mergeOverwriteData(
            ArrayList<SourceCompData> retrievedDataArray,
            ArrayList<SourceCompData> displayDataArray) {
        ArrayList<SourceCompData> newDataArray = new ArrayList<SourceCompData>(
                displayDataArray);

        /*
         * Loop over the arrays and compare the content. Overwrite any entries
         * that may be the same.
         */

        boolean foundMatchingScd = false;

        for (SourceCompData retrievedScd : retrievedDataArray) {
            foundMatchingScd = false;

            for (int i = 0; i < newDataArray.size(); i++) {
                if (newDataArray.get(i).getSourceName()
                        .compareTo(retrievedScd.getSourceName()) == 0) {
                    foundMatchingScd = true;
                    newDataArray.get(i).setAreaFFGValue(
                            retrievedScd.getAreaFFGValue());
                    newDataArray.get(i).mergeOverwriteCountyBasinData(
                            retrievedScd.getCountyBasinData());
                    break;
                }
            }

            if (foundMatchingScd == false) {
                newDataArray.add(retrievedScd);
            }
        }

        return newDataArray;
    }

    /**
     * Add a new source to the display.
     */
    @Override
    public void addNewSource(SourceCompData scd) {

        unselectAllSources();

        SourceComp sc = new SourceComp(mainControlComp, false, this, labelFont);
        sc.setSelectedBackground(true);
        sc.setGuidanceData(guidances);
        if (scd != null) {
            sc.setSourceCompData(scd);
        }
        sc.populateSourceComp();

        sourceCompArray.add(sc);
        selectedSrc = sc;

        /*
         * If adding a new source gets the list up to the maximum then disable
         * all of the "add" buttons.
         */
        if (sourceCompArray.size() == maxSources) {
            setAddButtonState(false);
        }

        packAllSources();
        shell.pack();
    }

    /**
     * Remove a source from the display.
     */
    @Override
    public void removeSource(SourceComp srcComp) {
        /*
         * Check to see it we are trying to remove the first source. Do not
         * remove the first source as there needs to be at least one source
         * component on the display.
         */
        if (sourceCompArray.indexOf(srcComp) == 0) {
            return;
        }

        unselectAllSources();
        sourceCompArray.remove(srcComp);
        sourceCompArray.get(sourceCompArray.size() - 1).setSelectedBackground(
                true);
        setAddButtonState(true);

        selectedSrc = sourceCompArray.get(sourceCompArray.size() - 1);

        srcComp.dispose();

        packAllSources();
        shell.pack();
    }

    /**
     * Select the specified source.
     */
    @Override
    public void selectSource(SourceComp srcComp) {

        unselectAllSources();
        srcComp.setSelectedBackground(true);

        selectedSrc = srcComp;

        packAllSources();
        shell.pack();
    }

    /**
     * Update the file status label to "Modified" with a yellow background if
     * the data for a loaded file have been modified.
     * 
     * @param isModified
     *            true if data have been modified
     */
    private void updateFileStatusLabel(boolean isModified) {
        if (isModified && (fileNameLbl.getText().trim().length() > 0)) {
            fileNameStatusLbl.setBackground(getDisplay().getSystemColor(
                    SWT.COLOR_YELLOW));
            fileNameStatusLbl.setText(MODIFIED);
        } else {
            fileNameStatusLbl.setText("");
        }
        packLabels();
    }

    private void packLabels() {
        fileNameLbl.pack();
        fileNameStatusLbl.pack();
        fileNameComp.layout();
    }

    private boolean isDialogClear() {
        ArrayList<SourceCompData> srcCompDataList = getSourceCompData();
        for (SourceCompData srcComp : srcCompDataList) {
            if ((srcComp.getAreaFFGValue() != null)
                    && (srcComp.getAreaFFGValue().length() > 0)) {
                return false;
            }

            if ((srcComp.getCountyBasinData() != null)
                    && (srcComp.getCountyBasinData().size() > 0)) {
                return false;
            }
        }

        return true;
    }

    /**
     * populate the dialog with the data retrieved via FFFGData.
     */
    private void populateDialog() {
        populateCountyLists();
        populateBasinLists(true);

        SourceComp sc = sourceCompArray.get(0);
        sc.setGuidanceData(guidances);
        sc.populateSourceComp();

        statusLbl.setText("");
        this.getParent()
                .setCursor(
                        this.getParent().getDisplay()
                                .getSystemCursor(SWT.CURSOR_ARROW));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ffmp.fffg.ISourceCompAction#setModified(boolean
     * )
     */
    @Override
    public void setModified(boolean isModified) {
        updateFileStatusLabel(isModified);
    }

    private boolean confirmClose() {
        MessageBox mb = new MessageBox(shell, SWT.YES | SWT.NO);
        mb.setText("Close Dialog?");
        mb.setMessage("Are you sure you want to exit?");
        int close = mb.open();
        if (close == SWT.YES) {
            return true;
        }

        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ffmp.fffg.IFFFGData#setCounties()
     */
    @Override
    public void setCounties(FFMPCounties counties) {
        this.counties = counties;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ffmp.fffg.IFFFGData#setTemplates()
     */
    @Override
    public void setTemplates(FFMPTemplates templates) {
        this.templates = templates;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ffmp.fffg.IFFFGData#setGuidances(java.util
     * .LinkedHashMap)
     */
    @Override
    public void setGuidances(
            LinkedHashMap<String, SrcDisplayDurationData> guidances) {
        this.guidances = guidances;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ffmp.fffg.IFFFGData#setDataLoadComplete(boolean
     * )
     */
    @Override
    public void dataLoadComplete() {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                if (!shell.isDisposed()) {
                    populateDialog();
                }
            }
        });
    }

    /**
     * FFFG GUI large font for: Text ffgValueTF and Spinner expireTimeSpnr.
     * 
     * @param ctrl
     *            : Control to change Font
     * @return: large Font
     */
    private Font getLargeFont(org.eclipse.swt.widgets.Control ctrl) {

        if (ctrl == null)
            return new Font(getDisplay(),
                    new org.eclipse.swt.graphics.FontData());

        Font font = ctrl.getFont();
        org.eclipse.swt.graphics.FontData[] fontData = font.getFontData();

        for (int i = 0; i < fontData.length; i++) {
            fontData[i].setStyle(SWT.BOLD);
            fontData[i].setHeight(16);
        }

        return new Font(getDisplay(), fontData);

    }

}