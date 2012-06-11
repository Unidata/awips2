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
package com.raytheon.viz.hydrobase;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.localization.LocalizationPerspectiveUtils;
import com.raytheon.uf.viz.localization.service.ILocalizationService;
import com.raytheon.viz.hydrobase.PreferencesData.SortCriteria;
import com.raytheon.viz.hydrobase.dialogs.AddModifyLocationDlg;
import com.raytheon.viz.hydrobase.dialogs.AdministrationDlg;
import com.raytheon.viz.hydrobase.dialogs.ArealDefinitionsDlg;
import com.raytheon.viz.hydrobase.dialogs.BenchmarkDlg;
import com.raytheon.viz.hydrobase.dialogs.CountyZoneUgcDlg;
import com.raytheon.viz.hydrobase.dialogs.DataAdjustFactorDlg;
import com.raytheon.viz.hydrobase.dialogs.DataIngestFilterDlg;
import com.raytheon.viz.hydrobase.dialogs.DataPurgeParamsDlg;
import com.raytheon.viz.hydrobase.dialogs.DatumDlg;
import com.raytheon.viz.hydrobase.dialogs.DescriptionDlg;
import com.raytheon.viz.hydrobase.dialogs.FloodCategoryDlg;
import com.raytheon.viz.hydrobase.dialogs.FloodDamageDlg;
import com.raytheon.viz.hydrobase.dialogs.GageHistoryDlg;
import com.raytheon.viz.hydrobase.dialogs.HydroGenConfigDlg;
import com.raytheon.viz.hydrobase.dialogs.LowWaterDlg;
import com.raytheon.viz.hydrobase.dialogs.NwrTransmitterDlg;
import com.raytheon.viz.hydrobase.dialogs.PreferencesDlg;
import com.raytheon.viz.hydrobase.dialogs.PublicationsDlg;
import com.raytheon.viz.hydrobase.dialogs.QcAlertAlarmLimitsDlg;
import com.raytheon.viz.hydrobase.dialogs.RadarLocationsDlg;
import com.raytheon.viz.hydrobase.dialogs.ReferenceFieldsDlg;
import com.raytheon.viz.hydrobase.dialogs.ReferencesDlg;
import com.raytheon.viz.hydrobase.dialogs.ReservoirDlg;
import com.raytheon.viz.hydrobase.dialogs.RiverGageDlg;
import com.raytheon.viz.hydrobase.dialogs.RiverProFcstGrpPointsDlg;
import com.raytheon.viz.hydrobase.dialogs.RiverProGenParamsDlg;
import com.raytheon.viz.hydrobase.dialogs.StatesCountiesZonesDlg;
import com.raytheon.viz.hydrobase.dialogs.StationFilterOptionsDlg;
import com.raytheon.viz.hydrobase.listeners.IPreferencesListener;
import com.raytheon.viz.hydrobase.listeners.IStationFilterListener;
import com.raytheon.viz.hydrobase.listeners.IStationListener;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.IGetSortType;
import com.raytheon.viz.hydrocommon.contacts.ContactsDlg;
import com.raytheon.viz.hydrocommon.cresthistory.CrestHistoryDlg;
import com.raytheon.viz.hydrocommon.data.AdministrationData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.hydrocommon.datasources.DataSourcesDlg;
import com.raytheon.viz.hydrocommon.impactstatement.ImpactStatementDlg;
import com.raytheon.viz.hydrocommon.lowwaterstatment.LowWaterStatementDlg;
import com.raytheon.viz.hydrocommon.ratingcurve.RatingCurveDlg;
import com.raytheon.viz.hydrocommon.textreport.TextReportDataManager;
import com.raytheon.viz.hydrocommon.textreport.TextReportDlg;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the main Hydrobase dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 4, 2008				lvenable	Initial creation.
 * 12/18/2008   1782        grichard    Added lid to constructors.
 * 03/27/2009   2145        mduff       Removed the Vector/Areal Definition menu items.
 * 04/23/2009   2181        mduff       Made station search case-insensitive.
 * 09/09/2009   2769        mduff       Made this a listener for add
 *                                      delete changes so the list
 *                                      updates on changes.
 * 11/03/2009   2770        mduff       Added password protection.
 * 01/22/2010   4245        mduff       Fixed bug with station search.
 * 07/03/2010   6586        mduff       Fixed problem introduced from the 
 *                                      CaveSWTDialog refactor.
 * 07/12/2010   5286       lbousaid     The edited station in Modify Location Gui will
 *                                      stay highlighted in the hydrobase list after
 *                                      an update.
 * 
 * 05/09/2011   9151	   lbousaid     open Modify Location window on double click
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class HydroBaseDlg extends CaveSWTDialog implements IGetSortType,
        IPreferencesListener, IStationFilterListener, IStationListener,
        KeyListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(HydroBaseDlg.class);

    /**
     * Flood category menu item.
     */
    private MenuItem floodCategoryMI;

    /**
     * Impact statement menu item.
     */
    private MenuItem impactStatementMI;

    /**
     * Low water statement menu item.
     */
    private MenuItem lowWaterStatementMI;

    /**
     * Flood damage menu item.
     */
    private MenuItem floodDamageMI;

    /**
     * Rating curve menu item.
     */
    private MenuItem ratingCurveMI;

    /**
     * Unit hydrograph menu item.
     */
    private MenuItem unitHydrographMI;

    /**
     * Crest history menu item.
     */
    private MenuItem crestHistoryMI;

    /**
     * Low water menu item.
     */
    private MenuItem lowWaterMI;

    /**
     * Benchmark menu item.
     */
    private MenuItem benchmarkMI;

    /**
     * Datum menu item.
     */
    private MenuItem datumMI;

    /**
     * Description menu item.
     */
    private MenuItem descriptionMI;

    /**
     * Publications menu item.
     */
    private MenuItem publicationsMI;

    /**
     * References menu item.
     */
    private MenuItem referencesMI;

    /**
     * Data list control.
     */
    private List dataList;

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Station count label.
     */
    private Label stationCountLbl;

    /**
     * Sort by combo box.
     */
    private Combo sortByCbo;

    /**
     * Station search text control.
     */
    private Text stationSearchTF;

    /**
     * Array of hydro station data.
     */
    private ArrayList<HydroStationData> stationArray;

    private ArrayList<MenuItem> riverGageMenuItems = new ArrayList<MenuItem>();

    private boolean showBasin;

    private boolean showLatLon;

    private boolean showRiverStream;

    private boolean showStateCounty;

    private Label listLbl;

    private String selectedLid = null;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param titleInfo
     *            Dialog title information.
     */
    public HydroBaseDlg(Shell parent, String titleInfo, String selectedLid) {
        super(parent, SWT.DIALOG_TRIM | SWT.MIN, CAVE.INDEPENDENT_SHELL);
        setText("HydroBase on " + titleInfo);
        this.selectedLid = selectedLid;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        controlFont.dispose();

        // release the text report data
        TextReportDataManager.getInstance().dispose();
        shell.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        stationArray = new ArrayList<HydroStationData>();
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        getColumnPreferences();

        // Create the menus
        createMenus();

        // Create the data list control
        createDataListControl();

        // Create the bottom controls
        createBottomControls();

        populateListControl();
    }

    /**
     * Create the menu bar and menus.
     */
    private void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createFileMenu(menuBar);
        createLocationMenu(menuBar);
        createRiverGageMenu(menuBar);
        createReservoirMenu(menuBar);
        createDataIngestMenu(menuBar);
        createReportsMenu(menuBar);
        createSetupMenu(menuBar);

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

        // Configure menu item
        MenuItem preferencesMI = new MenuItem(fileMenu, SWT.NONE);
        preferencesMI.setText("&Preferences...");
        preferencesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                openPreferencesDialog();
            }
        });

        new MenuItem(fileMenu, SWT.SEPARATOR);

        // Exit menu item
        MenuItem exitMI = new MenuItem(fileMenu, SWT.NONE);
        exitMI.setText("E&xit\tCtrl+X");
        exitMI.setAccelerator(SWT.CTRL + 'X');
        exitMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // release the text report data
                TextReportDataManager.getInstance().dispose();
                shell.dispose();
            }
        });
    }

    private void openPreferencesDialog() {
        PreferencesDlg prefDlg = new PreferencesDlg(shell);
        prefDlg.addListener(this);
        prefDlg.open();
        prefDlg.removeListener(this);
    }

    /**
     * Create the Location menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createLocationMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Location menu
        // -------------------------------------
        MenuItem locationMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        locationMenuItem.setText("&Location");

        // Create the Location menu item with a Location "dropdown" menu
        Menu locationMenu = new Menu(menuBar);
        locationMenuItem.setMenu(locationMenu);

        // -------------------------------------------------
        // Create all the items in the Location dropdown menu
        // -------------------------------------------------

        // Add Location menu item
        MenuItem addLocationMI = new MenuItem(locationMenu, SWT.NONE);
        addLocationMI.setText("&Add Location...\tCtrl+A");
        addLocationMI.setAccelerator(SWT.CTRL + 'A');
        addLocationMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                openAddDlg();
            }
        });

        // Modify Location menu item
        MenuItem modifyLocationMI = new MenuItem(locationMenu, SWT.NONE);
        modifyLocationMI.setText("&Modify Location...\tCtrl+M");
        modifyLocationMI.setAccelerator(SWT.CTRL + 'M');
        modifyLocationMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                openModifyDlg();
            }
        });

        new MenuItem(locationMenu, SWT.SEPARATOR);

        // Contacts menu item
        MenuItem contactsMI = new MenuItem(locationMenu, SWT.NONE);
        contactsMI.setText("&Contacts...\tCtrl+C");
        contactsMI.setAccelerator(SWT.CTRL + 'C');
        contactsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {

                String lid = getSelectedLocation().getStation();
                String lidLoc = getStationAndName();

                ContactsDlg contactsDlg = new ContactsDlg(shell, lidLoc, true,
                        lid);
                contactsDlg.open();
            }
        });

        // County/Zone UGC menu item
        MenuItem countyZoneUgcMI = new MenuItem(locationMenu, SWT.NONE);
        countyZoneUgcMI.setText("County/Zone &UGC...\tCtrl+U");
        countyZoneUgcMI.setAccelerator(SWT.CTRL + 'U');
        countyZoneUgcMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                CountyZoneUgcDlg czDlg = new CountyZoneUgcDlg(shell,
                        getStationAndName(), getSelectedLocation().getStation());
                czDlg.open();
            }
        });

        // Gage History menu item
        MenuItem gageHistoryMI = new MenuItem(locationMenu, SWT.NONE);
        gageHistoryMI.setText("Gage History...\tCtrl+G");
        gageHistoryMI.setAccelerator(SWT.CTRL + 'G');
        gageHistoryMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GageHistoryDlg ghDlg = new GageHistoryDlg(shell,
                        getStationAndName(), getSelectedLocation().getStation());
                ghDlg.open();
            }
        });

        // Data Sources menu item
        MenuItem dataSourcesMI = new MenuItem(locationMenu, SWT.NONE);
        dataSourcesMI.setText("Data Sources...\tCtrl+D");
        dataSourcesMI.setAccelerator(SWT.CTRL + 'D');
        dataSourcesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                DataSourcesDlg dataSourcesDlg = new DataSourcesDlg(shell,
                        getStationAndName(),
                        getSelectedLocation().getStation(), true);
                dataSourcesDlg.open();
            }
        });
    }

    /**
     * Create the River Gage menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createRiverGageMenu(Menu menuBar) {
        // -------------------------------------
        // Create the River Gage menu
        // -------------------------------------
        MenuItem riverGageMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        riverGageMenuItem.setText("&River Gage");

        // Create the River Gage menu item with a River Gage "dropdown" menu
        Menu riverGageMenu = new Menu(menuBar);
        riverGageMenuItem.setMenu(riverGageMenu);

        // ------------------------------------------------------
        // Create all the items in the River Gage dropdown menu
        // ------------------------------------------------------

        // River Gage menu item
        MenuItem riverGageMI = new MenuItem(riverGageMenu, SWT.NONE);
        riverGageMI.setText("&River Gage...\tCtrl+R");
        riverGageMI.setAccelerator(SWT.CTRL + 'R');
        riverGageMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                RiverGageDlg riverGageDlg = new RiverGageDlg(shell,
                        getStationAndName(), getSelectedLocation().getStation());
                riverGageDlg.open();
            }
        });

        new MenuItem(riverGageMenu, SWT.SEPARATOR);

        // Flood Category menu item
        floodCategoryMI = new MenuItem(riverGageMenu, SWT.NONE);
        floodCategoryMI.setText("Flood Ca&tegory...\tCtrl+T");
        floodCategoryMI.setAccelerator(SWT.CTRL + 'T');
        floodCategoryMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                FloodCategoryDlg floodCatDlg = new FloodCategoryDlg(shell,
                        getStationAndName(), getSelectedLocation().getStation());
                floodCatDlg.open();
            }
        });
        riverGageMenuItems.add(floodCategoryMI);

        // Impact Statement menu item
        impactStatementMI = new MenuItem(riverGageMenu, SWT.NONE);
        impactStatementMI.setText("&Impact Statement...\tCtrl+I");
        impactStatementMI.setAccelerator(SWT.CTRL + 'T');
        impactStatementMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ImpactStatementDlg impactStatementDlg = new ImpactStatementDlg(
                        shell, getStationAndName(), getSelectedLocation()
                                .getStation(), true);
                impactStatementDlg.open();
            }
        });
        riverGageMenuItems.add(impactStatementMI);

        // Low Water Statement menu item
        lowWaterStatementMI = new MenuItem(riverGageMenu, SWT.NONE);
        lowWaterStatementMI.setText("Lo&w Water Statement...\tCtrl+W");
        lowWaterStatementMI.setAccelerator(SWT.CTRL + 'W');
        lowWaterStatementMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                LowWaterStatementDlg lowWaterStmntDlg = new LowWaterStatementDlg(
                        shell, getStationAndName(), true, getSelectedLocation()
                                .getStation());
                lowWaterStmntDlg.open();
            }
        });
        riverGageMenuItems.add(lowWaterStatementMI);

        // Flood Damage menu item
        floodDamageMI = new MenuItem(riverGageMenu, SWT.NONE);
        floodDamageMI.setText("&Flood Damage...\tCtrl+F");
        floodDamageMI.setAccelerator(SWT.CTRL + 'F');
        floodDamageMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                FloodDamageDlg floodDamDlg = new FloodDamageDlg(shell,
                        getStationAndName(), getSelectedLocation().getStation());
                floodDamDlg.open();
            }
        });
        riverGageMenuItems.add(floodDamageMI);

        // Rating Curve menu item
        ratingCurveMI = new MenuItem(riverGageMenu, SWT.NONE);
        ratingCurveMI.setText("Rati&ng Curve...\tCtrl+N");
        ratingCurveMI.setAccelerator(SWT.CTRL + 'N');
        ratingCurveMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                RatingCurveDlg ratingCurveDlg = new RatingCurveDlg(shell,
                        getStationLid(), getStationAndName(), true);
                ratingCurveDlg.open();

            }
        });
        riverGageMenuItems.add(ratingCurveMI);

        // Unit Hydrograph menu item
        unitHydrographMI = new MenuItem(riverGageMenu, SWT.NONE);
        unitHydrographMI.setText("Unit Hydrograph...");
        unitHydrographMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                UnitHydrographAction action = new UnitHydrographAction();
                action.launch(getStationLid());
            }
        });
        riverGageMenuItems.add(unitHydrographMI);

        new MenuItem(riverGageMenu, SWT.SEPARATOR);

        // Crest History menu item
        crestHistoryMI = new MenuItem(riverGageMenu, SWT.NONE);
        crestHistoryMI.setText("Cre&st History...\tCtrl+S");
        crestHistoryMI.setAccelerator(SWT.CTRL + 'S');
        crestHistoryMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                CrestHistoryDlg crestHistDlg = new CrestHistoryDlg(shell,
                        getStationLid(), getStationAndName(), true);
                crestHistDlg.open();
            }
        });
        riverGageMenuItems.add(crestHistoryMI);

        // Low Water menu item
        lowWaterMI = new MenuItem(riverGageMenu, SWT.NONE);
        lowWaterMI.setText("&Low Water...\tCtrl+L");
        lowWaterMI.setAccelerator(SWT.CTRL + 'L');
        lowWaterMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                LowWaterDlg lowWaterDlg = new LowWaterDlg(shell,
                        getStationAndName(), getSelectedLocation().getStation());
                lowWaterDlg.open();
            }
        });
        riverGageMenuItems.add(lowWaterMI);

        new MenuItem(riverGageMenu, SWT.SEPARATOR);

        // Benchmark menu item
        benchmarkMI = new MenuItem(riverGageMenu, SWT.NONE);
        benchmarkMI.setText("&Benchmark...\tCtrl+B");
        benchmarkMI.setAccelerator(SWT.CTRL + 'B');
        benchmarkMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                BenchmarkDlg benchmarkDlg = new BenchmarkDlg(shell,
                        getStationAndName(), getSelectedLocation().getStation());
                benchmarkDlg.open();
            }
        });
        riverGageMenuItems.add(benchmarkMI);

        // Datum menu item
        datumMI = new MenuItem(riverGageMenu, SWT.NONE);
        datumMI.setText("Datu&m...\tCtrl+J");
        datumMI.setAccelerator(SWT.CTRL + 'J');
        datumMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                DatumDlg datumDlg = new DatumDlg(shell, getStationAndName(),
                        getSelectedLocation().getStation());
                datumDlg.open();
            }
        });
        riverGageMenuItems.add(datumMI);

        // Description menu item
        descriptionMI = new MenuItem(riverGageMenu, SWT.NONE);
        descriptionMI.setText("Des&cription...\tCtrl+K");
        descriptionMI.setAccelerator(SWT.CTRL + 'K');
        descriptionMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                DescriptionDlg descDlg = new DescriptionDlg(shell,
                        getStationAndName(), getSelectedLocation().getStation());
                descDlg.open();
            }
        });
        riverGageMenuItems.add(descriptionMI);

        new MenuItem(riverGageMenu, SWT.SEPARATOR);

        // Publications menu item
        publicationsMI = new MenuItem(riverGageMenu, SWT.NONE);
        publicationsMI.setText("&Publications...\tCtrl+P");
        publicationsMI.setAccelerator(SWT.CTRL + 'P');
        publicationsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                PublicationsDlg publicationsDlg = new PublicationsDlg(shell,
                        getStationAndName(), getSelectedLocation().getStation());
                publicationsDlg.open();
            }
        });
        riverGageMenuItems.add(publicationsMI);

        // References menu item
        referencesMI = new MenuItem(riverGageMenu, SWT.NONE);
        referencesMI.setText("R&eferences...\tCtrl+E");
        referencesMI.setAccelerator(SWT.CTRL + 'E');
        referencesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ReferencesDlg referencesDlg = new ReferencesDlg(shell,
                        getStationAndName(), getSelectedLocation().getStation());
                referencesDlg.open();
            }
        });
        riverGageMenuItems.add(referencesMI);
    }

    /**
     * Create the Reservoir menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createReservoirMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Reservoir menu
        // -------------------------------------
        MenuItem reservoirMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        reservoirMenuItem.setText("Reser&voir");

        // Create the Reservoir menu item with a Reservoir "dropdown" menu
        Menu reservoirMenu = new Menu(menuBar);
        reservoirMenuItem.setMenu(reservoirMenu);

        // -------------------------------------------------
        // Create all the items in the Reservoir dropdown menu
        // -------------------------------------------------

        // Reservoir menu item
        MenuItem reservoirMI = new MenuItem(reservoirMenu, SWT.NONE);
        reservoirMI.setText("Reser&voir...\tCtrl+V");
        reservoirMI.setAccelerator(SWT.CTRL + 'V');
        reservoirMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ReservoirDlg reservoirDlg = new ReservoirDlg(shell,
                        getStationAndName(), getSelectedLocation().getStation());
                reservoirDlg.open();
            }
        });
    }

    /**
     * Create the Data Ingest menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createDataIngestMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Data Ingest menu
        // -------------------------------------
        MenuItem dataIngestMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        dataIngestMenuItem.setText("Data &Ingest");

        // Create the Data Ingest menu item with a Data Ingest "dropdown" menu
        Menu dataIngestMenu = new Menu(menuBar);
        dataIngestMenuItem.setMenu(dataIngestMenu);

        // ------------------------------------------------------
        // Create all the items in the Data Ingest dropdown menu
        // ------------------------------------------------------

        // Data Ingest menu item
        MenuItem ingestFilterMI = new MenuItem(dataIngestMenu, SWT.NONE);
        ingestFilterMI.setText("&Ingest Filter...");
        ingestFilterMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                DataIngestFilterDlg dataIngetsDlg = new DataIngestFilterDlg(
                        shell);
                dataIngetsDlg.open();
            }
        });

        // Adjustment Factors menu item
        MenuItem adjustmentFactorsMI = new MenuItem(dataIngestMenu, SWT.NONE);
        adjustmentFactorsMI.setText("Adjustment Factors...");
        adjustmentFactorsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                DataAdjustFactorDlg dataAdjustDlg = new DataAdjustFactorDlg(
                        shell);
                dataAdjustDlg.open();
            }
        });

        // QC/Alert/Alarm menu item
        MenuItem qcAlertAlarmMI = new MenuItem(dataIngestMenu, SWT.NONE);
        qcAlertAlarmMI.setText("QC/Alert/Alarm Limits...");
        qcAlertAlarmMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                QcAlertAlarmLimitsDlg qcAlertAlarmDlg = new QcAlertAlarmLimitsDlg(
                        shell);
                qcAlertAlarmDlg.open();
            }
        });

        // Purge Parameters menu item
        MenuItem purgeParamsMI = new MenuItem(dataIngestMenu, SWT.NONE);
        purgeParamsMI.setText("&Purge Parameters...");
        purgeParamsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                DataPurgeParamsDlg dataPurgeDlg = new DataPurgeParamsDlg(shell);
                dataPurgeDlg.open();
            }
        });
    }

    /**
     * Create the Reports menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createReportsMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Reports menu
        // -------------------------------------
        MenuItem reportsMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        reportsMenuItem.setText("Re&ports");

        // Create the Reports menu item with a Reports "dropdown" menu
        Menu reportsMenu = new Menu(menuBar);
        reportsMenuItem.setMenu(reportsMenu);

        // ------------------------------------------------------
        // Create all the items in the Reports dropdown menu
        // ------------------------------------------------------

        // Flood Report menu item
        MenuItem floodReportMI = new MenuItem(reportsMenu, SWT.NONE);
        floodReportMI.setText("&Flood Report...");
        floodReportMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                FloodReportDlg floodReportDlg = new FloodReportDlg(shell);
                floodReportDlg.open();
            }
        });

        // Text Report menu item
        MenuItem textReportsMI = new MenuItem(reportsMenu, SWT.NONE);
        textReportsMI.setText("&Text Reports...");
        textReportsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                String lid = dataList.getItem(dataList.getSelectionIndex())
                        .trim();
                TextReportDlg textReportDlg = new TextReportDlg(shell, lid
                        .substring(0, lid.indexOf(" ")));
                textReportDlg.open();
                // MessageBox messageBox = new MessageBox(shell, SWT.OK);
                // messageBox.setText("Not Yet Implemented");
                // messageBox.setMessage("This Function Is Not Yet Implemented");
                // messageBox.open();
            }
        });
    }

    /**
     * Create the Setup menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createSetupMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Setup menu
        // -------------------------------------
        MenuItem setupMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        setupMenuItem.setText("&Setup");

        // Create the Setup menu item with a Setup "dropdown" menu
        Menu setupMenu = new Menu(menuBar);
        setupMenuItem.setMenu(setupMenu);

        // ------------------------------------------------------
        // Create all the items in the Setup dropdown menu
        // ------------------------------------------------------

        // Administration menu item
        MenuItem adminMI = new MenuItem(setupMenu, SWT.NONE);
        adminMI.setText("A&dministration...");
        adminMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                AdministrationDlg adminDlg = new AdministrationDlg(shell);
                adminDlg.open();
            }
        });

        /* Removed per Mr. Glaudemans at 16-17 April TIM in Omaha */
        // Cities menu item
        // MenuItem citiesMI = new MenuItem(setupMenu, SWT.NONE);
        // citiesMI.setText("&Cities...");
        // citiesMI.addSelectionListener(new SelectionAdapter() {
        // @Override
        // public void widgetSelected(SelectionEvent event) {
        // CitiesDlg citiesDlg = new CitiesDlg(shell);
        // citiesDlg.open();
        // }
        // });
        // Reference Fields menu item
        MenuItem referenceFieldsMI = new MenuItem(setupMenu, SWT.NONE);
        referenceFieldsMI.setText("R&eference Fields...");
        referenceFieldsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {

                ReferenceFieldsDlg referenceDlg = new ReferenceFieldsDlg(shell);
                referenceDlg.open();
            }
        });

        // States/Counties/Zones menu item
        MenuItem statesCountiesZonesMI = new MenuItem(setupMenu, SWT.NONE);
        statesCountiesZonesMI.setText("&States/Counties/Zones...");
        statesCountiesZonesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                StatesCountiesZonesDlg sczDlg = new StatesCountiesZonesDlg(
                        shell);
                sczDlg.open();
            }
        });

        new MenuItem(setupMenu, SWT.SEPARATOR);

        // River PRO general parameters menu item
        MenuItem riverProGenParamMI = new MenuItem(setupMenu, SWT.NONE);
        riverProGenParamMI.setText("RiverPro &General Parameters...");
        riverProGenParamMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                RiverProGenParamsDlg riverProGen = new RiverProGenParamsDlg(
                        shell);
                riverProGen.open();
            }
        });

        // River PRO forecast groups/points menu item
        MenuItem riverProForecastMI = new MenuItem(setupMenu, SWT.NONE);
        riverProForecastMI.setText("RiverPro &Forecast Groups/Points...");
        riverProForecastMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                RiverProFcstGrpPointsDlg riverProFcst = new RiverProFcstGrpPointsDlg(
                        shell);
                riverProFcst.open();
            }
        });

        new MenuItem(setupMenu, SWT.SEPARATOR);

        // RADAR locations menu item
        MenuItem radarLocationsMI = new MenuItem(setupMenu, SWT.NONE);
        radarLocationsMI.setText("&Radar Locations...");
        radarLocationsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                RadarLocationsDlg radarLocDlg = new RadarLocationsDlg(shell);
                radarLocDlg.open();
            }
        });

        new MenuItem(setupMenu, SWT.SEPARATOR);

        // Areal Definitions menu item
        MenuItem arealDefinitionsMI = new MenuItem(setupMenu, SWT.NONE);
        arealDefinitionsMI.setText("&Areal Definitions...");
        arealDefinitionsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ArealDefinitionsDlg arealDlg = new ArealDefinitionsDlg(shell);
                arealDlg.open();
            }
        });

        // Vector Definitions menu item
        // Vector Definitions menu not needed in AWIPS 2
        // MenuItem vectorDefinitionsMI = new MenuItem(setupMenu, SWT.NONE);
        // vectorDefinitionsMI.setText("&Vector Definitions...");
        // vectorDefinitionsMI.addSelectionListener(new SelectionAdapter() {
        // @Override
        // public void widgetSelected(SelectionEvent event) {
        // VectorDefinitionsDlg vectorDefDlg = new VectorDefinitionsDlg(
        // shell);
        // vectorDefDlg.open();
        // }
        // });

        new MenuItem(setupMenu, SWT.SEPARATOR);

        // NWR Transmitter Towers menu item
        MenuItem nwrTransmitterMI = new MenuItem(setupMenu, SWT.NONE);
        nwrTransmitterMI.setText("&NWR Transmitter Towers...");
        nwrTransmitterMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                NwrTransmitterDlg nwrTransDlg = new NwrTransmitterDlg(shell);
                nwrTransDlg.open();
            }
        });

        new MenuItem(setupMenu, SWT.SEPARATOR);

        // TimeSeries Group Configuration Towers menu item
        MenuItem timeSeriesGroupConfMI = new MenuItem(setupMenu, SWT.NONE);
        timeSeriesGroupConfMI.setText("TimeSeries Group Configuration...");
        timeSeriesGroupConfMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {

                IPathManager pm = PathManagerFactory.getPathManager();
                LocalizationFile file = pm
                        .getStaticLocalizationFile(HydroConstants.GROUP_DEFINITION);

                if (file != null) {
                    ILocalizationService service = LocalizationPerspectiveUtils
                            .changeToLocalizationPerspective();
                    service.selectFile(file);
                    service.openFile(file);
                } else {
                    MessageBox mb = new MessageBox(getParent(), SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("File Error");
                    mb.setMessage("The following file does not exist:\n"
                            + "group_definition.cfg");
                    mb.open();
                }
            }
        });

        new MenuItem(setupMenu, SWT.SEPARATOR);

        // HydroGen Configuration Towers menu item
        MenuItem hydroGenConfMI = new MenuItem(setupMenu, SWT.NONE);
        hydroGenConfMI.setText("HydroGen Configuration...");
        hydroGenConfMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                HydroGenConfigDlg hydroGenDlg = new HydroGenConfigDlg(shell);
                hydroGenDlg.open();
            }
        });
    }

    /**
     * Create the data list control.
     */
    private void createDataListControl() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);
        Composite listComp = new Composite(shell, SWT.NONE);
        listComp.setLayout(gl);
        listComp.setLayoutData(gd);

        listLbl = new Label(listComp, SWT.NONE);
        listLbl.setText(getListLabelText());
        listLbl.setFont(controlFont);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 900;
        gd.heightHint = 600;
        dataList = new List(shell, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL
                | SWT.H_SCROLL);
        dataList.setLayoutData(gd);

        dataList.setFont(controlFont);
        dataList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleSiteSelection();
            }
        });

        dataList.addMouseListener(new MouseListener() {

            @Override
            public void mouseDoubleClick(MouseEvent e) {
                // TODO Auto-generated method stub
                // open Modify Location window on double click
                openModifyDlg();
            }

            @Override
            public void mouseDown(MouseEvent e) {
                // TODO Auto-generated method stub

            }

            @Override
            public void mouseUp(MouseEvent e) {
                // TODO Auto-generated method stub

            }
        });

    }

    /**
     * Create the control at the bottom of the dialog.
     */
    private void createBottomControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(6, false);
        gl.marginHeight = 2;
        Composite controlComp = new Composite(shell, SWT.NONE);
        controlComp.setLayout(gl);
        controlComp.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        stationCountLbl = new Label(controlComp, SWT.NONE);
        stationCountLbl.setLayoutData(gd);

        Button stationFilterBtn = new Button(controlComp, SWT.PUSH);
        stationFilterBtn.setText("Station List Filter Options");
        stationFilterBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                openStationFilterOptions();
            }
        });

        gd = new GridData(150, SWT.DEFAULT);
        Label sortByLbl = new Label(controlComp, SWT.RIGHT);
        sortByLbl.setText("Sort By: ");
        sortByLbl.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        sortByCbo = new Combo(controlComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        sortByCbo.add("Station");
        sortByCbo.add("Name");
        sortByCbo.add("State,County");
        sortByCbo.select(0);
        sortByCbo.setLayoutData(gd);
        sortByCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                sortAndUpdateListControl();
            }
        });

        gd = new GridData(150, SWT.DEFAULT);
        Label stationSearchLbl = new Label(controlComp, SWT.RIGHT);
        stationSearchLbl.setText("Station Search: ");
        stationSearchLbl.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        stationSearchTF = new Text(controlComp, SWT.BORDER);
        stationSearchTF.setLayoutData(gd);
        stationSearchTF.addKeyListener(this);
    }

    /**
     * Update the station count.
     */
    private void updateStationCount() {
        int size = stationArray.size();
        stationCountLbl.setText("( " + size + " Stations )");
    }

    /**
     * Get the sort type.
     */
    @Override
    public String getSortType() {
        return sortByCbo.getItem(sortByCbo.getSelectionIndex());
    }

    /**
     * Sort the data and update the data list control.
     */
    private void sortAndUpdateListControl() {
        int index = 0;

        stationArray = HydroStationDataManager.getInstance().getStationData(
                this);

        dataList.removeAll();
        int selectedLid = 0;

        for (HydroStationData stationData : stationArray) {
            StringBuffer station = new StringBuffer();

            // Find the station selected in the main HydroView display
            if (stationData.getStation().equalsIgnoreCase(this.selectedLid)) {
                selectedLid = index;
            }
            index++;

            station.append(stationData.getLocationDisplayString());

            if (showStateCounty) {
                station.append(stationData.getStateCountyDisplayString());
            }
            if (showBasin) {
                station.append(stationData.getBasinDisplayString());
            }
            if (showRiverStream) {
                station.append(stationData.getStreamDisplayString());
            }
            if (showLatLon) {
                station.append(stationData.getLatLonDisplayString());
            }

            dataList.add(station.toString());
        }

        if (dataList.getItemCount() != 0) {
            dataList.select(selectedLid);
            dataList.showSelection();
        }

        handleSiteSelection();
        updateStationCount();
    }

    /**
     * Get the station and name in a formatted string, based on preferences.
     * 
     * @return The station and name.
     */
    private String getStationAndName() {
        String rval = "";

        if (dataList.getSelectionIndex() != -1) {
            HydroStationData data = getSelectedLocation();
            rval = PreferencesDataManager.getInstance().getTitleString(data);
        }

        return rval;
    }

    /**
     * Gets the station lid
     * 
     * @return
     */
    private String getStationLid() {
        String lid = null;

        if (dataList.getSelectionIndex() != -1) {
            HydroStationData data = getSelectedLocation();
            lid = data.getStation();
        }

        return lid;
    }

    private HydroStationData getSelectedLocation() {
        return stationArray.get(dataList.getSelectionIndex());
    }

    /**
     * Get the text for the data list label.
     * 
     * @return The label text;
     */
    private String getListLabelText() {
        StringBuffer format = new StringBuffer();
        format.append(String.format("%-8S %-25S", "Station", "Name"));

        // append the appropriate columns
        if (showStateCounty) {
            format.append(String.format(" %-24S", "State,County"));
        }
        if (showBasin) {
            format.append(String.format(" %-25S", "Basin"));
        }
        if (showRiverStream) {
            format.append(String.format(" %-25S", "Stream"));
        }
        if (showLatLon) {
            format.append(String.format(" %18.18S", "Latitude/Longitude"));
        }

        return format.toString();
    }

    private void populateListControl() {
        stationArray = HydroStationDataManager.getInstance().getStationData(
                this);

        sortAndUpdateListControl();
    }

    @Override
    public void notifyUpdate() {
        applyPreferences();
    }

    private void applyPreferences() {
        getColumnPreferences();

        listLbl.setText(getListLabelText());

        getSortPreferences();
    }

    private void getColumnPreferences() {
        PreferencesDataManager dm = PreferencesDataManager.getInstance();
        int columnsToDisplay = dm.getDisplayColumns();

        showBasin = (columnsToDisplay & PreferencesData.SHOW_BASIN) != 0;
        showLatLon = (columnsToDisplay & PreferencesData.SHOW_LAT_LON) != 0;
        showRiverStream = (columnsToDisplay & PreferencesData.SHOW_RIVER_STREAM) != 0;
        showStateCounty = (columnsToDisplay & PreferencesData.SHOW_STATE_COUNTY) != 0;
    }

    private void getSortPreferences() {
        PreferencesDataManager dm = PreferencesDataManager.getInstance();
        SortCriteria selectedSort = dm.getSortCriteria();
        // Set the sort criteria
        String[] data = sortByCbo.getItems();

        for (int i = 0; i < data.length; i++) {
            if (((data[i].compareTo("Name") == 0) && (selectedSort == SortCriteria.NAME))
                    || ((data[i].compareTo("Station") == 0) && (selectedSort == SortCriteria.STATION))
                    || ((data[i].compareTo("State,County") == 0) && (selectedSort == SortCriteria.STATE_COUNTY))) {
                sortByCbo.select(i);
                break;
            }
        }

        sortAndUpdateListControl();
    }

    @Override
    public void keyPressed(KeyEvent e) {
    }

    /**
     * Handles the Station Search functionality
     */
    @Override
    public void keyReleased(KeyEvent e) {
        String stationSearch = stationSearchTF.getText().toUpperCase();

        if (stationSearch.compareTo("") == 0) {
            dataList.setSelection(0);
            return;
        } else {
            for (int i = 0; i < stationArray.size(); i++) {
                if (stationArray.get(i).getStation().toUpperCase()
                        .startsWith(stationSearch)) {
                    dataList.setSelection(i);
                    dataList.showSelection();
                    handleSiteSelection();
                    break;
                }
            }
        }
    }

    @Override
    public void notifyFilterChange() {
        populateListControl();
    }

    /**
     * Opens the Station Filter Dialog
     */
    private void openStationFilterOptions() {
        StationFilterOptionsDlg stationFilterDlg = new StationFilterOptionsDlg(
                shell);
        stationFilterDlg.addListener(this);
        stationFilterDlg.open();
        stationFilterDlg.removeListener(this);
    }

    @Override
    public void notifyStationUpdate(String lid) {
        populateListControl();
        for (int i = 0; i < stationArray.size(); i++) {
            if (stationArray.get(i).getStation().equalsIgnoreCase(lid)) {
                dataList.setSelection(i);
                dataList.showSelection();
                handleSiteSelection();
                return;
            }
        } 
        
        dataList.setSelection(0);
        dataList.showSelection();
        handleSiteSelection();

    }

    /**
     * Open the Add Location Dialog.
     */
    private void openAddDlg() {
        AddModifyLocationDlg addLocDlg = new AddModifyLocationDlg(shell, false,
                getSelectedLocation().getStation(), getStationAndName());
        addLocDlg.addListener(this);
        addLocDlg.open();
    }

    /**
     * Open the Modify Location Dialog.
     */
    private void openModifyDlg() {
        AddModifyLocationDlg modLocDlg = new AddModifyLocationDlg(shell, true,
                getSelectedLocation().getStation(), getStationAndName());
        modLocDlg.addListener(this);
        modLocDlg.open();
    }

    /**
     * Upon site selection test to see if a the site is a river site. If so then
     * enable the river menu items.
     */
    private void handleSiteSelection() {

        if (dataList.getSelectionIndex() == -1) {
            dataList.setSelection(0);
        }

        if (dataList.getSelectionIndex() != -1) {
            String selection = dataList.getItem(dataList.getSelectionIndex());
            selection = selection.trim();
            String[] parts = selection.split("\\s+");
            String lid = parts[0];

            boolean enableRiverMenus = HydroStationDataManager.getInstance()
                    .isRiverSite(lid);

            for (int i = 0; i < riverGageMenuItems.size(); i++) {
                riverGageMenuItems.get(i).setEnabled(enableRiverMenus);
            }
        }
    }

    /**
     * Prompt for the password dialog box.
     */
    protected boolean promptForPassword(Shell shell) {
        HBPasswordDlg pwDlg = new HBPasswordDlg();
        return pwDlg.open(shell);
    }

    /**
     * Inner class for the password dialog.
     */
    private class HBPasswordDlg {
        private Text text;

        private Shell dialog;

        private int numTries = 0;

        private boolean verified = false;

        private String password = null;

        public HBPasswordDlg() {
            numTries = 0;
        }

        public boolean open(final Shell shell) {
            Display display = shell.getDisplay();
            password = getPassword();
            verified = false;
            if ((password == null) || (password.length() == 0)) {
                // Show message
                MessageBox messageBox = new MessageBox(shell, SWT.OK);
                messageBox.setText("Password");
                messageBox.setMessage("Please set a password for HydroBase\n"
                        + "in the Setup/Administration dialog.");
                messageBox.open();
            }

            dialog = new Shell(shell, SWT.APPLICATION_MODAL | SWT.DIALOG_TRIM);
            dialog.setText("Enter Password");

            FormLayout formLayout = new FormLayout();
            formLayout.marginWidth = 10;
            formLayout.marginHeight = 10;
            formLayout.spacing = 10;
            dialog.setLayout(formLayout);

            Label label = new Label(dialog, SWT.NONE);
            label.setText("Enter Password:");
            FormData data = new FormData();
            label.setLayoutData(data);

            Button cancel = new Button(dialog, SWT.PUSH);
            cancel.setText("Cancel");
            data = new FormData();
            data.width = 60;
            data.right = new FormAttachment(100, 0);
            data.bottom = new FormAttachment(100, 0);
            cancel.setLayoutData(data);
            cancel.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    // Close password dialog
                    dialog.close();
                }
            });

            text = new Text(dialog, SWT.BORDER);
            text.setFocus();
            data = new FormData();
            data.width = 200;
            data.left = new FormAttachment(label, 0, SWT.DEFAULT);
            data.right = new FormAttachment(100, 0);
            data.top = new FormAttachment(label, 0, SWT.CENTER);
            data.bottom = new FormAttachment(cancel, 0, SWT.DEFAULT);
            text.setLayoutData(data);
            text.setEchoChar('*');

            Button ok = new Button(dialog, SWT.PUSH);
            ok.setText("OK");
            data = new FormData();
            data.width = 60;
            data.right = new FormAttachment(cancel, 0, SWT.DEFAULT);
            data.bottom = new FormAttachment(100, 0);
            ok.setLayoutData(data);
            ok.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    numTries++;
                    dialog.setVisible(false);
                    if (!(text.getText().equals(password))) {
                        // password invalid, try again
                        if (numTries == 3) {
                            // after the 3rd failed attempt, exit
                            dialog.dispose();
                            MessageBox messageBox = new MessageBox(shell,
                                    SWT.OK);
                            messageBox.setText("ABORTING HydroBase");
                            messageBox
                                    .setMessage("Three failed password attempts - exiting HydroBase.");
                            messageBox.open();

                            return;
                        }

                        MessageBox messageBox = new MessageBox(shell, SWT.OK);
                        messageBox.setText("Invalid Password");
                        messageBox.setMessage("Invalid password entered.\n"
                                + "      Please try again.");
                        messageBox.open();

                        // Show the password dialog again
                        dialog.setVisible(true);
                    } else {
                        // Close the password dialog so HydroBase is accessible
                        dialog.dispose();
                        verified = true;
                    }
                }
            });

            dialog.addShellListener(new ShellAdapter() {
                @Override
                public void shellClosed(ShellEvent event) {
                    shell.dispose();
                }
            });

            dialog.setDefaultButton(ok);
            dialog.pack();
            dialog.open();

            while (!dialog.isDisposed()) {
                if (!display.readAndDispatch())
                    display.sleep();
            }

            if (dialog.isDisposed() == false) {
                dialog.dispose();
            }

            return verified;
        }

        private String getPassword() {
            String pw = null;
            try {
                ArrayList<AdministrationData> data = HydroDBDataManager
                        .getInstance().getData(AdministrationData.class);

                // if no data is returned, clear the current display data
                AdministrationData adminData = (data.size() > 0) ? data.get(0)
                        : null;
                pw = adminData.getHbPassword();
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, "Data Query:"
                        + " Error retrirving HB Password.");
            }

            return pw;
        }
    }
}
