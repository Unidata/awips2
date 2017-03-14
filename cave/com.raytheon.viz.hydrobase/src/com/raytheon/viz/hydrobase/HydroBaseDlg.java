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
import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
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
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.localization.perspective.service.ILocalizationService;
import com.raytheon.uf.viz.localization.perspective.service.LocalizationPerspectiveUtils;
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
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * This class displays the main Hydrobase dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
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
 * 04/16/2013   1790        rferrel     Changes for non-blocking AddModifyLocationDlg.
 *                                      Changes for non-blocking AdministrationDlg.
 *                                      Changes for non-blocking ArealDefinitionsDlg.
 *                                      Changes for non-blocking BenchmarkDlg.
 *                                      Changes for non-blocking CountyZoneUgcDlg.
 *                                      Changes for non-blocking DataAdjustFactorDlg.
 *                                      Changes for non-blocking DataIngestFilterDlg.
 *                                      Changes for non-blocking DataPurgeParamsDlg.
 *                                      Changes for non-blocking DatumDlg.
 *                                      Changes for non-blocking DescriptionDlg.
 *                                      Changes for non-blocking FloodCategoryDlg.
 *                                      Changes for non-blocking FloodDamageDlg.
 *                                      Changes for non-blocking GageHistoryDlg.
 *                                      Changes for non-blocking HydroGenConfigDlg.
 *                                      Changes for non-blocking LowWaterDlg.
 *                                      Changes for non-blocking NwrTransmitterDlg.
 *                                      Changes for non-blocking PreferencesDlg.
 *                                      Changes for non-blocking PublicationsDlg.
 *                                      Changes for non-blocking QcAlertAlarmLimitsDlg.
 *                                      Changes for non-blocking RadarLocationsDlg.
 *                                      Changes for non-blocking ReferenceFieldsDlg.
 *                                      Changes for non-blocking ReferencesDlg.
 *                                      Changes for non-blocking ReservoirDlg.
 * 06/11/2013   2088       rferrel      Changes for non-blocking RiverGageDlg.
 *                                      Changes for non-blocking RiverProFcstGrpPointsDlg.
 *                                      Changes for non-blocking RiverProGenParamsDlg.
 *                                      Changes for non-blocking StatesCountiesZonesDlg.
 *                                      Changes for non-blocking StationFilterOptionsDlg.
 *                                      Changes for non-blocking FloodReportDlg.
 *                                      Make dialog non-blocking.
 *                                      Changes for non-blocking ContactsDlg.
 *                                      Changes for non-blocking CrestHistoryDlg.
 *                                      Changes for non-blocking DataSourcesDlg.
 *                                      Changes for non-blocking ImpactStatementDlg.
 *                                      Changes for non-blocking LowWaterStatementDlg.
 *                                      Changes for non-blocking RatingCurveDlg.
 *                                      Changes for non-blocking TextReportDlg.
 * 02/16/2016    5354       bkowal      Prevent the closure of the password dialog from
 *                                      closing all of CAVE.
 * 04/11/2016   5483       dgilling     Fix hard-coded layouts in HBPasswordDlg.
 * 04/22/2016   5483       dgilling     Code cleanup.
 * Oct 27, 2016 5969       randerso     Add support for locating hydroapps on the correct monitor
 * 
 * </pre>
 * 
 * @author lvenable
 */
public class HydroBaseDlg extends CaveSWTDialog implements IGetSortType,
        IPreferencesListener, IStationFilterListener, IStationListener,
        KeyListener {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(HydroBaseDlg.class);

    /**
     * Allow only one dialog
     */
    private AddModifyLocationDlg addLocDlg;

    /**
     * Allow only one dialog per location.
     */
    private final Map<String, AddModifyLocationDlg> modLocDlgMap = new HashMap<>();

    /**
     * Administration information dialog.
     */
    private AdministrationDlg adminDlg;

    /**
     * Allow one areal definitions dialog.
     */
    private ArealDefinitionsDlg arealDlg;

    /**
     * Allow one benchmark dialog per location.
     */
    private final Map<String, BenchmarkDlg> benchmarkDlgMap = new HashMap<>();

    /**
     * Allow one County/Zone UGC dialog per location.
     */
    private final Map<String, CountyZoneUgcDlg> czDlgMap = new HashMap<>();

    /**
     * Allow one Data Adjustment Factor dialog.
     */
    private DataAdjustFactorDlg dataAdjustDlg;

    /**
     * Allow one Ingest Filter Dialog.
     */
    private DataIngestFilterDlg dataIngestDlg;

    /*
     * Allow one data purge paramertres dialog.
     */
    private DataPurgeParamsDlg dataPurgeDlg;

    /**
     * Allow one dataum dialog per station.
     */
    private final Map<String, DatumDlg> datumDlgMap = new HashMap<>();

    /**
     * Allow one description dialog per station.
     */
    private final Map<String, DescriptionDlg> descDlgMap = new HashMap<>();

    /**
     * Allow one river gage dialog per station.
     */
    private final Map<String, RiverGageDlg> riverGageDlgMap = new HashMap<>();

    /**
     * Allow one flood category dialog per station.
     */
    private final Map<String, FloodCategoryDlg> floodCatDlgMap = new HashMap<>();

    /**
     * Allow one flood damage dialog per station.
     */
    private final Map<String, FloodDamageDlg> floodDamDlgMap = new HashMap<>();

    /**
     * Allow one gage history dialog per station.
     */
    private final Map<String, GageHistoryDlg> ghDlgMap = new HashMap<>();

    /**
     * Allow one Hydrogen configuration dialog.
     */
    private HydroGenConfigDlg hydroGenDlg;

    /**
     * Allow one low water dialog per station.
     */
    private final Map<String, LowWaterDlg> lowWaterDlgMap = new HashMap<>();

    /**
     * Allow one NWR Transmitter Towers dialog.
     */
    private NwrTransmitterDlg nwrTransDlg;

    /**
     * Allow one preferences dialog.
     */
    private PreferencesDlg prefDlg;

    /**
     * Allow one publication dialog per station.
     */
    private final Map<String, PublicationsDlg> publicationsDlgMap = new HashMap<>();

    /**
     * Allow one QC alart/alarm limits dialog.
     */
    private QcAlertAlarmLimitsDlg qcAlertAlarmDlg;

    /**
     * Allow one Radar locations dialog.
     */
    private RadarLocationsDlg radarLocDlg;

    /**
     * Allow single instance of Reference Field dialog.
     */
    private ReferenceFieldsDlg referenceDlg;

    /**
     * Allow single instance of State/Counties/Zones dialog.
     */
    private StatesCountiesZonesDlg sczDlg;

    /**
     * Allow one References dialog per station.
     */
    private final Map<String, ReferencesDlg> referencesDlgMap = new HashMap<>();

    /**
     * Allow one Reservoir dialog per station.
     */
    private final Map<String, ReservoirDlg> reservoirDlgMap = new HashMap<>();

    /**
     * Allow one instance of the RiverPro General Parameters dialog.
     */
    private RiverProGenParamsDlg riverProGen;

    /**
     * Allow one instance of the RiverPro Forecast Groups/Points dialog.
     */
    private RiverProFcstGrpPointsDlg riverProFcst;

    /**
     * Allow one instance of the Station Filter Option dialog.
     */
    private StationFilterOptionsDlg stationFilterDlg;

    /**
     * Allow one instance of the Flood Report Dialog.
     */
    private FloodReportDlg floodReportDlg;

    /**
     * Allow one instance per lid.
     */
    private final Map<String, ContactsDlg> contactsDlgMap = new HashMap<>();

    /**
     * Allow one instance per station.
     */
    private final Map<String, CrestHistoryDlg> crestHistDlgMap = new HashMap<>();

    /**
     * Allow one instance per station.
     */
    private final Map<String, DataSourcesDlg> dataSourcesDlgMap = new HashMap<>();

    /**
     * Allow one instance per station.
     */
    private final Map<String, ImpactStatementDlg> impactStatementDlgMap = new HashMap<>();

    /**
     * Allow one instance per station.
     */
    private final Map<String, LowWaterStatementDlg> lowWaterStmntDlgMap = new HashMap<>();

    /**
     * Allow one instance per station.
     */
    private final Map<String, RatingCurveDlg> ratingCurveDlgMap = new HashMap<>();

    /**
     * Allow one instance per station.
     */
    private final Map<String, TextReportDlg> textReportDlgMap = new HashMap<>();

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

    private ArrayList<MenuItem> riverGageMenuItems = new ArrayList<>();

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
        super(parent, SWT.DIALOG_TRIM | SWT.MIN, CAVE.INDEPENDENT_SHELL
                | CAVE.DO_NOT_BLOCK);
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
        close();
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
        stationArray = new ArrayList<>();
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
        if (prefDlg == null || prefDlg.isDisposed()) {
            prefDlg = new PreferencesDlg(shell);
            prefDlg.addListener(this);
            prefDlg.open();
        } else {
            prefDlg.bringToTop();
        }
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
                ContactsDlg contactsDlg = contactsDlgMap.get(lid);
                if (contactsDlg == null || contactsDlg.isDisposed()) {
                    String lidLoc = getStationAndName();

                    contactsDlg = new ContactsDlg(shell, lidLoc, true, lid);
                    contactsDlg.setCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            if (returnValue instanceof String) {
                                String lid = returnValue.toString();
                                contactsDlgMap.remove(lid);
                            }
                        }
                    });
                    contactsDlg.open();
                    contactsDlgMap.put(lid, contactsDlg);
                } else {
                    contactsDlg.bringToTop();
                }
            }
        });

        // County/Zone UGC menu item
        MenuItem countyZoneUgcMI = new MenuItem(locationMenu, SWT.NONE);
        countyZoneUgcMI.setText("County/Zone &UGC...\tCtrl+U");
        countyZoneUgcMI.setAccelerator(SWT.CTRL + 'U');
        countyZoneUgcMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                String lid = getSelectedLocation().getStation();
                CountyZoneUgcDlg czDlg = czDlgMap.get(lid);
                if (czDlg == null) {
                    czDlg = new CountyZoneUgcDlg(shell, getStationAndName(),
                            lid);
                    czDlg.setCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            if (returnValue instanceof String) {
                                String lid = returnValue.toString();
                                czDlgMap.remove(lid);
                            }
                        }
                    });
                    czDlgMap.put(lid, czDlg);
                    czDlg.open();
                } else {
                    czDlg.bringToTop();
                }
            }
        });

        // Gage History menu item
        MenuItem gageHistoryMI = new MenuItem(locationMenu, SWT.NONE);
        gageHistoryMI.setText("Gage History...\tCtrl+G");
        gageHistoryMI.setAccelerator(SWT.CTRL + 'G');
        gageHistoryMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                String lid = getSelectedLocation().getStation();
                GageHistoryDlg ghDlg = ghDlgMap.get(lid);

                if (ghDlg == null) {
                    ghDlg = new GageHistoryDlg(shell, getStationAndName(), lid);
                    ghDlg.setCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            if (returnValue instanceof String) {
                                String lid = returnValue.toString();
                                ghDlgMap.remove(lid);
                            }
                        }
                    });
                    ghDlgMap.put(lid, ghDlg);
                    ghDlg.open();
                } else {
                    ghDlg.bringToTop();
                }
            }
        });

        // Data Sources menu item
        MenuItem dataSourcesMI = new MenuItem(locationMenu, SWT.NONE);
        dataSourcesMI.setText("Data Sources...\tCtrl+D");
        dataSourcesMI.setAccelerator(SWT.CTRL + 'D');
        dataSourcesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleDataSouceDlg();
            }
        });
    }

    /**
     * Display data source dialog for selected station.
     */
    private void handleDataSouceDlg() {
        final String lid = getSelectedLocation().getStation();
        DataSourcesDlg dataSourcesDlg = dataSourcesDlgMap.get(lid);
        if (dataSourcesDlg == null || dataSourcesDlg.isDisposed()) {
            dataSourcesDlg = new DataSourcesDlg(shell, getStationAndName(),
                    lid, true);
            dataSourcesDlg.addCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    dataSourcesDlgMap.remove(lid);
                }
            });
            dataSourcesDlg.open();
            dataSourcesDlgMap.put(lid, dataSourcesDlg);
        } else {
            dataSourcesDlg.bringToTop();
        }
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
                String lid = getSelectedLocation().getStation();
                RiverGageDlg riverGageDlg = riverGageDlgMap.get(lid);
                if (riverGageDlg == null) {
                    riverGageDlg = new RiverGageDlg(shell, getStationAndName(),
                            lid);
                    riverGageDlg.setCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            if (returnValue instanceof String) {
                                String lid = returnValue.toString();
                                riverGageDlgMap.remove(lid);
                            }
                        }
                    });
                    riverGageDlgMap.put(lid, riverGageDlg);
                    riverGageDlg.open();
                } else {
                    riverGageDlg.bringToTop();
                }
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
                String lid = getSelectedLocation().getStation();
                FloodCategoryDlg floodCatDlg = floodCatDlgMap.get(lid);

                if (floodCatDlg == null) {
                    floodCatDlg = new FloodCategoryDlg(shell,
                            getStationAndName(), lid);
                    floodCatDlg.setCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            if (returnValue instanceof String) {
                                String lid = returnValue.toString();
                                floodCatDlgMap.remove(lid);
                            }
                        }
                    });
                    floodCatDlgMap.put(lid, floodCatDlg);
                    floodCatDlg.open();
                } else {
                    floodCatDlg.bringToTop();
                }
            }
        });
        riverGageMenuItems.add(floodCategoryMI);

        // Impact Statement menu item
        impactStatementMI = new MenuItem(riverGageMenu, SWT.NONE);
        impactStatementMI.setText("&Impact Statement...\tCtrl+I");
        impactStatementMI.setAccelerator(SWT.CTRL + 'I');
        impactStatementMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleImpactStatementDlg();
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
                handleLowWaterStatementDlg();
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
                String lid = getSelectedLocation().getStation();
                FloodDamageDlg floodDamDlg = floodDamDlgMap.get(lid);

                if (floodDamDlg == null) {
                    floodDamDlg = new FloodDamageDlg(shell,
                            getStationAndName(), lid);
                    floodDamDlg.setCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            if (returnValue instanceof String) {
                                String lid = returnValue.toString();
                                floodDamDlgMap.remove(lid);
                            }
                        }
                    });
                    floodDamDlgMap.put(lid, floodDamDlg);
                    floodDamDlg.open();
                } else {
                    floodDamDlg.bringToTop();
                }
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
                handleRatingCurveDlg();
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
                action.launch(getShell(), getStationLid());
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
                handleCrestHistoryDlg();
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
                String lid = getSelectedLocation().getStation();
                LowWaterDlg lowWaterDlg = lowWaterDlgMap.get(lid);

                if (lowWaterDlg == null) {
                    lowWaterDlg = new LowWaterDlg(shell, getStationAndName(),
                            lid);
                    lowWaterDlg.setCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            if (returnValue instanceof String) {
                                String lid = returnValue.toString();
                                lowWaterDlgMap.remove(lid);
                            }
                        }
                    });
                    lowWaterDlgMap.put(lid, lowWaterDlg);
                    lowWaterDlg.open();
                } else {
                    lowWaterDlg.bringToTop();
                }
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
                String lid = getSelectedLocation().getStation();
                BenchmarkDlg benchmarkDlg = benchmarkDlgMap.get(lid);
                if (benchmarkDlg == null) {
                    benchmarkDlg = new BenchmarkDlg(shell, getStationAndName(),
                            lid);
                    benchmarkDlg.setCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            if (returnValue instanceof String) {
                                String lid = returnValue.toString();
                                benchmarkDlgMap.remove(lid);
                            }
                        }
                    });
                    benchmarkDlgMap.put(lid, benchmarkDlg);
                    benchmarkDlg.open();
                } else {
                    benchmarkDlg.bringToTop();
                }
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
                String lid = getSelectedLocation().getStation();
                DatumDlg datumDlg = datumDlgMap.get(lid);
                if (datumDlg == null) {
                    datumDlg = new DatumDlg(shell, getStationAndName(), lid);
                    datumDlg.setCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            if (returnValue instanceof String) {
                                String lid = returnValue.toString();
                                datumDlgMap.remove(lid);
                            }
                        }
                    });
                    datumDlgMap.put(lid, datumDlg);
                    datumDlg.open();
                } else {
                    datumDlg.bringToTop();
                }
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
                String lid = getSelectedLocation().getStation();
                DescriptionDlg descDlg = descDlgMap.get(lid);

                if (descDlg == null) {
                    descDlg = new DescriptionDlg(shell, getStationAndName(),
                            lid);
                    descDlg.setCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            if (returnValue instanceof String) {
                                String lid = returnValue.toString();
                                descDlgMap.remove(lid);
                            }
                        }
                    });
                    descDlgMap.put(lid, descDlg);
                    descDlg.open();
                } else {
                    descDlg.bringToTop();
                }
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
                String lid = getSelectedLocation().getStation();
                PublicationsDlg publicationsDlg = publicationsDlgMap.get(lid);
                if (publicationsDlg == null) {
                    publicationsDlg = new PublicationsDlg(shell,
                            getStationAndName(), lid);
                    publicationsDlg.setCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            if (returnValue instanceof String) {
                                String lid = returnValue.toString();
                                publicationsDlgMap.remove(lid);
                            }
                        }
                    });
                    publicationsDlgMap.put(lid, publicationsDlg);
                    publicationsDlg.open();
                } else {
                    publicationsDlg.bringToTop();
                }
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
                String lid = getSelectedLocation().getStation();
                ReferencesDlg referencesDlg = referencesDlgMap.get(lid);
                if (referencesDlg == null) {
                    referencesDlg = new ReferencesDlg(shell,
                            getStationAndName(), lid);
                    referencesDlg.setCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            if (returnValue instanceof String) {
                                String lid = returnValue.toString();
                                referencesDlgMap.remove(lid);
                            }
                        }
                    });
                    referencesDlgMap.put(lid, referencesDlg);
                    referencesDlg.open();
                } else {
                    referencesDlg.bringToTop();
                }
            }
        });
        riverGageMenuItems.add(referencesMI);
    }

    /**
     * Bring of Impact Statement Dialog for the selected station.
     */
    private void handleImpactStatementDlg() {
        String lid = getSelectedLocation().getStation();
        ImpactStatementDlg impactStatementDlg = impactStatementDlgMap.get(lid);

        if (impactStatementDlg == null || impactStatementDlg.isDisposed()) {
            impactStatementDlg = new ImpactStatementDlg(shell,
                    getStationAndName(), getSelectedLocation().getStation(),
                    true);
            impactStatementDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof String) {
                        String lid = returnValue.toString();
                        impactStatementDlgMap.remove(lid);
                    }
                }
            });
            impactStatementDlg.open();
            impactStatementDlgMap.put(lid, impactStatementDlg);
        } else {
            impactStatementDlg.bringToTop();
        }
    }

    /**
     * Display Crest History dialog for the station.
     */
    private void handleCrestHistoryDlg() {
        String lid = getStationLid();
        CrestHistoryDlg crestHistDlg = crestHistDlgMap.get(lid);
        if (crestHistDlg == null || crestHistDlg.isDisposed()) {
            crestHistDlg = new CrestHistoryDlg(shell, getStationLid(),
                    getStationAndName(), true);
            crestHistDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof String) {
                        String lid = returnValue.toString();
                        crestHistDlgMap.remove(lid);
                    }
                }
            });
            crestHistDlg.open();
            crestHistDlgMap.put(lid, crestHistDlg);
        } else {
            crestHistDlg.bringToTop();
        }
    }

    /**
     * Display Rating Curve Dialog for selected station.
     */
    private void handleRatingCurveDlg() {
        String lid = getStationLid();
        RatingCurveDlg ratingCurveDlg = ratingCurveDlgMap.get(lid);

        if (ratingCurveDlg == null || ratingCurveDlg.isDisposed()) {
            ratingCurveDlg = new RatingCurveDlg(shell, lid,
                    getStationAndName(), true);
            ratingCurveDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof String) {
                        String lid = returnValue.toString();
                        ratingCurveDlgMap.remove(lid);
                    }
                }
            });
            ratingCurveDlg.open();
            ratingCurveDlgMap.put(lid, ratingCurveDlg);
        } else {
            ratingCurveDlg.bringToTop();
        }
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
                String lid = getSelectedLocation().getStation();
                ReservoirDlg reservoirDlg = reservoirDlgMap.get(lid);
                if (reservoirDlg == null) {
                    reservoirDlg = new ReservoirDlg(shell, getStationAndName(),
                            lid);
                    reservoirDlg.setCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            if (returnValue instanceof String) {
                                String lid = returnValue.toString();
                                reservoirDlgMap.remove(lid);
                            }
                        }
                    });
                    reservoirDlgMap.put(lid, reservoirDlg);
                    reservoirDlg.open();
                }
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
                if (dataIngestDlg == null || dataIngestDlg.isDisposed()) {
                    dataIngestDlg = new DataIngestFilterDlg(shell);
                    dataIngestDlg.open();
                } else {
                    dataIngestDlg.bringToTop();
                }
            }
        });

        // Adjustment Factors menu item
        MenuItem adjustmentFactorsMI = new MenuItem(dataIngestMenu, SWT.NONE);
        adjustmentFactorsMI.setText("Adjustment Factors...");
        adjustmentFactorsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (dataAdjustDlg == null || dataAdjustDlg.isDisposed()) {
                    dataAdjustDlg = new DataAdjustFactorDlg(shell);
                    dataAdjustDlg.open();
                } else {
                    dataAdjustDlg.bringToTop();
                }
            }
        });

        // QC/Alert/Alarm menu item
        MenuItem qcAlertAlarmMI = new MenuItem(dataIngestMenu, SWT.NONE);
        qcAlertAlarmMI.setText("QC/Alert/Alarm Limits...");
        qcAlertAlarmMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (qcAlertAlarmDlg == null || qcAlertAlarmDlg.isDisposed()) {
                    qcAlertAlarmDlg = new QcAlertAlarmLimitsDlg(shell);
                    qcAlertAlarmDlg.open();
                } else {
                    qcAlertAlarmDlg.bringToTop();
                }
            }
        });

        // Purge Parameters menu item
        MenuItem purgeParamsMI = new MenuItem(dataIngestMenu, SWT.NONE);
        purgeParamsMI.setText("&Purge Parameters...");
        purgeParamsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (dataPurgeDlg == null || dataPurgeDlg.isDisposed()) {
                    dataPurgeDlg = new DataPurgeParamsDlg(shell);
                    dataPurgeDlg.open();
                } else {
                    dataPurgeDlg.bringToTop();
                }
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
                if (floodReportDlg == null || floodReportDlg.isDisposed()) {
                    floodReportDlg = new FloodReportDlg(shell);
                    floodReportDlg.open();
                } else {
                    floodReportDlg.bringToTop();
                }
            }
        });

        // Text Report menu item
        MenuItem textReportsMI = new MenuItem(reportsMenu, SWT.NONE);
        textReportsMI.setText("&Text Reports...");
        textReportsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleTextReportDlg();
            }
        });
    }

    /**
     * Display Text report dialog for the selected station.
     */
    private void handleTextReportDlg() {
        String lid = getStationLid();
        TextReportDlg textReportDlg = textReportDlgMap.get(lid);
        if (textReportDlg == null || textReportDlg.isDisposed()) {
            textReportDlg = new TextReportDlg(shell, lid);
            textReportDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof String) {
                        String lid = returnValue.toString();
                        textReportDlgMap.remove(lid);
                    }
                }
            });
            textReportDlg.open();
            textReportDlgMap.put(lid, textReportDlg);
        } else {
            textReportDlg.bringToTop();
        }
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
                if (adminDlg == null || adminDlg.isDisposed()) {
                    adminDlg = new AdministrationDlg(shell);
                    adminDlg.open();
                } else {
                    adminDlg.bringToTop();
                }
            }
        });

        // Reference Fields menu item
        MenuItem referenceFieldsMI = new MenuItem(setupMenu, SWT.NONE);
        referenceFieldsMI.setText("R&eference Fields...");
        referenceFieldsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (referenceDlg == null || referenceDlg.isDisposed()) {
                    referenceDlg = new ReferenceFieldsDlg(shell);
                    referenceDlg.open();
                } else {
                    referenceDlg.bringToTop();
                }
            }
        });

        // States/Counties/Zones menu item
        MenuItem statesCountiesZonesMI = new MenuItem(setupMenu, SWT.NONE);
        statesCountiesZonesMI.setText("&States/Counties/Zones...");
        statesCountiesZonesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (sczDlg == null || sczDlg.isDisposed()) {
                    sczDlg = new StatesCountiesZonesDlg(shell);
                    sczDlg.open();
                } else {
                    sczDlg.bringToTop();
                }
            }
        });

        new MenuItem(setupMenu, SWT.SEPARATOR);

        // River PRO general parameters menu item
        MenuItem riverProGenParamMI = new MenuItem(setupMenu, SWT.NONE);
        riverProGenParamMI.setText("RiverPro &General Parameters...");
        riverProGenParamMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (riverProGen == null || riverProGen.isDisposed()) {
                    riverProGen = new RiverProGenParamsDlg(shell);
                    riverProGen.open();
                } else {
                    riverProGen.bringToTop();
                }
            }
        });

        // River PRO forecast groups/points menu item
        MenuItem riverProForecastMI = new MenuItem(setupMenu, SWT.NONE);
        riverProForecastMI.setText("RiverPro &Forecast Groups/Points...");
        riverProForecastMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (riverProFcst == null || riverProFcst.isDisposed()) {
                    riverProFcst = new RiverProFcstGrpPointsDlg(shell);
                    riverProFcst.open();
                } else {
                    riverProFcst.bringToTop();
                }
            }
        });

        new MenuItem(setupMenu, SWT.SEPARATOR);

        // RADAR locations menu item
        MenuItem radarLocationsMI = new MenuItem(setupMenu, SWT.NONE);
        radarLocationsMI.setText("&Radar Locations...");
        radarLocationsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (radarLocDlg == null || radarLocDlg.isDisposed()) {
                    radarLocDlg = new RadarLocationsDlg(shell);
                    radarLocDlg.open();
                } else {
                    radarLocDlg.bringToTop();
                }
            }
        });

        new MenuItem(setupMenu, SWT.SEPARATOR);

        // Areal Definitions menu item
        MenuItem arealDefinitionsMI = new MenuItem(setupMenu, SWT.NONE);
        arealDefinitionsMI.setText("&Areal Definitions...");
        arealDefinitionsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (arealDlg == null || arealDlg.isDisposed()) {
                    arealDlg = new ArealDefinitionsDlg(shell);
                    arealDlg.open();
                } else {
                    arealDlg.bringToTop();
                }
            }
        });

        new MenuItem(setupMenu, SWT.SEPARATOR);

        // NWR Transmitter Towers menu item
        MenuItem nwrTransmitterMI = new MenuItem(setupMenu, SWT.NONE);
        nwrTransmitterMI.setText("&NWR Transmitter Towers...");
        nwrTransmitterMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (nwrTransDlg == null || nwrTransDlg.isDisposed()) {
                    nwrTransDlg = new NwrTransmitterDlg(shell);
                    nwrTransDlg.open();
                } else {
                    nwrTransDlg.bringToTop();
                }
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
                if (hydroGenDlg == null || hydroGenDlg.isDisposed()) {
                    hydroGenDlg = new HydroGenConfigDlg(shell);
                    hydroGenDlg.open();
                } else {
                    hydroGenDlg.bringToTop();
                }
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
                // open Modify Location window on double click
                openModifyDlg();
            }

            @Override
            public void mouseDown(MouseEvent e) {
            }

            @Override
            public void mouseUp(MouseEvent e) {
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
        if (stationFilterDlg == null || stationFilterDlg.isDisposed()) {
            stationFilterDlg = new StationFilterOptionsDlg(shell);
            stationFilterDlg.addListener(this);
            stationFilterDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    stationFilterDlg.removeListener(HydroBaseDlg.this);
                }
            });
            stationFilterDlg.open();
        } else {
            stationFilterDlg.bringToTop();
        }
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
        if (addLocDlg == null || addLocDlg.isDisposed()) {
            addLocDlg = new AddModifyLocationDlg(shell, false,
                    getSelectedLocation().getStation(), getStationAndName());
            addLocDlg.addListener(this);
            addLocDlg.open();
        } else {
            addLocDlg.bringToTop();
        }
    }

    /**
     * Open the Modify Location Dialog.
     */
    private void openModifyDlg() {
        String lid = getSelectedLocation().getStation();
        AddModifyLocationDlg modLocDlg = modLocDlgMap.get(lid);
        if (modLocDlg == null) {
            modLocDlg = new AddModifyLocationDlg(shell, true,
                    getSelectedLocation().getStation(), getStationAndName());
            modLocDlg.addListener(this);
            modLocDlgMap.put(lid, modLocDlg);
            modLocDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof String) {
                        String lid = returnValue.toString();
                        modLocDlgMap.remove(lid);
                    }
                }
            });
            modLocDlg.open();
        } else {
            modLocDlg.bringToTop();
        }
    }

    /**
     * Display the Low Water Statement dialog for the selected station.
     */
    private void handleLowWaterStatementDlg() {
        String lid = getSelectedLocation().getStation();
        LowWaterStatementDlg lowWaterStmntDlg = lowWaterStmntDlgMap.get(lid);

        if (lowWaterStmntDlg == null || lowWaterStmntDlg.isDisposed()) {
            lowWaterStmntDlg = new LowWaterStatementDlg(shell,
                    getStationAndName(), true, lid);
            lowWaterStmntDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof String) {
                        String lid = returnValue.toString();
                        lowWaterStmntDlgMap.remove(lid);
                    }
                }
            });
            lowWaterStmntDlg.open();
            lowWaterStmntDlgMap.put(lid, lowWaterStmntDlg);
        } else {
            lowWaterStmntDlg.bringToTop();
        }
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
        HBPasswordDlg dialog = new HBPasswordDlg(shell);
        int returnCode = dialog.open();
        return ((returnCode == Window.OK) && (dialog.isVerified()));
    }

    /**
     * Inner class for the password dialog.
     */
    private class HBPasswordDlg extends CaveJFACEDialog {

        private final String password;

        private Text text;

        private int numTries;

        private boolean verified;

        protected HBPasswordDlg(Shell parentShell) {
            super(parentShell);
            setShellStyle(SWT.DIALOG_TRIM);
            setBlockOnOpen(true);

            this.numTries = 0;
            this.verified = false;
            this.password = getPassword();
        }

        @Override
        public int open() {
            if ((password == null) || (password.isEmpty())) {
                MessageDialog.openInformation(getShell(), "Password",
                        "Please set a password for HydroBase\n"
                                + "in the Setup/Administration dialog.");
            }
            return super.open();
        }

        @Override
        protected Control createDialogArea(Composite parent) {
            Composite composite = (Composite) super.createDialogArea(parent);
            composite.setLayout(new GridLayout(2, false));

            Label label = new Label(composite, SWT.NONE);
            label.setText("Enter Password:");
            label.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, true));

            text = new Text(composite, SWT.SINGLE | SWT.PASSWORD | SWT.BORDER);
            text.setFocus();
            GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
            GC gc = new GC(text);
            gd.widthHint = gc.getFontMetrics().getAverageCharWidth() * 20;
            gc.dispose();
            text.setLayoutData(gd);

            return composite;
        }

        @Override
        protected void okPressed() {
            numTries++;
            getShell().setVisible(false);

            if (text.getText().equals(password)) {
                verified = true;
            } else {
                String dialogTitle;
                String message;
                if (numTries < 3) {
                    dialogTitle = "Invalid Password";
                    message = "Invalid password entered.\n"
                            + "\tPlease try again.";
                } else {
                    dialogTitle = "ABORTING HydroBase";
                    message = "Three failed password attempts - exiting HydroBase.";
                }
                MessageDialog.openError(getShell(), dialogTitle, message);
                getShell().setVisible(true);
                text.setFocus();
            }

            if (verified) {
                super.okPressed();
            } else if (numTries == 3) {
                super.cancelPressed();
            }
        }

        @Override
        protected void configureShell(Shell newShell) {
            super.configureShell(newShell);
            newShell.setText("Enter Password");
        }

        public boolean isVerified() {
            return verified;
        }

        private String getPassword() {
            String pw = null;
            try {
                java.util.List<AdministrationData> data = HydroDBDataManager
                        .getInstance().getData(AdministrationData.class);
                if (!data.isEmpty()) {
                    pw = data.get(0).getHbPassword();
                }
            } catch (VizException e) {
                statusHandler.error("Data Query:"
                        + " Error retrirving HB Password.", e);
            }

            return pw;
        }
    }
}
