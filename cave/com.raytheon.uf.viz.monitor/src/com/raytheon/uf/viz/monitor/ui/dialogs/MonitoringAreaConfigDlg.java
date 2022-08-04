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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.xml.AreaIdXML;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.monitor.Activator;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Monitoring area configuration dialog.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#    Engineer  Description
 * ------------- ---------- --------- ------------------------------------------
 * Apr 06, 2009             lvenable  Initial creation
 * Jun 24, 2010  5885/5886  zhao      added initZoneStationLists(), and revised
 *                                    accordingly
 * Apr 29, 2011  8986       zhao      Read in "Counties" instead of "Forecast
 *                                    Zones"
 * Feb 22, 2012  14413      zhao      modified to reduce calls to database
 * Nov 16, 2012  1297       skorolev  Changes for non-blocking dialog.
 * Feb 06, 2013  1578       skorolev  Fixed a cursor problem for checkboxes.
 * Oct 07, 2013  2443       lvenable  Fixed image memory leak.
 * Jan 29, 2014  2757       skorolev  Added status variables.
 * Apr 23, 2014  3054       skorolev  Fixed issue with removing from list a new
 *                                    zone and a new station.
 * Apr 28, 2014  3086       skorolev  Updated getConfigManager method.
 * Sep 16, 2014  2757       skorolev  Updated createBottomButtons method.
 * Sep 24, 2014  2757       skorolev  Fixed problem with adding and removing
 *                                    zones.
 * Oct 27, 2014  3667       skorolev  Corrected functionality of dialog. Cleaned
 *                                    code.
 * Nov 12, 2014  3650       skorolev  Added confirmation box for unsaved changes
 *                                    in the dialog.
 * Mar 08, 2015  3888       dhladky   Restored threshold pop-up when adding new
 *                                    stations/zones.
 * Sep 18, 2015  3873       skorolev  Added formIsValid method.
 * Oct 19, 2015  3841       skorolev  Corrected formIsValid messages.
 * Nov 12, 2015  3841       dhladky   Augmented Slav's fix for moving platforms.
 * Dec 02, 2015  3873       dhladky   Pulled 3841 to 16.1.1.
 * Jun 18, 2018  7023       tgurney   Handle multiple stations returned from
 *                                    Delete Station dialog
 * Jul 25, 2018  6748       randerso  Fixed to work with changes in
 *                                    CaveSWTDialog.shouldClose(). Code cleanup.
 *
 * </pre>
 *
 * @author lvenable
 */
public abstract class MonitoringAreaConfigDlg extends CaveSWTDialog
        implements INewZoneStnAction {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MonitoringAreaConfigDlg.class);

    /** Zone radio button. **/
    private Button zoneRdo;

    /** Selected Zone text control. **/
    private Text selectedStnZoneTF;

    /** Monitor Area label. **/
    private Label montiorAreaLbl;

    /** Monitor area list control. **/
    private List monitorAreaList;

    /** Associated label. **/
    private Label associatedLbl;

    /** Associated list control. **/
    private List associatedList;

    /** Additional label. **/
    private Label additionalLbl;

    /** Additional list control. **/
    private List additionalList;

    /** MA radio button. **/
    private Button maRdo;

    /** Regional Stations radio button. **/
    private Button regionalRdo;

    /** MA regional list control. **/
    private List maRegionalList;

    /** Add New button. **/
    private Button addNewBtn;

    /** Edit/Delete button. **/
    private Button editDeleteBtn;

    /** Time window control. **/
    protected Scale timeWindow;

    /** Time window status. */
    protected boolean timeWindowChanged = false;

    /** Time scale label to display the value set by the time scale. **/
    private Label timeWindowLbl;

    /** Ship Distance scale. **/
    protected Scale shipDistance;

    /** Ship Distance status. */
    protected boolean shipDistanceChanged = false;

    /**
     * Ship Distance scale label to display the value set by the distance scale.
     **/
    private Label shipDistanceLBl;

    /** Arrow up image. **/
    private Image arrowUpImg;

    /** Arrow down image. **/
    private Image arrowDownImg;

    /** Fog check button. **/
    protected Button fogChk;

    /** Fog check button status. */
    protected boolean fogChkChanged = false;

    /** Control font. **/
    private Font controlFont;

    /** Application name. **/
    private final CommonConfig.AppName appName;

    /** The current site. **/
    protected static String currentSite = null;

    /** monitor area zones **/
    private java.util.List<String> maZones = null;

    /** monitor area zones status. */
    protected boolean maZonesChanged = false;

    /** monitor area stations **/
    private java.util.List<String> maStations = null;

    /** monitor area stations status. */
    protected boolean maStationsChanged = false;

    /** monitor area additional zones **/
    private java.util.List<String> additionalZones = null;

    /** monitor area additional stations in the region **/
    private java.util.List<String> additionalStns = null;

    /** current Monitor Configuration Manager **/
    protected FSSObsMonitorConfigurationManager configMgr;

    /** Table mode **/
    private static enum Mode {
        Zone, Station
    };

    /** mode by default **/
    private Mode mode = Mode.Zone;

    /** Add new Zone dialog. */
    private AddNewZoneDlg addNewZoneDlg;

    /** Add new Station dialog. */
    private AddNewStationDlg addNewStnDlg;

    /** Edit newly added zone dialog. */
    private EditNewZoneDlg editDlg;

    /** Delete a Newly Entered Station dialog */
    private DeleteStationDlg deleteStnDlg;

    /** Flag set when user wants to close with unsaved modifications. */
    protected boolean closeFlag = false;

    private static final String INVALID_AREA_MSG_C = "Invalid Area ID = '%s' entered.\n"
            + "Please enter a correctly formatted Area ID:\n"
            + "Zone ID must have three letters and three digits.\n"
            + "A third character should be C for county";

    private static final String INVALID_AREA_MSG_Z = "and Z for marine zone.\n";

    private static final String INVALID_COORD_MSG = "Invalid Lat/Lon entered:\n"
            + "Latitude = '%s'\n" + "Longitude = '%s'\n"
            + "Please enter correctly formatted Lat and Lon values:\n"
            + "Latitude should be between -90,90.\n"
            + "Longitude should be between -180,180.";

    private static final String MODIFY_THRESHOLD_MSG = "New zones have been added, and their monitoring thresholds "
            + "have been set to default values; would you like to modify "
            + "their threshold values now?";

    /** County constant char */
    private static final char C = 'C';

    /** Zone constant char */
    private static final char Z = 'Z';

    /**
     * Constructor.
     *
     * @param parent
     *            Parent shell.
     * @param title
     *            Dialog title.
     * @param appName
     *            Application name.
     */
    public MonitoringAreaConfigDlg(Shell parent, String title,
            CommonConfig.AppName appName) {
        super(parent, SWT.DIALOG_TRIM,
                CAVE.DO_NOT_BLOCK | CAVE.INDEPENDENT_SHELL);
        setText(title);
        this.appName = appName;
        synchronized (MonitoringAreaConfigDlg.class) {
            currentSite = LocalizationManager.getInstance().getCurrentSite();
        }
        configMgr = getInstance();
    }

    /**
     * Initialize maZones/Stations and additionalZones/Stations
     */
    private void initZoneStationLists() {
        // (1) set monitor area zones
        maZones = configMgr.getAreaList();
        Collections.sort(maZones);
        // (2) set monitor area stations
        maStations = new ArrayList<>();
        try {
            for (String zone : maZones) {
                java.util.List<String> stns = configMgr
                        .getAreaStationsWithType(zone);
                for (String stn : stns) {
                    if (!maStations.contains(stn)) {
                        maStations.add(stn);
                    }
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    " Error initiate Zone/Stations list.", e);
        }
        Collections.sort(maStations);
        // (3) set additional zones in the neighborhood of the monitor area
        additionalZones = configMgr.getAdjacentAreaList();
        Collections.sort(additionalZones);
        // (4) set additional stations
        additionalStns = new ArrayList<>();
        try {
            for (String zone : additionalZones) {
                java.util.List<String> stns = configMgr
                        .getAdjacentAreaStationsWithType(zone);
                for (String stn : stns) {
                    if (!additionalStns.contains(stn)) {
                        additionalStns.add(stn);
                    }
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    " Error initiate Additional Zone/Stations list.", e);
        }
        Collections.sort(additionalStns);
        mode = Mode.Zone;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        mainLayout.verticalSpacing = 1;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        // Initialize the font and images
        initFontAndImages();
        // Initialize all of the controls and layouts
        initComponents();
        // initialize zone/station lists
        initZoneStationLists();
        // Populate the dialog
        populateLeftLists("");
        setValues();
    }

    @Override
    public boolean shouldClose() {
        if (closeFlag || !dataIsChanged()) {
            return true;
        }

        if (verifyClose()) {
            resetStatus();
        }

        return true;
    }

    /**
     * Initialize the images and font.
     */
    private void initFontAndImages() {
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);
        ImageDescriptor id = Activator.imageDescriptorFromPlugin(
                Activator.PLUGIN_ID, "images/arrowDn.png");
        arrowDownImg = id.createImage();
        id = Activator.imageDescriptorFromPlugin(Activator.PLUGIN_ID,
                "images/arrowUp.png");
        arrowUpImg = id.createImage();
    }

    /**
     * Initialize the components on the display.
     */
    private void initComponents() {
        Composite mainListComp = new Composite(shell, SWT.NONE);
        mainListComp.setLayout(new GridLayout(2, false));
        mainListComp.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        createTopConfigControl(mainListComp);
        createLeftListsAndControls(mainListComp);
        createRightListsAndControls(mainListComp);
        createBottomScaleControls(mainListComp);
        createBottomButtons();
    }

    /**
     * Creates the top configuration controls.
     *
     * @param parentComp
     */
    private void createTopConfigControl(Composite parentComp) {
        Composite radioComp = new Composite(parentComp, SWT.NONE);
        radioComp.setLayout(new GridLayout(3, false));

        Label configLbl = new Label(radioComp, SWT.NONE);
        configLbl.setText("Configure: ");

        zoneRdo = new Button(radioComp, SWT.RADIO);
        zoneRdo.setText("Zone");
        zoneRdo.setSelection(true);
        zoneRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                getShell().setCursor(
                        getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
                mode = Mode.Zone;
                changeZoneStationControls();
                populateLeftLists("");
                if (!getShell().isDisposed()) {
                    getShell().setCursor(null);
                }
            }
        });

        Button stationRdo = new Button(radioComp, SWT.RADIO);
        stationRdo.setText("Station");
        stationRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                getShell().setCursor(
                        getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
                mode = Mode.Station;
                changeZoneStationControls();
                populateLeftLists("");
                if (!getShell().isDisposed()) {
                    getShell().setCursor(null);
                }
            }
        });

        /*
         * Create the Selected Area Zone/Station text control.
         */
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        selectedStnZoneTF = new Text(parentComp, SWT.BORDER);
        selectedStnZoneTF.setEditable(false);
        selectedStnZoneTF.setLayoutData(gd);
    }

    /**
     * Creates the Monitor/Additional label and list controls.
     *
     * @param parentComp
     *            Parent composite.
     */
    private void createLeftListsAndControls(Composite parentComp) {
        Composite leftComp = new Composite(parentComp, SWT.NONE);
        leftComp.setLayout(new GridLayout(2, true));
        leftComp.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        /*
         * Create the Monitor Area label and list control.
         */
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        montiorAreaLbl = new Label(leftComp, SWT.NONE);
        montiorAreaLbl.setText("Monitor Area Zones:");
        montiorAreaLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 185;
        gd.heightHint = 200;
        gd.horizontalSpan = 2;
        monitorAreaList = new List(leftComp,
                SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        monitorAreaList.setFont(controlFont);
        monitorAreaList.setLayoutData(gd);
        monitorAreaList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleMonitorAreaListSelection();
            }
        });

        /*
         * Create the Monitor Area Add and Remove buttons.
         */
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        Button monAreaAddBtn = new Button(leftComp, SWT.PUSH);
        monAreaAddBtn.setText("Add");
        monAreaAddBtn.setImage(arrowUpImg);
        monAreaAddBtn.setLayoutData(gd);
        monAreaAddBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                addZoneStn();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        Button monAreaRemoveBtn = new Button(leftComp, SWT.PUSH);
        monAreaRemoveBtn.setText("Remove");
        monAreaRemoveBtn.setImage(arrowDownImg);
        monAreaRemoveBtn.setLayoutData(gd);
        monAreaRemoveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                removeZoneStn();
            }
        });

        /*
         * Create the Additional label and list control.
         */
        gd = new GridData(SWT.FILL, SWT.BOTTOM, true, false);
        gd.verticalIndent = 5;
        gd.heightHint = 20;
        gd.horizontalSpan = 2;
        additionalLbl = new Label(leftComp, SWT.NONE);
        additionalLbl.setText("Additional Zones:");
        additionalLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 185;
        gd.heightHint = 200;
        gd.horizontalSpan = 2;
        additionalList = new List(leftComp,
                SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        additionalList.setFont(controlFont);
        additionalList.setLayoutData(gd);

        /*
         * Create the Add New Zone/Station button.
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        addNewBtn = new Button(leftComp, SWT.PUSH);
        addNewBtn.setText("Add a New Zone to Monitor Area...");
        addNewBtn.setLayoutData(gd);
        addNewBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleAddNewAction();
            }
        });
    }

    /**
     * Creates the Associated & MA/Regional labels and controls.
     *
     * @param parentComp
     *            Parent composite.
     */
    private void createRightListsAndControls(Composite parentComp) {
        Composite rightComp = new Composite(parentComp, SWT.NONE);
        rightComp.setLayout(new GridLayout(2, true));
        rightComp.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        /*
         * Create the Associated label.
         */
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        associatedLbl = new Label(rightComp, SWT.NONE);
        associatedLbl.setText("Associated Stations:");
        associatedLbl.setLayoutData(gd);
        /*
         * Create Associated list control.
         */
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 185;
        gd.heightHint = 200;
        gd.horizontalSpan = 2;
        associatedList = new List(rightComp,
                SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        associatedList.setFont(controlFont);
        associatedList.setLayoutData(gd);
        /*
         * Create the Monitor Area Add and Remove buttons.
         */
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        Button assocAddBtn = new Button(rightComp, SWT.PUSH);
        assocAddBtn.setText("Add");
        assocAddBtn.setImage(arrowUpImg);
        assocAddBtn.setLayoutData(gd);
        assocAddBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                addAssociated();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        Button assocRemoveBtn = new Button(rightComp, SWT.PUSH);
        assocRemoveBtn.setText("Remove");
        assocRemoveBtn.setImage(arrowDownImg);
        assocRemoveBtn.setLayoutData(gd);
        assocRemoveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                removeAssociated();
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        gd.verticalIndent = 5;
        gd.heightHint = 20;
        gd.widthHint = 110;
        maRdo = new Button(rightComp, SWT.RADIO);
        maRdo.setText("MA Stns");
        maRdo.setSelection(true);
        maRdo.setLayoutData(gd);
        maRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                getShell().setCursor(
                        getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
                populateMaRegionalList();
                if (!getShell().isDisposed()) {
                    getShell().setCursor(null);
                }
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        gd.verticalIndent = 5;
        gd.heightHint = 20;
        gd.widthHint = 130;
        regionalRdo = new Button(rightComp, SWT.RADIO);
        regionalRdo.setText("Regional Stns");
        regionalRdo.setLayoutData(gd);
        regionalRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                getShell().setCursor(
                        getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
                populateMaRegionalList();
                if (!getShell().isDisposed()) {
                    getShell().setCursor(null);
                }
            }
        });
        /*
         * Create the Additional stations list control.
         */
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 185;
        gd.heightHint = 200;
        gd.horizontalSpan = 2;
        maRegionalList = new List(rightComp,
                SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        maRegionalList.setFont(controlFont);
        maRegionalList.setLayoutData(gd);
        /*
         * Create the Add New Zone/Station button.
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        editDeleteBtn = new Button(rightComp, SWT.PUSH);
        editDeleteBtn.setText("Edit a Newly added Zone...");
        editDeleteBtn.setLayoutData(gd);
        editDeleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleEditDeleteAction();
            }
        });
    }

    /**
     * Creates the bottom scale controls.
     *
     * @param parentComp
     *            Parent composite.
     */
    private void createBottomScaleControls(Composite parentComp) {
        addSeparator(parentComp);

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Composite scaleComp = new Composite(parentComp, SWT.NONE);
        scaleComp.setLayout(new GridLayout(2, false));
        scaleComp.setLayoutData(gd);

        /*
         * Create the Time Window controls.
         */
        gd = new GridData();
        gd.horizontalSpan = 2;
        Label timeLbl = new Label(scaleComp, SWT.NONE);
        timeLbl.setText("Time window (hrs)");
        timeLbl.setLayoutData(gd);

        // Timewindow scale should be from 0.25(15 min) to 8 hours with step
        // 0.05 hour(3 min).
        int max = (int) Math.round((8.00 - 0.25) / .05);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        timeWindow = new Scale(scaleComp, SWT.HORIZONTAL);
        timeWindow.setMinimum(0);
        timeWindow.setMaximum(max);
        timeWindow.setLayoutData(gd);
        timeWindow.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setTimeScaleLabel();
                timeWindowChanged = true;
            }
        });

        gd = new GridData(50, SWT.DEFAULT);
        timeWindowLbl = new Label(scaleComp, SWT.NONE);
        timeWindowLbl.setFont(controlFont);
        timeWindowLbl.setLayoutData(gd);

        setTimeScaleLabel();

        // If this is a snow dialog then return since we don't need to
        // create the Ship Distance scale and Fog check controls.
        if (appName == AppName.SNOW) {
            return;
        }
        /*
         * Create the Ship Distance controls.
         */
        addSeparator(scaleComp);

        gd = new GridData();
        gd.horizontalSpan = 2;
        Label distanceLbl = new Label(scaleComp, SWT.NONE);
        distanceLbl.setText("Ship Distance (Nautical Miles):");
        distanceLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        shipDistance = new Scale(scaleComp, SWT.HORIZONTAL);
        shipDistance.setMinimum(0);
        shipDistance.setMaximum(200);
        shipDistance.setSelection(100);
        shipDistance.setLayoutData(gd);
        shipDistance.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setShipDistScaleLabel();
                shipDistanceChanged = true;
            }
        });

        gd = new GridData(50, SWT.DEFAULT);
        shipDistanceLBl = new Label(scaleComp, SWT.NONE);
        shipDistanceLBl.setFont(controlFont);
        shipDistanceLBl.setLayoutData(gd);

        /*
         * Create the Fog check box.
         */
        addSeparator(scaleComp);

        gd = new GridData();
        gd.horizontalSpan = 2;
        fogChk = new Button(scaleComp, SWT.CHECK);
        fogChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                fogChkChanged = true;
            }
        });
        setAlgorithmText();
    }

    /**
     * Creates the bottom OK/Cancel buttons.
     */
    private void createBottomButtons() {
        addSeparator(shell);

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainButtonComp = new Composite(shell, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                try {
                    handleOkBtnSelection();
                } catch (LocalizationException | SerializationException
                        | IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "There is a problem saving changes. ", e);
                }
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (verifyClose()) {
                    resetStatus();
                    close();
                } else {
                    event.doit = false;
                }
            }
        });
    }

    /**
     * Adds a separator bar to the display.
     *
     * @param parentComp
     *            Parent composite.
     */
    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout) parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Rounds a value to the hundredths decimal place.
     *
     * @param val
     *            Value.
     * @return Rounded value.
     */
    private double roundToHundredths(double val) {
        double roundedVal = 0.0;

        roundedVal = Math.round(val * 100) / 100.0;

        return roundedVal;
    }

    /**
     * Sets the time scale label in hours.
     */
    protected void setTimeScaleLabel() {
        // Conversion of a scale to hour.
        double val = timeWindow.getSelection() * .05 + .25;
        val = roundToHundredths(val);
        timeWindowLbl.setText(String.format("%5.2f", val));
    }

    /**
     * Sets the ship distance scale label.
     */
    protected void setShipDistScaleLabel() {
        shipDistanceLBl
                .setText(String.format("%5d", shipDistance.getSelection()));
    }

    /**
     * Changes the Zone and Station controls.
     */
    private void changeZoneStationControls() {
        if (mode == Mode.Zone) {
            montiorAreaLbl.setText("Monitor Area Zones:");
            additionalLbl.setText("Additional Zones:");
            addNewBtn.setText("Add a New Zone to Monitor Area...");
            associatedLbl.setText("Associated Stations:");
            maRdo.setText("MA Stns");
            regionalRdo.setText("Regional Stns");
            editDeleteBtn.setText("Edit a Newly added Zone...");
        } else {
            montiorAreaLbl.setText("Monitor Area Stations:");
            additionalLbl.setText("Additional Stations:");
            addNewBtn.setText("Add a New Stn to Monitor Area...");
            associatedLbl.setText("Associated Zones:");
            maRdo.setText("MA Zones");
            regionalRdo.setText("Regional Zones");
            editDeleteBtn.setText("Delete a Newly added Station...");
        }
    }

    /**
     * Handles the Add New button click.
     */
    private void handleAddNewAction() {
        // Zone configure
        if (zoneRdo.getSelection()) {
            if (addNewZoneDlg == null) {
                addNewZoneDlg = new AddNewZoneDlg(shell, appName, this);
                addNewZoneDlg.addCloseCallback(new ICloseCallback() {
                    @Override
                    public void dialogClosed(Object returnValue) {
                        if ((Boolean) returnValue) {
                            // Update the dialog
                            populateLeftLists("");
                        }
                        addNewZoneDlg = null;
                    }
                });
            }
            addNewZoneDlg.open();
        } else {
            // Station configure
            if (maRegionalList.getSelectionIndex() != -1) {
                String area = maRegionalList
                        .getItem(maRegionalList.getSelectionIndex());
                if (addNewStnDlg == null) {
                    addNewStnDlg = new AddNewStationDlg(shell, appName, area,
                            this);
                    addNewStnDlg.addCloseCallback(new ICloseCallback() {
                        @Override
                        public void dialogClosed(Object returnValue) {
                            if ((Boolean) returnValue) {
                                // Update the dialog
                                populateLeftLists("");
                            }
                            addNewStnDlg = null;
                        }
                    });
                }
                addNewStnDlg.open();
            } else {
                MessageBox messageBox = new MessageBox(shell,
                        SWT.ICON_INFORMATION | SWT.NONE);
                messageBox.setText("Selection error.");
                messageBox.setMessage("Please select a monitoring zone.");
                messageBox.open();
                maRegionalList.select(0);
            }
        }
    }

    /**
     * Handles the Edit/Delete button click.
     */
    private void handleEditDeleteAction() {
        if (zoneRdo.getSelection()) {
            if (editDlg == null) {
                editDlg = new EditNewZoneDlg(shell, appName, this);
                editDlg.addCloseCallback(new ICloseCallback() {
                    @Override
                    public void dialogClosed(Object returnValue) {
                        if (returnValue instanceof String) {
                            // Update the edit dialog
                            String selectedZone = returnValue.toString();
                            maZones.remove(selectedZone);
                            populateLeftLists("");
                        }
                        editDlg = null;
                    }
                });
            }
            editDlg.open();
        } else {
            if (deleteStnDlg == null) {
                deleteStnDlg = new DeleteStationDlg(shell, appName, this);
                deleteStnDlg.addCloseCallback(new ICloseCallback() {
                    @Override
                    public void dialogClosed(Object returnValue) {
                        if (returnValue instanceof java.util.List) {
                            java.util.List<String> stations = (java.util.List<String>) returnValue;
                            for (String s : stations) {
                                maStations.remove(s);
                            }
                            populateLeftLists("");
                        }
                        deleteStnDlg = null;
                    }
                });
            }
            deleteStnDlg.open();
        }
    }

    /**
     * Populates the MA-Regional list box.
     */
    private void populateMaRegionalList() {
        maRegionalList.removeAll();
        if (mode == Mode.Zone) {
            if (maRdo.getSelection()) {
                maRegionalList.setItems(
                        maStations.toArray(new String[maStations.size()]));
            } else {
                maRegionalList.setItems(additionalStns
                        .toArray(new String[additionalStns.size()]));
            }
        } else {
            // Station Mode
            if (maRdo.getSelection()) {
                maRegionalList
                        .setItems(maZones.toArray(new String[maZones.size()]));
            } else {
                maRegionalList.setItems(additionalZones
                        .toArray(new String[additionalZones.size()]));
            }
            maRegionalList.select(0);
        }
    }

    /**
     * Populates the zone list boxes.
     */
    protected void populateLeftLists(String selected) {
        if (mode == Mode.Zone) {
            /** Zone Mode */
            Collections.sort(maZones);
            monitorAreaList
                    .setItems(maZones.toArray(new String[maZones.size()]));
            Collections.sort(additionalZones);
            additionalList.setItems(additionalZones
                    .toArray(new String[additionalZones.size()]));
        } else {
            /** Station Mode */
            Collections.sort(maStations);
            monitorAreaList.setItems(
                    maStations.toArray(new String[maStations.size()]));
            Collections.sort(additionalStns);
            additionalList.setItems(
                    additionalStns.toArray(new String[additionalStns.size()]));
        }
        if (monitorAreaList.getItemCount() > 0) {
            // select top of the list
            if ("".equals(selected)) {
                monitorAreaList.setSelection(0);
            } else {
                monitorAreaList.setSelection(monitorAreaList.indexOf(selected));
            }
            handleMonitorAreaListSelection();
        }
    }

    /**
     * Sets the slider values and the check box.
     */
    protected void setValues() {
        // Conversion scale to hour.
        Double val = (configMgr.getTimeWindow() - .25) * 20;
        timeWindow.setSelection(val.intValue());
        setTimeScaleLabel();
        // Set other values.
        if (appName != AppName.SNOW) {
            shipDistance.setSelection(configMgr.getShipDistance());
            setShipDistScaleLabel();
            fogChk.setSelection(configMgr.isUseAlgorithms());
        }
    }

    /**
     * Gets changed slider values and the check box.
     */
    protected void getValues() {
        // Conversion of a hour to scale.
        double time = timeWindow.getSelection() * .05 + .25;
        time = roundToHundredths(time);
        configMgr.setTimeWindow(time);
        // Get other values.
        if (appName != AppName.SNOW) {
            configMgr.setShipDistance(shipDistance.getSelection());
            configMgr.setUseAlgorithms(fogChk.getSelection());
        }

    }

    /**
     * Shows a dialog message.
     *
     * @param shell
     *            The parent shell
     * @param style
     *            The dialog style
     * @param title
     *            The dialog title
     * @param msg
     *            The dialog message
     * @return The value representing the button clicked on the dialog
     */
    protected int showMessage(Shell shell, int style, String title,
            String msg) {
        MessageBox messageBox = new MessageBox(shell, style);
        messageBox.setText(title);
        int cnt = title.length() - msg.length();
        if (cnt > 0) {
            // expand message by spaces
            msg = msg + String.format("%" + (cnt + 3) + "s", " ");
        }
        messageBox.setMessage(msg);
        return messageBox.open();
    }

    /**
     * Adds a zone or station to the monitoring area.
     */
    private void addZoneStn() {

        if (additionalList.getSelectionCount() == 0) {
            if (mode == Mode.Zone) {
                showMessage(shell, SWT.ERROR, "Selection Needed",
                        "You must select an additional zone to add.");
            } else {
                showMessage(shell, SWT.ERROR, "Selection Needed",
                        "You must select an additional station to add.");
            }
            return;
        }
        if (mode == Mode.Zone) {
            String zone = additionalList
                    .getItem(additionalList.getSelectionIndex());
            AreaIdXML zoneXML = configMgr.getAdjAreaXML(zone);
            additionalList.remove(additionalList.getSelectionIndex());
            maZones.add(zone);
            Collections.sort(maZones);
            monitorAreaList
                    .setItems(maZones.toArray(new String[maZones.size()]));
            monitorAreaList.setSelection(maZones.indexOf(zone));
            additionalZones.remove(zone);
            configMgr.addArea(zoneXML);
            handleMonitorAreaListSelection();
            if (!configMgr.getAddedZones().contains(zone)) {
                configMgr.getAddedZones().add(zone);
            }
            configMgr.removeAdjArea(zoneXML);
        } else {
            // Station mode
            if (associatedList.getSelectionCount() == 0) {
                showMessage(shell, SWT.ERROR, "Selection Needed",
                        "You must select an associated zone first.");
                return;
            }
            String stn = additionalList
                    .getItem(additionalList.getSelectionIndex());
            additionalList.remove(additionalList.getSelectionIndex());
            maStations.add(stn);
            Collections.sort(maStations);
            monitorAreaList.setItems(
                    maStations.toArray(new String[maStations.size()]));
            monitorAreaList.setSelection(maStations.indexOf(stn));
            additionalStns.remove(stn);
            String zone = associatedList
                    .getItem(associatedList.getSelectionIndex());
            String stnId = stn.substring(0, stn.indexOf('#'));
            String stnType = stn.substring(stn.indexOf('#') + 1);
            configMgr.addNewStation(zone, stnId, stnType,
                    configMgr.getAddedStations().contains(stnId));
            handleMonitorAreaListSelection();
        }

        maZonesChanged = true;
    }

    /**
     * Removes a zone or station from the monitoring area.
     */
    private void removeZoneStn() {
        if (monitorAreaList.getSelectionCount() == 0) {
            if (mode == Mode.Zone) {
                showMessage(shell, SWT.ERROR, "Selection Needed",
                        "You must select a monitor area zone to remove.");
            } else {
                showMessage(shell, SWT.ERROR, "Selection Needed",
                        "You must select a monitor area station to remove.");
            }
            return;
        }
        String entry = monitorAreaList
                .getItem(monitorAreaList.getSelectionIndex());

        if (mode == Mode.Zone) {
            monitorAreaList.remove(monitorAreaList.getSelectionIndex());
            // entry is a zone to remove.
            AreaIdXML zoneXML = configMgr.getAreaXml(entry);
            if (!additionalZones.contains(entry)) {
                additionalZones.add(entry);
            }
            Collections.sort(additionalZones);
            additionalList.setItems(additionalZones
                    .toArray(new String[additionalZones.size()]));
            additionalList.setSelection(additionalZones.indexOf(entry));
            maZones.remove(entry);
            configMgr.removeArea(zoneXML);
            if (configMgr.getAddedZones().contains(entry)) {
                configMgr.getAddedZones().remove(entry);
            }
            configMgr.addAdjArea(zoneXML);
            associatedList.removeAll();
        } else {
            // Station mode. entry is name#type of station.
            if (associatedList.getItemCount() >= 1
                    && associatedList.getSelectionIndex() < 0) {
                showMessage(shell, SWT.ERROR, "Selection Needed",
                        "You must select a associated area zone to remove.");
                return;
            }
            monitorAreaList.remove(monitorAreaList.getSelectionIndex());
            String stnZone = associatedList.getSelection()[0];
            additionalStns.add(entry);
            Collections.sort(additionalStns);
            additionalList.setItems(
                    additionalStns.toArray(new String[additionalStns.size()]));
            additionalList.setSelection(additionalStns.indexOf(entry));
            maStations.remove(entry);
            // station removes from configuration XML files
            configMgr.removeStationFromArea(stnZone,
                    entry.substring(0, entry.indexOf('#')));
        }

        maZonesChanged = true;
    }

    /**
     * Adds an associated zone or station.
     */
    private void addAssociated() {
        if (monitorAreaList.getSelectionCount() == 0) {
            if (mode == Mode.Zone) {
                showMessage(shell, SWT.ERROR, "Select Needed",
                        "You must select a monitor area zone to add.");
                monitorAreaList.select(0);
            } else {
                if (additionalList.getSelectionCount() == 0) {
                    showMessage(shell, SWT.ERROR, "Select Needed",
                            "You must select a monitor area station to add.");
                }
            }
            return;
        }
        if (maRegionalList.getSelectionCount() == 0) {
            if (mode == Mode.Zone) {
                showMessage(shell, SWT.ERROR, "Select Needed",
                        "You must select a station to add into Associated Stations.");
            } else {
                showMessage(shell, SWT.ERROR, "Select Needed",
                        "You must select a zone to add into Associated Zones.");
                associatedList.select(0);
            }
            return;
        }
        String entry = maRegionalList
                .getItem(maRegionalList.getSelectionIndex());
        java.util.List<String> itemList = new LinkedList<>(
                Arrays.asList(associatedList.getItems()));
        if (itemList.contains(entry)) {
            /**
             * if selected entry is already in associated list: highlight the
             * entry, and no need to do anything else
             */
            associatedList.setSelection(itemList.indexOf(entry));
            return;
        }
        itemList.add(entry);
        Collections.sort(itemList);
        associatedList.setItems(itemList.toArray(new String[itemList.size()]));
        associatedList.setSelection(itemList.indexOf(entry));
        /**
         * Make changes to the zone/station lists accordingly, if needed, and
         * store the changes in Monitor Configuration Manager
         */
        if (mode == Mode.Zone) {
            if (regionalRdo.getSelection()) {
                // entry is a station selected from additional stations
                maStations.add(entry);
                Collections.sort(maStations);
                additionalStns.remove(entry);
                maRegionalList.remove(maRegionalList.getSelectionIndex());
            }
            String zone = monitorAreaList
                    .getItem(monitorAreaList.getSelectionIndex());
            String stnId = entry.substring(0, entry.indexOf('#'));
            String stnType = entry.substring(entry.indexOf('#') + 1);

            configMgr.addNewStation(zone, stnId, stnType,
                    configMgr.getAddedStations().contains(stnId));
        } else {
            // Station mode
            if (regionalRdo.getSelection()) {
                // entry is a zone selected from additional zones
                AreaIdXML zoneXML = configMgr.getAdjAreaXML(entry);
                maZones.add(entry);
                Collections.sort(maZones);
                additionalZones.remove(entry);
                maRegionalList.remove(maRegionalList.getSelectionIndex());
                configMgr.addArea(zoneXML);
            }
            String stn = monitorAreaList
                    .getItem(monitorAreaList.getSelectionIndex());
            String stnId = stn.substring(0, stn.indexOf('#'));
            String stnType = stn.substring(stn.indexOf('#') + 1);
            configMgr.addNewStation(entry, stnId, stnType,
                    configMgr.getAddedStations().contains(stnId));
        }

        maStationsChanged = true;
    }

    /**
     * Removes an associated zone or station.
     */
    private void removeAssociated() {
        if (associatedList.getSelectionCount() == 0) {
            if (mode == Mode.Zone) {
                showMessage(shell, SWT.ERROR, "Select Needed",
                        "You must select an associated station to remove.");
            } else {
                showMessage(shell, SWT.ERROR, "Select Needed",
                        "You must select an associated zone to remove.");
            }
            return;
        }
        String entry = associatedList
                .getItem(associatedList.getSelectionIndex());
        associatedList.remove(associatedList.getSelectionIndex());
        if (mode == Mode.Zone) {
            String zone = monitorAreaList
                    .getItem(monitorAreaList.getSelectionIndex());
            configMgr.removeStationFromArea(zone, entry);
            java.util.List<String> zones = configMgr
                    .getAreaByStationId(entry.substring(0, entry.indexOf('#')));
            if (zones.isEmpty()) {
                // entry is no longer an MA station
                maStations.remove(entry);
                additionalStns.add(entry);
                Collections.sort(additionalStns);
                if (maRdo.getSelection()) {
                    maRegionalList.setItems(
                            maStations.toArray(new String[maStations.size()]));
                } else {
                    maRegionalList.setItems(additionalStns
                            .toArray(new String[additionalStns.size()]));
                }
            }

        } else {
            // Station mode
            String stn = monitorAreaList
                    .getItem(monitorAreaList.getSelectionIndex());
            configMgr.removeStationFromArea(entry, stn);

        }

        maStationsChanged = true;
    }

    /**
     * Handles the monitor area list selection.
     */
    private void handleMonitorAreaListSelection() {
        if (monitorAreaList.getSelectionIndex() < 0) {
            return;
        }
        if (mode == Mode.Zone) {
            String zone = monitorAreaList
                    .getItem(monitorAreaList.getSelectionIndex());
            selectedStnZoneTF.setText(zone);
            java.util.List<String> stations = configMgr
                    .getAreaStationsWithType(zone);
            if (stations.size() > 1) {
                Collections.sort(stations);
            }
            associatedList.removeAll();
            if (!stations.isEmpty()) {
                associatedList.setItems(
                        stations.toArray(new String[stations.size()]));
            }
        } else {
            // Station mode
            String station = monitorAreaList
                    .getItem(monitorAreaList.getSelectionIndex());
            selectedStnZoneTF.setText(station);
            java.util.List<String> zones = configMgr.getAreaByStationId(
                    station.substring(0, station.indexOf('#')));
            if (zones.size() > 1) {
                Collections.sort(zones);
            }
            associatedList.removeAll();
            if (!zones.isEmpty()) {
                associatedList
                        .setItems(zones.toArray(new String[zones.size()]));
            }
        }
        populateMaRegionalList();
    }

    /**
     * Save configuration parameters.
     *
     * @throws SerializationException
     * @throws LocalizationException
     * @throws IOException
     */
    protected void saveConfigs()
            throws LocalizationException, SerializationException, IOException {
        getValues();
        configMgr.saveConfigXml();
        configMgr.saveAdjacentAreaConfigXml();
    }

    /**
     * Sets algorithm text.
     */
    protected void setAlgorithmText() {
        fogChk.setText("The Fog Monitor overall threat level is "
                + "considered when determining the anchor color.");
    }

    /**
     * Handles OK button. Save changes and close the dialog (or just close if
     * there are no changes).
     *
     * @throws IOException
     * @throws SerializationException
     * @throws LocalizationException
     */
    protected abstract void handleOkBtnSelection()
            throws LocalizationException, SerializationException, IOException;

    /**
     * Adds a new zone to monitor area and refresh GUI
     *
     * @param zone
     */
    public void addZoneToMA(String zone) {
        maZones.add(zone);
        Collections.sort(maZones);
        populateLeftLists(zone);
    }

    /**
     * Adds a new station to monitor area and refresh GUI
     *
     * @param stnWithType
     *            (String of station ID with type)
     */
    public void addStationToMA(String stnWithType) {
        maStations.add(stnWithType);
        Collections.sort(maStations);
        populateLeftLists("");
    }

    @Override
    public void addNewZoneAction(String id, String lat, String log) {
        addZoneToMA(id);
    }

    @Override
    public boolean isExistingZone(String zone) {
        if (maZones.contains(zone) || additionalZones.contains(zone)) {
            return true;
        }
        return false;
    }

    /**
     * Testing input in the form.
     *
     * @param area
     * @param latString
     * @param lonString
     * @return
     */
    public boolean formIsValid(String area, String latString,
            String lonString) {
        boolean retVal = true;
        if ("".equals(area) || area.length() != 6
                || (area.charAt(2) != C && area.charAt(2) != Z)) {
            StringBuilder invalidMsg = new StringBuilder(INVALID_AREA_MSG_C);
            if (appName.equals(AppName.SNOW)) {
                invalidMsg.append(".");
            } else {
                invalidMsg.append(" ");
                invalidMsg.append(INVALID_AREA_MSG_Z);
            }
            displayInputErrorMsg(String.format(invalidMsg.toString(), area));
            retVal = false;
        }
        if (latString == null || latString.isEmpty() || lonString == null
                || lonString.isEmpty()) {
            latLonErrorMsg(latString, lonString);
            retVal = false;
        }
        return retVal;
    }

    @Override
    public void addNewStationAction(String stnWithType) {
        addStationToMA(stnWithType);
    }

    @Override
    public boolean isExistingStation(String stnWithType) {
        if (maStations.contains(stnWithType)
                || additionalStns.contains(stnWithType)) {
            return true;
        }
        return false;
    }

    @Override
    protected void disposed() {
        controlFont.dispose();
        arrowUpImg.dispose();
        arrowDownImg.dispose();
    }

    @Override
    public void latLonErrorMsg(String latStr, String lonStr) {
        MessageBox messageBox = new MessageBox(shell,
                SWT.ICON_INFORMATION | SWT.OK);
        messageBox.setMessage(INVALID_COORD_MSG);
        messageBox.open();
    }

    /**
     * Reset data status.
     */
    protected void resetStatus() {
        this.timeWindowChanged = false;
        this.maZonesChanged = false;
        this.maStationsChanged = false;
        this.shipDistanceChanged = false;
        this.fogChkChanged = false;
    }

    /**
     * Check if data and data states have been changed.
     *
     * @return
     */
    protected boolean dataIsChanged() {
        if (!configMgr.getAddedZones().isEmpty()
                || !configMgr.getAddedStations().isEmpty()
                || this.timeWindowChanged || this.shipDistanceChanged
                || this.fogChkChanged || this.maZonesChanged
                || this.maStationsChanged) {
            return true;
        }
        return false;
    }

    /**
     * Dialog asking to edit thresholds.
     *
     * @return
     */
    protected int editDialog() {
        int yesno = showMessage(shell, SWT.ICON_QUESTION | SWT.YES | SWT.NO,
                "Edit Thresholds Now?", MODIFY_THRESHOLD_MSG);
        return yesno;
    }

    /**
     * When unsaved modifications this asks the user to verify the close.
     *
     * @return true when okay to close.
     */
    protected boolean verifyClose() {
        boolean state = true;
        if (dataIsChanged()) {
            MessageBox box = new MessageBox(shell,
                    SWT.ICON_WARNING | SWT.OK | SWT.CANCEL);
            box.setText("Confirm Close.");
            box.setMessage("Unsaved changes.\nSelect OK to discard changes.");
            state = box.open() == SWT.OK;
        }
        closeFlag = state;
        return state;
    }

    public java.util.List<String> getMaZones() {
        return maZones;
    }

    public java.util.List<String> getMaStations() {
        return maStations;
    }

    public java.util.List<String> getAdditionalZones() {
        return additionalZones;
    }

    public java.util.List<String> getAdditionalStns() {
        return additionalStns;
    }

    /**
     * Displays Input Error Message
     *
     * @param msg
     */
    public void displayInputErrorMsg(String msg) {
        MessageBox messageBox = new MessageBox(shell,
                SWT.ICON_INFORMATION | SWT.OK);
        messageBox.setText("Invalid input");
        messageBox.setMessage(msg);
        messageBox.open();
    }

    /**
     * Gets Configuration manager.
     *
     * @return manager
     */
    protected abstract FSSObsMonitorConfigurationManager getInstance();
}