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

import java.util.ArrayList;
import java.util.Collections;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
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

import com.raytheon.uf.common.monitor.config.FogMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.MonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.SSMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.SnowMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.xml.AreaIdXML.ZoneType;
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
 * Date         Ticket#       Engineer     Description
 * ------------ ----------    -----------  --------------------------
 * Apr 6, 2009                lvenable     Initial creation
 * Jun 24, 2010 5885/5886     zhao         added initZoneStationLists(),
 *                                           and revised accordingly
 * Apr 29, 2011 DR#8986       zhao         Read in "Counties" instead of "Forecast Zones"
 * Feb 22, 2012 14413         zhao         modified to reduce calls to database
 * Nov 16, 2012 1297          skorolev     Changes for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public abstract class MonitoringAreaConfigDlg extends CaveSWTDialog implements
        INewZoneStnAction {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MonitoringAreaConfigDlg.class);

    /** Zone radio button. **/
    private Button zoneRdo;

    /** Station radio button. **/
    private Button stationRdo;

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

    /** Time scale control. **/
    protected Scale timeScale;

    /** Time scale label to display the value set by the time scale. **/
    private Label timeScaleLbl;

    /** Distance scale. **/
    protected Scale distanceScale;

    /** Distance scale label to display the value set by the distance scale. **/
    private Label distanceScaleLBl;

    /** Monitor area Add button. **/
    private Button monAreaAddBtn;

    /** Monitor area Remove button. **/
    private Button monAreaRemoveBtn;

    /** Associated Add button. **/
    private Button assocAddBtn;

    /** Associated Remove button. **/
    private Button assocRemoveBtn;

    /** Arrow up image. **/
    private Image arrowUpImg;

    /** Arrow down image. **/
    private Image arrowDownImg;

    /** Fog check button. **/
    protected Button fogChk;

    /** Control font. **/
    private Font controlFont;

    /** Application name. **/
    private CommonConfig.AppName appName;

    /** The current site. **/
    protected String currentSite = null;

    /** Wait Cursor **/
    private Cursor waitCursor = null;

    /** Arrow Cursor **/
    private Cursor arrowCursor = null;

    /** monitor area zones **/
    private java.util.List<String> maZones = null;

    /** monitor area stations **/
    private java.util.List<String> maStations = null;

    /** monitor area additional zones **/
    private java.util.List<String> additionalZones = null;

    /** monitor area additional stations in the region **/
    private java.util.List<String> additionalStns = null;

    /** Monitor Configuration Manager **/
    private MonitorConfigurationManager configMgr = null;

    /** Table mode **/
    private static enum Mode {
        Zone, Station
    };

    /** mode by default **/
    private Mode mode = Mode.Zone;

    /**
     * Add new Zone dialog.
     */
    private AddNewZoneDlg addNewZoneDlg;

    /**
     * Add new Station dialog.
     */
    private AddNewStationDlg addNewStnDlg;

    /**
     * Edit newly added zone dialog.
     */
    private EditNewZoneDlg editDlg;

    /**
     * Delete a Newly Entered Station dialog
     */
    private DeleteStationDlg deleteStnDlg;

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
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK
                | CAVE.INDEPENDENT_SHELL);
        setText(title);
        this.appName = appName;
        currentSite = LocalizationManager.getInstance().getCurrentSite();
        waitCursor = new Cursor(parent.getDisplay(), SWT.CURSOR_WAIT);
        arrowCursor = new Cursor(parent.getDisplay(), SWT.CURSOR_ARROW);
    }

    /**
     * Initialize maZones/Stations and additionalZones/Stations
     */
    private void initZoneStationLists() {
        // (1) set monitor area zones
        maZones = configMgr.getAreaList();
        Collections.sort(maZones);
        // (2) set monitor area stations
        maStations = new ArrayList<String>();
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
        additionalZones = configMgr.getAdjacentAreaList(); // adjMgr.getAdjZones();
        Collections.sort(additionalZones);
        // (4) set additional stations
        additionalStns = new ArrayList<String>();
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
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        mainLayout.verticalSpacing = 1;
        return mainLayout;
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
        setReturnValue(false);
        // Initialize the font and images
        initFontAndImages();
        // Initialize all of the controls and layouts
        initComponents();
        // set configuration and adjacent managers
        configMgr = getConfigManager();
        // initialize zone/station lists
        initZoneStationLists();
        // Populate the dialog
        populateLeftLists();
        // populateRightLists(); // this is called from populateLeftLists()
        setValues();
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
        mainListComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        createTopConfigControl(mainListComp);
        createLeftListsAndControls(mainListComp);
        createRightListsAndControls(mainListComp);
        createBottomScaleControls(mainListComp);
        createBottomButtons();
    }

    /**
     * Create the top configuration controls.
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
                shell.setCursor(waitCursor);
                mode = Mode.Zone;
                changeZoneStationControls();
                populateLeftLists();
                shell.setCursor(arrowCursor);
            }
        });

        stationRdo = new Button(radioComp, SWT.RADIO);
        stationRdo.setText("Station");
        stationRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.setCursor(waitCursor);
                mode = Mode.Station;
                changeZoneStationControls();
                populateLeftLists();
                shell.setCursor(arrowCursor);
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
     * Create the Monitor/Additional label and list controls.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createLeftListsAndControls(Composite parentComp) {
        Composite leftComp = new Composite(parentComp, SWT.NONE);
        leftComp.setLayout(new GridLayout(2, true));
        leftComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

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
        monitorAreaList = new List(leftComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
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
        monAreaAddBtn = new Button(leftComp, SWT.PUSH);
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
        monAreaRemoveBtn = new Button(leftComp, SWT.PUSH);
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
        additionalList = new List(leftComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
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
     * Create the Associated & MA/Regional labels and controls.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createRightListsAndControls(Composite parentComp) {
        Composite rightComp = new Composite(parentComp, SWT.NONE);
        rightComp.setLayout(new GridLayout(2, true));
        rightComp
                .setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        /*
         * Create the Associated label and list control.
         */
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        associatedLbl = new Label(rightComp, SWT.NONE);
        associatedLbl.setText("Associated Stations:");
        associatedLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 185;
        gd.heightHint = 200;
        gd.horizontalSpan = 2;
        associatedList = new List(rightComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        associatedList.setFont(controlFont);
        associatedList.setLayoutData(gd);
        /*
         * Create the Monitor Area Add and Remove buttons.
         */
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        assocAddBtn = new Button(rightComp, SWT.PUSH);
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
        assocRemoveBtn = new Button(rightComp, SWT.PUSH);
        assocRemoveBtn.setText("Remove");
        assocRemoveBtn.setImage(arrowDownImg);
        assocRemoveBtn.setLayoutData(gd);
        assocRemoveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                removeAssociated();
            }
        });
        /*
         * Create the Additional label and list control.
         */
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
                shell.setCursor(waitCursor);
                populateMaRegionalList();
                shell.setCursor(arrowCursor);
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
                shell.setCursor(waitCursor);
                populateMaRegionalList();
                shell.setCursor(arrowCursor);
            }
        });

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 185;
        gd.heightHint = 200;
        gd.horizontalSpan = 2;
        maRegionalList = new List(rightComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
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
     * Create the bottom scale controls.
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

        int max = (int) Math.round((8.00 - 0.25) / .05);
        int defaultVal = (int) Math.round((2.00 - 0.25) / .05);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        timeScale = new Scale(scaleComp, SWT.HORIZONTAL);
        timeScale.setMinimum(0);
        timeScale.setMaximum(max);
        timeScale.setSelection(defaultVal);
        timeScale.setLayoutData(gd);
        timeScale.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setTimeScaleLabel();
            }
        });

        gd = new GridData(50, SWT.DEFAULT);
        timeScaleLbl = new Label(scaleComp, SWT.NONE);
        timeScaleLbl.setFont(controlFont);
        timeScaleLbl.setLayoutData(gd);

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
        distanceScale = new Scale(scaleComp, SWT.HORIZONTAL);
        distanceScale.setMinimum(0);
        distanceScale.setMaximum(200);
        distanceScale.setSelection(100);
        distanceScale.setLayoutData(gd);
        distanceScale.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setShipDistScaleLabel();
            }
        });

        gd = new GridData(50, SWT.DEFAULT);
        distanceScaleLBl = new Label(scaleComp, SWT.NONE);
        distanceScaleLBl.setFont(controlFont);
        distanceScaleLBl.setLayoutData(gd);

        setShipDistScaleLabel();

        /*
         * Create the Fog check box.
         */
        addSeparator(scaleComp);

        gd = new GridData();
        gd.horizontalSpan = 2;
        fogChk = new Button(scaleComp, SWT.CHECK);
        setAlgorithmText();
    }

    /**
     * Create the bottom OK/Cancel buttons.
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
                handleOkBtnSelection();
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                closeWithoutSave();
            }
        });
    }

    /**
     * Add a separator bar to the display.
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
     * Round a value to the hundredths decimal place.
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
     * Set the time scale label.
     */
    private void setTimeScaleLabel() {
        double val = timeScale.getSelection() * .05 + .25;
        val = roundToHundredths(val);
        timeScaleLbl.setText(String.format("%5.2f", val));
    }

    /**
     * Set the ship distance scale label.
     */
    private void setShipDistScaleLabel() {
        distanceScaleLBl.setText(String.format("%5d",
                distanceScale.getSelection()));
    }

    /**
     * Change the Zone and Station controls.
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
     * Handle the Add New button click.
     */
    private void handleAddNewAction() {
        if (zoneRdo.getSelection() == true) {
            if (addNewZoneDlg == null) {
                addNewZoneDlg = new AddNewZoneDlg(shell, appName, this);
                addNewZoneDlg.setCloseCallback(new ICloseCallback() {
                    @Override
                    public void dialogClosed(Object returnValue) {
                        if ((Boolean) returnValue) {
                            // Update the dialog
                            populateLeftLists();
                        }
                        addNewZoneDlg = null;
                    }
                });
            }
            addNewZoneDlg.open();
        } else {
            if (associatedList.getSelectionIndex() == -1) {
                associatedList.setSelection(0);
            }
            String area = associatedList.getItem(associatedList
                    .getSelectionIndex());
            if (addNewStnDlg == null) {
                addNewStnDlg = new AddNewStationDlg(shell, appName, area, this);
                addNewStnDlg.setCloseCallback(new ICloseCallback() {
                    @Override
                    public void dialogClosed(Object returnValue) {
                        if ((Boolean) returnValue) {
                            // Update the dialog
                            populateLeftLists();
                        }
                        addNewStnDlg = null;
                    }
                });
            }
            addNewStnDlg.open();
        }
    }

    /**
     * Handle the Edit/Delete button click.
     */
    private void handleEditDeleteAction() {
        if (zoneRdo.getSelection() == true) {
            if (editDlg == null) {
                editDlg = new EditNewZoneDlg(shell, appName);
                editDlg.setCloseCallback(new ICloseCallback() {
                    @Override
                    public void dialogClosed(Object returnValue) {
                        if ((Boolean) returnValue) {
                            // Update the dialog
                            populateLeftLists();
                        }
                        editDlg = null;
                    }
                });
            }
            editDlg.open();
        } else {
            if (deleteStnDlg == null) {
                deleteStnDlg = new DeleteStationDlg(shell, appName);
                deleteStnDlg.setCloseCallback(new ICloseCallback() {
                    @Override
                    public void dialogClosed(Object returnValue) {
                        if ((Boolean) returnValue) {
                            // Update the dialog
                            populateLeftLists();
                        }
                        deleteStnDlg = null;
                    }
                });
            }
            deleteStnDlg.open();
        }
    }

    /**
     * Populate the MA-Regional list box.
     */
    private void populateMaRegionalList() {
        maRegionalList.removeAll();
        if (mode == Mode.Zone) {
            if (maRdo.getSelection()) {
                maRegionalList.setItems(maStations
                        .toArray(new String[maStations.size()]));
            } else {
                maRegionalList.setItems(additionalStns
                        .toArray(new String[additionalStns.size()]));
            }
        } else { // Station Mode
            if (maRdo.getSelection()) {
                maRegionalList.setItems(maZones.toArray(new String[maZones
                        .size()]));
            } else {
                maRegionalList.setItems(additionalZones
                        .toArray(new String[additionalZones.size()]));
            }
        }
    }

    /**
     * Populate the zone list boxes.
     */
    private void populateLeftLists() {
        if (mode == Mode.Zone) {
            /**
             * Zone Mode
             */
            Collections.sort(maZones);
            monitorAreaList
                    .setItems(maZones.toArray(new String[maZones.size()]));
            Collections.sort(additionalZones);
            additionalList.setItems(additionalZones
                    .toArray(new String[additionalZones.size()]));
        } else {
            /**
             * Station Mode
             */
            Collections.sort(maStations);
            monitorAreaList.setItems(maStations.toArray(new String[maStations
                    .size()]));
            Collections.sort(additionalStns);
            additionalList.setItems(additionalStns
                    .toArray(new String[additionalStns.size()]));
        }
        if (monitorAreaList.getItemCount() > 0) {
            monitorAreaList.setSelection(0);
            handleMonitorAreaListSelection();
        }
    }

    /**
     * Set the slider values and the check box.
     */
    private void setValues() {
        MonitorConfigurationManager configManager = getConfigManager();

        if (appName == AppName.SAFESEAS) {
            fogChk.setSelection(((SSMonitorConfigurationManager) configManager)
                    .isUseAlgorithms());
            distanceScale
                    .setSelection(((SSMonitorConfigurationManager) configManager)
                            .getShipDistance());
            setShipDistScaleLabel();
        } else if (appName == AppName.FOG) {
            fogChk.setSelection(((FogMonitorConfigurationManager) configManager)
                    .isUseAlgorithms());
            distanceScale
                    .setSelection(((FogMonitorConfigurationManager) configManager)
                            .getShipDistance());
            setShipDistScaleLabel();
        }
        timeScale.setSelection(configManager.getTimeWindow());
        setTimeScaleLabel();
    }

    /**
     * Show a dialog message.
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
    protected int showMessage(Shell shell, int style, String title, String msg) {
        MessageBox messageBox = new MessageBox(shell, style);
        messageBox.setText(title);
        messageBox.setMessage(msg);
        return messageBox.open();
    }

    /**
     * Add a zone or station to the monitoring area.
     */
    private void addZoneStn() {

        if (additionalList.getSelectionCount() == 0) {
            if (mode == Mode.Zone) {
                showMessage(shell, SWT.ERROR, "Selection Needed",
                        "You must select a station first");
            } else {
                showMessage(shell, SWT.ERROR, "Selection Needed",
                        "You must select a zone first");
            }
            return;
        }
        String entry = additionalList.getItem(additionalList
                .getSelectionIndex());
        additionalList.remove(additionalList.getSelectionIndex());
        if (mode == Mode.Zone) {
            maZones.add(entry);
            Collections.sort(maZones);
            monitorAreaList
                    .setItems(maZones.toArray(new String[maZones.size()]));
            monitorAreaList.setSelection(maZones.indexOf(entry));
            handleMonitorAreaListSelection();

            additionalZones.remove(entry);

            configMgr.addArea(entry, entry.charAt(2) == 'Z' ? ZoneType.MARITIME
                    : ZoneType.REGULAR);

            if (!configMgr.getAddedZones().contains(entry)) {
                configMgr.getAddedZones().add(entry);
            }
        } else { // Station mode
            maStations.add(entry);
            Collections.sort(maStations);
            monitorAreaList.setItems(maStations.toArray(new String[maStations
                    .size()]));
            monitorAreaList.setSelection(maStations.indexOf(entry));
            handleMonitorAreaListSelection();
            additionalStns.remove(entry);
        }
    }

    /**
     * Remove a zone or station from the monitoring area.
     */
    private void removeZoneStn() {
        if (monitorAreaList.getSelectionCount() == 0) {
            if (mode == Mode.Zone) {
                showMessage(shell, SWT.ERROR, "Selection Needed",
                        "You must select a station first");
            } else {
                showMessage(shell, SWT.ERROR, "Selection Needed",
                        "You must select a zone first");
            }
            return;
        }
        String entry = monitorAreaList.getItem(monitorAreaList
                .getSelectionIndex());
        monitorAreaList.remove(monitorAreaList.getSelectionIndex());
        associatedList.removeAll();
        if (mode == Mode.Zone) {
            additionalZones.add(entry);
            Collections.sort(additionalZones);
            additionalList.setItems(additionalZones
                    .toArray(new String[additionalZones.size()]));
            additionalList.setSelection(additionalZones.indexOf(entry));
            maZones.remove(entry);
            configMgr.removeArea(entry);
            if (configMgr.getAddedZones().contains(entry)) {
                configMgr.getAddedZones().remove(entry);
            }
        } else { // Station mode
            additionalStns.add(entry);
            Collections.sort(additionalStns);
            additionalList.setItems(additionalStns
                    .toArray(new String[additionalStns.size()]));
            additionalList.setSelection(additionalStns.indexOf(entry));
            maStations.remove(entry);
            configMgr.removeStation(entry.substring(0, entry.indexOf('#')));
        }
    }

    /**
     * Add an associated zone or station.
     */
    private void addAssociated() {
        if (monitorAreaList.getSelectionCount() == 0) {
            if (mode == Mode.Zone) {
                showMessage(shell, SWT.ERROR, "Select Needed",
                        "You must select a zone");
            } else {
                showMessage(shell, SWT.ERROR, "Select Needed",
                        "You must select a station");
            }
            return;
        }
        if (maRegionalList.getSelectionCount() == 0) {
            if (mode == Mode.Zone) {
                showMessage(shell, SWT.ERROR, "Select Needed",
                        "You must select a station");
            } else {
                showMessage(shell, SWT.ERROR, "Select Needed",
                        "You must select a zone");
            }
            return;
        }
        String entry = maRegionalList.getItem(maRegionalList
                .getSelectionIndex());
        String[] items = associatedList.getItems();
        ArrayList<String> itemList = new ArrayList<String>();
        for (String item : items) {
            itemList.add(item);
        }
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
            String zone = monitorAreaList.getItem(monitorAreaList
                    .getSelectionIndex());
            String stnId = entry.substring(0, entry.indexOf('#'));
            String stnType = entry.substring(entry.indexOf('#') + 1);
            configMgr.addStation(zone, stnId, stnType, configMgr
                    .getAddedStations().contains(stnId));
        } else { // Station mode
            if (regionalRdo.getSelection()) {
                // entry is a zone selected from additional zones
                maZones.add(entry);
                Collections.sort(maZones);
                additionalZones.remove(entry);
                maRegionalList.remove(maRegionalList.getSelectionIndex());
                configMgr.addArea(entry,
                        entry.charAt(2) == 'Z' ? ZoneType.MARITIME
                                : ZoneType.REGULAR);
            }
            String stn = monitorAreaList.getItem(monitorAreaList
                    .getSelectionIndex());
            String stnId = stn.substring(0, stn.indexOf('#'));
            String stnType = stn.substring(stn.indexOf('#') + 1);
            configMgr.addStation(entry, stnId, stnType, configMgr
                    .getAddedStations().contains(stnId));
        }
    }

    /**
     * Remove an associated zone or station.
     */
    private void removeAssociated() {
        if (associatedList.getItemCount() == 0) {
            if (mode == Mode.Zone) {
                showMessage(shell, SWT.ERROR, "Select Needed",
                        "You must select a station");
            } else {
                showMessage(shell, SWT.ERROR, "Select Needed",
                        "You must select a zone");
            }
            return;
        }
        String entry = associatedList.getItem(associatedList
                .getSelectionIndex());
        associatedList.remove(associatedList.getSelectionIndex());
        if (mode == Mode.Zone) {
            String zone = monitorAreaList.getItem(monitorAreaList
                    .getSelectionIndex());
            configMgr.removeStation(zone, entry);
            java.util.List<String> zones = configMgr.getAreaByStationId(entry
                    .substring(0, entry.indexOf('#')));
            if (zones.size() == 0) {
                // entry is no longer an MA station
                maStations.remove(entry);
                additionalStns.add(entry);
                Collections.sort(additionalStns);
                if (maRdo.getSelection()) {
                    maRegionalList.setItems(maStations
                            .toArray(new String[maStations.size()]));
                } else {
                    maRegionalList.setItems(additionalStns
                            .toArray(new String[additionalStns.size()]));
                }
            }
        } else { // Station mode
            String stn = monitorAreaList.getItem(monitorAreaList
                    .getSelectionIndex());
            configMgr.removeStation(entry, stn);
        }
    }

    /**
     * Handle the monitor area list selection.
     */
    private void handleMonitorAreaListSelection() {
        if (mode == Mode.Zone) {
            String zone = monitorAreaList.getItem(monitorAreaList
                    .getSelectionIndex());
            selectedStnZoneTF.setText(zone);

            java.util.List<String> stations = configMgr
                    .getAreaStationsWithType(zone);
            if (stations.size() > 1) {
                Collections.sort(stations);
            }
            associatedList.removeAll();
            if (stations.size() > 0) {
                associatedList.setItems(stations.toArray(new String[stations
                        .size()]));
            }
        } else { // Station mode
            String station = monitorAreaList.getItem(monitorAreaList
                    .getSelectionIndex());
            selectedStnZoneTF.setText(station);
            java.util.List<String> zones = configMgr.getAreaByStationId(station
                    .substring(0, station.indexOf('#')));
            if (zones.size() > 1) {
                Collections.sort(zones);
            }
            associatedList.removeAll();
            if (zones.size() > 0) {
                associatedList
                        .setItems(zones.toArray(new String[zones.size()]));
            }
        }
        populateMaRegionalList();
    }

    /**
     * Get the appropriate configuration manager.
     * 
     * @return The correct MonitorConfigurationManager
     */
    protected MonitorConfigurationManager getConfigManager() {
        MonitorConfigurationManager configManager = null;
        if (appName == AppName.FOG) {
            configManager = FogMonitorConfigurationManager.getInstance();
        } else if (appName == AppName.SNOW) {
            configManager = SnowMonitorConfigurationManager.getInstance();
        } else if (appName == AppName.SAFESEAS) {
            configManager = SSMonitorConfigurationManager.getInstance();
        }
        return configManager;
    }

    /**
     * Called when the cancel or "X" button is clicked.
     */
    private void closeWithoutSave() {
        int choice = showMessage(shell, SWT.YES | SWT.NO, appName
                + " Monitor Exit", "Are you sure you want to exit?");
        if (choice == SWT.YES) {
            MonitorConfigurationManager configManager = getConfigManager();
            configManager.setAddedZones(new ArrayList<String>());
            configManager.setAddedStations(new ArrayList<String>());
            setReturnValue(true);
            close();
        }
    }

    /**
     * Sets algorithm text.
     */
    protected abstract void setAlgorithmText();

    /**
     * Handles OK button.
     */
    protected abstract void handleOkBtnSelection();

    /**
     * Reads configuration file.
     */
    protected abstract void readConfigData();

    /**
     * Add a new zone to monitor area and refresh GUI
     * 
     * @param zone
     */
    public void addZoneToMA(String zone) {
        maZones.add(zone);
        Collections.sort(maZones);
        populateLeftLists();
    }

    /**
     * Add a new station to monitor area and refresh GUI
     * 
     * @param stnWithType
     *            (String of station ID with type)
     */
    public void addStationToMA(String stnWithType) {
        maStations.add(stnWithType);
        Collections.sort(maStations);
        populateLeftLists();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.INewZoneStnAction#addNewZoneAction
     * (java.lang.String, java.lang.String, java.lang.String)
     */
    @Override
    public void addNewZoneAction(String id, String lat, String log) {
        addZoneToMA(id);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.INewZoneStnAction#isExistingZone
     * (java.lang.String)
     */
    @Override
    public boolean isExistingZone(String zone) {
        if (maZones.contains(zone) || additionalZones.contains(zone)) {
            return true;
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.INewZoneStnAction#addNewStationAction
     * (java.lang.String)
     */
    @Override
    public void addNewStationAction(String stnWithType) {
        addStationToMA(stnWithType);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.INewZoneStnAction#isExistingStation
     * (java.lang.String)
     */
    @Override
    public boolean isExistingStation(String stnWithType) {
        if (maStations.contains(stnWithType)
                || additionalStns.contains(stnWithType)) {
            return true;
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        waitCursor.dispose();
        arrowCursor.dispose();
        controlFont.dispose();
    }
}
