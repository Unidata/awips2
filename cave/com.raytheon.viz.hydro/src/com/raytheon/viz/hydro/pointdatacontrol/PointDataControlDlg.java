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

package com.raytheon.viz.hydro.pointdatacontrol;

import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants.ElevationFilterOperation;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants.QueryMode;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants.RiverValueDisplayMode;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants.ValueFilterOperation;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants.ValueType;
import com.raytheon.viz.hydro.pointdatacontrol.data.PointControlPeTs;
import com.raytheon.viz.hydro.pointdatacontrol.data.PointDataPreset;
import com.raytheon.viz.hydro.pointdatacontrol.db.PDCDataManager;
import com.raytheon.viz.hydro.pointdatacontrol.util.PDCUtils;
import com.raytheon.viz.hydro.resource.MultiPointResource;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.colorscalemgr.HydroColorManager;
import com.raytheon.viz.hydrocommon.colorscalemgr.NamedColorSetGroup;
import com.raytheon.viz.hydrocommon.events.StationDisplayUpdateEvent;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Point Data Control dialog for Hydroview.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation/
 * 22 FEB 2010  4672       mpduff      TS filter widgets are never disabled.
 * 22 FEB 2010  4673       mpduff      Filter dialog's Apply button now updates the map.
 * 23 Nov 2010  #5521      lbousaidi   added keylistener for key release and listener 
 * 									   to validate values typed in elevation and values boxes
 * 25 JAN 2011  #7907      bkowal      the updateData indicator is now set to false
 *                                     every time data is successfully loaded.
 *                                     The actionListeners for certain controls
 *                                     have been updated so that they will set it
 *                                     to true when an update is actually required.
 *
 * 03 OCT 2012 #15395                  Added code to handle TimeStep when default is set 
 * 									   to be "30 minutes Instantaneous" in the database.
 * 09 OCT 2012 #15396				   Fixed Instantaneous precip index so legend and map display 
 * 									   will change each time duration is incremented or decremented
 * 									   for the "30 minutes Instantaneous" rainfall map .
 * 04 Dec 2012 15602     wkwock        Fix Hrs hour capped at 100.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class PointDataControlDlg extends CaveSWTDialog {
    private static PointDataControlDlg instance = null;

    private static final String RAW_VALUE = "Raw Value";

    private static final String RAW_VALUE_FLOOD_LEVEL = "Raw Value/Flood Level";

    private static final String RAW_VALUE_STAGE_FLOW = "Raw Value/Stage Flow";

    private static final String FLOOD_DEPART = "Flood Depart";

    private static final String FLOOD_DEPART_LEVEL = "Flood Depart/Level";

    private static final String THIRTY_MINUTES = "1/2";

    private static final String ONE_HOUR = "1";

    private static final String TWO_HOUR = "2";

    private static final String THREE_HOUR = "3";

    private static final String FOUR_HOUR = "4";

    private static final String SIX_HOUR = "6";

    private static final String TWELVE_HOUR = "12";

    private static final String EIGHTEEN_HOUR = "18";

    private static final String TWENTY_FOUR_HOUR = "24";

    private static final int TYPE_VALUE = 0;

    private static final int TYPE_DEPART = 1;

    private static final String[] TIMESTEP_DATA_ELEMENT_STRING_ARRAY = {
            // RIVER
            "STAGE/POOL", "FLOW/STORAGE", "DEPTH ABOVE FLOOD STAGE",
            "PERCENT OF FLOOD FLOW",

            // RAIN
            "INSTANTANEOUS", "1-HOUR PRECIP TOTAL", "3-HOUR PRECIP TOTAL",
            "6-HOUR PRECIP TOTAL", "24-HOUR TOTAL (12Z)",

            // SNOW
            "SNOW WATER EQUIVALENT", "SWE - 24 HOUR CHANGE",

            // TEMPERATURE
            "TEMPERATURE", "TEMP. 24 HOUR CHANGE", "MAX TEMP", "MIN TEMP",

            // HUMIDITY
            "DEWPOINT", "DEWPT - 24 HR CHANGE", "RELATIVE HUMIDITY",

            // WIND
            "WIND SPEED", "WIND DIRECTION" };

    /**
     * The Stack Composite.
     */
    private Composite stackComp;

    /**
     * The Stack Layout.
     */
    private final StackLayout stackLayout = new StackLayout();

    /**
     * The instantaneous precip group.
     */
    private Group precipGroup;

    /**
     * The value time group.
     */
    private Group valueTimeGroup;

    /**
     * Instantaneous precip accum text field.
     */
    private Text precipTimeTF;

    /**
     * Instantaneous precip index.
     */
    private int precipIndex = 0;

    /**
     * The down hour precip button.
     */
    private Button downPrecipBtn;

    /**
     * The up hour precip button.
     */
    private Button upPrecipBtn;

    /**
     * Selected preset combo box.
     */
    private Combo selPresetCbo;

    /**
     * Font used for controls.
     */
    private Font font;

    /**
     * Ad-Hoc radio button.
     */
    private Button adHocRdo;

    /**
     * Time step radio button.
     */
    private Button timeStepRdo;

    /**
     * Save button.
     */
    private Button saveBtn;

    /**
     * Delete button.
     */
    private Button deleteBtn;

    /**
     * Element type combo box.
     */
    private Combo elementTypeCbo;

    /**
     * Physical Elements combo box.
     */
    private Combo physicalElementCbo;

    /**
     * Time step data object.
     */
    private TimeStepData timeStepData;

    /**
     * Ad-Hoc data object.
     */
    private AdHocData adHocData;

    /**
     * Increase time button.
     */
    private Button upTimeBtn;

    /**
     * Decrease time button.
     */
    private Button dnTimeBtn;

    /**
     * Increase hours button.
     */
    private Button upHoursBtn;

    /**
     * Decrease hours button.Omaha AWIPS
     */
    private Button dnHoursBtn;

    /**
     * Time text control.
     */
    private Text timeTF;

    /**
     * Hours spinner control.
     */
    private Spinner hoursSpnr;

    /**
     * Hours label.
     */
    private Label hoursLbl;

    /**
     * "Value Is" combo box.
     */
    private Combo valueIsCbo;

    /**
     * "Value Is" label.
     */
    private Label valueLbl;

    /**
     * Type source check box.
     */
    private Button typeSourceChk;

    /**
     * Type source button.
     */
    private Button typeSourceBtn;

    /**
     * Stations composite container.
     */
    private Composite stationsComp;

    /**
     * Stations label.
     */
    private Label stationsLbl;

    /**
     * Stations label.
     */
    private Combo stationsCbo;

    /**
     * Service area check box.
     */
    private Button serviceAreaChk;

    /**
     * Service area button.
     */
    private Button serviceAreaBtn;

    /**
     * Data source check box.
     */
    private Button dataSourceChk;

    /**
     * Data source button.
     */
    private Button dataSourceBtn;

    /**
     * Show non-forecast check box.
     */
    private Button showNonFcstPtsChk;

    /**
     * Show missing check box.
     */
    private Button showMissingChk;

    /**
     * Filter by value combo box.
     */
    private Combo filterByValueCbo;

    /**
     * Filter by elevation combo box.
     */
    private Combo filterByElevationCbo;

    /**
     * Filter by value text control.
     */
    private Text filterByValueTF;

    /**
     * Filter by elevation text control.static
     */
    private Text filterByElevationTF;

    /**
     * Value check box.
     */
    private Button valueChk;

    /**
     * ID check box.
     */
    private Button idChk;

    /**
     * Name check box.
     */
    private Button nameChk;

    /**
     * Icon check box.
     */
    private Button iconChk;

    /**
     * None radio button.
     */
    private Button noneRdo;

    /**
     * Time radio button.
     */
    private Button timeRdo;

    /**
     * Elevation radio button.
     */
    private Button elevRdo;

    /**
     * Parameter code radio button.
     */
    private Button paramCodeRdo;

    /**
     * Color river icon check box.
     */
    private Button colorRiverIconChk;

    /**
     * The map button.
     */
    private Button mapBtn;

    /**
     * "River color/value based on" combo box
     */
    private Combo riverColorValCbo;

    /**
     * Display value combo box.
     */
    private Combo displayValAsCbo;

    /**
     * "Display Values As" label.
     */
    private Label displayAsLbl;

    /**
     * Format for the date/time control.
     */
    private SimpleDateFormat dateTimeFmt = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm");

    /**
     * Array list of Preset data.
     */
    ArrayList<PointDataPreset> presets;

    /**
     * Calendar.
     */
    private Calendar cal;

    /**
     * Flag indicating if data needs to be update from IHFS.
     */
    private boolean updateData = true;

    /**
     * Flag to determine if drawing is needed.
     */
    private boolean disableDrawing = false;

    /**
     * Holds the previous date for comparison.
     */
    private Date previousDate = SimulatedTime.getSystemTime().getTime();

    /**
     * Holds the previous hours for comparison.
     */
    private int previousHours;

    /**
     * The Point Data Control Data Manager.
     */
    private PDCDataManager dataManager = PDCDataManager.getInstance();

    /**
     * The previous query mode.
     */
    private PDCConstants.QueryMode previousQueryMode;

    /**
     * Instance of the HydroDisplaymanager.
     */
    private HydroDisplayManager manager = HydroDisplayManager.getInstance();

    /**
     * The wait mouse pointer.
     */
    private Cursor waitCursor = null;

    /**
     * The normal arrow mouse pointer.
     */
    private Cursor arrowCursor = null;

    private DecimalFormat entryFormat = new DecimalFormat();

    public static synchronized PointDataControlDlg getInstance(Shell shell) {
        if (instance == null) {
            instance = new PointDataControlDlg(shell);
        }

        return instance;
    }

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    private PointDataControlDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Point Data Control");

        waitCursor = parent.getDisplay().getSystemCursor(SWT.CURSOR_WAIT);
        arrowCursor = parent.getDisplay().getSystemCursor(SWT.CURSOR_ARROW);
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        pdcManager.setPointDataControlDialogInstance(this);
        dateTimeFmt.setTimeZone(TimeZone.getTimeZone("GMT"));

        // Set up the colors
        HydroColorManager colorManager = HydroColorManager.getInstance();
        NamedColorSetGroup ncsg = colorManager.getDefaultColorSetGroup();
        colorManager.populateDefaultColorUseSets(ncsg.getColorGroupArray());
        // colorManager.readColorValuesFromDatabase();
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        font.dispose();
    }

    @Override
    protected void initializeComponents(final Shell shell) {
        font = new Font(shell.getDisplay(), "Courier", 10, SWT.NORMAL);

        prepareArrayData();

        // Initialize all of the controls and layouts
        initializeComponents();

        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent event) {
                // Block the disposal of this dialog by way of the "X" button.
                shell.setVisible(false);
                event.doit = false;
            }
        });
    }

    @Override
    protected void opened() {
        shell.setCursor(waitCursor);
        drawMap();
        shell.setCursor(arrowCursor);
    }

    /**
     * Create the Time Step and Ad Hoc data.
     */
    private void prepareArrayData() {
        timeStepData = new TimeStepData();
        adHocData = new AdHocData();
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
        createPresetsQueryModeGroup();
        createElementsGroup();
        createValueTimeGroup();
        createFilteringGroup();
        createDisplayGroup();
        createBottomButtons();

        showAdHocControls(true);

        Date d = SimulatedTime.getSystemTime().getTime();
        cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTime(d);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        timeTF.setText(dateTimeFmt.format(cal.getTime()));

        populatePresetData(null);
        
        /* this is when in the database, the timeStep is set to be the 
           default one */
         
        if (timeStepRdo.getSelection() == true) {
            handleQueryModeSelection(PDCConstants.QueryMode.TIME_STEP_MODE);
            previousQueryMode = PDCConstants.QueryMode.TIME_STEP_MODE;
            shell.setCursor(waitCursor);
            updateData = true;
            drawMap();
            shell.setCursor(arrowCursor);

        }

    }

    /**
     * Create the preset query mode group and controls.
     */
    private void createPresetsQueryModeGroup() {
        Group presetQueryGroup = new Group(shell, SWT.NONE);
        presetQueryGroup.setText(" Presets/Query Mode ");
        GridLayout groupLayout = new GridLayout(1, false);
        presetQueryGroup.setLayout(groupLayout);

        // --------------------------------------------
        // Create the left side of the Presets/Query
        // Mode group
        // --------------------------------------------

        // Create a container to hold the label and the combo box.
        Composite presetComp = new Composite(presetQueryGroup, SWT.NONE);
        GridLayout presetCompLayout = new GridLayout(2, false);
        presetCompLayout.horizontalSpacing = 10;
        // presetCompLayout.spacing = 5;
        presetComp.setLayout(presetCompLayout);

        Label selPresetLbl = new Label(presetComp, SWT.CENTER);
        selPresetLbl.setText("Selected\nPreset:");

        GridData rd = new GridData(275, SWT.DEFAULT);
        selPresetCbo = new Combo(presetComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        selPresetCbo.setTextLimit(30);
        selPresetCbo.setLayoutData(rd);
        selPresetCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handlePresetSelection();
                shell.setCursor(waitCursor);
                updateData = true;
                drawMap();
                shell.setCursor(arrowCursor);
            }
        });

        if (selPresetCbo.getItemCount() > 0) {
            selPresetCbo.select(0);
        }

        Composite queryModeComp = new Composite(presetQueryGroup, SWT.NONE);
        GridLayout queryModeCompLayout = new GridLayout(3, false);
        // queryModeCompLayout.spacing = 5;
        queryModeComp.setLayout(queryModeCompLayout);

        // Create a Group for the Ad-Hoc and the Time-Step buttons
        Group adHocTimeStepGroup = new Group(queryModeComp, SWT.NONE);
        adHocTimeStepGroup.setText(" Query Mode ");
        RowLayout adHocTimeStepLayout = new RowLayout();
        adHocTimeStepLayout.spacing = 5;
        adHocTimeStepGroup.setLayout(adHocTimeStepLayout);

        adHocRdo = new Button(adHocTimeStepGroup, SWT.RADIO);
        adHocRdo.setText("Ad Hoc");
        adHocRdo.setSelection(true);
        adHocRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (adHocRdo.getSelection() == true) {
                    handleQueryModeSelection(PDCConstants.QueryMode.AD_HOC_MODE);
                    previousQueryMode = PDCConstants.QueryMode.AD_HOC_MODE;
                    shell.setCursor(waitCursor);
                    updateData = true;
                    drawMap();
                    shell.setCursor(arrowCursor);
                }
            }
        });

        timeStepRdo = new Button(adHocTimeStepGroup, SWT.RADIO);
        timeStepRdo.setText("Time-Step");
        timeStepRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (timeStepRdo.getSelection() == true) {
                    handleQueryModeSelection(PDCConstants.QueryMode.TIME_STEP_MODE);
                    previousQueryMode = PDCConstants.QueryMode.TIME_STEP_MODE;
                    shell.setCursor(waitCursor);
                    updateData = true;
                    drawMap();
                    shell.setCursor(arrowCursor);
                }
            }
        });

        GridData gd = new GridData(75, 27);
        saveBtn = new Button(queryModeComp, SWT.PUSH);
        saveBtn.setText("Save...");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                openSaveDialog();
            }
        });

        gd = new GridData(75, 27);
        deleteBtn = new Button(queryModeComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                boolean delete = MessageDialog.openConfirm(shell,
                        "Confirm Preset Delete",
                        "Are you sure you want to delete the selected preset?");
                if (delete) {
                    delete();
                    populatePresetData(null);
                }
            }
        });
    }

    /**
     * Create the elements group and controls.
     */
    private void createElementsGroup() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        Group elementsGroup = new Group(shell, SWT.NONE);
        elementsGroup.setText(" Elements ");
        GridLayout groupLayout = new GridLayout(2, false);
        elementsGroup.setLayout(groupLayout);
        elementsGroup.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(135, SWT.DEFAULT);
        elementTypeCbo = new Combo(elementsGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
        elementTypeCbo.setLayoutData(gd);
        elementTypeCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (timeStepRdo.getSelection() == false) {
                    handleAdhocElementComboSelection();
                } else {
                    handleTimeStepElementComboSelection();
                    handleTsPhysicalElementSelection(physicalElementCbo
                            .getItem(physicalElementCbo.getSelectionIndex()));
                    if ((physicalElementCbo.getSelectionIndex() == 0)
                            && (elementTypeCbo.getSelectionIndex() == 1)) {
                        stackLayout.topControl = precipGroup;
                        stackComp.layout();
                    } else {
                        stackLayout.topControl = valueTimeGroup;
                        stackComp.layout();
                    }
                }
                HydroDisplayManager.getInstance().setDataChanged(true);
                shell.setCursor(waitCursor);
                updateData = true;
                drawMap();
                shell.setCursor(arrowCursor);
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        physicalElementCbo = new Combo(elementsGroup, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        physicalElementCbo.setLayoutData(gd);
        physicalElementCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (adHocRdo.getSelection()) {
                    handlePeSelection();
                    // pcOptions.setAhDataElement(ahDataElement);
                } else {
                    handlePeSelection();
                    if ((physicalElementCbo.getSelectionIndex() == 0)
                            && (elementTypeCbo.getSelectionIndex() == 1)) {
                        stackLayout.topControl = precipGroup;
                        stackComp.layout();
                    } else {
                        stackLayout.topControl = valueTimeGroup;
                        stackComp.layout();
                    }
                    handleTsPhysicalElementSelection(physicalElementCbo
                            .getItem(physicalElementCbo.getSelectionIndex()));
                    loadTimeInfo();
                }
                HydroDisplayManager.getInstance().setDataChanged(true);
                shell.setCursor(waitCursor);
                updateData = true;
                drawMap();
                shell.setCursor(arrowCursor);
            }
        });

        if (pcOptions.getQueryMode() == 0) {
            populateCombosWithAdHoc();
        } else {
            populateCombosWithTimeStep();
        }
    }

    /**
     * Create the Value/Time group and controls.
     */
    private void createValueTimeGroup() {
        stackComp = new Composite(shell, SWT.NONE);
        stackComp.setLayout(stackLayout);
        stackComp
                .setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        valueTimeGroup = new Group(stackComp, SWT.NONE);
        stackLayout.topControl = valueTimeGroup;
        valueTimeGroup.setText(" Value/Time ");
        GridLayout gl = new GridLayout(1, false);
        valueTimeGroup.setLayout(gl);

        precipGroup = new Group(stackComp, SWT.NONE);
        precipGroup.setText(" Value/Time ");
        GridLayout gl2 = new GridLayout(1, false);
        precipGroup.setLayout(gl2);

        // Set the group layout data so it will take up the width of the
        // shell.
        GridData precipGridData = new GridData(SWT.FILL, SWT.DEFAULT, true,
                false);
        precipGroup.setLayoutData(precipGridData);

        // Set the group layout data so it will take up the width of the
        // shell.
        GridData mainGridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        valueTimeGroup.setLayoutData(mainGridData);

        Composite precipComp = new Composite(precipGroup, SWT.NONE);
        GridLayout pgl = new GridLayout(3, false);
        precipComp.setLayout(pgl);

        GridData gd = new GridData(35, SWT.DEFAULT);
        precipTimeTF = new Text(precipComp, SWT.BORDER);
        precipTimeTF.setFont(font);
        precipTimeTF.setLayoutData(gd);
        precipTimeTF.setText("1/2");
        precipTimeTF.setEditable(false);

        HydroConstants.InstPrecipSelection.PRECIP_TIME_30_MINUTES
                .getInstPrecipSelection();

        Composite precipBtnComp = new Composite(precipComp, SWT.NONE);
        RowLayout precipArrowRl = new RowLayout(SWT.VERTICAL);
        precipBtnComp.setLayout(precipArrowRl);

        RowData rd = new RowData(25, 25);
        upPrecipBtn = new Button(precipBtnComp, SWT.ARROW);
        upPrecipBtn.setLayoutData(rd);
        upPrecipBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
            	PDCOptionData pcOptions = PDCOptionData.getInstance();
                if (precipIndex >= HydroConstants.InstPrecipSelection.values().length - 1) {
                    precipIndex = 0;
                } else {
                    precipIndex++;
                    if  (precipIndex == HydroConstants.InstPrecipSelection.
                            values().length - 1) {
                    	precipIndex=0;
                    }

                }
            	pcOptions.setInstPrecipAccumTimeSelection(precipIndex);
                setInstPrecipAccumText();
                shell.setCursor(waitCursor);
                updateData = true;
                drawMap();
                shell.setCursor(arrowCursor);
            }
        });

        downPrecipBtn = new Button(precipBtnComp, SWT.ARROW | SWT.DOWN);
        downPrecipBtn.setLayoutData(rd);
        downPrecipBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
            	PDCOptionData pcOptions = PDCOptionData.getInstance();
                if (precipIndex == 0) {
                    precipIndex = HydroConstants.InstPrecipSelection.
                    								values().length - 1;
                    if  (precipIndex == HydroConstants.InstPrecipSelection.
                    										values().length - 1) {
                    	precipIndex=HydroConstants.InstPrecipSelection.
                            							values().length - 2;
                    } 

                } else {
                    precipIndex--;
                }
                
                pcOptions.setInstPrecipAccumTimeSelection(precipIndex);
                setInstPrecipAccumText();
                shell.setCursor(waitCursor);
                updateData = true;
                drawMap();
                shell.setCursor(arrowCursor);
            }
        });

        Label hrsLbl = new Label(precipComp, SWT.NONE);
        hrsLbl.setText("Hrs");
        // -------------------------------------
        // Create the time controls composite
        // -------------------------------------
        Composite timeComp = new Composite(valueTimeGroup, SWT.NONE);
        GridLayout timeGl = new GridLayout(5, false);
        timeComp.setLayout(timeGl);

        // Add the time arrow buttons
        Composite timeArrowsComp = new Composite(timeComp, SWT.NONE);
        RowLayout timeArrowRl = new RowLayout(SWT.VERTICAL);
        timeArrowsComp.setLayout(timeArrowRl);

        rd = new RowData(25, 25);
        upTimeBtn = new Button(timeArrowsComp, SWT.ARROW);
        upTimeBtn.setLayoutData(rd);
        upTimeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                cal.add(Calendar.DAY_OF_MONTH, 1);
                timeTF.setText(dateTimeFmt.format(cal.getTime()));
                shell.setCursor(waitCursor);
                updateData = true;
                drawMap();
                shell.setCursor(arrowCursor);
            }
        });

        rd = new RowData(25, 25);
        dnTimeBtn = new Button(timeArrowsComp, SWT.ARROW | SWT.DOWN);
        dnTimeBtn.setLayoutData(rd);
        dnTimeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                cal.add(Calendar.DAY_OF_MONTH, -1);
                timeTF.setText(dateTimeFmt.format(cal.getTime()));
                shell.setCursor(waitCursor);
                updateData = true;
                drawMap();
                shell.setCursor(arrowCursor);
            }
        });

        // Add the time text field
        gd = new GridData(160, SWT.DEFAULT);
        timeTF = new Text(timeComp, SWT.BORDER);
        timeTF.setFont(font);
        timeTF.setLayoutData(gd);
        timeTF.setEditable(false);

        // Add the hours arrows button
        Composite hoursArrowsComp = new Composite(timeComp, SWT.NONE);
        RowLayout hoursArrowRl = new RowLayout(SWT.VERTICAL);
        hoursArrowsComp.setLayout(hoursArrowRl);

        rd = new RowData(25, 25);
        upHoursBtn = new Button(hoursArrowsComp, SWT.ARROW);
        upHoursBtn.setLayoutData(rd);
        upHoursBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int adjustmentHour = getAdjustmentHours();
                cal.setTimeInMillis(PDCOptionData.getInstance().getValidTime()
                        .getTime());
                cal.add(Calendar.HOUR_OF_DAY, adjustmentHour);
                timeTF.setText(dateTimeFmt.format(cal.getTime()));
                shell.setCursor(waitCursor);
                updateData = true;
                drawMap();
                shell.setCursor(arrowCursor);
            }
        });

        rd = new RowData(25, 25);
        dnHoursBtn = new Button(hoursArrowsComp, SWT.ARROW | SWT.DOWN);
        dnHoursBtn.setLayoutData(rd);
        dnHoursBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int adjustmentHour = getAdjustmentHours();
                cal.setTimeInMillis(PDCOptionData.getInstance().getValidTime()
                        .getTime());
                cal.add(Calendar.HOUR_OF_DAY, adjustmentHour * -1);
                timeTF.setText(dateTimeFmt.format(cal.getTime()));
                shell.setCursor(waitCursor);
                updateData = true;
                drawMap();
                shell.setCursor(arrowCursor);
            }
        });

        // ------------------------------------
        // Add the hours text field & label
        // ------------------------------------
        gd = new GridData(33, SWT.DEFAULT);
        hoursSpnr = new Spinner(timeComp, SWT.BORDER);
        hoursSpnr.setFont(font);
        hoursSpnr.setDigits(0);
        hoursSpnr.setIncrement(1);
        hoursSpnr.setPageIncrement(5);
        hoursSpnr.setSelection(24);
        hoursSpnr.setLayoutData(gd);
        hoursSpnr.setMaximum(1000);
        // hoursSpnr.addSelectionListener(new SelectionAdapter() {
        // @Override
        // public void widgetSelected(SelectionEvent event) {
        // PDCOptionData pcOptions = PDCOptionData.getInstance();
        // pcOptions.setDurHours(hoursSpnr.getSelection());
        // shell.setCursor(waitCursor);
        // drawMap();
        // shell.setCursor(arrowCursor);
        //
        // }
        // });

        hoursLbl = new Label(timeComp, SWT.NONE);
        hoursLbl.setText("Hrs");

        // -------------------------------------
        // Create the values composite
        // -------------------------------------
        Composite valuesComp = new Composite(valueTimeGroup, SWT.NONE);
        GridLayout valuesGl = new GridLayout(2, false);
        valuesComp.setLayout(valuesGl);

        valueLbl = new Label(valuesComp, SWT.NONE);
        valueLbl.setText("Value Is");

        valueIsCbo = new Combo(valuesComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        valueIsCbo.add("Latest Value");
        valueIsCbo.add("Value for Selected Time");
        valueIsCbo.add("Min Value in Window");
        valueIsCbo.add("Max Value in Window");
        valueIsCbo.add("Value Change in Window");
        valueIsCbo.select(0);
        valueIsCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                PDCOptionData pcOptions = PDCOptionData.getInstance();
                pcOptions.setTimeMode(valueIsCbo.getSelectionIndex());
                shell.setCursor(waitCursor);
                updateData = true;
                drawMap();
                shell.setCursor(arrowCursor);

            }
        });
        stackComp.layout();
    }

    /**
     * Create the filtering group and controls.
     */
    private void createFilteringGroup() {
        Group filteringGroup = new Group(shell, SWT.NONE);
        filteringGroup.setText(" Filtering ");
        GridLayout gl = new GridLayout(4, false);
        filteringGroup.setLayout(gl);

        // Set the group layout data so it will take up the width of the
        // shell.
        GridData mainGridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        filteringGroup.setLayoutData(mainGridData);

        // ---------------------------------------
        // Top section of the filter group
        // ---------------------------------------

        GridData gd = new GridData(20, SWT.DEFAULT);
        typeSourceChk = new Button(filteringGroup, SWT.CHECK);
        typeSourceChk.setLayoutData(gd);
        typeSourceChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                PDCOptionData pcOptions = PDCOptionData.getInstance();

                if (typeSourceChk.getSelection()) {
                    pcOptions.setFilterByTypeSource(1);
                } else {
                    pcOptions.setFilterByTypeSource(0);
                }
                shell.setCursor(waitCursor);
                drawMap();
                shell.setCursor(arrowCursor);
            }
        });

        gd = new GridData(120, SWT.DEFAULT);
        typeSourceBtn = new Button(filteringGroup, SWT.PUSH);
        typeSourceBtn.setText("Type/Source...");
        typeSourceBtn.setLayoutData(gd);
        typeSourceBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                FilteringDlg typeSourceDlg = new FilteringDlg(shell,
                        "Type Source Selection Dialog",
                        FilteringDlg.DialogType.TYPE_SOURCE,
                        physicalElementCbo.getItem(physicalElementCbo
                                .getSelectionIndex()));
                boolean redraw = (Boolean) typeSourceDlg.open();
                if (redraw) {
                    shell.setCursor(waitCursor);
                    drawMap();
                    shell.setCursor(arrowCursor);
                }
            }
        });

        // Create the station composite container
        gd = new GridData();
        gd.horizontalSpan = 2;
        stationsComp = new Composite(filteringGroup, SWT.NONE);
        GridLayout stationLayout = new GridLayout(2, false);
        stationsComp.setLayout(stationLayout);
        stationsComp.setLayoutData(gd);

        gd = new GridData(70, SWT.DEFAULT);
        stationsLbl = new Label(stationsComp, SWT.NONE | SWT.RIGHT);
        stationsLbl.setText("Stations: ");
        stationsLbl.setLayoutData(gd);

        gd = new GridData(110, SWT.DEFAULT);
        stationsCbo = new Combo(stationsComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        stationsCbo.add("All");
        stationsCbo.add("Stream");
        stationsCbo.add("Reservoir");
        stationsCbo.select(0);
        stationsCbo.setLayoutData(gd);
        stationsCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                PDCOptionData pcOptions = PDCOptionData.getInstance();
                pcOptions.setRiverStationFilter(stationsCbo.getSelectionIndex());
                shell.setCursor(waitCursor);
                drawMap();
                shell.setCursor(arrowCursor);
            }
        });

        // Create the service area check box
        gd = new GridData(20, SWT.DEFAULT);
        serviceAreaChk = new Button(filteringGroup, SWT.CHECK);
        serviceAreaChk.setLayoutData(gd);
        serviceAreaChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                PDCOptionData pcOptions = PDCOptionData.getInstance();
                if (serviceAreaChk.getSelection()) {
                    pcOptions.setFilterByHSA(1);
                } else {
                    pcOptions.setFilterByHSA(0);
                }
                shell.setCursor(waitCursor);
                drawMap();
                shell.setCursor(arrowCursor);
            }
        });

        gd = new GridData(120, SWT.DEFAULT);
        serviceAreaBtn = new Button(filteringGroup, SWT.PUSH);
        serviceAreaBtn.setText("Service Area...");
        serviceAreaBtn.setLayoutData(gd);
        serviceAreaBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                FilteringDlg serviceDlg = new FilteringDlg(shell,
                        "Service Area", FilteringDlg.DialogType.SERVICE_AREA);
                boolean redraw = (Boolean) serviceDlg.open();
                if (redraw) {
                    shell.setCursor(waitCursor);
                    drawMap();
                    shell.setCursor(arrowCursor);
                }
            }
        });

        gd = new GridData(20, SWT.DEFAULT);
        gd.horizontalAlignment = SWT.END;
        dataSourceChk = new Button(filteringGroup, SWT.CHECK);
        dataSourceChk.setLayoutData(gd);
        dataSourceChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                PDCOptionData pcOptions = PDCOptionData.getInstance();
                if (dataSourceChk.getSelection()) {
                    pcOptions.setFilterByDataSource(1);
                } else {
                    pcOptions.setFilterByDataSource(0);
                }
                shell.setCursor(waitCursor);
                drawMap();
                shell.setCursor(arrowCursor);
            }
        });

        gd = new GridData(120, SWT.DEFAULT);
        dataSourceBtn = new Button(filteringGroup, SWT.PUSH);
        dataSourceBtn.setText("Data Source...");
        dataSourceBtn.setLayoutData(gd);
        dataSourceBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                FilteringDlg dataSourceDlg = new FilteringDlg(shell,
                        "Data Source", FilteringDlg.DialogType.DATA_SOURCE);
                boolean redraw = (Boolean) dataSourceDlg.open();
                if (redraw) {
                    shell.setCursor(waitCursor);
                    drawMap();
                    shell.setCursor(arrowCursor);
                }
            }
        });

        // ---------------------
        // Add a line separator
        // ---------------------
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 4;
        Label sepLbl = new Label(filteringGroup, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        // ---------------------------------------
        // Bottom part of the filter group
        // ---------------------------------------

        gd = new GridData();
        gd.horizontalSpan = 2;
        showNonFcstPtsChk = new Button(filteringGroup, SWT.CHECK);
        showNonFcstPtsChk.setText("Show NonFcstPts");
        showNonFcstPtsChk.setLayoutData(gd);
        showNonFcstPtsChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                PDCOptionData pcOptions = PDCOptionData.getInstance();
                if (showNonFcstPtsChk.getSelection()) {
                    pcOptions.setFcstptsOnly(0);
                } else {
                    pcOptions.setFcstptsOnly(1);
                }
                shell.setCursor(waitCursor);
                drawMap();
                shell.setCursor(arrowCursor);
            }
        });

        gd = new GridData();
        gd.horizontalSpan = 2;
        showMissingChk = new Button(filteringGroup, SWT.CHECK);
        showMissingChk.setText("Show Missing");
        showMissingChk.setLayoutData(gd);
        showMissingChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                PDCOptionData pcOptions = PDCOptionData.getInstance();
                if (showMissingChk.getSelection()) {
                    pcOptions.setSupressMissing(1);
                } else {
                    pcOptions.setSupressMissing(0);
                }
                updateData = true;
                shell.setCursor(waitCursor);
                drawMap();
                shell.setCursor(arrowCursor);

            }
        });

        // -----------------------------------------------
        // Create a new composite with a 3 column layout
        // for the show components.
        // -----------------------------------------------

        gd = new GridData();
        gd.horizontalSpan = 4;
        Composite showComp = new Composite(filteringGroup, SWT.NONE);
        GridLayout showGl = new GridLayout(3, false);
        showComp.setLayout(showGl);
        showComp.setLayoutData(gd);

        Label show1Lbl = new Label(showComp, SWT.NONE);
        show1Lbl.setText("Show Pts With ");

        gd = new GridData(130, SWT.DEFAULT);
        filterByValueCbo = new Combo(showComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        filterByValueCbo.add("Any Value");
        filterByValueCbo.add("Value =");
        filterByValueCbo.add("Value Not =");
        filterByValueCbo.add("Value >=");
        filterByValueCbo.add("Value <=");
        filterByValueCbo.add("Value >");
        filterByValueCbo.add("Value <");
        filterByValueCbo.select(0);
        filterByValueCbo.setLayoutData(gd);
        filterByValueCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleFilterByValueSelection();
            }
        });

        filterByValueTF = new Text(showComp, SWT.BORDER);
        gd = new GridData(60, SWT.DEFAULT);
        filterByValueTF.setLayoutData(gd);
        filterByValueTF.addFocusListener(new FocusListener() {
            @Override
            public void focusLost(FocusEvent event) {
                PDCOptionData pcOptions = PDCOptionData.getInstance();
                if ((filterByValueTF.getText() != null)
                        && !filterByValueTF.getText().isEmpty()) {

                    try {
                        pcOptions.setValueFilterValue(Double
                                .parseDouble(filterByValueTF.getText()));
                    } catch (NumberFormatException e) {
                        e.printStackTrace();
                        MessageDialog.openError(shell, "Invalid Value",
                                "Invalid value entered for Value Filter.");
                    }
                }
            }

            public void focusGained(FocusEvent event) {
            }
        });

        filterByValueTF.addListener(SWT.Verify, new Listener() {

            @Override
            public void handleEvent(Event event) {
                validateShowPtsEntries(event, filterByValueTF);

            }
        });

        addListenerToTextValue(filterByValueTF);

        Label show2Lbl = new Label(showComp, SWT.NONE);
        show2Lbl.setText("Show Pts With ");

        gd = new GridData(130, SWT.DEFAULT);
        filterByElevationCbo = new Combo(showComp, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        filterByElevationCbo.add("Any Elev");
        filterByElevationCbo.add("Elev =");
        filterByElevationCbo.add("Elev Not =");
        filterByElevationCbo.add("Elev >=");
        filterByElevationCbo.add("Elev <=");
        filterByElevationCbo.add("Elev >");
        filterByElevationCbo.add("Elev <");
        filterByElevationCbo.select(0);
        filterByElevationCbo.setLayoutData(gd);
        filterByElevationCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleFilterByElevationSelection();
            }
        });

        filterByElevationTF = new Text(showComp, SWT.BORDER);
        gd = new GridData(60, SWT.DEFAULT);
        filterByElevationTF.setLayoutData(gd);
        filterByElevationTF.addFocusListener(new FocusListener() {
            @Override
            public void focusLost(FocusEvent event) {
                PDCOptionData pcOptions = PDCOptionData.getInstance();
                if ((filterByElevationTF.getText() != null)
                        && !filterByElevationTF.getText().isEmpty()) {
                    try {
                        pcOptions.setElevFilterValue(Double
                                .parseDouble(filterByElevationTF.getText()));
                    } catch (NumberFormatException e) {
                        e.printStackTrace();
                        MessageDialog.openError(shell, "Invalid Value",
                                "Invalid value entered for Elevation Filter.");
                    }
                }
            }

            public void focusGained(FocusEvent event) {
            }
        });

        filterByElevationTF.addListener(SWT.Verify, new Listener() {

            @Override
            public void handleEvent(Event event) {
                validateShowPtsEntries(event, filterByElevationTF);
            }
        });

        addListenerToTextValue(filterByElevationTF);

    }

    /**
     * Create the display group and controls.
     */
    private void createDisplayGroup() {
        Group displayGroup = new Group(shell, SWT.NONE);
        displayGroup.setText(" Display ");
        GridLayout gl = new GridLayout(1, false);
        displayGroup.setLayout(gl);

        // Set the group layout data so it will take up the width of the
        // shell.
        GridData mainGridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        displayGroup.setLayoutData(mainGridData);

        // ------------------------------------------------------
        // Setup the composite of display check boxes
        // ------------------------------------------------------
        Composite chkBxComp = new Composite(displayGroup, SWT.NONE);
        RowLayout chkBxCompRl = new RowLayout();
        chkBxCompRl.spacing = 15;
        chkBxComp.setLayout(chkBxCompRl);

        valueChk = new Button(chkBxComp, SWT.CHECK);
        valueChk.setText("Value");
        valueChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                PDCOptionData pcOptions = PDCOptionData.getInstance();
                if (valueChk.getSelection()) {
                    pcOptions.setValue(1);
                } else {
                    pcOptions.setValue(0);
                }
                fireUpdateEvent();
            }
        });

        idChk = new Button(chkBxComp, SWT.CHECK);
        idChk.setText("Id");
        idChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                PDCOptionData pcOptions = PDCOptionData.getInstance();
                if (idChk.getSelection()) {
                    pcOptions.setId(1);
                } else {
                    pcOptions.setId(0);
                }
                fireUpdateEvent();
            }
        });

        nameChk = new Button(chkBxComp, SWT.CHECK);
        nameChk.setText("Name");
        nameChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                PDCOptionData pcOptions = PDCOptionData.getInstance();
                if (nameChk.getSelection()) {
                    pcOptions.setName(1);
                } else {
                    pcOptions.setName(0);
                }
                fireUpdateEvent();
            }
        });

        iconChk = new Button(chkBxComp, SWT.CHECK);
        iconChk.setText("Icon");
        iconChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                PDCOptionData pcOptions = PDCOptionData.getInstance();
                if (iconChk.getSelection()) {
                    pcOptions.setIcon(1);
                } else {
                    pcOptions.setIcon(0);
                }
                fireUpdateEvent();
            }
        });

        // ------------------------------------------------------
        // Setup the composite of display radio buttons
        // ------------------------------------------------------
        Composite rdoBtnComp = new Composite(displayGroup, SWT.NONE);
        RowLayout rdoBtnCompRl = new RowLayout();
        chkBxCompRl.spacing = 15;
        rdoBtnComp.setLayout(rdoBtnCompRl);

        RowData rd = new RowData(65, SWT.DEFAULT);
        noneRdo = new Button(rdoBtnComp, SWT.RADIO);
        noneRdo.setText("None");
        noneRdo.setSelection(true);
        noneRdo.setLayoutData(rd);
        noneRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleDisplayRadioSelection();
                fireUpdateEvent();
            }
        });

        rd = new RowData(65, SWT.DEFAULT);
        timeRdo = new Button(rdoBtnComp, SWT.RADIO);
        timeRdo.setText("Time");
        timeRdo.setLayoutData(rd);
        timeRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleDisplayRadioSelection();
                fireUpdateEvent();
            }
        });

        rd = new RowData(100, SWT.DEFAULT);
        elevRdo = new Button(rdoBtnComp, SWT.RADIO);
        elevRdo.setText("Elevation");
        elevRdo.setLayoutData(rd);
        elevRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleDisplayRadioSelection();
                fireUpdateEvent();
            }
        });

        paramCodeRdo = new Button(rdoBtnComp, SWT.RADIO);
        paramCodeRdo.setText("Param Code");
        paramCodeRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleDisplayRadioSelection();
                fireUpdateEvent();
            }
        });

        // ---------------------
        // Add a line separator
        // ---------------------
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(displayGroup, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        // ---------------------------------------------------
        // Setup the composite for color river icon check box
        // ---------------------------------------------------
        Composite colorRiverComp = new Composite(displayGroup, SWT.NONE);
        RowLayout colorRiverCompRl = new RowLayout();
        colorRiverComp.setLayout(colorRiverCompRl);

        colorRiverIconChk = new Button(colorRiverComp, SWT.CHECK);
        colorRiverIconChk.setText("Color River Icons");
        colorRiverIconChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                PDCOptionData pcOptions = PDCOptionData.getInstance();
                if (colorRiverIconChk.getSelection()) {
                    pcOptions.setRiverStatus(1);
                } else {
                    pcOptions.setRiverStatus(0);
                }
                fireUpdateEvent();
            }
        });

        // ---------------------------------------------------
        // Setup the composite for river color based on combo
        // ---------------------------------------------------
        Composite colorValComp = new Composite(displayGroup, SWT.NONE);
        GridLayout colorValCompGl = new GridLayout(2, false);
        colorValComp.setLayout(colorValCompGl);

        Label colorValLbl = new Label(colorValComp, SWT.NONE);
        colorValLbl.setText("River Color/Value Based On: ");

        riverColorValCbo = new Combo(colorValComp, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        riverColorValCbo.add("Observed Value");
        riverColorValCbo.add("Forecast");
        riverColorValCbo.add("Max (Obs, Fcst)");
        riverColorValCbo.select(0);
        riverColorValCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleRiverColorChange();
                shell.setCursor(waitCursor);
                updateData = true;
                drawMap();
                shell.setCursor(arrowCursor);
            }
        });

        // ---------------------------------------------------
        // Setup the composite for "Display Value As" combo
        // ---------------------------------------------------
        Composite displayAsComp = new Composite(displayGroup, SWT.NONE);
        GridLayout displayAsCompGl = new GridLayout(2, false);
        displayAsComp.setLayout(displayAsCompGl);

        displayAsLbl = new Label(displayAsComp, SWT.NONE);
        displayAsLbl.setText("Display Values As: ");

        displayValAsCbo = new Combo(displayAsComp, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        displayValAsCbo.add(RAW_VALUE);
        displayValAsCbo.add(RAW_VALUE_FLOOD_LEVEL);
        displayValAsCbo.add(RAW_VALUE_STAGE_FLOW);
        displayValAsCbo.add(FLOOD_DEPART);
        displayValAsCbo.add(FLOOD_DEPART_LEVEL);
        displayValAsCbo.select(0);
        displayValAsCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateData = true;
                handleDisplayValueChange();
            }
        });
    }

    /**
     * Create the bottom dialog buttons.
     */
    private void createBottomButtons() {
        Composite bottomBtnComp = new Composite(shell, SWT.NONE);
        GridLayout bottomBtnCompGl = new GridLayout(4, true);
        bottomBtnCompGl.horizontalSpacing = 20;
        bottomBtnComp.setLayout(bottomBtnCompGl);

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 75;
        Button tabulateBtn = new Button(bottomBtnComp, SWT.PUSH);
        tabulateBtn.setText("Tabulate");
        tabulateBtn.setLayoutData(gd);
        tabulateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                manager.setDrawStation(false);
                TabularDisplayDlg tabDisplay = new TabularDisplayDlg(shell);
                tabDisplay.open();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 75;
        Button unMapBtn = new Button(bottomBtnComp, SWT.PUSH);
        unMapBtn.setText("Unmap");
        unMapBtn.setLayoutData(gd);
        unMapBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                /*
                 * Clear the datamap that holds the data to be mapped, then draw
                 * without any data resulting in an empty map.
                 */
                PointDataControlManager pdcManager = PointDataControlManager
                        .getInstance();
                MultiPointResource mpr = pdcManager.getMultiPointResource();
                mpr.resetDataMap();
                mpr.unmap();
                mpr.issueRefresh();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 75;
        mapBtn = new Button(bottomBtnComp, SWT.PUSH);
        mapBtn.setText("Map");
        mapBtn.setLayoutData(gd);
        mapBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.setCursor(waitCursor);
                updateData = true;
                drawMap();
                shell.setCursor(arrowCursor);
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 75;
        Button closeBtn = new Button(bottomBtnComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // shell.dispose();
                shell.setVisible(false);
            }
        });
    }

    /**
     * Populate the PDCOptionData object to select the appropriate values in the
     * Point Data Control dialog.
     * 
     * @param id
     *            The preset id
     */
    protected void populatePresetData(String id) {
        selPresetCbo.removeAll();

        try {
            presets = (ArrayList<PointDataPreset>) dataManager.getPresets();
            if (presets.size() > 0) {
                for (int i = 0; i < presets.size(); i++) {
                    selPresetCbo.add(presets.get(i).getDescription());
                    if ((id != null)
                            && (presets.get(i).getPresetId()
                                    .equalsIgnoreCase(id))) {
                        selPresetCbo.select(i);
                        setPcOptionsUsingPresets(presets.get(i));
                    }
                }

                if (id == null) {
                    selPresetCbo.select(0);
                    setPcOptionsUsingPresets(presets.get(0));
                }

                setGUIFromOptionsData();
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /**
     * Populate the Elements combo boxes with Ad-Hoc data.
     */
    private void populateCombosWithAdHoc() {
        PointDataControlManager manager = PointDataControlManager.getInstance();
        PointControlPeTs petsdata = manager.getPCPeTsData();
        elementTypeCbo.removeAll();
        physicalElementCbo.removeAll();

        ArrayList<AdHocElements> adHocDataArray = adHocData
                .getDataElementArray();

        for (AdHocElements adHocElements : adHocDataArray) {
            elementTypeCbo.add(adHocElements.getElementType());
        }

        AdHocElements elements;
        ArrayList<String> dataElements;
        elements = adHocData.getDataElement(0);
        dataElements = elements.getDataElementArray();

        for (String elementStr : dataElements) {
            physicalElementCbo.add(elementStr);
        }

        petsdata.setElementCount(adHocDataArray.size());

        elementTypeCbo.select(0);
        physicalElementCbo.select(0);

    }

    /**
     * Populate the Elements combo boxes with Time Step data.
     */
    private void populateCombosWithTimeStep() {
        elementTypeCbo.removeAll();
        physicalElementCbo.removeAll();

        int index = 0;

        ArrayList<TimeStepElements> timeStepDataArray = timeStepData
                .getDataElementArray();

        for (TimeStepElements timeStepElements : timeStepDataArray) {
            elementTypeCbo.add(timeStepElements.getElementType());
        }

        TimeStepElements elements = timeStepData.getDataElement(index);
        ArrayList<String> dataElements = elements.getDataElementArray();

        for (String elementStr : dataElements) {
            physicalElementCbo.add(elementStr);
        }

        elementTypeCbo.select(0);
        physicalElementCbo.select(0);
    }

    /**
     * When in Time Step mode, if a selection is made in the element type combo
     * box then the data elements combo box is updated with new data.
     */
    private void handleTimeStepElementComboSelection() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        physicalElementCbo.removeAll();

        int index = elementTypeCbo.getSelectionIndex();

        pcOptions.setElementType(index);

        TimeStepElements elements = timeStepData.getDataElement(index);
        ArrayList<String> dataElements = elements.getDataElementArray();

        for (String elementStr : dataElements) {
            physicalElementCbo.add(elementStr);
        }

        // physicalElementCbo.select(pcOptions.getPeSelection());
        physicalElementCbo.select(0);
    }

    private void handleAdhocElementComboSelection() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        physicalElementCbo.removeAll();

        int index = elementTypeCbo.getSelectionIndex();

        pcOptions.setElementType(index);

        AdHocElements elements = adHocData.getDataElement(index);
        ArrayList<String> dataElements = elements.getDataElementArray();

        for (String elementStr : dataElements) {
            physicalElementCbo.add(elementStr);
        }

        // physicalElementCbo.select(pcOptions.getPeSelection());
        physicalElementCbo.select(0);
        handlePeSelection();
    }

    private void handlePresetSelection() {
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        if (selPresetCbo.getSelectionIndex() == -1) {
            /* No selection made */
            return;
        }

        PDCOptionData pcOptions = PDCOptionData.getInstance();

        /*
         * Indicate to the pointcontrol manager routines that new data must be
         * retrieved from the IHFS database.
         */
        pdcManager.setRedraw(true);

        PointDataPreset presetData = presets.get(selPresetCbo
                .getSelectionIndex());

        /*
         * Even if there is an option value list to process, first set all of
         * the members of the PDCOptionData to a default value. This needs to be
         * done just in case not all of the options are specified in the
         * predefined option set. We need to make sure that all options have a
         * valid value.
         */

        pcOptions = getDefaultOptionData();
        setPcOptionsUsingPresets(presetData);
        setGUIFromOptionsData();
    }

    /**
     * Show the Ad Hoc controls or the Time Step controls depending on the flag
     * passed to this method.
     * 
     * If the Ad Hoc controls are displayed the Time Step controls are hidden.
     * If the Time Step controls are displayed the Ad Hoc controls are hidden.
     * 
     * @param flag
     *            If true display the Ad Hoc Controls and hide the Time Step
     *            controls. If false hide the Ad Hoc controls and display the
     *            Time Step controls.
     */
    private void showAdHocControls(boolean flag) {
        // Time Step control
        stationsLbl.setVisible(!flag);
        stationsCbo.setVisible(!flag);

        // Ad Hoc control
        dataSourceChk.setVisible(flag);
        dataSourceBtn.setVisible(flag);

        // Ad Hoc control
        hoursLbl.setVisible(flag);
        hoursSpnr.setVisible(flag);

        // Ad Hoc control
        valueLbl.setVisible(flag);
        valueIsCbo.setVisible(flag);

        // Ad Hoc control
        displayAsLbl.setVisible(flag);
        displayValAsCbo.setVisible(flag);
    }

    private void delete() {
        PointDataPreset presetSelection = presets.get(selPresetCbo
                .getSelectionIndex());

        try {
            dataManager.deletePreset(presetSelection.getPresetId());
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
            MessageDialog.openError(
                    shell,
                    "Error Occurred",
                    "An error occurred during the delete function.\n"
                            + e.getCause());
        }

        populatePresetData(null);
    }

    private PDCOptionData getDefaultOptionData() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        pcOptions.reset();

        return pcOptions;
    }

    private void openSaveDialog() {
        PDC_SaveDlg saveDlg = new PDC_SaveDlg(shell,
                selPresetCbo.getSelectionIndex(), this);
        saveDlg.open();
    }

    protected void setTimeFields() throws ParseException {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        String timeValue = timeTF.getText();
        Date date = dateTimeFmt.parse(timeValue);

        int hours = hoursSpnr.getSelection();
        pcOptions.setValidTime(date);
        pcOptions.setPcTimeStr(PDCConstants.DATE_FORMAT.format(date));
        pcOptions.setDurHours(hours);

        /*
         * check if this time has changed from the last time in order to
         * determine whether a retrieval is needed
         */
        if ((date.compareTo(previousDate) != 0) || (hours != previousHours)) {
            if (pcOptions.getQueryMode() == 0) {
                updateData = true;
            }
            previousDate = date;
            previousHours = hours;
        } else {
            // updateData = false;
        }
    }

    private void handleFilterByElevationSelection() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        String valueElevationSelection = filterByElevationCbo
                .getItem(filterByElevationCbo.getSelectionIndex());
        boolean enable = false;

        int count = 0;
        for (ElevationFilterOperation fo : ElevationFilterOperation.values()) {
            if (fo.getFilterType().equals(valueElevationSelection)) {
                pcOptions.setElevFilterOperation(count);
                enable = true;
                break;
            }
            count++;
        }

        if (filterByElevationCbo.getSelectionIndex() == 0) {
            filterByElevationTF.setEnabled(false);
        } else {
            filterByElevationTF.setEnabled(enable);
        }

        shell.setCursor(waitCursor);
        drawMap();
        shell.setCursor(arrowCursor);
    }

    private void handleFilterByValueSelection() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        String valueFilterSelection = filterByValueCbo.getItem(filterByValueCbo
                .getSelectionIndex());
        boolean enable = false;
        int count = 0;

        for (ValueFilterOperation fo : ValueFilterOperation.values()) {
            if (fo.getFilterType().equals(valueFilterSelection)) {
                pcOptions.setValueFilterOperation(count);
                enable = true;
                break;
            }
            count++;
        }

        if (filterByValueCbo.getSelectionIndex() == 0) {
            filterByValueTF.setEnabled(false);
        } else {
            filterByValueTF.setEnabled(enable);
        }

        shell.setCursor(waitCursor);
        drawMap();
        shell.setCursor(arrowCursor);
    }

    /*
     * validate the values typed in Show Pts with value and Show Pts elevation.
     * Only allows digits, '.' and '-'
     */

    private void validateShowPtsEntries(Event event, Text val) {
        String valueTyped = event.text;
        char[] valueChars = new char[valueTyped.length()];
        valueTyped.getChars(0, valueChars.length, valueChars, 0);
        for (int i = 0; i < valueChars.length; i++) {
            char decimalSeparator = entryFormat.getDecimalFormatSymbols()
                    .getDecimalSeparator();
            ;
            if (!((valueTyped.charAt(i) == '-') && (i == 0) && (event.start == 0))
                    && !((('0' <= valueChars[i]) && (valueChars[i] <= '9')) || ((valueChars[i] == decimalSeparator) && (val
                            .getText().indexOf(decimalSeparator) == -1)))) {

                event.doit = false;
                return;
            }
        }
    }

    /*
     * add listener to key release for Show Pts with value and Show Pts
     * elevation.
     */
    private void addListenerToTextValue(final Text val) {
        val.addKeyListener(new KeyListener() {
            public void keyPressed(KeyEvent e) {
                // Do nothing
            }

            @Override
            public void keyReleased(KeyEvent e) {
                parseValueTyped(val);
                shell.setCursor(waitCursor);
                drawMap();
                shell.setCursor(arrowCursor);

            }
        });
    }

    /*
     * parse the value entered in Show Pts with value and Show Pts elevation.
     */

    private void parseValueTyped(Text val) {
        PDCOptionData pcOptions = PDCOptionData.getInstance();

        if ((val.getText() != null) && !val.getText().isEmpty()) {

            switch (val.getText().charAt(0)) {
            case '.':
            case '-':
                break;
            default:
                try {
                    double doubleVal = Double.parseDouble(val.getText());
                    // its a number
                    if (val == filterByElevationTF) {
                        pcOptions.setElevFilterValue(doubleVal);
                    }
                    if (val == filterByValueTF) {
                        pcOptions.setValueFilterValue(doubleVal);
                    }
                    shell.setCursor(waitCursor);
                    drawMap();
                    shell.setCursor(arrowCursor);
                } catch (NumberFormatException e1) {
                    // its not a number add text box message
                    e1.printStackTrace();
                }
                break;
            }
        }
    }

    /**
     * Handles the change to the "River Color/Value Based On" combo box.
     * 
     * This selection only applies to the time mode option of Latest. If a time
     * is specified then observed data are used.
     * 
     * In Time Step mode this option controls only the color of the icon, the
     * values are always observed for the selected time.
     */
    private void handleRiverColorChange() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        String selection = riverColorValCbo.getItem(riverColorValCbo
                .getSelectionIndex());
        if (selection
                .equals(PDCConstants.StageBasis.BASIS_FCST.getStageBasis())) {
            pcOptions.setStageBasis(1);
        } else if (selection.equals(PDCConstants.StageBasis.BASIS_OBS
                .getStageBasis())) {
            pcOptions.setStageBasis(0);
        } else if (selection.equals(PDCConstants.StageBasis.BASIS_MOFO
                .getStageBasis())) {
            pcOptions.setStageBasis(2);
        }
    }

    private void handleDisplayValueChange() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        String selection = displayValAsCbo.getItem(displayValAsCbo
                .getSelectionIndex());
        if (selection.equals(RAW_VALUE)) {
            pcOptions.setValueType(TYPE_VALUE);
            pcOptions.setFloodLevel(0);
            pcOptions.setDeriveStageFlow(0);
        } else if (selection.equals(RAW_VALUE_FLOOD_LEVEL)) {
            pcOptions.setValueType(TYPE_VALUE);
            pcOptions.setFloodLevel(1);
            pcOptions.setDeriveStageFlow(0);
        } else if (selection.equals(RAW_VALUE_STAGE_FLOW)) {
            pcOptions.setValueType(TYPE_VALUE);
            pcOptions.setFloodLevel(0);
            pcOptions.setDeriveStageFlow(1);
        } else if (selection.equals(FLOOD_DEPART)) {
            pcOptions.setValueType(TYPE_DEPART);
            pcOptions.setFloodLevel(0);
            pcOptions.setDeriveStageFlow(0);
        } else if (selection.equals(FLOOD_DEPART_LEVEL)) {
            pcOptions.setValueType(TYPE_DEPART);
            pcOptions.setFloodLevel(1);
            pcOptions.setDeriveStageFlow(0);
        }

        shell.setCursor(waitCursor);
        drawMap();
        shell.setCursor(arrowCursor);
    }

    private void handleQueryModeSelection(PDCConstants.QueryMode queryMode) {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        if (queryMode == PDCConstants.QueryMode.AD_HOC_MODE) {
            populateCombosWithAdHoc();
            showAdHocControls(true);
            pcOptions.setQueryMode(0);
            setElementTypeByOppositeModeElementType();
            handleAdhocElementComboSelection();
            handleTsPhysicalElementSelection(physicalElementCbo
                    .getItem(physicalElementCbo.getSelectionIndex()));
            stackLayout.topControl = valueTimeGroup;
            stackComp.layout();
        } else {
            populateCombosWithTimeStep();
            showAdHocControls(false);
            pcOptions.setQueryMode(1);
            setElementTypeByOppositeModeElementType();
            handleTimeStepElementComboSelection();
            handleTsPhysicalElementSelection(physicalElementCbo
                    .getItem(physicalElementCbo.getSelectionIndex()));
            if ((physicalElementCbo.getSelectionIndex() == 0)
                    && (elementTypeCbo.getSelectionIndex() == 1)) {
                stackLayout.topControl = precipGroup;
                stackComp.layout();
            } else {
                stackLayout.topControl = valueTimeGroup;
                stackComp.layout();
            }
        }
    }

    /**
     * Handle the Display radio button selections.
     */
    private void handleDisplayRadioSelection() {
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        if (timeRdo.getSelection()) {
            pcOptions.setTime(1);
            pcOptions.setElevation(0);
            pcOptions.setParamCode(0);
        } else if (elevRdo.getSelection()) {
            pcOptions.setTime(0);
            pcOptions.setElevation(1);
            pcOptions.setParamCode(0);
        } else if (paramCodeRdo.getSelection()) {
            pcOptions.setTime(0);
            pcOptions.setElevation(0);
            pcOptions.setParamCode(1);
        } else {
            pcOptions.setTime(0);
            pcOptions.setElevation(0);
            pcOptions.setParamCode(0);
        }
        pdcManager.setRedraw(true);
    }

    private void handlePeSelection() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        if (adHocRdo.getSelection()) {
            pcOptions.setPcAndpp(0);
            pcOptions.setPrimary(0);

            /* Store just the pe in the SelectedAdHocElement String */
            String[] parts = physicalElementCbo.getItem(
                    physicalElementCbo.getSelectionIndex()).split(" ", 2);
            pcOptions.setSelectedAdHocElementString(parts[0]);

            /* Store the whole selection in the SelectedAdHocElementFullString */
            pcOptions.setSelectedAdHocElementFullString(physicalElementCbo
                    .getItem(physicalElementCbo.getSelectionIndex()));
        }

        pcOptions.setPeSelection(physicalElementCbo.getSelectionIndex());

        /*
         * Toggle the states of the pc_options Primary and PCandPP flags.
         */
        if ((physicalElementCbo.getSelectionIndex() == 0)
                && (pcOptions.getElementType() == HydroConstants.AdHocDataElementType.RIVER_AD_HOC_TYPE
                        .getAdHocDataElementType())) {
            pcOptions.setPrimary(1);
        }

        if ((physicalElementCbo.getSelectionIndex() == 0)
                && (pcOptions.getElementType() == HydroConstants.AdHocDataElementType.RAIN_AD_HOC_TYPE
                        .getAdHocDataElementType())) {
            pcOptions.setPcAndpp(1);
        }

        // Don't need this code according to DR #4672
        // if (pcOptions.getPrimary() == 1) {
        // typeSourceChk.setSelection(false);
        // typeSourceChk.setEnabled(false);
        // typeSourceBtn.setEnabled(false);
        // } else {
        // typeSourceChk.setEnabled(true);
        // typeSourceBtn.setEnabled(true);
        // }
    }

    private void handleTsPhysicalElementSelection(String selection) {
        PDCOptionData pcOptions = PDCOptionData.getInstance();

        /*
         * Note the dataElementStringArray needs to be in the SAME order as the
         * TimeStepDataElement enum
         */
        for (int i = 0; i < HydroConstants.TimeStepDataElement.TIME_STEP_DATA_ELEMENT_COUNT
                .getElementType(); i++) {
            if (selection
                    .equalsIgnoreCase(TIMESTEP_DATA_ELEMENT_STRING_ARRAY[i])) {
                pcOptions.setTsDataElement(i);
                break;
            }
        }

    }

    private void setElementTypeByOppositeModeElementType() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        int oldElementType = pcOptions.getElementType();
        int newElementType = 0;

        if (previousQueryMode == PDCConstants.QueryMode.TIME_STEP_MODE) {
            // Note, the element types in TimeStepDataElementType and
            // AdHocDataElementType need to match each other in order so
            // that their values are equal
            // 4 = HUMIDITY_TIME_STEP_TYPE
            if ((oldElementType >= 0) && (oldElementType < 4)) {
                newElementType = oldElementType;
            } else {
                // I am stuck, because there are no standard ad_hoc types that
                // go beyond TEMPERATURE
                // 0 = RIVER_AD_HOC_TYPE
                newElementType = 0;

            }
        } else {
            // 4 = OTHER_AD_HOC_TYPE
            if ((oldElementType >= 0) && (oldElementType < 4)) {
                newElementType = oldElementType;
            }
        }

        elementTypeCbo.select(newElementType);
        handlePeSelection();
    }

    private void setPcOptionsUsingPresets(PointDataPreset preset) {
        String presetString = preset.getPresetString();

        try {
            HashMap<String, String[]> presetData = (HashMap<String, String[]>) PDCUtils
                    .tokenizePresetString(presetString);

            setPCOptions(presetData);

        } catch (ParseException e) {
            e.printStackTrace();
        }
    }

    private void setPCOptions(HashMap<String, String[]> presetData) {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        pcOptions.reset();
        String[] valueArr = null;

        for (int i = 0; i < PDCConstants.pcOptionArray.length; i++) {
            String key = PDCConstants.pcOptionArray[i];
            if (presetData.get(key) != null) {
                valueArr = presetData.get(key);
                if (valueArr.length > 0) {
                    switch (i) {
                    case 0: // InstantaneousPrecipAccumTime,
                        int value = Integer.parseInt(valueArr[0]);
                        pcOptions.setInstPrecipAccumTimeSelection(value);
                        break;

                    case 1: // SelectedTypeSources
                        List<String> tsList = new ArrayList<String>();

                        for (String s : valueArr) {
                            tsList.add(s);
                        }
                        pcOptions.setTypeSourceChosenList(tsList);
                        pcOptions.setTypeSourceChosenCount(tsList.size());
                        break;

                    case 2: // DataType
                        pcOptions.setElementType(Integer.parseInt(valueArr[0]));
                        break;

                    case 3: // DeriveStageFlow
                        pcOptions.setDeriveStageFlow(Integer
                                .parseInt(valueArr[0]));
                        break;

                    case 4: // DurHours
                        pcOptions.setDurHours(Integer.parseInt(valueArr[0]));
                        break;

                    case 5: // FilterByDataSource
                        pcOptions.setFilterByDataSource(Integer
                                .parseInt(valueArr[0]));
                        break;

                    case 6: // DateTime
                        if (valueArr.length == 2) {
                            int numberOfDays = Integer.parseInt(valueArr[0]);
                            if (valueArr[0].length() > 0) {
                                /*
                                 * Set the hours and minutes to those retrieved
                                 * from the presets group. Hours and minutes are
                                 * absolute while the day is relative.
                                 */
                                String[] timeArr = valueArr[1].split(":");
                                Calendar now = Calendar.getInstance(TimeZone
                                        .getTimeZone("GMT"));

                                now.add(Calendar.DAY_OF_MONTH, numberOfDays);
                                int hours = Integer.parseInt(timeArr[0]);
                                now.set(Calendar.HOUR_OF_DAY, hours);

                                int minutes;
                                if (timeArr.length > 1) {
                                    minutes = Integer.parseInt(timeArr[1]);
                                } else {
                                    minutes = 0;
                                }

                                now.set(Calendar.MINUTE, minutes);

                                pcOptions.setValidTime(now.getTime());
                            }
                        }
                        break;

                    case 7: // ElevFilterOperation
                        if (valueArr.length > 1) {
                            pcOptions.setElevFilterOperation(Integer
                                    .parseInt(valueArr[0]));

                            /* if the value is not a valid number (0-7) */
                            if ((pcOptions.getElevFilterOperation() < 0)
                                    || (pcOptions.getElevFilterOperation() >= 7)) {
                                pcOptions.setElevFilterOperation(0);
                            }
                        } else {
                            pcOptions.setElevFilterOperation(0);
                        }
                        break;

                    case 8: // ElevFilterValue
                        pcOptions.setElevFilterValue(Double
                                .parseDouble(valueArr[0]));
                        break;

                    case 9: // FloodLevel
                        pcOptions.setFloodLevel(Integer.parseInt(valueArr[0]));
                        break;

                    case 10: // FcstOnly
                        pcOptions.setFcstptsOnly(Integer.parseInt(valueArr[0]));
                        break;

                    case 11: // ShowIcon
                        pcOptions.setIcon(Integer.parseInt(valueArr[0]));
                        break;

                    case 12: // ShowId
                        pcOptions.setId(Integer.parseInt(valueArr[0]));
                        break;

                    case 13: // ShowName
                        pcOptions.setName(Integer.parseInt(valueArr[0]));
                        break;

                    case 14: // NumSources
                        pcOptions.setDataSourceChosenCount(Integer
                                .parseInt(valueArr[0]));
                        break;

                    case 15: // SelectedAdHocPE
                        String[] parts = valueArr[0].split(" ", 2);
                        pcOptions.setSelectedAdHocElementString(parts[0]);
                        break;

                    case 16: // PCandPP
                        pcOptions.setPcAndpp(Integer.parseInt(valueArr[0]));
                        break;

                    case 17: // Primary
                        pcOptions.setPrimary(Integer.parseInt(valueArr[0]));
                        break;

                    case 18: // ProcessSelected
                        pcOptions.setProcessSelected(Integer
                                .parseInt(valueArr[0]));
                        break;

                    case 19: // SelectedQueryMode
                        pcOptions.setQueryMode(Integer.parseInt(valueArr[0]));

                        /* 0 is Ad_hoc mode, 1 is time-step mode */
                        if (pcOptions.getQueryMode() != 1) {
                            pcOptions.setQueryMode(0);
                        }
                        break;

                    case 20: // ShowRiverStatus
                        pcOptions.setRiverStatus(Integer.parseInt(valueArr[0]));
                        break;

                    case 21: // StageBasis
                        pcOptions.setStageBasis(Integer.parseInt(valueArr[0]));
                        break;

                    case 22: // ShowElevation
                        pcOptions.setElevation(Integer.parseInt(valueArr[0]));
                        break;

                    case 23: // StreamStationFilter
                        pcOptions.setRiverStationFilter(Integer
                                .parseInt(valueArr[0]));
                        break;

                    case 24: // SuppressMissing
                        pcOptions.setSupressMissing(Integer
                                .parseInt(valueArr[0]));
                        break;

                    case 25: // ShowParamCode
                        pcOptions.setParamCode(Integer.parseInt(valueArr[0]));
                        break;

                    case 26: // SourceList
                        // Not using any more, get count from array
                        // int count = pcOptions.getDataSourceChosenCount();
                        // int minCount = PDCConstants.MISSING_VALUE;
                        List<String> sourceList = new ArrayList<String>();

                        // if (count < valueArr.length) {
                        // minCount = count;
                        // } else {
                        // minCount = valueArr.length;
                        // }

                        for (int j = 0; j < valueArr.length; j++) {
                            sourceList.add(valueArr[j]);
                        }

                        pcOptions.setDataSourcesChosen(sourceList
                                .toArray(new String[sourceList.size()]));
                        pcOptions.setDataSourceChosenCount(sourceList.size());
                        break;

                    case 27: // SelectedTimeStepElement
                        pcOptions.setTsDataElement(Integer
                                .parseInt(valueArr[0]));
                        break;

                    case 28: // ServiceBackup
                        pcOptions.setFilterByHSA(Integer.parseInt(valueArr[0]));
                        break;

                    case 29: // SupressZero
                        int suppressZero = Integer.parseInt(valueArr[0]);

                        /*
                         * This suppressZero options has been replaced by value
                         * filtering. This case is for backward compatibility
                         * with older presets saved in the database. Sets the
                         * Filter to 'Value Not ='
                         */
                        if (suppressZero == 1) {
                            pcOptions.setValueFilterOperation(2);
                            pcOptions.setValueFilterValue(0.0);
                        }
                        break;

                    case 30: // ShowTime
                        pcOptions.setTime(Integer.parseInt(valueArr[0]));
                        break;

                    case 31: // TimeMode
                        pcOptions.setTimeMode(Integer.parseInt(valueArr[0]));
                        break;

                    case 32: // TimeStepPrecipPEFilter
                        pcOptions.setPrecipPeFilter(Integer
                                .parseInt(valueArr[0]));
                        break;

                    case 33: // FilterByTypeSource
                        pcOptions.setFilterByTypeSource(Integer
                                .parseInt(valueArr[0]));
                        break;

                    case 34: // ShowValue
                        pcOptions.setValue(Integer.parseInt(valueArr[0]));
                        break;

                    case 35: // ValueFilterOperation
                        if (valueArr[0].length() > 0) {
                            pcOptions.setValueFilterOperation(Integer
                                    .parseInt(valueArr[0]));

                            /* if the value is not a valid number (0-7) */
                            if ((pcOptions.getValueFilterOperation() < 0)
                                    || (pcOptions.getValueFilterOperation() >= 7)) {
                                pcOptions.setValueFilterOperation(0);
                            }
                        } else {
                            pcOptions.setValueFilterOperation(0);
                        }
                        break;

                    case 36: // ValueType
                        pcOptions.setValueType(Integer.parseInt(valueArr[0]));
                        break;

                    case 37: // ValueFilterValue
                        pcOptions.setValueFilterValue(Double
                                .parseDouble(valueArr[0]));
                        break;

                    case 38: // WFOlist
                        List<String> hsaList = new ArrayList<String>();
                        if (valueArr.length > 0) {

                            for (int j = 0; j < valueArr.length; j++) {
                                hsaList.add(valueArr[j]);
                            }
                            pcOptions.setHsaList(hsaList);
                        } else {
                            hsaList.add(PDCConstants.ALL_AREAS);
                            pcOptions.setHsaList(hsaList);
                        }
                        break;

                    case PDCConstants.PE_SELECTION_INDEX:
                        pcOptions.setPeSelection(Integer.parseInt(valueArr[0]));

                    }
                }
            }
        }
    }

    /**
     * Set the date/time and hours duration widgets.
     */
    private void loadTimeInfo() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        if (pcOptions.getQueryMode() == 0) { // AdHoc Mode
            timeTF.setText(dateTimeFmt.format(pcOptions.getValidTime()));
            cal.setTime(pcOptions.getValidTime());
            hoursSpnr.setSelection(pcOptions.getDurHours());
        } else { // Time Step Mode
            int roundingHours = 1;
            int additionalHours = 0;
            long validTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"))
                    .getTimeInMillis();

            if (pcOptions.getElementType() == HydroConstants.TimeStepDataElementType.RAIN_TIME_STEP_TYPE
                    .getTimeStepDataElementType()) {
                if (pcOptions.getTsDataElement() == HydroConstants.TimeStepDataElement.INSTANTANEOUS_PRECIP_TSDE
                        .getElementType()) {
                    // Do Nothing
                } else if (pcOptions.getTsDataElement() == HydroConstants.TimeStepDataElement.HOURLY_PRECIP_TSDE
                        .getElementType()) {
                    roundingHours = 1;
                } else if (pcOptions.getTsDataElement() == HydroConstants.TimeStepDataElement.THREE_HOUR_PRECIP_TSDE
                        .getElementType()) {
                    roundingHours = 3;
                } else if (pcOptions.getTsDataElement() == HydroConstants.TimeStepDataElement.SIX_HOUR_PRECIP_TSDE
                        .getElementType()) {
                    roundingHours = 6;
                } else if (pcOptions.getTsDataElement() == HydroConstants.TimeStepDataElement.DAILY_PRECIP_TSDE
                        .getElementType()) {
                    roundingHours = 24;
                    additionalHours = 12; // daily periods end at 12Z
                } else {
                    System.err
                            .println("PointDataControlDlg.loadTimeInfo(): "
                                    + "ERROR - I don't know what kind of precip I am dealing with. Examine code for errors.");
                }

                validTime /= (roundingHours * (PDCConstants.MILLIS_PER_HOUR));
                validTime *= (roundingHours * (PDCConstants.MILLIS_PER_HOUR));

                validTime += (additionalHours * (PDCConstants.MILLIS_PER_HOUR));
            } else if ((pcOptions.getTsDataElement() == HydroConstants.TimeStepDataElement.TEMP_MAX_TSDE
                    .getElementType())
                    || (pcOptions.getTsDataElement() == HydroConstants.TimeStepDataElement.TEMP_MIN_TSDE
                            .getElementType())) {
                roundingHours = 24;
                additionalHours = 12;

                validTime /= roundingHours * (PDCConstants.MILLIS_PER_HOUR);
                validTime *= roundingHours * (PDCConstants.MILLIS_PER_HOUR);

                validTime += additionalHours * (PDCConstants.MILLIS_PER_HOUR);
            } else {
                // other types don't need rounding
                validTime = pcOptions.getValidTime().getTime();
                validTime /= HydroConstants.MILLIS_PER_HOUR;
                validTime *= HydroConstants.MILLIS_PER_HOUR;
                // do nothing
            }
            Date d = SimulatedTime.getSystemTime().getTime();
            d.setTime(validTime);
            pcOptions.setValidTime(d);

            timeTF.setText(dateTimeFmt.format(pcOptions.getValidTime()));
        }
    }

    private void setGUIFromOptionsData() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();

        /* set the query mode radio button */
        if (pcOptions.getQueryMode() == 0) {
            adHocRdo.setSelection(true);
            timeStepRdo.setSelection(false);
        } else {
            timeStepRdo.setSelection(true);
            adHocRdo.setSelection(false);
        }

        /* Set the element Type */
        int elementType = pcOptions.getElementType();
        elementTypeCbo.select(elementType);

        if (pcOptions.getQueryMode() == 0) {
            physicalElementCbo.select(pcOptions.getPeSelection());
        } else {
            timeStepRdo.setSelection(true);
        }

        /* These event handlers will select the correct TS */
        if (timeStepRdo.getSelection() == false) {
            handleAdhocElementComboSelection();
        } else {
            handleTimeStepElementComboSelection();
        }

        /* set the time mode */
        valueIsCbo.select(pcOptions.getTimeMode());

        /* set the endtime and the duration */
        loadTimeInfo();

        // set the options for the Instantaneous precip accumulation time
        // Only is visible when in time-step mode, and instantaneous precip has
        // been
        // selected
        int selection = pcOptions.getInstPrecipAccumTimeSelection();
        // TODO - Check this against the VPN for selections
        if ((selection < 0)
                || (selection > HydroConstants.InstPrecipSelection.PRECIP_TIME_24_HOURS
                        .getInstPrecipSelection())) {
            pcOptions
                    .setInstPrecipAccumTimeSelection(HydroConstants.InstPrecipSelection.PRECIP_TIME_30_MINUTES
                            .getInstPrecipSelection());
        }

        // set the toggle button that corresponds to the filter by typesource
        // option
        if (pcOptions.getFilterByTypeSource() == 1) {
            typeSourceChk.setSelection(true);
        } else {
            typeSourceChk.setSelection(false);
        }

        /*
         * Set the service backup according to the value in the pc_options
         * structure.
         */
        if ((pcOptions.getFilterByHSA() == 1)
                && (pcOptions.getHsaList().size() > 0)) {
            serviceAreaChk.setSelection(true);
        } else {
            serviceAreaChk.setSelection(false);
        }

        /* set the datasource toggle button */
        if (pcOptions.getFilterByDataSource() == 1) {
            dataSourceChk.setSelection(true);
        } else {
            dataSourceChk.setSelection(false);
        }

        /* set the filter options */
        if (pcOptions.getSupressMissing() == 1) { // reversed polarity!
            showMissingChk.setSelection(true);
        } else {
            showMissingChk.setSelection(false);
        }

        /* set the pc_pc_riverStationFilterOM based on river_station_filter */
        stationsCbo.select(pcOptions.getRiverStationFilter());

        /* set the pc_precipPeOM based on precip_pe_filter */
        pcOptions.getPrecipPeFilter();
        // TODO - This is invisible if not a rain PE Verify with the vpn

        /* set the pc_filterElevationOM */
        filterByElevationCbo.select(pcOptions.getElevFilterOperation());
        if (pcOptions.getElevFilterOperation() == 0) {
            filterByElevationTF.setEnabled(false);
        } else {
            filterByElevationTF.setEnabled(true);
        }

        /* Set the text field that holds the associated filter value */
        filterByElevationTF.setText(String.valueOf(pcOptions
                .getElevFilterValue()));

        /* set the pc_filterValueOM, */
        filterByValueCbo.select(pcOptions.getValueFilterOperation());

        if (pcOptions.getValueFilterOperation() == 0) {
            filterByValueTF.setEnabled(false);
        } else {
            filterByValueTF.setEnabled(true);
        }

        /* Set the text field that holds the associated filter value */
        filterByValueTF
                .setText(String.valueOf(pcOptions.getValueFilterValue()));

        if (pcOptions.getFcstptsOnly() == 1) { // reversed polarity!
            showNonFcstPtsChk.setSelection(false);
        } else {
            showNonFcstPtsChk.setSelection(true);
        }

        /* set the map display options */
        if (pcOptions.getValue() == 1) {
            valueChk.setSelection(true);
        } else {
            valueChk.setSelection(false);
        }

        if (pcOptions.getId() == 1) {
            idChk.setSelection(true);
        } else {
            idChk.setSelection(false);
        }

        if (pcOptions.getName() == 1) {
            nameChk.setSelection(true);
        } else {
            nameChk.setSelection(false);
        }

        if (pcOptions.getIcon() == 1) {
            iconChk.setSelection(true);
        } else {
            iconChk.setSelection(false);
        }

        if (pcOptions.getTime() == 1) {
            noneRdo.setSelection(false);
            timeRdo.setSelection(true);
            elevRdo.setSelection(false);
            paramCodeRdo.setSelection(false);
        } else if (pcOptions.getElevation() == 1) {
            noneRdo.setSelection(false);
            timeRdo.setSelection(false);
            elevRdo.setSelection(true);
            paramCodeRdo.setSelection(false);
        } else if (pcOptions.getParamCode() == 1) {
            noneRdo.setSelection(false);
            timeRdo.setSelection(false);
            elevRdo.setSelection(false);
            paramCodeRdo.setSelection(true);
        } else {
            noneRdo.setSelection(true);
            timeRdo.setSelection(false);
            elevRdo.setSelection(false);
            paramCodeRdo.setSelection(false);
        }

        /* set the river options. */
        riverColorValCbo.select(pcOptions.getStageBasis());

        selection = getRiverValueDisplayMode();
        displayValAsCbo.select(selection);

        if (pcOptions.getRiverStatus() == 1) {
            colorRiverIconChk.setSelection(true);
        } else {
            colorRiverIconChk.setSelection(false);
        }
    }

    /**
     * Determine the river value display mode.
     * 
     * @return the river Value Display Mode
     */
    private int getRiverValueDisplayMode() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        int mode = RiverValueDisplayMode.RAW_VALUE_ONLY.getIntValue();

        if ((pcOptions.getValueType() == ValueType.TYPE_VALUE.getValueType())
                && (pcOptions.getFloodLevel() == 0)
                && (pcOptions.getDeriveStageFlow() == 0)) {
            mode = RiverValueDisplayMode.RAW_VALUE_ONLY.getIntValue();
        } else if ((pcOptions.getValueType() == ValueType.TYPE_VALUE
                .getValueType()) && (pcOptions.getFloodLevel() == 1)) {
            mode = RiverValueDisplayMode.RAW_VALUE_FLOOD_LEVEL.getIntValue();
        } else if ((pcOptions.getValueType() == ValueType.TYPE_VALUE
                .getValueType()) && (pcOptions.getDeriveStageFlow() == 1)) {
            mode = RiverValueDisplayMode.RAW_VALUE_STAGE_FLOW.getIntValue();
        } else if ((pcOptions.getValueType() == ValueType.TYPE_DEPART
                .getValueType()) && (pcOptions.getFloodLevel() == 0)) {
            mode = RiverValueDisplayMode.FLOOD_DEPARTURE.getIntValue();
        } else if ((pcOptions.getValueType() == ValueType.TYPE_DEPART
                .getValueType()) && (pcOptions.getFloodLevel() == 1)) {
            mode = RiverValueDisplayMode.FLOOD_DEPARTURE_FLOOD_LEVEL
                    .getIntValue();
        }

        return mode;
    }

    /**
     * Set the Instantaneous precip accum time option.
     */
    private void setPrecipAccumTime() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        if (timeTF.getText().equals(THIRTY_MINUTES)) {
            pcOptions
                    .setInstPrecipAccumTimeSelection(HydroConstants.InstPrecipSelection.PRECIP_TIME_30_MINUTES
                            .getInstPrecipSelection());
        } else if (timeTF.getText().equals(ONE_HOUR)) {
            pcOptions
                    .setInstPrecipAccumTimeSelection(HydroConstants.InstPrecipSelection.PRECIP_TIME_1_HOUR
                            .getInstPrecipSelection());
        } else if (timeTF.getText().equals(TWO_HOUR)) {
            pcOptions
                    .setInstPrecipAccumTimeSelection(HydroConstants.InstPrecipSelection.PRECIP_TIME_2_HOURS
                            .getInstPrecipSelection());
        } else if (timeTF.getText().equals(THREE_HOUR)) {
            pcOptions
                    .setInstPrecipAccumTimeSelection(HydroConstants.InstPrecipSelection.PRECIP_TIME_3_HOURS
                            .getInstPrecipSelection());
        } else if (timeTF.getText().equals(FOUR_HOUR)) {
            pcOptions
                    .setInstPrecipAccumTimeSelection(HydroConstants.InstPrecipSelection.PRECIP_TIME_4_HOURS
                            .getInstPrecipSelection());
        } else if (timeTF.getText().equals(SIX_HOUR)) {
            pcOptions
                    .setInstPrecipAccumTimeSelection(HydroConstants.InstPrecipSelection.PRECIP_TIME_6_HOURS
                            .getInstPrecipSelection());
        } else if (timeTF.getText().equals(TWELVE_HOUR)) {
            pcOptions
                    .setInstPrecipAccumTimeSelection(HydroConstants.InstPrecipSelection.PRECIP_TIME_12_HOURS
                            .getInstPrecipSelection());
        } else if (timeTF.getText().equals(EIGHTEEN_HOUR)) {
            pcOptions
                    .setInstPrecipAccumTimeSelection(HydroConstants.InstPrecipSelection.PRECIP_TIME_18_HOURS
                            .getInstPrecipSelection());
        } else if (timeTF.getText().equals(TWENTY_FOUR_HOUR)) {
            pcOptions
                    .setInstPrecipAccumTimeSelection(HydroConstants.InstPrecipSelection.PRECIP_TIME_24_HOURS
                            .getInstPrecipSelection());
        }
    }

    private void setInstPrecipAccumText() {
        if (precipIndex == HydroConstants.InstPrecipSelection.PRECIP_TIME_30_MINUTES
                .getInstPrecipSelection()) {
            precipTimeTF.setText(THIRTY_MINUTES);
        } else if (precipIndex == HydroConstants.InstPrecipSelection.PRECIP_TIME_1_HOUR
                .getInstPrecipSelection()) {
            precipTimeTF.setText(ONE_HOUR);
        } else if (precipIndex == HydroConstants.InstPrecipSelection.PRECIP_TIME_2_HOURS
                .getInstPrecipSelection()) {
            precipTimeTF.setText(TWO_HOUR);
        } else if (precipIndex == HydroConstants.InstPrecipSelection.PRECIP_TIME_3_HOURS
                .getInstPrecipSelection()) {
            precipTimeTF.setText(THREE_HOUR);
        } else if (precipIndex == HydroConstants.InstPrecipSelection.PRECIP_TIME_4_HOURS
                .getInstPrecipSelection()) {
            precipTimeTF.setText(FOUR_HOUR);
        } else if (precipIndex == HydroConstants.InstPrecipSelection.PRECIP_TIME_6_HOURS
                .getInstPrecipSelection()) {
            precipTimeTF.setText(SIX_HOUR);
        } else if (precipIndex == HydroConstants.InstPrecipSelection.PRECIP_TIME_12_HOURS
                .getInstPrecipSelection()) {
            precipTimeTF.setText(TWELVE_HOUR);
        } else if (precipIndex == HydroConstants.InstPrecipSelection.PRECIP_TIME_18_HOURS
                .getInstPrecipSelection()) {
            precipTimeTF.setText(EIGHTEEN_HOUR);
        } else if (precipIndex == HydroConstants.InstPrecipSelection.PRECIP_TIME_24_HOURS
                .getInstPrecipSelection()) {
            precipTimeTF.setText(TWENTY_FOUR_HOUR);
        }
        setPrecipAccumTime();
    }

    /**
     * This method handles the clicking of the map button.
     */
    private void drawMap() {
        if (!disableDrawing) {
            PointDataControlManager pdcManager = PointDataControlManager
                    .getInstance();

            /* Set the map symbol options */
            pdcManager.setGage(iconChk.getSelection());
            pdcManager.setID(idChk.getSelection());
            pdcManager.setValue(valueChk.getSelection());
            pdcManager.setName(nameChk.getSelection());
            pdcManager.setTime(timeRdo.getSelection());
            pdcManager.setPE(paramCodeRdo.getSelection());
            pdcManager.setElevation(elevRdo.getSelection());

            /*
             * Enable the plotting of station data. Inform the hv areal routines
             * that they should plot the station data on the screen.
             */
            manager.setDrawStation(true);

            /* get and set the time fields */
            try {
                setTimeFields();

                /* now map the data */
                retrieveAndMapData();
            } catch (ParseException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Determine the number of hours to use when changing the time.
     * 
     * @return The number of hours for the increment
     */
    private int getAdjustmentHours() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();

        int adjustmentHours = 1;

        if (pcOptions.getQueryMode() == 1) {
            int element = pcOptions.getTsDataElement();

            if (element == 6) {
                // 3-HOUR PRECIP TOTAL
                adjustmentHours = 3;
            } else if (element == 7) {
                // 6-HOUR PRECIP TOTAL
                adjustmentHours = 6;
            } else if (element == 8) {
                // 24-HOUR TOTAL (12Z)
                adjustmentHours = 24;
            } else if ((element == 13) || (element == 14)) {
                // Max/Min Temperature
                adjustmentHours = 24;
            }
            // has no meaning for INSTANTANEOUS
            // same answer for hourly as default
        }

        return adjustmentHours;
    }

    private void retrieveAndMapData() {
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        boolean drawStationFlag = manager.isDrawStation();
        if (drawStationFlag) {
            if (pcOptions.getQueryMode() == QueryMode.AD_HOC_MODE
                    .getQueryMode()) {
                pdcManager.scheduleRequest(this.updateData,
                        PointDataControlManager.REQUEST_TYPE.REQUEST_AD_HOC);
            } else {
                pdcManager.scheduleRequest(this.updateData,
                        PointDataControlManager.REQUEST_TYPE.REQUEST_TIME_STEP);
            }
        }

        /*
         * We Will Set Update Data To 'true', When The User Modfies A Selection
         * In: 1) "Presets / Query Mode" 2) "Elements" 3) "Value / Time"
         */
        this.updateData = false;
    }

    private void fireUpdateEvent() {
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        StationDisplayUpdateEvent event = new StationDisplayUpdateEvent(this,
                valueChk.getSelection(), iconChk.getSelection(),
                idChk.getSelection(), nameChk.getSelection(),
                timeRdo.getSelection(), paramCodeRdo.getSelection(),
                elevRdo.getSelection());

        pdcManager.fireUpdateEvent(event);
    }

    public void showDialog() {
        shell.setVisible(true);
        shell.setFocus();
    }

    /**
     * Increment the time for stepping through time.
     * 
     * @param direction
     *            negative value is backwards in time, positive value is forward
     *            in time
     */
    public void incrementTime(int direction) {
        int adjustmentHour = getAdjustmentHours();
        cal.setTimeInMillis(PDCOptionData.getInstance().getValidTime()
                .getTime());

        if (direction < 0) {
            cal.add(Calendar.HOUR_OF_DAY, adjustmentHour * -1);
        } else {
            cal.add(Calendar.HOUR_OF_DAY, adjustmentHour);
        }

        this.updateData = true;
        timeTF.setText(dateTimeFmt.format(cal.getTime()));
        shell.setCursor(waitCursor);
        drawMap();
        shell.setCursor(arrowCursor);

    }
}