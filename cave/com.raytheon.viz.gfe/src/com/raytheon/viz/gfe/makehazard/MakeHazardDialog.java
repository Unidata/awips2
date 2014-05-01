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
package com.raytheon.viz.gfe.makehazard;

import java.nio.ByteBuffer;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TimeZone;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.UIJob;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteDefinition;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.PythonPreferenceStore;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.IReferenceSetManager;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.product.TextDBUtil;
import com.raytheon.viz.gfe.ui.zoneselector.ZoneSelector;
import com.raytheon.viz.gfe.ui.zoneselector.ZoneSelector.IZoneSelectionListener;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.statusline.StatusMessage.Importance;
import com.raytheon.viz.ui.statusline.StatusStore;

/**
 * Make Hazard Dialog
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- -----------  --------------------------
 * 	Jun 5, 2008				Eric Babin  Initial Creation
 *  Sep 27,2010 5813        gzhou       get etn from param pattern hazXXXnnn
 *  Feb 28,2012 14436		mli		    Add RP.S - Rip Current
 *  Apr 03,2012 436         randerso    Reworked dialog to be called by Python MakeHazard procedure
 *  Apr 09,2012 436         randerso    Merged RNK's MakeHazards_Elevation procedure
 *  May 30,2012 2028        randerso    Cleaned up dialog layout
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class MakeHazardDialog extends CaveSWTDialog implements
        IZoneSelectionListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MakeHazardDialog.class);

    private static final String DEFAULT_MAP_COLOR = "red";

    private static final int LEGEND_HEIGHT = 16;

    private static final double DEFAULT_AREA_THRESHOLD = 0.10;

    private static final String ZONES_MSG = "USING MAP SELECTIONS";

    private static final String EDIT_AREA_MSG = "USING ACTIVE EDIT AREA";

    private List<String> tropicalHaz;

    private int natlBaseETN;

    private Map<String, List<String>> localEffectAreas;

    private Map<String, List<Object>> localAreaData;

    /**
     * Zoom step size.
     */
    private static final double ZOOM_STEP = 0.75;

    private Map<String, List<String>> hazardDict;

    private Text etnSegNumberField;

    private Scale startTimeSlider, endTimeSlider;

    private Label startTimeLabel, endTimeLabel;

    private String gmtPattern = "HH:mm'Z' EEE dd-MMM-yy";

    private SimpleDateFormat dateFormatter;

    private ZoneSelector zoneSelector;

    /**
     * Used by hazard start and end Time.
     * 
     */
    private int toHours = 96;

    private String hazardStartTime;

    private org.eclipse.swt.widgets.List selectedHazardList;

    private Button zoomInButton, zoomOutButton, zoomOneToOneButton,
            clearAllButton, selectAllButton, showMapLabelsButton, runButton,
            runDismissButton, cancelButton;

    /**
     * The Python script to load and run methods from.
     */
    private String defaultHazardType;

    private Map<String, List<String>> mapNames;

    private Group hazardTypeGroup;

    private Group etnSegNumGroup;

    private Group leGroup;

    private Combo leCombo;

    private Label usingLabel;

    private int defaultMapWidth;

    private RGB mapColor;

    private int timeScaleEndTime;

    private double areaThreshold = DEFAULT_AREA_THRESHOLD;

    private TimeRange selectedTimeRange;

    private List<String> defaultAreaList;

    private String defaultHazard;

    private String defaultSegment;

    private DataManager dataManager;

    private List<String> tcmList;

    private String tcmProduct = null;

    private Composite topComp;

    private Canvas excluded;

    private Canvas included;

    private Map<String, Integer> comboDict;

    private String currentHazardType;

    private String hazLocalEffect;

    private boolean running;

    private org.eclipse.swt.widgets.List hazardGroupList;

    public MakeHazardDialog(Shell parent, DataManager dataManager,
            String colorName, int defaultMapWidth, int timeScaleEndTime,
            float areaThreshold, String defaultHazardType,
            Map<String, List<String>> mapNames,
            Map<String, List<String>> hazardDict, List<String> tcmList,
            List<String> tropicalHaz, int natlBaseETN,
            Map<String, List<String>> localEffectAreas,
            Map<String, List<Object>> localAreaData) {

        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK
                | CAVE.NO_PACK);
        this.dataManager = dataManager;
        this.defaultMapWidth = defaultMapWidth;
        this.timeScaleEndTime = timeScaleEndTime;
        this.areaThreshold = areaThreshold;
        this.defaultHazardType = defaultHazardType;
        this.mapNames = mapNames;
        this.hazardDict = hazardDict;
        this.tcmList = tcmList;
        this.tropicalHaz = tropicalHaz;
        this.natlBaseETN = natlBaseETN;
        this.localEffectAreas = localEffectAreas;
        this.localAreaData = localAreaData;
        this.hazLocalEffect = "None";

        try {
            this.mapColor = RGBColors.getRGBColor(colorName);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Invalid mapColor in MakeHazard.py. Using default", e);
            this.mapColor = RGBColors.getRGBColor(DEFAULT_MAP_COLOR);
        }

        this.setText("MakeHazard");

        dateFormatter = new SimpleDateFormat(gmtPattern);
        dateFormatter.setTimeZone(TimeZone.getTimeZone("GMT"));
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
    protected void initializeComponents(Shell shell) {
        // Initialize all of the controls and layouts
        initializeComponents();
        shell.pack();

        this.comboDict = new HashMap<String, Integer>();
        setHazardType(this.defaultHazardType);
        loadInitialData();
        getInitialSelections();
    }

    private void getInitialSelections() {
        selectedTimeRange = getSelectedTimeRange();
        defaultAreaList = getZoneList();
        defaultHazard = getHazard();
        defaultSegment = etnSegNumberField.getText();
    }

    private TimeRange getSelectedTimeRange() {
        Date startTime = null;
        Date endTime = null;
        TimeRange uiValues;

        try {
            startTime = dateFormatter.parse(startTimeLabel.getText());
            endTime = dateFormatter.parse(endTimeLabel.getText());
            uiValues = new TimeRange(startTime, endTime);
        } catch (ParseException e) {
            // If we're here, something is seriously wrong.
            e.printStackTrace();
            uiValues = new TimeRange();
        }

        return uiValues;
    }

    private String getHazard() {
        // get the selected hazard from the hazard list
        String[] hazArray = selectedHazardList.getSelection();
        // Make sure a hazard is selected
        if (hazArray.length != 1) {
            return null;
        }

        return hazArray[0];
    }

    private int getSegment() {
        // get the ETN
        // get the segment #
        String segString = this.etnSegNumberField.getText();
        int segNumber = Integer.MIN_VALUE;

        // Validate the segment number
        segString = segString.trim();
        if (!"".equals(segString)) {
            try {
                segNumber = Integer.parseInt(segString);
                if (segNumber < 1) {
                    throw new NumberFormatException(String.format(
                            "Bad segment number '%s'", segString));
                }
            } catch (NumberFormatException e) {
                return Integer.MIN_VALUE;
            }
        }

        return segNumber;
    }

    /**
     * Try to pre-load the dialog with data from the grid manager.
     */
    private void loadInitialData() {
        IParmManager parmManager = dataManager.getParmManager();
        Parm[] parms = parmManager.getSelectedParms();
        TimeRange timeRange = null;
        Parm parm = null;
        String parmName = null;
        if (parms != null && parms.length == 1) {
            parm = parms[0];
            parmName = parm.getParmID().getParmName();
        }

        // Check for an ETN or segment #
        if (parmName != null && parmName.startsWith("haz")) {

            // DR 5813
            // parmName example: hazXXXnnn
            // where XXX = hazard type, nnn = etn (or segment number)
            int index = parmName.length();
            while (Character.isDigit(parmName.charAt(--index))) {
            }

            if (index < parmName.length() - 1) {
                String etn = parmName.substring(index + 1);
                if (Integer.parseInt(etn) > 0) {
                    this.etnSegNumberField.setText(etn);
                }
            }
        }

        timeRange = determineTimeRange(parm);

        if (parmName == null || !parmName.startsWith("haz")) {
            setTRSliders(timeRange);
            return;
        }

        IGridData[] grids = null;
        if (timeRange != null) {
            grids = parm.getGridInventory(timeRange);
            if (grids == null || grids.length != 1) {
                timeRange = null;
            } else {
                timeRange = grids[0].getGridTime();
            }
        }
        if (timeRange == null) {
            return;
        } else {
            setTRSliders(timeRange);
        }

        String phen_sig = parmName.substring(3, 5) + "." + parmName.charAt(5);
        pickDefaultCategory(phen_sig);
        selectMapFromGrid(grids[0]);
    }

    /**
     * @param timeRange
     */
    private void setTRSliders(TimeRange timeRange) {
        if (timeRange != null) {
            // set the start and end time sliders
            Date zeroDate = null;
            try {
                zeroDate = dateFormatter.parse(hazardStartTime);
            } catch (ParseException e) {
                statusHandler.handle(Priority.EVENTB,
                        "Hazard start time cannot be parsed.", e);
            }

            if (zeroDate != null) {
                long zeroLong = zeroDate.getTime();
                Date startDate = timeRange.getStart();
                Date endDate = timeRange.getEnd();
                long startLong = startDate.getTime();
                long endLong = endDate.getTime();
                int startHour = (int) (startLong - zeroLong) / (60 * 60 * 1000);
                if (startHour < 0) {
                    startHour = 0;
                }
                if (startHour > toHours) {
                    statusHandler.handle(Priority.VERBOSE,
                            "Hazard start time is beyond date slider limit");
                    startHour = toHours;
                }
                startTimeSlider.setSelection(startHour);
                updateTime(startHour, startTimeLabel);

                int endHour = (int) (endLong - zeroLong) / (60 * 60 * 1000);
                if (endHour < startHour + 1) {
                    endHour = startHour + 1;
                }
                if (endHour > toHours + 1) {
                    statusHandler.handle(Priority.VERBOSE,
                            "Hazard end time is beyond date slider limit");
                    endHour = toHours + 1;
                }
                endTimeSlider.setSelection(endHour);
                updateTime(endHour, endTimeLabel);
            }
        }
    }

    /**
     * In AWIPS I, MakeHazard was passed a time range parameter obtained from
     * EditActionProcessor's determineTimeRange(), which is part of the
     * preview() process. Currently, we don't implement preview(), but we
     * probably will at some point in the future. When we do, we can get rid of
     * this method and let preview() do this for us like AWIPS I did.
     * 
     * @param parm
     * @return
     */
    private TimeRange determineTimeRange(Parm parm) {
        Date seTime = dataManager.getSpatialDisplayManager()
                .getSpatialEditorTime();

        TimeRange timeRange = dataManager.getParmOp().getSelectionTimeRange();
        if (timeRange != null && timeRange.isValid()) {
            if (!timeRange.contains(seTime)) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Time range does not intersect Spatial Editor time. "
                                        + "Please set Time so results will be visible.");
            }
        } else {
            timeRange = null;
            if (parm != null) {
                timeRange = parm.getParmState().getSelectedTimeRange();
                if (timeRange != null && !timeRange.isValid()) {
                    timeRange = null;
                }
            }
            if (timeRange == null) {
                if (seTime == null) {
                    timeRange = new TimeRange();
                } else {
                    timeRange = new TimeRange(seTime, 10 * 1000);
                }
            }
        }

        if (timeRange != null && parm != null) {
            IGridData[] grids = parm.getGridInventory(timeRange);
            if (grids != null && grids.length > 1) {
                // There should be a yes/no dialog pop up...
            }
        }

        return timeRange;
    }

    /**
     * Figure out the hazard type (i.e., "Winter Weather") and db tables to use,
     * based on phen_sig. The same phensig sometimes appears in multiple hazard
     * types, so pick the one that draws from the most db tables.
     * 
     * @param phen_sig
     */
    protected void pickDefaultCategory(String phen_sig) {
        String hazardType = null;
        List<String> hazTables = null;
        Map<String, List<String>> hazDict = getHazardsDictionary();
        for (Map.Entry<String, List<String>> entry : hazDict.entrySet()) {
            List<String> phenSigsForType = entry.getValue();
            if (phenSigsForType != null && phenSigsForType.contains(phen_sig)) {
                String entryHazardType = entry.getKey();
                List<String> entryHazTables = mapNames.get(entryHazardType);
                if (hazTables == null
                        || hazTables.size() < entryHazTables.size()) {
                    hazardType = entryHazardType;
                    hazTables = entryHazTables;
                }
            }
        }

        // Set the radio button and selection list entry
        if (hazardType != null) {
            setHazardType(hazardType);
            setHazard(phen_sig);
        }
    }

    /**
     * Initialize the controls on the display.
     */
    private void initializeComponents() {
        topComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        topComp.setLayout(gl);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        topComp.setLayoutData(gd);

        Composite mapComp = new Composite(topComp, SWT.BORDER);
        createMapComponent(mapComp);

        Composite hazardComp = new Composite(topComp, SWT.NONE);
        createSelectHazardComponent(hazardComp);

        Composite timeComp = new Composite(topComp, SWT.NONE);
        createTimeSelectComponent(timeComp);
        createLocalEffectComponent(timeComp);
        createButtons();
    }

    /**
     * Create the hazard in response to the run or run/dismiss button. Indicate
     * whether the run succeeded so run/dismiss knows whether it's OK to close
     * the dialog.
     * 
     * @return true if the hazard was created, false otherwise.
     */
    private boolean doRunInternal(boolean dismiss) {

        // Obtain information from the controls
        TimeRange timeRange = getSelectedTimeRange();
        if (!timeRange.isValid()) {
            return false;
        }

        // get the zones selected
        List<String> zoneList = getZoneList();

        // Validate the control inputs

        // get the selected hazard from the hazard list
        String hazard = getHazard();
        // Make sure a hazard is selected
        if (hazard == null) {
            StatusStore.updateStatus(StatusConstants.CATEGORY_GFE,
                    "You must select a hazard", Importance.SIGNIFICANT);
            return false;
        }

        String tropicalETN = null;

        // get the tropical ETN
        if (tcmProduct != null) {
            tropicalETN = tcmETNforTrop(tcmProduct);
        }

        // get the segment #
        String phenSig = hazard.substring(0, 4);
        int segNum = getSegment();
        String segmentNumber = "";
        // Validate the segment number
        if ((this.tropicalHaz.contains(phenSig))
                && !(this.dataManager.getSiteID().equals("GUM"))) {
            // if ETN is already correctly assigned, use it
            if (segNum >= this.natlBaseETN) {
                segmentNumber = Integer.toString(segNum);
            } else if ((etnSegNumberField.getText().length() > 0)
                    && (segNum < this.natlBaseETN)) {
                // if there is a number in the box, but it is not correct, send
                // message
                String msg = "Must use national tropical hazard ETN. Remove number\n"
                        + "from the ETN box and choose the appropriate TCM to get the ETN.";
                StatusStore.updateStatus(StatusConstants.CATEGORY_GFE, msg,
                        Importance.SIGNIFICANT);
                return false;
            } else if ((etnSegNumberField.getText().length() == 0)
                    && (tropicalETN != null)) {
                // If there is no number assigned and they have chosen a TCM,
                // use that
                segmentNumber = tropicalETN;
                // LogStream.logUse("ETN from TCM was:", tropicalETN)
            } else {
                // If we got here, then no TCM was chosen nor a national ETN
                // otherwise assigned. Send message.
                String msg = "Must use national tropical hazard ETN. \n"
                        + "Choose the appropriate TCM to get the ETN when creating the grid.";
                StatusStore.updateStatus(StatusConstants.CATEGORY_GFE, msg,
                        Importance.SIGNIFICANT);
                return false;
            }

        } else {
            if (segNum > 0) {
                segmentNumber = Integer.toString(segNum);
            }
        }

        // We need to validate that there are points selected. However, if there
        // are no zones selected, it may be because we are creating a hazard for
        // an edit area instead of zones.
        // The latter is much easier to check in Python, so the validation is
        // done in the script.

        // Use the validated inputs

        // Put arguments in a map to pass to the script.
        Map<String, Object> argmap = new HashMap<String, Object>();
        argmap.put("selectedHazard", hazard);
        argmap.put("timeRange", timeRange);
        argmap.put("areaList", zoneList);
        argmap.put("segmentNumber", segmentNumber);
        argmap.put("selectedTimeRange", selectedTimeRange);
        argmap.put("defaultAreaList", defaultAreaList);
        argmap.put("defaultHazard", defaultHazard);
        argmap.put("defaultSegment", defaultSegment);
        argmap.put("hazLocalEffect", hazLocalEffect);
        argmap.put("dismiss", dismiss);

        setReturnValue(argmap);
        running = false;

        return true;
    }

    private String tcmETNforTrop(String tcmProductId) {
        System.out.println("chosen TCM is " + tcmProductId);

        String[] tcmProduct = getTextProductFromDB(tcmProductId);
        String tropicalETN = null;
        if (tcmProduct.length < 3) {
            String msg = tcmProductId
                    + " could not be retrieved from the text database.";
            statusHandler.handle(Priority.SIGNIFICANT, msg);

            // Just return if no TCM is found. Something's really wrong
            return null;
        } else {
            String altFileName = getAltInfoFilename(tcmProduct);
            String stormNum = altFileName.substring(2, 4);

            System.out.println("storm number is " + stormNum);

            String nationalBase = "10";
            tropicalETN = nationalBase + stormNum;

            System.out.println("Tropical ETN is: " + tropicalETN);
            System.out.println("length of tropical ETN is: "
                    + tropicalETN.length());
        }
        return tropicalETN;
    }

    private String getAltInfoFilename(String[] tcmProduct) {
        String altFilename = null;
        for (int i = 0; i < tcmProduct.length; i++) {
            if (tcmProduct[i].contains("NATIONAL HURRICANE CENTER")) {
                String[] parts = tcmProduct[i].split("\\s");
                altFilename = parts[parts.length - 1];
                break;
            }
        }
        return altFilename;
    }

    private String[] getTextProductFromDB(String productID) {
        PythonPreferenceStore prefStore = Activator.getDefault()
                .getPreferenceStore();
        boolean testVtec;
        if (prefStore.contains("TestVTECDecode")) {
            testVtec = prefStore.getBoolean("TestVTECDecode");
        } else {
            testVtec = false;
        }
        boolean gfeMode = (this.dataManager.getOpMode()
                .equals(CAVEMode.OPERATIONAL));
        boolean opMode = testVtec || gfeMode;
        String fullText = TextDBUtil.retrieveProduct(productID, opMode);
        String[] textList = fullText.split("\n");
        return textList;
    }

    private List<String> getZoneList() {
        List<List<String>> zoneGroupings = zoneSelector.getZoneGroupings();
        List<String> zoneList = Collections.emptyList();
        if (zoneGroupings.size() > 0) {
            zoneList = zoneGroupings.get(0);
        }
        return zoneList;
    }

    private void createButtons() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite buttons = new Composite(shell, SWT.NONE);
        buttons.setLayoutData(gd);
        FillLayout fillLayout = new FillLayout();
        fillLayout.spacing = 5;
        buttons.setLayout(fillLayout);
        runButton = new Button(buttons, SWT.PUSH);
        runButton.setText("Run");
        runButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                doRunInternal(false);
            }
        });

        runDismissButton = new Button(buttons, SWT.PUSH);
        runDismissButton.setText("Run/Dismiss");
        runDismissButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                doRunInternal(true);
            }
        });

        cancelButton = new Button(buttons, SWT.PUSH);
        cancelButton.setText("Cancel");
        cancelButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                running = false;
            }
        });
    }

    private void createMapComponent(Composite mapComp) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        mapComp.setLayout(gl);
        mapComp.setLayoutData(gd);
        Composite theMapComposite = new Composite(mapComp, SWT.NONE);

        gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        theMapComposite.setLayout(gl);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.minimumHeight = 100;
        gd.minimumWidth = 100;
        gd.widthHint = this.defaultMapWidth;
        theMapComposite.setLayoutData(gd);
        try {
            initializeShapeComponent(theMapComposite);
        } catch (Exception e) {
            statusHandler.handle(Priority.SIGNIFICANT, e.getLocalizedMessage(),
                    e);
        }

        Composite buttonComp = new Composite(mapComp, SWT.NONE);
        GridLayout layout = new GridLayout(4, false);
        buttonComp.setLayout(layout);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        buttonComp.setLayoutData(layoutData);

        Composite comp = new Composite(buttonComp, SWT.NONE);
        layout = new GridLayout(1, true);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        comp.setLayout(layout);
        layoutData = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        comp.setLayoutData(layoutData);

        zoomInButton = new Button(comp, SWT.PUSH);
        zoomInButton.setText("Zoom In");
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        zoomInButton.setLayoutData(layoutData);

        zoomOutButton = new Button(comp, SWT.PUSH);
        zoomOutButton.setText("Zoom Out");
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        zoomOutButton.setLayoutData(layoutData);

        zoomOneToOneButton = new Button(comp, SWT.PUSH);
        zoomOneToOneButton.setText("Zoom 1:1");
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        zoomOneToOneButton.setLayoutData(layoutData);

        comp = new Composite(buttonComp, SWT.NONE);
        layout = new GridLayout(1, true);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        comp.setLayout(layout);
        layoutData = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        comp.setLayoutData(layoutData);

        clearAllButton = new Button(comp, SWT.PUSH);
        clearAllButton.setText("Clear All");
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        clearAllButton.setLayoutData(layoutData);

        selectAllButton = new Button(comp, SWT.PUSH);
        selectAllButton.setText("Select All");
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        selectAllButton.setLayoutData(layoutData);

        comp = new Composite(buttonComp, SWT.NONE);
        layout = new GridLayout(1, false);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        comp.setLayout(layout);
        layoutData = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        comp.setLayoutData(layoutData);

        showMapLabelsButton = new Button(comp, SWT.CHECK);
        showMapLabelsButton.setText("Map Labels");

        comp = new Composite(buttonComp, SWT.NONE);
        layout = new GridLayout(1, false);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        comp.setLayout(layout);
        layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        comp.setLayoutData(layoutData);

        excluded = new Canvas(comp, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = LEGEND_HEIGHT;
        excluded.setLayoutData(gd);
        excluded.addPaintListener(new PaintListener() {

            @Override
            public void paintControl(PaintEvent e) {
                paintLegend(e.gc, zoneSelector.getNoZoneColor(),
                        "Zones not included");
            }
        });

        included = new Canvas(comp, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = 16;
        included.setLayoutData(gd);
        included.addPaintListener(new PaintListener() {

            @Override
            public void paintControl(PaintEvent e) {
                paintLegend(e.gc, mapColor, "Zones included");
            }
        });

        showMapLabelsButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                zoneSelector.setLabelZones(((Button) e.getSource())
                        .getSelection());
            }
        });

        selectAllButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                Map<String, Integer> comboDict = new HashMap<String, Integer>();
                for (String zoneName : zoneSelector.getZoneNames()) {
                    comboDict.put(zoneName, 1);
                }
                zoneSelector.updateCombos(comboDict);
                zoneSelected(null);
            }
        });

        clearAllButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                zoneSelector.updateCombos(new HashMap<String, Integer>());
                zoneSelected(null);
            }
        });

        zoomInButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                zoneSelector.setZoomLevel(zoneSelector.getZoomLevel()
                        * ZOOM_STEP);
            }
        });

        zoomOutButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                zoneSelector.setZoomLevel(zoneSelector.getZoomLevel()
                        / ZOOM_STEP);
            }
        });

        zoomOneToOneButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                // reset the display to fully zoomed extent
                zoneSelector.setZoomLevel(1);
            }
        });

        usingLabel = new Label(buttonComp, SWT.NORMAL);
        usingLabel.setAlignment(SWT.CENTER);
        layoutData = new GridData(SWT.CENTER, SWT.CENTER, true, false);
        layoutData.horizontalSpan = 4;
        usingLabel.setLayoutData(layoutData);
        usingLabel.setText(EDIT_AREA_MSG);
    }

    private void paintLegend(GC gc, RGB color, String text) {
        Color bg = new Color(getDisplay(), color);
        gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        gc.setBackground(bg);
        int height = gc.getFontMetrics().getHeight();
        gc.fillRectangle(height, 0, height, height);
        gc.drawRectangle(height, 0, height, height);

        gc.setBackground(CAVEMode.getBackgroundColor());
        gc.setForeground(CAVEMode.getForegroundColor());
        gc.drawText(text, height * 2
                + gc.getFontMetrics().getAverageCharWidth(), 0);
        bg.dispose();
    }

    private void createSelectHazardComponent(Composite hazardComp) {
        GridLayout gl = new GridLayout(2, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        hazardComp.setLayout(gl);
        GridData gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        hazardComp.setLayoutData(gd);

        Group hazardGroup = new Group(hazardComp, SWT.BORDER);
        hazardGroup.setText("Select Hazard");
        gl = new GridLayout(1, true);
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        hazardGroup.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.horizontalSpan = 2;
        hazardGroup.setLayoutData(gd);

        selectedHazardList = new org.eclipse.swt.widgets.List(hazardGroup,
                SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL | SWT.SINGLE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = selectedHazardList.getItemHeight() * 16
                + selectedHazardList.getBorderWidth();
        selectedHazardList.setLayoutData(gd);

        hazardTypeGroup = new Group(hazardComp, SWT.BORDER);
        hazardTypeGroup.setText("Hazard Type");
        gl = new GridLayout(1, true);
        gl.verticalSpacing = 0;
        hazardTypeGroup.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        hazardTypeGroup.setLayoutData(gd);

        etnSegNumGroup = new Group(hazardComp, SWT.BORDER);
        etnSegNumGroup.setText("ETN/Segment Number");
        etnSegNumGroup.setLayout(new GridLayout(1, true));
        etnSegNumGroup.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                true));

        List<String> groups = new ArrayList<String>(getHazardsDictionary()
                .keySet());
        this.currentHazardType = "";

        SelectionAdapter selAdapt = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                org.eclipse.swt.widgets.List list = (org.eclipse.swt.widgets.List) e
                        .getSource();
                String[] sel = list.getSelection();
                if (sel.length > 0) {
                    setHazardType(sel[0]);
                }
            }
        };

        hazardGroupList = new org.eclipse.swt.widgets.List(hazardTypeGroup,
                SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL | SWT.SINGLE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = hazardGroupList.getItemHeight()
                * Math.min(12, groups.size())
                + hazardGroupList.getBorderWidth();
        hazardGroupList.setLayoutData(gd);
        hazardGroupList.addSelectionListener(selAdapt);
        for (String k : groups) {
            hazardGroupList.add(k);
            if (k.equals(this.defaultHazardType)) {
                hazardGroupList.select(hazardGroupList.getItemCount() - 1);
            }
        }

        etnSegNumberField = new Text(etnSegNumGroup, SWT.BORDER);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        etnSegNumberField.setTextLimit(4);
        GC gc = new GC(etnSegNumberField);
        gd.widthHint = 4 * gc.getCharWidth('0');
        gc.dispose();
        etnSegNumberField.setLayoutData(gd);
        etnSegNumberField.addVerifyListener(new VerifyListener() {

            @Override
            public void verifyText(VerifyEvent e) {
                for (char c : e.text.toCharArray()) {
                    if ((!Character.isDigit(c)) && (!Character.isISOControl(c))) {
                        e.doit = false;
                        break;
                    }
                }
            }
        });

        if (!tcmList.isEmpty()) {
            Composite tcmFrameComp = new Composite(etnSegNumGroup, SWT.NONE);
            tcmFrameComp.setLayout(new GridLayout(1, true));
            tcmFrameComp.setLayoutData(new GridData(GridData.FILL, SWT.BOTTOM,
                    true, false));
            for (String s : tcmList) {
                Button radioButton = new Button(tcmFrameComp, SWT.RADIO);
                radioButton.setText(s);
                radioButton.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent event) {
                        Button b = ((Button) (event.getSource()));
                        if (b.getSelection()) {
                            tcmProduct = ((Button) (event.getSource()))
                                    .getText();
                        }
                    }
                });
            }
        }
    }

    /**
     * Creates a pair of sliders in the sash form for user to choose the start
     * and end times of the hazard that is being created. The sliders are set up
     * to require hazards to start and end on an hour boundary, and have a
     * duration of at least an hour.
     */
    private void createTimeSelectComponent(Composite timeComp) {
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        timeComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, false, true);
        timeComp.setLayoutData(gd);

        Group startGroup = new Group(timeComp, SWT.BORDER);
        startGroup.setText("Hazard Start Time");
        gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        startGroup.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.FILL, true, false);
        startGroup.setLayoutData(gd);

        // create a label to show the start time
        startTimeLabel = new Label(startGroup, SWT.CENTER);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        startTimeLabel.setLayoutData(gd);

        // create the start time slider
        startTimeSlider = new Scale(startGroup, SWT.HORIZONTAL);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.minimumWidth = 200;
        startTimeSlider.setLayoutData(gd);
        startTimeSlider.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                // Not called for sliders
            }

            /**
             * Event handler for when the user moves the slider. This updates
             * the slider label and changes the end time slider if the end time
             * is before the start time.
             * 
             * @param e
             *            The event that caused this handler to be called.
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateTime(startTimeSlider.getSelection(), startTimeLabel);
                if (startTimeSlider.getSelection() >= endTimeSlider
                        .getSelection()) {
                    endTimeSlider.setSelection(startTimeSlider.getSelection() + 1);
                    updateTime(endTimeSlider.getSelection(), endTimeLabel);
                }
            }
        });
        startTimeSlider.setMinimum(0);
        startTimeSlider.setMaximum(this.timeScaleEndTime);
        startTimeSlider.setIncrement(1);
        startTimeSlider.setPageIncrement(1);

        // Force start time to an hourly boundary
        Calendar cal = Calendar.getInstance();
        cal.setTime(SimulatedTime.getSystemTime().getTime());
        cal.set(Calendar.MINUTE, 0);
        hazardStartTime = dateFormatter.format(cal.getTime());

        updateTime(0, startTimeLabel);

        Group endGroup = new Group(timeComp, SWT.BORDER);
        endGroup.setText("Hazard End Time");
        gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        endGroup.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.FILL, true, false);
        endGroup.setLayoutData(gd);

        // Create a label to show the user the hazard end time
        endTimeLabel = new Label(endGroup, SWT.CENTER);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        endTimeLabel.setLayoutData(gd);
        updateTime(1, endTimeLabel);

        // Create the end time slider
        endTimeSlider = new Scale(endGroup, SWT.HORIZONTAL);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.minimumWidth = 200;
        endTimeSlider.setLayoutData(gd);

        endTimeSlider.setMinimum(1);
        endTimeSlider.setMaximum(toHours + 1);
        endTimeSlider.setIncrement(1);
        endTimeSlider.setPageIncrement(1);
        endTimeSlider.setSelection(1);
        endTimeSlider.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                // Not called for sliders
            }

            /**
             * Event handler for when the user moves the slider. This updates
             * the slider label and changes the start time slider if the end
             * time is before the start time.
             * 
             * @param e
             *            The event that caused this handler to be called.
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateTime(endTimeSlider.getSelection(), endTimeLabel);
                if (endTimeSlider.getSelection() <= startTimeSlider
                        .getSelection()) {
                    startTimeSlider.setSelection(endTimeSlider.getSelection() - 1);
                    updateTime(startTimeSlider.getSelection(), startTimeLabel);
                }
            }
        });

    }

    private void createLocalEffectComponent(Composite comp) {
        this.leGroup = new Group(comp, SWT.BORDER);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 0;
        leGroup.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        leGroup.setLayoutData(gd);
        leGroup.setText("Local Effect Area");

        SelectionAdapter selAdapt = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                Combo c = (Combo) e.getSource();
                hazardLocalEffectSelected((String) c.getData(c.getText()));
            }
        };

        leCombo = new Combo(leGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        leCombo.setLayoutData(gd);
        leCombo.addSelectionListener(selAdapt);

        GC gc = new GC(this.getDisplay());
        String longest = "";
        int widest = 0;
        for (List<String> list : localEffectAreas.values()) {
            for (String s : list) {
                int width = gc.stringExtent(s).x;
                if (width > widest) {
                    widest = width;
                    longest = s;
                }
            }
        }
        gc.dispose();
        leCombo.add(longest);
    }

    private Map<String, List<String>> getHazardsDictionary() {
        return hazardDict;
    }

    private List<String> getHazardListForType(String key) {

        Map<String, List<String>> hazardDict = getHazardsDictionary();
        if (hazardDict.containsKey(key)) {
            return hazardDict.get(key);
        }
        return null;
    }

    private void updateSelectedHazardList(String hazType) {
        String prevType = this.currentHazardType;
        this.currentHazardType = hazType;
        String siteId = dataManager.getSiteID();
        DiscreteDefinition discreteDef = DiscreteKey.discreteDefinition(siteId);

        java.util.List<String> list = null;
        if ((list = getHazardListForType(hazType)) != null) {
            selectedHazardList.removeAll();
            for (String s : list) {
                String sdesc = discreteDef.keyDesc("Hazards_SFC", s);
                if (sdesc.isEmpty()) {
                    if ("CF.S".equals(s)) {
                        sdesc = "COASTAL FLOOD STATEMENT";
                    } else if ("LS.S".equals(s)) {
                        sdesc = "LAKESHORE FLOOD STATEMENT";
                    } else if ("MA.S".equals(s)) {
                        sdesc = "MARINE WEATHER STATEMENT";
                    } else if ("HU.S".equals(s)) {
                        sdesc = "TROPICAL CYCLONE LOCAL STATEMENT";
                    } else {
                        sdesc = "UNKNOWN PHENOMENON.SIGNIFICANCE";
                    }
                }
                selectedHazardList.add(s + " -- " + sdesc);
            }
            selectedHazardList.select(0);
            selectedHazardList.deselect(0);
        }

        List<String> mapNames = this.mapNames.get(hazType);

        if (mapNames == null) {
            statusHandler.error("MakeHazardConfig.mapNames has no entry for \""
                    + hazType + "\"");
            return;
        }

        if (!prevType.equals(hazType)
                && !mapNames.equals(this.mapNames.get(prevType))) {
            this.zoneSelector.setMap(mapNames, this.comboDict,
                    Arrays.asList(mapColor));
        }
    }

    /**
     * Add <code>selection</code> hours to <code>hazardStartTime</code> (that
     * is, the default starting time), and set the text of <code>label</code> to
     * the result, formatted by <code>dateFormatter</code>.
     * 
     * @param selection
     *            The integer slider value selected by the user.
     * @param label
     *            The label whose text should be changed.
     */
    private void updateTime(int selection, Label label) {
        try {
            Date theDate = dateFormatter.parse(hazardStartTime);
            Calendar cal = Calendar.getInstance();
            cal.setTime(theDate);
            cal.add(Calendar.HOUR_OF_DAY, selection);
            label.setText(dateFormatter.format(cal.getTime()));
        } catch (ParseException e) {
            // This shouldn't ever happen...
            statusHandler.handle(Priority.ERROR, "Invalid hazardStartTime", e);
        }

    }

    private void initializeShapeComponent(Composite controlComp)
            throws TransformException, FactoryException, VizException {

        GridLocation gloc = dataManager.getParmManager()
                .compositeGridLocation();
        zoneSelector = new ZoneSelector(controlComp, gloc, null, this);
        zoneSelector.setLimitToOneGroup(true);
    }

    /**
     * Set the hazard type in the radio button control. If the hazard type is
     * changed, the radio button selection listener will fire, changing and
     * clearing selectedHazardList as a side effect.
     * 
     * @param hazardType
     *            the hazard type to select.
     */
    public void setHazardType(String hazardType) {
        hazardGroupList.setSelection(hazardGroupList.indexOf(hazardType));
        updateSelectedHazardList(hazardType);

        if (this.localEffectAreas.containsKey(hazardType)) {
            leGroup.setVisible(true);
            ((GridData) leGroup.getLayoutData()).exclude = false;

            leCombo.removeAll();
            leCombo.add("None");
            leCombo.setText("None");
            for (String eaName : this.localEffectAreas.get(hazardType)) {
                List<Object> laData = this.localAreaData.get(eaName);
                if (laData != null) {
                    String label = eaName;
                    String display = (String) laData.get(1);
                    if (display != null && !display.isEmpty()) {
                        label = display;
                    }
                    leCombo.add(label);
                    leCombo.setData(label, eaName);
                } else {
                    statusHandler.error("No entry found for '" + eaName
                            + "' in localAreaData");
                }
            }
        } else {
            leGroup.setVisible(false);
            ((GridData) leGroup.getLayoutData()).exclude = true;
            this.hazLocalEffect = "None";
            String s = etnSegNumberField.getText();
            for (Entry<String, List<Object>> entry : localAreaData.entrySet()) {
                if (s.equals(entry.getValue().get(0))) {
                    this.etnSegNumberField.setText("");
                    this.etnSegNumberField.setSelection(0);
                    break;
                }
            }
        }
    }

    /**
     * Set the selection in selectedHazardList to the first item that starts
     * with phen_sig. If phen_sig is null or an empty string, clears all
     * selections.
     * 
     * @param phen_sig
     *            The phen_sig to select
     */
    public void setHazard(String phen_sig) {
        if (phen_sig == null || phen_sig.length() == 0) {
            selectedHazardList.deselectAll();
            return;
        }

        String[] items = selectedHazardList.getItems();
        int index = 0;
        for (String item : items) {
            if (item.startsWith(phen_sig)) {
                selectedHazardList.select(index);
                break;
            }
            index++;
        }
    }

    private void hazardLocalEffectSelected(String le) {
        this.hazLocalEffect = le;
        List<Object> laData = this.localAreaData.get(le);
        if (laData != null) {
            // get the segment number
            Integer segment = (Integer) laData.get(0);
            if (segment != null) {
                String segText = segment.toString();
                this.etnSegNumberField.setText(segText);
                this.etnSegNumberField.setSelection(segText.length());

            } else {
                String s = etnSegNumberField.getText();
                for (Entry<String, List<Object>> entry : localAreaData
                        .entrySet()) {
                    if (s.equals(entry.getValue().get(0))) {
                        this.etnSegNumberField.setText("");
                        this.etnSegNumberField.setSelection(0);
                        break;
                    }
                }
            }

            @SuppressWarnings("unchecked")
            List<String> zoneList = (List<String>) laData.get(2);
            if (zoneList != null && !zoneList.isEmpty()) {
                Map<String, Integer> comboDict = new HashMap<String, Integer>(
                        zoneList.size());
                for (String z : zoneList) {
                    comboDict.put(z, 1);
                }
                this.zoneSelector.updateCombos(comboDict);
            }
        }
    }

    /**
     * @param grid
     *            The parm grid to select from
     */
    private void selectMapFromGrid(IGridData grid) {
        IGridSlice slice = grid.getGridSlice();
        if (slice instanceof DiscreteGridSlice) {
            // Get the byte grid out of the grid
            DiscreteGridSlice dslice = (DiscreteGridSlice) slice;
            Grid2DByte byteGrid = dslice.getDiscreteGrid();

            // Make a bit grid from the byte grid.
            // Try to avoid copying.
            // (ReferenceData ctor will copy, so copy here is redundant)
            Grid2DBit grid2DBit = null;
            if (byteGrid instanceof Grid2DBit) {
                grid2DBit = (Grid2DBit) byteGrid;
            } else {
                int xDim = byteGrid.getXdim();
                int yDim = byteGrid.getYdim();
                ByteBuffer byteBuf = byteGrid.getBuffer();
                grid2DBit = new Grid2DBit(xDim, yDim, byteBuf);
            }

            List<String> zoneIdsToShow = getAreaList(grid2DBit);

            // Select the zones
            if (zoneIdsToShow != null && zoneIdsToShow.size() > 0) {
                usingLabel.setText(ZONES_MSG);
                for (String z : zoneIdsToShow) {
                    this.comboDict.put(z, 1);
                }
                zoneSelector.updateCombos(this.comboDict);
            } else {
                usingLabel.setText(EDIT_AREA_MSG);
            }
        } else {
            IllegalArgumentException iae = new IllegalArgumentException(
                    "Grid parameter must be an instance of DiscreteGridSlice.");
            iae.fillInStackTrace();
            throw iae;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.ui.zoneselector.ZoneSelector.IZoneSelectionListener
     * #zonesSelected()
     */
    @Override
    public void zoneSelected(String zone) {
        Set<String> selectedZones = zoneSelector.getCombos().keySet();
        if (selectedZones.size() > 0) {
            usingLabel.setText(ZONES_MSG);
        } else {
            usingLabel.setText(EDIT_AREA_MSG);
        }
        usingLabel.pack(true);
    }

    /**
     * Defend against bad values for areaThreshold in config file.
     */
    protected void initAreaThreshold() {
        if (this.areaThreshold > 1.0) {
            // This wouldn't select any zones
            statusHandler.handle(Priority.WARN,
                    "Area threshold is greater than 100%. Using default.");
            this.areaThreshold = DEFAULT_AREA_THRESHOLD;
        } else if (this.areaThreshold <= 0.0) {
            // This would always select all zones
            statusHandler.handle(Priority.WARN,
                    "Area threshold is 0% or below. Using default.");
            this.areaThreshold = DEFAULT_AREA_THRESHOLD;
        }
    }

    /**
     * @param grid2dBit
     * @return
     */
    private List<String> getAreaList(Grid2DBit mask) {
        IReferenceSetManager rm = this.dataManager.getRefManager();

        List<String> areas = this.zoneSelector.getZoneNames();
        List<String> areaList = new ArrayList<String>();

        for (String area : areas) {
            ReferenceID refId = new ReferenceID(area);
            ReferenceData refData = rm.loadRefSet(refId);
            Grid2DBit editAreaMask = refData.getGrid();
            Grid2DBit intersect = editAreaMask.and(mask);
            float ratio = (float) intersect.numberOfBitsSet()
                    / (float) editAreaMask.numberOfBitsSet();
            if (ratio >= areaThreshold) {
                areaList.add(refData.getId().getName());
            }
        }

        return areaList;
    }

    public void openFromPython() {
        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                open();
            }
        });

    }

    public Object runFromPython() {
        final Object[] retVal = new Object[1];

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                running = true;
                setReturnValue(null);
                while (running) {
                    if (!getDisplay().readAndDispatch()) {
                        getDisplay().sleep();
                    }
                }
                retVal[0] = getReturnValue();
            }
        });

        return retVal[0];
    }

    public void closeFromPython() {
        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                close();
            }
        });
    }

    @SuppressWarnings("unchecked")
    static public MakeHazardDialog createFromPython(Map<String, Object> args) {

        final DataManager dataManager = (DataManager) args.get("dataManager");
        final String colorName = (String) args.get("mapColor");
        final int defaultMapWidth = (Integer) args.get("defaultMapWidth");
        final int timeScaleEndTime = (Integer) args.get("timeScaleEndTime");
        final float areaThreshold = (Float) args.get("areaThreshold");
        final String defaultHazardType = (String) args.get("defaultHazardType");
        final Map<String, List<String>> mapNames = (Map<String, List<String>>) args
                .get("mapNames");
        final Map<String, List<String>> hazardDict = (Map<String, List<String>>) args
                .get("hazardDict");
        final List<String> tcmList = (List<String>) args.get("tcmList");
        final List<String> tropicalHaz = (List<String>) args.get("tropicalHaz");
        final int natlBaseETN = (Integer) args.get("natlBaseETN");
        final Map<String, List<String>> localEffectAreas = (Map<String, List<String>>) args
                .get("localEffectAreas");
        final Map<String, List<Object>> localAreaData = (Map<String, List<Object>>) args
                .get("localAreaData");

        final MakeHazardDialog[] dlg = new MakeHazardDialog[1];
        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                Shell shell = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell();
                dlg[0] = new MakeHazardDialog(shell, dataManager, colorName,
                        defaultMapWidth, timeScaleEndTime, areaThreshold,
                        defaultHazardType, mapNames, hazardDict, tcmList,
                        tropicalHaz, natlBaseETN, localEffectAreas,
                        localAreaData);
            }
        });
        return dlg[0];
    }

    public void setSegmentNumber(final int segNum) {
        Job updateSegNumField = new UIJob("Updating ETN/segment number.") {

            @Override
            public IStatus runInUIThread(IProgressMonitor monitor) {
                if ((!MakeHazardDialog.this.etnSegNumberField.isDisposed())
                        && (MakeHazardDialog.this.etnSegNumberField != null)) {
                    MakeHazardDialog.this.etnSegNumberField.setText(Integer
                            .toString(segNum));
                    return Status.OK_STATUS;
                }

                return Status.CANCEL_STATUS;
            }
        };
        updateSegNumField.setSystem(true);
        updateSegNumField.schedule();
    }

    @Override
    protected void disposed() {
        this.running = false;
        super.disposed();
    }

}
