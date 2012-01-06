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
package com.raytheon.viz.ghg.makehazard;

import static com.raytheon.viz.ghg.constants.StatusConstants.CATEGORY_GHG;

import java.io.File;
import java.nio.ByteBuffer;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeMap;

import jep.JepException;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
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
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.IReferenceSetManager;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.ui.zoneselector.ZoneSelector;
import com.raytheon.viz.gfe.ui.zoneselector.ZoneSelector.IZoneSelectionListener;
import com.raytheon.viz.ghg.Activator;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.statusline.StatusMessage.Importance;
import com.raytheon.viz.ui.statusline.StatusStore;

/**
 * Make Hazard Dialog
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	Jun 5, 2008				Eric Babin Initial Creation
 *  Sep 27,2010 5813        gzhou       get etn from param pattern hazXXXnnn
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class MakeHazardDialog extends CaveSWTDialog implements
        SelectionListener, IZoneSelectionListener {
    private static final int LEGEND_HEIGHT = 16;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MakeHazardDialog.class);

    private static final double DEFAULT_AREA_THRESHOLD = 0.1;

    private static final String ZONES_MSG = "USING MAP SELECTIONS";

    private static final String EDIT_AREA_MSG = "USING ACTIVE EDIT AREA";

    private static final String JEP_SEPARATOR = ":";

    private static final Set<String> TROPICAL_HAZ = new HashSet<String>(
            Arrays.asList("HU.W", "HU.A", "HU.S", "TR.W", "TR.A"));

    private static final int NATL_BASE_ETN = 1001;

    /**
     * Zoom step size.
     */
    private static final double ZOOM_STEP = 0.75;

    private Map<String, List<String>> hazardMap;

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
    private PythonScript makeHazardScript;

    private String defaultHazardType;

    private Map<String, List<String>> mapNames;

    private Group radioGroup;

    private Group etnSegNumGroup;

    private Label usingLabel;

    private String defaultMapWidth;

    private RGB mapColor;

    private String timeScaleEndTime;

    private String areaThresholdStr;

    private double areaThreshold = DEFAULT_AREA_THRESHOLD;

    private String includePath;

    private TimeRange selectedTimeRange;

    private List<String> defaultAreaList;

    private String defaultHazard;

    private String defaultSegment;

    private DataManager dataManager;

    private List<String> tcmList;

    private String tcmProduct = null;

    private PythonScript tcmDecoder;

    private Composite topComp;

    private Canvas excluded;

    private Canvas included;

    private Map<String, Integer> comboDict;

    private String currentHazardType;

    public MakeHazardDialog(Shell parent, DataManager dataManager) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE);
        this.dataManager = dataManager;
        setText("MakeHazard");

        dateFormatter = new SimpleDateFormat(gmtPattern);
        dateFormatter.setTimeZone(TimeZone.getTimeZone("GMT"));

        initializePython();
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

        this.comboDict = new HashMap<String, Integer>();
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
        String hazard = hazArray[0].split(" ", 2)[0];

        return hazard;
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

    @Override
    protected boolean shouldOpen() {
        return ensureSeparated();
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

            int pos = parmName.indexOf(':');
            if (pos >= 0) {
                String etn = parmName.substring(pos + 1);
                this.etnSegNumberField.setText(etn);
            } else {
                // DR 5813
                // parmName example: hazXXXnnn where XXX = hazard type, nnn =
                // etn)
                // or segment number)
                int index = 0;
                for (index = parmName.length() - 1; index > 0; index--) {
                    if (!Character.isLetter(parmName.charAt(index))) {
                        continue;
                    } else {
                        break;
                    }
                }

                if (index < parmName.length() - 1) {
                    String etn = parmName.substring(index + 1);
                    try {
                        if (etn != null && etn.trim().length() > 0
                                && Integer.parseInt(etn) > 0) {
                            this.etnSegNumberField.setText(etn);
                        }
                    } catch (NumberFormatException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Hazard ETN/segment number INVALID.", e);
                    }
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

        // Check for an ETN or segment #
        int pos = parmName.indexOf(':');
        if (pos >= 0) {
            String etn = parmName.substring(pos + 1);
            this.etnSegNumberField.setText(etn);
        }

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
     * Create a python interpreter and load a script, the methods of which we
     * will invoke later.
     */
    @SuppressWarnings("unchecked")
    private void initializePython() {
        ResourceBundle bundle = ResourceBundle
                .getBundle("com.raytheon.viz.ghg.rsc.MakeHazard");

        String bdlIncludePath = bundle.getString("includePath");

        IPathManager pathMgr = PathManagerFactory.getPathManager();

        // Expand each component in includePath into a full path name for Jep
        String[] subPaths = bdlIncludePath.split(JEP_SEPARATOR);
        StringBuilder sb = new StringBuilder();
        String sep = "";
        for (String subPath : subPaths) {
            File dir = pathMgr.getStaticFile(subPath);
            if (dir == null) {
                throw new RuntimeException("Path component " + subPath
                        + " cannot be located.");
            } else if (dir.isFile()) {
                dir = dir.getParentFile();
            } else if (!dir.isDirectory()) {
                throw new RuntimeException("Path component " + subPath
                        + " is not a directory.");
            }
            sb.append(sep);
            sb.append(dir.toString());
            sep = JEP_SEPARATOR;
        }
        includePath = sb.toString();

        String scriptName = bundle.getString("scriptName");
        scriptName = pathMgr.getStaticFile(scriptName).getAbsolutePath();
        makeHazardScript = null;
        try {
            // Open the script to make sure it parses
            makeHazardScript = new PythonScript(scriptName, includePath,
                    getClass().getClassLoader());
        } catch (JepException e) {
            if (makeHazardScript != null) {
                makeHazardScript.dispose();
            }
            makeHazardScript = null;
            throw new RuntimeException("Error creating Python script "
                    + scriptName, e);
        }

        String tcmScriptName = bundle.getString("tcmScriptName");
        tcmScriptName = pathMgr.getStaticFile(tcmScriptName).getAbsolutePath();
        tcmDecoder = null;
        try {
            // Open the script to make sure it parses
            tcmDecoder = new PythonScript(tcmScriptName, includePath,
                    getClass().getClassLoader());
        } catch (JepException e) {
            if (tcmDecoder != null) {
                tcmDecoder.dispose();
            }
            tcmDecoder = null;
            throw new RuntimeException("Error creating Python script "
                    + tcmScriptName, e);
        }

        List<String> preEvals = new ArrayList<String>(6);

        // The hazards dictionary resides in its own script.
        String hazardDictScriptName = bundle.getString("hazardDict");
        hazardDictScriptName = pathMgr.getStaticFile(hazardDictScriptName)
                .getAbsolutePath();

        // Add some getters we need "in the script" that we want hidden
        preEvals.add("from JUtil import pyDictToJavaMap, pylistToJavaStringList");
        preEvals.add("def getHazardDictionary() :\n     return pyDictToJavaMap(hazardDict)");
        preEvals.add("def getDefaultHazardType() :\n     return defaultHazardType");
        preEvals.add("def getMapNames() :\n     return pyDictToJavaMap(mapNames)");
        preEvals.add("def getTCMList() :\n     return pylistToJavaStringList(tcmList)");

        // Getter for quickConfigVal()
        preEvals.add("def getVal(name) :\n    if name in globals(): return eval(name)");

        // Open the Python interpreter using the designated script.
        PythonScript hazardDictScript = null;
        try {
            hazardDictScript = new PythonScript(hazardDictScriptName,
                    includePath, getClass().getClassLoader(), preEvals);
            mapColor = RGBColors.getRGBColor("red");
            String colorName = null;
            try {
                colorName = quickConfigVal("mapColor", hazardDictScript);
                if (colorName != null) {
                    mapColor = RGBColors.getRGBColor(colorName);
                }
            } catch (Exception e) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Invalid mapColor in MakeHazardConfig.py: "
                                        + colorName, e);
            }

            defaultMapWidth = quickConfigVal("defaultMapWidth",
                    hazardDictScript);
            timeScaleEndTime = quickConfigVal("timeScaleEndTime",
                    hazardDictScript);
            areaThresholdStr = quickConfigVal("areaThreshold", hazardDictScript);
            defaultHazardType = (String) hazardDictScript.execute(
                    "getDefaultHazardType", null);
            mapNames = (Map<String, List<String>>) hazardDictScript.execute(
                    "getMapNames", null);
            hazardMap = (Map<String, List<String>>) (hazardDictScript.execute(
                    "getHazardDictionary", null));
            tcmList = (List<String>) (hazardDictScript.execute("getTCMList",
                    null));
        } catch (JepException e) {
            throw new RuntimeException("Error creating Python script "
                    + hazardDictScriptName, e);
        } finally {
            if (hazardDictScript != null) {
                hazardDictScript.dispose();
            }
            hazardDictScript = null;
        }

        initAreaThreshold();
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

        createMapComponent();
        createSelectHazardComponent();
        createTimeSelectComponent();
        createButtons();
    }

    /**
     * Create the hazard in response to the run or run/dismiss button. Indicate
     * whether the run succeeded so run/dismiss knows whether it's OK to close
     * the dialog.
     * 
     * @return true if the hazard was created, false otherwise.
     */
    private boolean doRunInternal() {
        // //// Obtain information from the controls
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
            StatusStore.updateStatus(CATEGORY_GHG, "You must select a hazard",
                    Importance.SIGNIFICANT);
            return false;
        }

        PythonScript script = null;
        String tropicalETN = null;
        String method = "tcmETNforTrop";
        String instance = null;
        Map<String, Object> argmap = new HashMap<String, Object>();

        // get the tropical ETN
        if (tcmProduct != null) {
            argmap.put("tcmProduct", tcmProduct);
            try {
                tropicalETN = (String) tcmDecoder.execute(method, instance,
                        argmap);
            } catch (JepException e) {
                // The script crashed. Write the error to the log and notify the
                // user.
                Activator
                        .getDefault()
                        .getLog()
                        .log(new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                                "Error in Python script", e));
                StatusStore.updateStatus("GHG", "Error in Python script"
                        + " (see error log for details).",
                        Importance.SIGNIFICANT);
            } finally {
                if (script != null) {
                    script.dispose();
                }
            }
        }

        // get the segment #
        int segNum = getSegment();
        String segmentNumber = "";
        // Validate the segment number
        if ((TROPICAL_HAZ.contains(hazard))
                && !(DataManager.getCurrentInstance().getSiteID().equals("GUM"))) {
            // if ETN is already correctly assigned, use it
            if (segNum >= NATL_BASE_ETN) {
                segmentNumber = Integer.toString(segNum);
            } else if ((etnSegNumberField.getText().length() > 0)
                    && (segNum < NATL_BASE_ETN)) {
                // if there is a number in the box, but it is not correct, send
                // message
                String msg = "Must use national tropical hazard ETN. Remove number\n"
                        + "from the ETN box and choose the appropriate TCM to get the ETN.";
                StatusStore.updateStatus(CATEGORY_GHG, msg,
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
                StatusStore.updateStatus(CATEGORY_GHG, msg,
                        Importance.SIGNIFICANT);
                return false;
            }

        } else {
            if (segNum > 0) {
                segmentNumber = Integer.toString(segNum);
            }
        }

        // We need to validate that there are points selected. However, if
        // there
        // are no zones
        // selected, it may be because we are creating a hazard for an edit
        // area
        // instead of zones.
        // The latter is much easier to check in Python, so the validation
        // is
        // done in the script.

        // //// Use the validated inputs

        method = "makeHazard";
        instance = null;

        // Put arguments in a map to pass to the script.
        argmap.clear();
        argmap.put("hazard", hazard);
        argmap.put("dbss", dataManager);
        argmap.put("zones", zoneList);
        argmap.put("segment", segmentNumber);
        argmap.put("timeRange", timeRange);
        argmap.put("selectedTimeRange", selectedTimeRange);
        argmap.put("defaultAreaList", defaultAreaList);
        argmap.put("defaultHazard", defaultHazard);
        argmap.put("defaultSegment", defaultSegment);

        // Execute the method.
        // It may return false if its internal validation rejects the user
        // input.
        Boolean result = Boolean.valueOf(false);
        script = null;
        try {
            result = (Boolean) makeHazardScript.execute(method, instance,
                    argmap);
        } catch (JepException e) {
            // The script crashed. Write the error to the log and notify the
            // user.
            Activator
                    .getDefault()
                    .getLog()
                    .log(new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                            "Error in Python script", e));
            StatusStore.updateStatus("GHG", "Error in Python script"
                    + " (see error log for details).", Importance.SIGNIFICANT);
        }

        return result.booleanValue();
    }

    private List<String> getZoneList() {
        List<List<String>> zoneGroupings = zoneSelector.getZoneGroupings();
        List<String> zoneList = null;
        if (zoneGroupings.size() > 0) {
            zoneList = zoneGroupings.get(0);
        }
        return zoneList;
    }

    /**
     * The method invoked when the 'Run' button is pressed. This is a wrapper
     * around doRunInternal(). Since the dialog won't be closed, the return
     * value is ignored.
     */
    private void doRun() {
        doRunInternal();
    }

    /**
     * The method invoked when the 'Run/Dismiss' button is pressed. This is
     * another wrapper around doRunInternal(), but it will close the dialog if
     * the hazard is successfully created.
     */
    private void doRunDismiss() {
        if (doRunInternal()) {
            shell.close();
        }
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
        runButton.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                // Never called for pushbuttons
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                doRun();
            }
        });

        runDismissButton = new Button(buttons, SWT.PUSH);
        runDismissButton.setText("Run/Dismiss");
        runDismissButton.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                // Never called for pushbuttons
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                doRunDismiss();
            }
        });

        cancelButton = new Button(buttons, SWT.PUSH);
        cancelButton.setText("Cancel");
        cancelButton.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                // Never called for pushbuttons

            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                shell.close();
            }
        });
    }

    private void createMapComponent() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite mapComp = new Composite(topComp, SWT.BORDER);
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

        int width = 400;
        if (defaultMapWidth != null) {
            try {
                width = Integer.parseInt(defaultMapWidth);
            } catch (NumberFormatException e) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "defaultMapWidth in MakeHazardConfig.py must be an integer value");
            }
        }

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.minimumHeight = 100;
        gd.minimumWidth = 100;
        gd.heightHint = width;
        gd.widthHint = width;
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

        showMapLabelsButton.addSelectionListener(this);
        selectAllButton.addSelectionListener(this);
        clearAllButton.addSelectionListener(this);
        zoomInButton.addSelectionListener(this);
        zoomOutButton.addSelectionListener(this);
        zoomOneToOneButton.addSelectionListener(this);

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

    private void createSelectHazardComponent() {
        Composite hazardComp = new Composite(topComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        hazardComp.setLayout(gl);
        GridData gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        hazardComp.setLayoutData(gd);

        selectedHazardList = new org.eclipse.swt.widgets.List(hazardComp,
                SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL | SWT.SINGLE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = selectedHazardList.getItemHeight() * 16
                + selectedHazardList.getBorderWidth();
        gd.horizontalSpan = 2;
        selectedHazardList.setLayoutData(gd);

        radioGroup = new Group(hazardComp, SWT.BORDER);
        radioGroup.setText("Hazard Type");
        gl = new GridLayout(1, true);
        gl.verticalSpacing = 0;
        radioGroup.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        radioGroup.setLayoutData(gd);

        etnSegNumGroup = new Group(hazardComp, SWT.BORDER);
        etnSegNumGroup.setText("ETN/Segment Number");
        etnSegNumGroup.setLayout(new GridLayout(1, true));
        etnSegNumGroup.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                true));

        List<String> groups = new ArrayList<String>(getHazardsDictionary()
                .keySet());
        Collections.sort(groups);
        this.currentHazardType = "";
        for (String k : groups) {
            // add a radio button control to the radioComposite
            Button radioButton = new Button(radioGroup, SWT.RADIO);
            if (k.equalsIgnoreCase(defaultHazardType)) {
                radioButton.setSelection(true);
                try {
                    updateSelectedHazardList(defaultHazardType);
                } catch (GFEServerException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to update selected hazard list", e);
                }
            }
            radioButton.setText(k);
            radioButton.addSelectionListener(this);
        }

        etnSegNumberField = new Text(etnSegNumGroup, SWT.BORDER);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 40;
        etnSegNumberField.setLayoutData(gd);
        if (tcmList.size() > 0) {
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
    private void createTimeSelectComponent() {
        Composite timeComp = new Composite(topComp, SWT.BORDER);
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
        startTimeLabel = new Label(startGroup, SWT.NONE);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        startTimeLabel.setLayoutData(gd);

        // create a label to identify the start time slider as the start time
        // Label lab = new Label(timeComp, SWT.NONE);
        // lab.setText("Hazard Start Time");
        // lab.setLayoutData(gd);

        toHours = 96;
        if (timeScaleEndTime != null) {
            try {
                toHours = Integer.parseInt(timeScaleEndTime);
            } catch (NumberFormatException nfe) {
                statusHandler.handle(Priority.WARN, "timeScaleEndTime value "
                        + timeScaleEndTime + " cannot be parsed.");
            }
        }

        // create the start time slider
        startTimeSlider = new Scale(startGroup, SWT.HORIZONTAL);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
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
        startTimeSlider.setMaximum(toHours);
        startTimeSlider.setIncrement(1);
        startTimeSlider.setPageIncrement(1);
        startTimeSlider.setLayoutData(new GridData(200, SWT.DEFAULT));

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
        endTimeLabel = new Label(endGroup, SWT.NONE);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.horizontalAlignment = SWT.CENTER;
        endTimeLabel.setLayoutData(gd);
        updateTime(1, endTimeLabel);

        // Create a label to identify the end time slider
        // Label lab2 = new Label(timeComp, SWT.NONE);
        // lab2.setText("Hazard End Time");
        // gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        // gd.horizontalAlignment = SWT.CENTER;
        // lab2.setLayoutData(gd);

        // Create the end time slider
        endTimeSlider = new Scale(endGroup, SWT.HORIZONTAL);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        endTimeSlider.setLayoutData(gd);

        endTimeSlider.setMinimum(1);
        endTimeSlider.setMaximum(toHours + 1);
        endTimeSlider.setIncrement(1);
        endTimeSlider.setPageIncrement(1);
        endTimeSlider.setSelection(1);
        endTimeSlider.setLayoutData(new GridData(200, SWT.DEFAULT));
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

    private Map<String, List<String>> getHazardsDictionary() {
        if (hazardMap == null) {
            hazardMap = new TreeMap<String, List<String>>();
            hazardMap.put("Winter Weather", Arrays.asList("BZ.W", "BZ.A",
                    "BS.Y", "ZR.Y", "HS.W", "IS.W", "LE.Y", "LE.W", "LE.A",
                    "LB.Y", "IP.Y", "IP.W", "SN.Y", "SB.Y", "WC.Y", "WC.W",
                    "WC.A", "WS.W", "WS.A", "WW.Y"));
            hazardMap.put("Hydrology", Arrays.asList("FF.A", "FA.A"));
            hazardMap.put("Fire Weather", Arrays.asList("FW.A", "FW.W"));
            hazardMap.put("Convective Watches", Arrays.asList("SV.A", "TO.A"));
            hazardMap.put("Coastal Flood", Arrays.asList("CF.S", "LS.S",
                    "CF.Y", "CF.W", "CF.A", "SU.Y", "SU.W", "LS.Y", "LS.W",
                    "LS.A"));
            hazardMap.put("Non-Precipitation", Arrays.asList("AF.W", "AF.Y",
                    "AS.Y", "DU.Y", "DS.W", "EH.W", "EH.A", "EC.W", "EC.A",
                    "FG.Y", "FZ.W", "FZ.A", "HZ.W", "HZ.A", "ZF.Y", "FR.Y",
                    "HT.Y", "HW.W", "HW.A", "LW.Y", "SM.Y", "WI.Y"));
            hazardMap.put("Common Marine/NPW",
                    Arrays.asList("AF.W", "AF.Y", "FG.Y", "SM.Y"));
            hazardMap.put("Marine", Arrays.asList("MA.S", "MH.W", "MH.Y",
                    "BW.Y", "UP.Y", "FG.Y", "GL.A", "GL.W", "SE.A", "SE.W",
                    "UP.A", "UP.W", "HF.A", "HF.W", "LO.Y", "SC.Y", "SW.Y",
                    "RB.Y", "SI.Y", "SM.Y", "SR.A", "SR.W"));
            hazardMap.put("Tropical Cyclone",
                    Arrays.asList("HU.W", "HU.A", "HU.S", "TR.W", "TR.A"));
            hazardMap.put("Tsunami", Arrays.asList("TS.A", "TS.W"));
        }

        return hazardMap;
    }

    private List<String> getHazardListForType(String key) {

        Map<String, List<String>> hazardDict = getHazardsDictionary();
        if (hazardDict.containsKey(key)) {
            return hazardDict.get(key);
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse
     * .swt.events.SelectionEvent)
     */
    @Override
    public void widgetDefaultSelected(SelectionEvent e) {
        // No default action defined
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt
     * .events.SelectionEvent)
     */
    @Override
    public void widgetSelected(SelectionEvent e) {
        if (e.getSource() == this.showMapLabelsButton) {
            this.zoneSelector.setLabelZones(this.showMapLabelsButton
                    .getSelection());
            return;
        } else if (e.getSource() == this.zoomInButton) {
            zoneSelector.setZoomLevel(zoneSelector.getZoomLevel() * ZOOM_STEP);
            return;
        } else if (e.getSource() == this.zoomOutButton) {
            zoneSelector.setZoomLevel(zoneSelector.getZoomLevel() / ZOOM_STEP);
            return;
        } else if (e.getSource() == this.zoomOneToOneButton) {
            // reset the display to fully zoomed extent
            zoneSelector.setZoomLevel(1);
            return;
        } else if (e.getSource() == this.clearAllButton) {
            this.zoneSelector.updateCombos(new HashMap<String, Integer>());

            return;
        } else if (e.getSource() == this.selectAllButton) {
            Map<String, Integer> comboDict = new HashMap<String, Integer>();
            for (String zoneName : this.zoneSelector.getZoneNames()) {
                comboDict.put(zoneName, 1);
            }
            this.zoneSelector.updateCombos(comboDict);

            return;
        }

        Button b = (Button) e.getSource();
        String key = b.getText();
        try {
            updateSelectedHazardList(key);
        } catch (GFEServerException e1) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to update selected hazard list", e1);
        }
    }

    private void updateSelectedHazardList(String hazType)
            throws GFEServerException {
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
            label.pack(true);
        } catch (ParseException e) {
            // This shouldn't ever happen...
            e.printStackTrace();
        }

    }

    private void initializeShapeComponent(Composite controlComp)
            throws TransformException, FactoryException, VizException {

        GridLocation gloc = dataManager.getParmManager()
                .compositeGridLocation();
        zoneSelector = new ZoneSelector(controlComp, gloc, null);
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
        Control[] radioControls = radioGroup.getChildren();
        for (Control radioControl : radioControls) {
            if ((radioControl.getStyle() & SWT.RADIO) != 0) {
                Button radioButton = (Button) radioControl;
                if (radioButton.getText().equals(hazardType)) {
                    if (!radioButton.getSelection()) {
                        radioButton.setSelection(true);
                        try {
                            updateSelectedHazardList(hazardType);
                        } catch (GFEServerException e) {
                            statusHandler
                                    .handle(Priority.PROBLEM,
                                            "Server error changing hazard type radio button",
                                            e);
                        }
                    }
                } else {
                    radioButton.setSelection(false);
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
     * Ensure that the hazard grids are separated.
     * 
     * @return false if the hazard grids are not separated and cannot be
     *         separated (normally because another station has a lock), true
     *         otherwise.
     */
    private boolean ensureSeparated() {
        Map<String, Object> argMap = new HashMap<String, Object>();
        argMap.put("dbss", dataManager);
        boolean separated = false;
        PythonScript script = null;
        try {
            separated = (Boolean) makeHazardScript.execute("ensureSeparated",
                    argMap);
        } catch (JepException e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Python error separating grids", e);
        } finally {
            if (script != null) {
                script.dispose();
            }
        }
        return separated;
    }

    /**
     * @param name
     * @return
     */
    private String quickConfigVal(String name, PythonScript script) {
        Map<String, Object> args = new HashMap<String, Object>();
        args.put("name", name);
        Object obj = null;
        try {
            obj = script.execute("getVal", args);
        } catch (JepException e) {
            statusHandler.handle(Priority.WARN,
                    "Error loading configuration value " + args.get("name"), e);
        }
        String str = null;
        if (obj != null) {
            str = obj.toString();
        }
        return str;
    }

    /**
     * Defend against bad values for areaThreshold in config file.
     */
    protected void initAreaThreshold() {
        if (areaThresholdStr != null) {
            Double atTemp = null;
            Exception exc = null;
            try {
                atTemp = Double.valueOf(areaThresholdStr);
            } catch (NumberFormatException e) {
                exc = e;
            }
            if (atTemp == null) {
                statusHandler.handle(Priority.WARN,
                        "Error setting area threshold", exc);
            } else if (atTemp.doubleValue() > 1.0) {
                // This wouldn't select any zones
                statusHandler.handle(Priority.WARN,
                        "Area threshold is greater than 100%. Using default.");
            } else if (atTemp.doubleValue() <= 0.0) {
                // This would always select all zones
                statusHandler.handle(Priority.WARN,
                        "Area threshold is 0% or below. Using default.");
            } else {
                areaThreshold = atTemp.doubleValue();
            }
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

}
