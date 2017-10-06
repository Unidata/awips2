package gov.noaa.nws.mdl.viz.boundaryTool.ui.dialog;

import gov.noaa.nws.mdl.viz.boundaryTool.boundaries.state.xml.ReadBoundariesXmlFile;
import gov.noaa.nws.mdl.viz.boundaryTool.boundaries.state.xml.WriteBoundariesXmlFile;
import gov.noaa.nws.mdl.viz.boundaryTool.common.boundary.BoundaryState;
import gov.noaa.nws.mdl.viz.boundaryTool.common.boundary.BoundaryState.Mode;
import gov.noaa.nws.mdl.viz.boundaryTool.common.boundary.BoundaryState.UserAction;
import gov.noaa.nws.mdl.viz.boundaryTool.ui.layer.BoundaryEditorLayer;

import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.xml.datatype.DatatypeConfigurationException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * 
 * @author Mamoudou Ba
 * @version 1.0
 * 
 *          April 28, 2011
 * 
 *          Based on A2 "TimeOfArrivalDialog" class
 */

public class BoundaryEditorDialog extends CaveJFACEDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(BoundaryEditorDialog.class);

    protected int boundaryIdLabel;

    protected int durationLabel;

    private Composite top = null;

    private Composite boundaryFieldComposite;

    private Combo editCbo;

    // private Combo deleteCombo;

    private Combo regimeCbo;

    private Combo motionCbo;

    private Combo boundaryLifeSpanCbo;

    private LinkedHashMap<String, String> boundaryIdsMap;

    private LinkedHashMap<String, String> durationMap;

    private LinkedHashMap<String, String> regimeMap;

    private LinkedHashMap<String, String> boundaryModeMap;

    private final BoundaryEditorLayer boundarylayer;

    private Button insertBoundaryBtn, adjustMotionBtn, deleteBtn, saveDataBtn,
            cancelBtn;

    private String[] idStrings = null;

    private String[] durationStrings = null;

    private String duration;

    private String[] regimeStrings = null;

    private String[] modeStrings = null;

    private final String Moving = "Moving";

    private final String Stationary = "Stationary";

    // Boundary Mode (Stationary or Moving
    private final String boundaryMode[] = { "Stationary", "Moving" };

    // Atmospheric regimes
    private final String regime[] = { "COLD FRONT", "STATIONARY/WARM FRONT",
            "SEA/LAKE BREEZE", "DRY LINE", "GUST FRONT",
            "SELECT TO DEFINE TYPE" };

    private final int userRegimeIndex = 5;

    private void setupEditMenu(UserAction type) {

        String displayName = null;

        boundaryIdsMap = new LinkedHashMap<String, String>();

        // Populate edit and delete boundary menu

        // 0 is reserved id for ANC software
        // valid Id start with 1

        for (int i : boundarylayer.getBoundaryState().boundariesMap.keySet()) {

            displayName = "" + i;

            boundaryIdsMap.put(displayName, displayName);
        }
        // displayName = "---";
        // populating edit and delete drop menu

        idStrings = new String[boundaryIdsMap.size()];
        Iterator<String> bndIterator = null;

        bndIterator = boundaryIdsMap.keySet().iterator();

        for (int i = 0; i < idStrings.length; i++) {
            idStrings[i] = bndIterator.next();

        }
    }

    private void setupDurationMenu() {

        String displayName = null;

        durationMap = new LinkedHashMap<String, String>();

        // Populate duration menu

        for (int i = 1; i < 9; i++) {

            displayName = "" + i;

            durationMap.put(displayName, displayName);
        }
        // displayName = "---";
        // populating boundary duration drop menu

        duration = "" + durationMap.size();

        durationStrings = new String[durationMap.size()];
        Iterator<String> durationIterator = null;

        durationIterator = durationMap.keySet().iterator();

        for (int i = 0; i < durationStrings.length; i++) {
            durationStrings[i] = durationIterator.next();

        }
    }

    private void setupDataMenu() {
        String displayName = null;

        regimeMap = new LinkedHashMap<String, String>();
        boundaryModeMap = new LinkedHashMap<String, String>();

        // Populate regime menu; Populate boundary mode menu

        for (int i = 0; i < regime.length; i++) {

            displayName = regime[i];
            regimeMap.put(displayName, displayName);

        }

        for (int i = 0; i < boundaryMode.length; i++) {

            displayName = boundaryMode[i];
            boundaryModeMap.put(displayName, displayName);

        }

        regimeStrings = new String[regimeMap.size()];
        Iterator<String> regimeIterator = null;

        regimeIterator = regimeMap.keySet().iterator();

        for (int i = 0; i < regimeStrings.length; i++) {
            regimeStrings[i] = regimeIterator.next();

        }

        // The motion mode is proprely handled in the BoundaryDisplay class
        //
        modeStrings = new String[boundaryModeMap.size()];
        Iterator<String> modeIterator = null;

        modeIterator = boundaryModeMap.keySet().iterator();

        for (int i = 0; i < modeStrings.length; i++) {
            modeStrings[i] = modeIterator.next();

        }

    }

    public BoundaryEditorDialog(Shell parShell,
            BoundaryEditorLayer boundarylayer) {
        super(parShell);
        this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE);
        this.boundarylayer = boundarylayer;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    public Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);
        GridLayout gridLayout = new GridLayout(1, false);
        top.setLayout(gridLayout);
        setupDataMenu();
        setupEditMenu(UserAction.EDIT_BOUNDARY);
        setupDurationMenu();
        initializeComponents();
        return top;

    }

    private void initializeComponents() {

        boundaryFieldComposite = new Composite(top, SWT.NONE);
        boundaryFieldComposite.setLayout(new GridLayout(2, false));
        GridData data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        insertBoundaryBtn = new Button(boundaryFieldComposite, SWT.PUSH);
        insertBoundaryBtn.setText("Insert Boundary");
        insertBoundaryBtn.setLayoutData(data);

        insertBoundaryBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                // Check if regime is set for each inserted boundary
                if (boundarylayer.getBoundaryState().boundariesMap.size() != 0) {
                    Iterator<Integer> iterator = boundarylayer
                            .getBoundaryState().boundariesMap.keySet()
                            .iterator();
                    int boundaryId;
                    String s = "";
                    while (iterator.hasNext()) {
                        boundaryId = iterator.next();
                        if (boundarylayer.getBoundaryState().boundaryTypeMap
                                .get(boundaryId) == null) {
                            s = s + boundaryId;
                        }

                    }
                    if (!s.isEmpty()) {
                        MessageDialog.openWarning(Display.getCurrent()
                                .getActiveShell(),
                                "Previous Boundary Type Not Set",
                                "Please set the boundary type for ids: " + s
                                        + " before inserting another boundary");
                        return;
                    }
                }
                updateDisplayType(UserAction.INSERT_BOUNDARY);
                Display display = Display.getCurrent();
                /*
                 * Highlight "Save Boundary Data" button is red: data not yet
                 * save after a new boundary is inserted or an existing boundary
                 * is modified.
                 */
                saveDataBtn.setBackground(display.getSystemColor(SWT.COLOR_RED));
            }

        });
        deleteBtn = new Button(boundaryFieldComposite, SWT.PUSH);
        deleteBtn.setText("Delete Boundary...");
        data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        deleteBtn.setLayoutData(data);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {

                deleteAction();
            }
        });

        Label boundaryIdLabel = new Label(boundaryFieldComposite, SWT.NONE);
        boundaryIdLabel.setText("Edit Boundary:");
        // boundaryIdLabel.setLayoutData(createFormLayoutLoc(true, 0));
        data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        boundaryIdLabel.setLayoutData(data);
        editCbo = new Combo(boundaryFieldComposite, SWT.DROP_DOWN);
        editCbo.setItems(idStrings);
        data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        editCbo.setLayoutData(data);

        editCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {

                editAction();
            }
        });

        Label durationLabel = new Label(boundaryFieldComposite, SWT.NONE);
        durationLabel.setText("Boundary Duration (hrs):");
        // durationLabel.setLayoutData(createFormLayoutLoc(true, 0));
        data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        durationLabel.setLayoutData(data);
        boundaryLifeSpanCbo = new Combo(boundaryFieldComposite, SWT.DROP_DOWN);
        boundaryLifeSpanCbo.setItems(durationStrings);
        data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        boundaryLifeSpanCbo.setLayoutData(data);

        boundaryLifeSpanCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                lifeSpanAction();
            }
        });

        Label regimeLabel = new Label(boundaryFieldComposite, SWT.NONE);
        regimeLabel.setText("Boundary Type:");
        // regimeLabel.setLayoutData(createFormLayoutLoc(true, 0));
        data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        regimeLabel.setLayoutData(data);
        regimeCbo = new Combo(boundaryFieldComposite, SWT.DROP_DOWN);
        regimeCbo.setItems(regimeStrings);
        regimeCbo.setLayoutData(data);

        regimeCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                regimeAction();
            }

        });

        // Boundary motion mode drop down menu

        Label modeLabel = new Label(boundaryFieldComposite, SWT.NONE);
        modeLabel.setText("Boundary Mode:");
        data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        modeLabel.setLayoutData(data);
        data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        motionCbo = new Combo(boundaryFieldComposite, SWT.DROP_DOWN);
        motionCbo.setItems(modeStrings);
        motionCbo.setLayoutData(data);

        motionCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                modeAction();
            }
        });

        /*
         * Boundary Motion Modification: This will allow the user to adjust the
         * boundary position
         */
        adjustMotionBtn = new Button(boundaryFieldComposite, SWT.PUSH);
        adjustMotionBtn.setText("Modify the Motion");
        adjustMotionBtn
                .setToolTipText("Clicking this button will allow you to adjust the boundary position to "
                        + " its current location and recompute its motion;  the boundary will be reset to stationary"
                        + " so that you can readjust its position and compute its current motion accordingly");
        data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        adjustMotionBtn.setLayoutData(data);
        adjustMotionBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                adjustMotionAction();
            }
        });

        cancelBtn = new Button(boundaryFieldComposite, SWT.PUSH);
        cancelBtn.setText("Cancel Modification");
        cancelBtn
                .setToolTipText("Clicking this button will cancel the modification you just made");
        data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        cancelBtn.setLayoutData(data);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                cancelAction();
            }

        });

        saveDataBtn = new Button(boundaryFieldComposite, SWT.PUSH);
        saveDataBtn.setText("Save Boundary Data");
        data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        saveDataBtn.setLayoutData(data);
        saveDataBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                saveDataAction();
            }
        });

        // Boundary Id = 0 is always forbidden

        boundarylayer.getBoundaryState().forbiddenBoundaryIdsMap.put(0, 0);

        /* Set Display Text To First Element In Combo Box */

        fillBoundaryMenus();

    }

    /**
    *
    */
    @Override
    protected void createButtonsForButtonBar(Composite parents) {
        // Don't do anything since we don't want the OK and cancel buttons
    }

    @Override
    protected boolean canHandleShellCloseEvent() {
        Composite closeDialogOption = new Composite(top, SWT.NONE);
        closeDialogOption.setLayout(new FormLayout());
        boolean optionSelected = false;

        MessageBox messageBox = new MessageBox(getShell(), SWT.ICON_QUESTION
                | SWT.YES | SWT.NO);
        messageBox.setText("Closing?");
        messageBox.setMessage("Are you sure you want to close the dialog?");
        int buttonID = messageBox.open();
        switch (buttonID) {
        case SWT.YES:
            optionSelected = true;
            boundaryFieldComposite.getShell().dispose();
            boundarylayer.getBoundaryState().dialogObject = null;
            break;
        }
        return optionSelected;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.window.Window#configureShell(
     * editCbo.setItems(idStrings);org.eclipse.swt.widgets .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText("Boundary Editor");
    }

    private void updateDisplayType(UserAction type) {
        boundarylayer.getBoundaryState().userAction = type;
        boundarylayer.getBoundaryState().geomChanged = true;
        cancelBtn.setEnabled(true);
        saveDataBtn.setEnabled(true);
        deleteBtn.setEnabled(true);
        // KS - corrected coding error
        int idMax = 0;

        for (int id : boundarylayer.getBoundaryState().boundariesMap.keySet()) {

            if (id > idMax) {
                idMax = id;
            }
        }
        ++idMax;
        while (boundarylayer.getBoundaryState().forbiddenBoundaryIdsMap
                .get(idMax) != null) {
            idMax = idMax + 1;
        }

        // 99 is the highest boundary Id allowed; If this number is reached, the
        // second number from forbidden list is used
        // zero is a reserved id number for ANC is the first element in
        // forbiddenBoundaruIdsMap

        if (idMax > 99) {
            Iterator<Integer> iterator = boundarylayer.getBoundaryState().forbiddenBoundaryIdsMap
                    .keySet().iterator();
            int boundaryId = 0;
            while (iterator.hasNext()) {
                boundaryId = iterator.next();
                if (boundaryId > 0) {
                    boundarylayer.getBoundaryState().forbiddenBoundaryIdsMap
                            .remove(boundaryId);
                    break;
                }
            }
            idMax = boundaryId;
        }
        boundarylayer.getBoundaryState().boundaryId = idMax;
        String s = null;
        s = "new" + " boundary(" + idMax + ")";
        boundarylayer.getBoundaryState().logMap.put(idMax, s);

        boundarylayer.getBoundaryState().dragMeLine = null;
        boundarylayer.getBoundaryState().boundariesMap.put(
                boundarylayer.getBoundaryState().boundaryId,
                boundarylayer.getBoundaryState().dragMeLine);
        setupEditMenu(UserAction.EDIT_BOUNDARY);
        editCbo.setItems(idStrings);

        String boundaryId = "" + boundarylayer.getBoundaryState().boundaryId;
        editCbo.setText(boundaryIdsMap.get(boundaryId));

        motionCbo.setText(modeStrings[0]);

        boundarylayer.getBoundaryState().isMovingMap.put(
                boundarylayer.getBoundaryState().boundaryId, false);
        boundarylayer.getBoundaryState().mode = Mode.DRAG_ME;
        regimeCbo.setText("Select Boundary Type");
        boundarylayer.issueRefresh();
    }

    private void saveBoundaryData(UserAction type) {

        boundarylayer.getBoundaryState().userAction = type;
        boundarylayer.issueRefresh();
        saveData(boundarylayer.getBoundaryState(),
                boundarylayer.getBoundaryState().currentDataTimes,
                boundarylayer.getBoundaryState().timeIndex);
        boundarylayer.getBoundaryState().userAction = UserAction.NONE;
    }

    /**
     * Save data to localization file (run new thread to save data).
     */
    public void saveData(final BoundaryState currentState,
            final DataTime[] dataTimes, final int timeIndex) {
        Job job = new Job("Writing data to localization file") {
            @Override
            protected IStatus run(IProgressMonitor monitor) {
                WriteBoundariesXmlFile writeXMLFile = new WriteBoundariesXmlFile();
                try {
                    writeXMLFile.writeBoundariesXmlFile(currentState,
                            dataTimes, timeIndex);
                } catch (VizException | DatatypeConfigurationException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Writing boundary data fails", e);
                }
                writeXMLFile = null;
                return Status.OK_STATUS;
            }
        };
        job.setSystem(true);
        job.schedule();
    }

    //
    private void getEditedBoundaryId(UserAction type, String s) {
        boundarylayer.getBoundaryState().userAction = type;
        try {
            // convert boundaryId string into integer

            int i = Integer.parseInt(s.trim());

            boundarylayer.getBoundaryState().boundaryId = i;
        } catch (NumberFormatException nfe) {
            // ignore values that are not numbers
        }
        boundarylayer.issueRefresh();
    }

    private void getDuration(String s) {
        // boundarylayer.getBoundaryState().userAction = type;
        try {
            // convert boundaryId string into integer

            int i = Integer.parseInt(s.trim());

            boundarylayer.getBoundaryState().boundaryDuration = i;
        } catch (NumberFormatException nfe) {
            statusHandler.handle(Priority.PROBLEM, nfe.getMessage(), nfe);
        }
        boundarylayer.issueRefresh();
    }

    // user action methods

    public void saveDataAction() {

        if (boundarylayer.getBoundaryState().boundariesMap.get(boundarylayer
                .getBoundaryState().boundaryId) != null) {

            if (boundarylayer.getBoundaryState().boundariesMap.size() != 0) {
                Iterator<Integer> iterator = boundarylayer.getBoundaryState().boundariesMap
                        .keySet().iterator();
                int boundaryId;
                String s = "";
                while (iterator.hasNext()) {
                    boundaryId = iterator.next();
                    if (boundarylayer.getBoundaryState().boundaryTypeMap
                            .get(boundaryId) == null) {
                        s = s + boundaryId;
                    }

                }
                if (!s.isEmpty()) {
                    // Need to define the boundary type before saving
                    // the data
                    MessageBox messageBox = new MessageBox(getShell(),
                            SWT.ICON_WARNING | SWT.OK);
                    messageBox.setText("Warning !!!");
                    messageBox.setMessage("Boundary type is not defined yet"
                            + " for ids: " + s
                            + " set boundary type before saving the data");
                    int buttonID = messageBox.open();
                    switch (buttonID) {
                    case SWT.OK:
                        break;
                    }
                    return;
                } else {
                    saveBoundaryData(UserAction.SAVE);
                }
            }
            setupEditMenu(UserAction.EDIT_BOUNDARY);
            int id = boundarylayer.getBoundaryState().boundaryId;
            String s = "" + id;
            if (id == 0) {
                s = "";
            }
            editCbo.setItems(idStrings);
            editCbo.setText(s);
            s = boundarylayer.getBoundaryState().boundaryTypeMap.get(id);
            regimeCbo.setItems(regimeStrings);
            regimeCbo.setText(s);
            if (boundarylayer.getBoundaryState().isMovingMap.get(id) == true) {
                s = Moving;
            } else {
                s = Stationary;
            }
            motionCbo.setItems(modeStrings);
            motionCbo.setText(s);
        }
        saveDataBtn.setEnabled(false);
        cancelBtn.setEnabled(false);
        deleteBtn.setEnabled(false);
        adjustMotionBtn.setEnabled(false);
        /*
         * Set "Save Boundary Data" button gray: when data are saved
         */
        saveDataBtn.setBackground(Display.getCurrent().getSystemColor(
                SWT.COLOR_GRAY));
        boundarylayer.issueRefresh();
    }

    public void cancelAction() {
        MessageBox messageBox = new MessageBox(getShell(), SWT.ICON_QUESTION
                | SWT.YES);
        messageBox.setText("Canceling?");
        messageBox
                .setMessage("Are you sure you want cancel your modification?");
        int buttonID = messageBox.open();
        switch (buttonID) {
        case SWT.YES:

            boundarylayer.getBoundaryState().userAction = UserAction.CANCEL_MODIFICATION;

            setupEditMenu(UserAction.EDIT_BOUNDARY);
            Map<Integer, Integer> activeBoundariesMap = new HashMap<Integer, Integer>();

            for (int i : boundarylayer.getBoundaryState().boundariesMap
                    .keySet()) {
                // active boundaries saved on memory
                activeBoundariesMap.put(i, i);
            }

            // remove all boundaries from memories;
            for (int id : activeBoundariesMap.keySet()) {
                boundarylayer.getBoundaryState().boundariesMap.remove(id);
                boundarylayer.getBoundaryState().boundaryTypeMap.remove(id);
                boundarylayer.getBoundaryState().boundariesMap.remove(id);
                boundarylayer.getBoundaryState().timePointsMap.remove(id);
                boundarylayer.getBoundaryState().dragMePointMap.remove(id);
                boundarylayer.getBoundaryState().lineMovedMap.remove(id);
                boundarylayer.getBoundaryState().isMovingMap.remove(id);
                boundarylayer.getBoundaryState().boundaryDurationMap.remove(id);
                boundarylayer.getBoundaryState().createTimeMap.remove(id);
                boundarylayer.getBoundaryState().editedTimeMap.remove(id);
                boundarylayer.getBoundaryState().expirationTimeMap.remove(id);
                boundarylayer.getBoundaryState().existingBoundaryNotEmptyMap
                        .remove(id);
                boundarylayer.getBoundaryState().vertexAngleMap.remove(id);
                boundarylayer.getBoundaryState().vertexSpeedMap.remove(id);
            }

            // Now Read boundary data from disc
            ReadBoundariesXmlFile readXMLFile = new ReadBoundariesXmlFile();
            try {
                readXMLFile.readBoundariesXmlFile(boundarylayer
                        .getBoundaryState());
            } catch (FileNotFoundException e) {
                statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
            } catch (DatatypeConfigurationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
            }

            break;
        default:
            saveBoundaryData(UserAction.SAVE);
            boundarylayer.getBoundaryState().userAction = UserAction.NONE;
            break;
        }
        fillBoundaryMenus();
        cancelBtn.setEnabled(false);
        // If "Save Boundary Data" button is gray:
        // data are saved
        saveDataBtn.setBackground(Display.getCurrent().getSystemColor(
                SWT.COLOR_GRAY));
        boundarylayer.issueRefresh();
    }

    public void modeAction() {
        if (motionCbo.getSelectionIndex() != -1
                && boundarylayer.getBoundaryState().boundariesMap
                        .get(boundarylayer.getBoundaryState().boundaryId) != null) {

            if (boundaryModeMap.get(motionCbo.getText()) == Stationary) {
                boundarylayer.getBoundaryState().isMovingMap.put(
                        boundarylayer.getBoundaryState().boundaryId, false);

                boundarylayer.getBoundaryState().vertexAngleMap
                        .remove(boundarylayer.getBoundaryState().boundaryId);
                boundarylayer.getBoundaryState().vertexSpeedMap
                        .remove(boundarylayer.getBoundaryState().boundaryId);
                boundarylayer.getBoundaryState().timePointsMap
                        .remove(boundarylayer.getBoundaryState().boundaryId);
                boundarylayer.getBoundaryState().dragingLineNotAllowed = false;
                boundarylayer.getBoundaryState().lineIsMoving = false;

            } else {
                if (boundarylayer.getBoundaryState().isMovingMap
                        .get(boundarylayer.getBoundaryState().boundaryId) == false) {

                    if (boundarylayer.getBoundaryState().editedLineForMotionComputation == null) {
                        boundarylayer.getBoundaryState().editedLineForMotionComputation = boundarylayer
                                .getBoundaryState().boundariesMap
                                .get(boundarylayer.getBoundaryState().boundaryId);

                    }

                    boundarylayer.getBoundaryState().dragMePointMap
                            .put(boundarylayer.getBoundaryState().boundaryId,
                                    boundarylayer.getBoundaryState().boundariesMap.get(boundarylayer
                                            .getBoundaryState().boundaryId));
                    boundarylayer.getBoundaryState().isMovingMap
                            .remove(boundarylayer.getBoundaryState().boundaryId);
                    boundarylayer.getBoundaryState().isMovingMap.put(
                            boundarylayer.getBoundaryState().boundaryId, true);
                    // Setting speed for newly created moving boundary
                    // Need to move to another frame
                    boundarylayer.getBoundaryState().dragingLineNotAllowed = true;
                    boundarylayer.getBoundaryState().lineIsMoving = true;
                }
                boundarylayer.getBoundaryState().existingBoundaryNotEmptyMap
                        .remove(boundarylayer.getBoundaryState().boundaryId);
                boundarylayer.getBoundaryState().existingBoundaryNotEmptyMap
                        .put(boundarylayer.getBoundaryState().boundaryId, false);
            }
            cancelBtn.setEnabled(true);
            saveDataBtn.setEnabled(true);
            // Highlight "Save Boundary Data" button is red: data not
            // yet saved after a new boundary
            // is inserted or an existing boundary is modified.
            saveDataBtn.setBackground(Display.getCurrent().getSystemColor(
                    SWT.COLOR_RED));
            boundarylayer.issueRefresh();
        }
    }

    public void adjustMotionAction() {

        MessageBox messageBox = new MessageBox(getShell(), SWT.ICON_QUESTION
                | SWT.YES);
        messageBox.setText("Modifying the motion?");
        messageBox
                .setMessage("Are you sure you want to modify the motion of this boundary?");
        int buttonID = messageBox.open();
        switch (buttonID) {
        case SWT.YES:

            resetToStationary();
            break;
        default:
            boundarylayer.getBoundaryState().userAction = UserAction.NONE;
            break;
        }
        adjustMotionBtn.setEnabled(false);
        cancelBtn.setEnabled(true);
        saveDataBtn.setEnabled(true);
        /*
         * Highlight "Save Boundary Data" button is red: data not yet saved
         * after a new boundary is inserted or an existing boundary is modified.
         */
        saveDataBtn.setBackground(Display.getCurrent().getSystemColor(
                SWT.COLOR_RED));
        boundarylayer.issueRefresh();
    }

    public void outOfrangeError() {
        resetToStationary();
        adjustMotionBtn.setEnabled(false);
        cancelBtn.setEnabled(true);
        saveDataBtn.setEnabled(true);
        /*
         * Highlight "Save Boundary Data" button is red: data not yet saved
         * after a new boundary is inserted or an existing boundary is modified.
         */
        saveDataBtn.setBackground(Display.getCurrent().getSystemColor(
                SWT.COLOR_RED));
        boundarylayer.issueRefresh();
    }

    public void resetToStationary() {

        if (boundaryModeMap.get(motionCbo.getText()) == Moving) {
            boundarylayer.getBoundaryState().isMovingMap.remove(boundarylayer
                    .getBoundaryState().boundaryId);
            boundarylayer.getBoundaryState().isMovingMap.put(
                    boundarylayer.getBoundaryState().boundaryId, false);

            boundarylayer.getBoundaryState().vertexAngleMap
                    .remove(boundarylayer.getBoundaryState().boundaryId);
            boundarylayer.getBoundaryState().vertexSpeedMap
                    .remove(boundarylayer.getBoundaryState().boundaryId);
            boundarylayer.getBoundaryState().timePointsMap.remove(boundarylayer
                    .getBoundaryState().boundaryId);
            motionCbo.setItems(modeStrings);
            motionCbo.setText(Stationary);
            boundarylayer.getBoundaryState().lineIsMoving = false;
            boundarylayer.getBoundaryState().dragingLineNotAllowed = false;
            boundarylayer.getBoundaryState().movingEdited = false;
            boundarylayer.getBoundaryState().editedLineForMotionComputation = boundarylayer
                    .getBoundaryState().boundariesMap.get(boundarylayer
                    .getBoundaryState().boundaryId);
            boundarylayer.getBoundaryState().motionIsResetToStationary = true;

        }

    }

    public void regimeAction() {
        if (regimeCbo.getSelectionIndex() != -1
                && boundarylayer.getBoundaryState().boundariesMap
                        .get(boundarylayer.getBoundaryState().boundaryId) != null) {
            // User define boundary Type
            if (regimeCbo.getText().equals(regime[userRegimeIndex])) {
                UserInputBndType dlg = new UserInputBndType(Display
                        .getCurrent().getActiveShell());
                dlg.open();
                String input = dlg.userInput;

                if (UserInputBndType.isOK) {
                    if (!input.isEmpty()) {
                        for (String i : regimeMap.keySet()) {
                            if (regimeMap.get(i)
                                    .equals(regime[userRegimeIndex])) {
                                regimeMap.remove(i);
                                regimeMap.put(input, input);
                            }
                        }
                        regimeCbo.setText(input);
                        if (regimeMap.get(regimeCbo.getText()) == null) {
                            regimeMap.put(input, input);
                        }

                    } else {
                        MessageDialog.openWarning(Display.getCurrent()
                                .getActiveShell(), "Input is empty",
                                "Please define a boundary type \n"
                                        + " 'SELECT TO DEFINE TYPE' again "
                                        + "from Boundary Type dropmenu");
                        regimeCbo.setText("Select Boundary Type");
                        return;
                    }
                } else {
                    return;
                }

            }
            boundarylayer.getBoundaryState().boundaryTypeMap.put(
                    boundarylayer.getBoundaryState().boundaryId,
                    regimeMap.get(regimeCbo.getText()));

            setupEditMenu(UserAction.EDIT_BOUNDARY);
            editCbo.setItems(idStrings);
            String s = "" + boundarylayer.getBoundaryState().boundaryId;
            editCbo.setText(s);
            s = "" + 8;
            boundarylayer.getBoundaryState().boundaryDurationMap
                    .remove(boundarylayer.getBoundaryState().boundaryId);
            boundarylayer.getBoundaryState().boundaryDurationMap.put(
                    boundarylayer.getBoundaryState().boundaryId, 8);
            boundaryLifeSpanCbo.setText(s);
            cancelBtn.setEnabled(true);
            saveDataBtn.setEnabled(true);
            boundarylayer.issueRefresh();
        }
    }

    public void lifeSpanAction() {
        int id = 0;

        if (boundaryLifeSpanCbo.getSelectionIndex() != -1
                && boundarylayer.getBoundaryState().boundariesMap
                        .get(boundarylayer.getBoundaryState().boundaryId) != null) {
            String s = durationMap.get(boundaryLifeSpanCbo.getText());
            getDuration(s);
            try {
                id = Integer.parseInt(s);
            } catch (NumberFormatException nfe) {
                // ignore values that are not numbers
            }
            boundarylayer.getBoundaryState().boundaryDurationMap
                    .remove(boundarylayer.getBoundaryState().boundaryId);
            boundarylayer.getBoundaryState().boundaryDurationMap.put(
                    boundarylayer.getBoundaryState().boundaryId, id);
            cancelBtn.setEnabled(true);
            saveDataBtn.setEnabled(true);
            boundarylayer.issueRefresh();
        }
    }

    public void editAction() {
        deleteBtn.setEnabled(true);
        int id = 0;
        if (editCbo.getSelectionIndex() != -1
                && boundarylayer.getBoundaryState().boundariesMap
                        .get(boundarylayer.getBoundaryState().boundaryId) != null) {
            String s = boundaryIdsMap.get(editCbo.getText());
            getEditedBoundaryId(UserAction.EDIT_BOUNDARY, s);
            try {
                id = Integer.parseInt(s);

            } catch (NumberFormatException nfe) {
                // ignore values that are not numbers
            }

            s = "boundary(" + id + ") is edited";
            boundarylayer.getBoundaryState().logMap.put(id, s);

            if (boundarylayer.getBoundaryState().boundaryTypeMap.get(id) != null) {
                regimeCbo
                        .setText(boundarylayer.getBoundaryState().boundaryTypeMap
                                .get(id));
            } else {
                // this will prevent a null exception
                regimeCbo.setText("Boundary type not yet defined");
            }
            if (boundarylayer.getBoundaryState().isMovingMap.get(id) == true) {
                s = Moving;
                boundarylayer.getBoundaryState().lineIsMoving = true;
                boundarylayer.getBoundaryState().dragingLineNotAllowed = true;
                boundarylayer.getBoundaryState().movingEdited = true;

                adjustMotionBtn.setEnabled(true);
            } else {
                s = Stationary;
                boundarylayer.getBoundaryState().lineIsMoving = false;
                boundarylayer.getBoundaryState().dragingLineNotAllowed = false;
                boundarylayer.getBoundaryState().movingEdited = false;
                adjustMotionBtn.setEnabled(false);
            }
            motionCbo.setText(s);
            s = ""
                    + boundarylayer.getBoundaryState().boundaryDurationMap
                            .get(boundarylayer.getBoundaryState().boundaryId);
            boundaryLifeSpanCbo.setText(s);
            boundarylayer.getBoundaryState().existingBoundaryNotEmptyMap
                    .remove(boundarylayer.getBoundaryState().boundaryId);
            boundarylayer.getBoundaryState().existingBoundaryNotEmptyMap.put(
                    boundarylayer.getBoundaryState().boundaryId, false);

            if (!boundarylayer.getBoundaryState().isMovingMap.get(boundarylayer
                    .getBoundaryState().boundaryId)) {
                boundarylayer.getBoundaryState().prevBoundary = boundarylayer
                        .getBoundaryState().boundariesMap.get(boundarylayer
                        .getBoundaryState().boundaryId);
            }

            boundarylayer.issueRefresh();

        }
    }

    public void deleteAction() {
        MessageBox messageBox = new MessageBox(getShell(), SWT.ICON_QUESTION
                | SWT.YES);
        messageBox.setText("Deleting?");
        messageBox.setMessage("Are you sure you want to delete this boundary?");
        int buttonID = messageBox.open();
        switch (buttonID) {
        case SWT.YES:

            if (boundarylayer.getBoundaryState().boundariesMap
                    .get(boundarylayer.getBoundaryState().boundaryId) != null) {
                String s = "" + boundarylayer.getBoundaryState().boundaryId;
                boundaryIdsMap.remove(s);
                int id = boundarylayer.getBoundaryState().boundaryId;
                boundarylayer.getBoundaryState().boundaryTypeMap.remove(id);
                boundarylayer.getBoundaryState().boundariesMap.remove(id);
                boundarylayer.getBoundaryState().timePointsMap.remove(id);
                boundarylayer.getBoundaryState().dragMePointMap.remove(id);
                boundarylayer.getBoundaryState().lineMovedMap.remove(id);
                boundarylayer.getBoundaryState().isMovingMap.remove(id);
                boundarylayer.getBoundaryState().boundaryDurationMap.remove(id);
                boundarylayer.getBoundaryState().createTimeMap.remove(id);
                boundarylayer.getBoundaryState().editedTimeMap.remove(id);
                boundarylayer.getBoundaryState().expirationTimeMap.remove(id);
                boundarylayer.getBoundaryState().existingBoundaryNotEmptyMap
                        .remove(id);
                ;
                boundarylayer.getBoundaryState().vertexAngleMap.remove(id);
                boundarylayer.getBoundaryState().vertexSpeedMap.remove(id);
                s = "" + "boundary(" + id + ") is deleted";
                boundarylayer.getBoundaryState().logMap.put(id, s);
                // Map the delete boundaries Ids list
                boundarylayer.getBoundaryState().forbiddenBoundaryIdsMap.put(
                        boundarylayer.getBoundaryState().boundaryId,
                        boundarylayer.getBoundaryState().boundaryId);

                //
                setupEditMenu(UserAction.EDIT_BOUNDARY);
                editCbo.setItems(idStrings);

                int idMax = 0;

                for (int id1 : boundarylayer.getBoundaryState().boundariesMap
                        .keySet()) {

                    if (id1 > idMax) {
                        idMax = id1;
                    }
                }
                s = "" + idMax;
                if (idMax == 0) {
                    s = "   ";
                }

                getEditedBoundaryId(UserAction.DELETE_BOUNDARY, s);
                editCbo.setText(s);
                if (boundarylayer.getBoundaryState().boundaryTypeMap.get(idMax) != null) {
                    regimeCbo
                            .setText(boundarylayer.getBoundaryState().boundaryTypeMap
                                    .get(idMax));
                } else {
                    regimeCbo.setText("   ");
                }

                if (boundarylayer.getBoundaryState().boundaryTypeMap.get(idMax) == null) {

                    s = Stationary;

                } else {
                    if (boundarylayer.getBoundaryState().isMovingMap.get(idMax) == true) {
                        s = Moving;
                    } else {
                        s = Stationary;
                    }

                }
                motionCbo.setText(s);
                if (boundarylayer.getBoundaryState().boundariesMap
                        .get(boundarylayer.getBoundaryState().boundaryId) != null) {
                    s = ""
                            + boundarylayer.getBoundaryState().boundaryDurationMap
                                    .get(idMax);
                } else {
                    s = "8";
                    regimeCbo.setText("Select boundary type");
                }
                boundaryLifeSpanCbo.setText(s);
                saveBoundaryData(UserAction.SAVE);
                deleteBtn.setEnabled(false);
                boundarylayer.issueRefresh();
            }

            break;
        default:
            boundarylayer.getBoundaryState().userAction = UserAction.NONE;
            break;
        }
    }

    public void setBtnFalseAfterCancelAction() {
        cancelBtn.setEnabled(false);
        saveDataBtn.setEnabled(false);
        saveDataBtn.setBackground(Display.getCurrent().getSystemColor(
                SWT.COLOR_GRAY));
    }

    public void enableMotionSelection(boolean enabled) {
        motionCbo.setEnabled(enabled);
    }

    public void setBtnTrueAfterCancelAction() {
        cancelBtn.setEnabled(true);
        saveDataBtn.setEnabled(true);
        saveDataBtn.setBackground(Display.getCurrent().getSystemColor(
                SWT.COLOR_RED));
    }

    private void fillBoundaryMenus() {
        cancelBtn.setEnabled(false);
        saveDataBtn.setEnabled(false);
        deleteBtn.setEnabled(false);
        adjustMotionBtn.setEnabled(false);
        if (boundarylayer.getBoundaryState().boundariesMap.size() != 0) {
            Iterator<Integer> iterator = boundarylayer.getBoundaryState().boundariesMap
                    .keySet().iterator();
            int id = 0;
            int boundaryId = 0;
            String s = null;
            setupEditMenu(UserAction.EDIT_BOUNDARY);
            while (iterator.hasNext()) {
                boundaryId = iterator.next();
                idStrings[id] = "" + boundaryId;
                id++;
            }

            setupEditMenu(UserAction.EDIT_BOUNDARY);
            editCbo.setItems(idStrings);
            s = "" + boundarylayer.getBoundaryState().boundaryId;
            editCbo.setText(s);
            s = ""
                    + boundarylayer.getBoundaryState().boundaryTypeMap
                            .get(boundaryId);
            regimeCbo.setItems(regimeStrings);
            regimeCbo.setText(s);
            if (boundarylayer.getBoundaryState().isMovingMap.get(boundaryId) == true) {
                s = Moving;
            } else {
                s = Stationary;
            }
            motionCbo.setItems(modeStrings);
            motionCbo.setText(s);
            s = ""
                    + boundarylayer.getBoundaryState().boundaryDurationMap
                            .get(boundaryId);
            boundaryLifeSpanCbo.setItems(durationStrings);
            boundaryLifeSpanCbo.setText(s);
        } else {
            regimeCbo.setText("Select boundary type");
            motionCbo.setText(modeStrings[0]);
            boundaryLifeSpanCbo.setText(duration);
            boundarylayer.getBoundaryState().isMovingMap.put(
                    boundarylayer.getBoundaryState().boundaryId, false);
        }
    }

}
