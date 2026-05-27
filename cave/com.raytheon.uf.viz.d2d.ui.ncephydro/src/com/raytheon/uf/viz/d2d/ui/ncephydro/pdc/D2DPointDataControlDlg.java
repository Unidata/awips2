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
package com.raytheon.uf.viz.d2d.ui.ncephydro.pdc;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IEditorPart;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.pdc.AdHocData;
import com.raytheon.uf.viz.pdc.AdHocElements;
import com.raytheon.uf.viz.pdc.FilteringDlg;
import com.raytheon.uf.viz.pdc.ISavePdcPresets;
import com.raytheon.uf.viz.pdc.PDC_SaveDlg;
import com.raytheon.uf.viz.pdc.PointDataControlManager;
import com.raytheon.uf.viz.pdc.PointDataControlPresets;
import com.raytheon.uf.viz.pdc.data.PointControlPeTs;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.colorscalemgr.HydroColorManager;
import com.raytheon.viz.hydrocommon.colorscalemgr.NamedColorSetGroup;
import com.raytheon.viz.hydrocommon.data.StationDisplayData;
import com.raytheon.viz.hydrocommon.datamanager.PDCDataManager;
import com.raytheon.viz.hydrocommon.events.StationDisplayUpdateEvent;
import com.raytheon.viz.hydrocommon.pdc.PDCConstants;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;
import com.raytheon.viz.hydrocommon.pdc.data.PointDataPreset;
import com.raytheon.viz.hydrocommon.resource.AbstractMultiPointResource;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * D2D Point Data Control dialog. Formerly known as Station Obs Viewer in A1.
 * This code is based off the Hydro PointDataControlDlg. This version does not
 * use the timestep mode so no timestep code is in this class.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 13, 2018   7379     mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 */

public class D2DPointDataControlDlg extends CaveSWTDialog
        implements ISavePdcPresets {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(D2DPointDataControlDlg.class);

    /** Type sources to always include. */
    private static final String[] RAIN_TYPE_SOURCES = { "RG", "RP", "RM", "RR",
            "RZ", "R2" };

    private ThreadLocal<SimpleDateFormat> sdf = new ThreadLocal<SimpleDateFormat>() {
        @Override
        protected SimpleDateFormat initialValue() {
            return new SimpleDateFormat("yyyy-MM-dd HH:mm");
        }
    };

    private Combo selPresetCbo;

    private Combo elementTypeCbo;

    private Combo physicalElementCbo;

    private Text timeTF;

    private Spinner hoursSpnr;

    private Combo valueIsCbo;

    private Button typeSourceChk;

    private Button serviceAreaChk;

    private Button dataSourceChk;

    /** Filter dialog for Type/Source. */
    private FilteringDlg typeSourceDlg;

    /** Filter dialog for Service Area. */
    private FilteringDlg serviceDlg;

    /** Filter dialog for Data Source . */
    private FilteringDlg dataSourceDlg;

    /** Dialog to save preset options */
    private PDC_SaveDlg saveDlg;

    private Button suppressMissingChk;

    private Button suppressZeroChk;

    private Button suppressNonFcstPtsChk;

    private Button valueChk;

    private Button idChk;

    private Button nameChk;

    private Button iconChk;

    private Button timeChk;

    private Combo riverColorValCbo;

    private PDCDataManager dataManager = PDCDataManager.getInstance();

    private List<PointDataPreset> presets;

    private AdHocData adHocData;

    private Date previousDate = SimulatedTime.getSystemTime().getTime();

    private int previousHours;

    private Calendar selectedTime;

    public D2DPointDataControlDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Point Data Control");

        // Set up the colors
        HydroColorManager colorManager = HydroColorManager.getInstance();
        NamedColorSetGroup ncsg = colorManager.getDefaultColorSetGroup();
        colorManager.populateDefaultColorUseSets(ncsg.getColorGroupArray());

        // Initialize by calling
        D2DStationDisplay.getInstance();
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
        D2DStationDisplay.getInstance().dispose();
    }

    @Override
    protected void initializeComponents(final Shell shell) {
        adHocData = new AdHocData();
        createPresetsGroup();
        createElementsGroup();
        createValueTimeGroup();
        createFilteringGroup();
        createDisplayGroup();
        createButtons();

        populatePresetData(null);
        drawMap();
    }

    @Override
    protected void preOpened() {
        StationDisplayData dispData = new StationDisplayData();
        /* Set the map symbol options */
        dispData.setGage(iconChk.getSelection());
        dispData.setId(idChk.getSelection());
        dispData.setValue(valueChk.getSelection());
        dispData.setName(nameChk.getSelection());
        dispData.setTime(timeChk.getSelection());

        /*
         * Enable the plotting of station data. Inform the hv areal routines
         * that they should plot the station data on the screen.
         */
        HydroDisplayManager manager = HydroDisplayManager.getInstance();
        manager.setDrawStation(true);
        manager.setStationData(dispData);
    }

    private void createPresetsGroup() {
        GridLayout groupLayout = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group presetGroup = new Group(shell, SWT.NONE);
        presetGroup.setText(" Predefined Option Set ");
        presetGroup.setLayout(groupLayout);
        presetGroup.setLayoutData(gd);

        // Create a container to hold the label and the combo box.
        Composite presetComp = new Composite(presetGroup, SWT.NONE);
        GridLayout presetCompLayout = new GridLayout(2, false);
        presetCompLayout.horizontalSpacing = 10;
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
                shell.setCursor(getWaitCursor());
                drawMap();
                shell.setCursor(null);
            }
        });

        if (selPresetCbo.getItemCount() > 0) {
            selPresetCbo.select(0);
        }

        GridLayout gl = new GridLayout(2, true);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(presetGroup, SWT.NONE);
        buttonComp.setLayout(gl);
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button insertUpdateBtn = new Button(buttonComp, SWT.PUSH);
        insertUpdateBtn.setText("Insert/Update");
        insertUpdateBtn.setLayoutData(gd);
        insertUpdateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                openSaveDialog();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
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
        // PDCOptionData pcOptions = PDCOptionData.getInstance();
        Group elementsGroup = new Group(shell, SWT.NONE);
        elementsGroup.setText(" Elements ");
        GridLayout groupLayout = new GridLayout(2, false);
        elementsGroup.setLayout(groupLayout);
        elementsGroup.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        GridData gd = new GridData(135, SWT.DEFAULT);
        elementTypeCbo = new Combo(elementsGroup,
                SWT.DROP_DOWN | SWT.READ_ONLY);
        elementTypeCbo.setLayoutData(gd);
        elementTypeCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleElementTypeAction();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        physicalElementCbo = new Combo(elementsGroup,
                SWT.DROP_DOWN | SWT.READ_ONLY);
        physicalElementCbo.setLayoutData(gd);
        physicalElementCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handlePhysicalElementAction();
            }
        });

        populateCombosWithAdHoc();
    }

    private void createValueTimeGroup() {
        Group valueTimeGroup = new Group(shell, SWT.NONE);
        valueTimeGroup.setText(" Value/Time ");
        GridLayout gl = new GridLayout(1, false);
        GridData mainGridData = new GridData(SWT.FILL, SWT.DEFAULT, true,
                false);
        valueTimeGroup.setLayout(gl);
        valueTimeGroup.setLayoutData(mainGridData);

        Composite timeComp = new Composite(valueTimeGroup, SWT.NONE);
        GridLayout timeGl = new GridLayout(5, false);
        timeComp.setLayout(timeGl);

        // Add the time arrow buttons
        Composite timeArrowsComp = new Composite(timeComp, SWT.NONE);
        GridLayout timeArrowLayout = new GridLayout(1, true);
        timeArrowLayout.verticalSpacing = 0;
        timeArrowLayout.marginWidth = 0;
        timeArrowLayout.marginHeight = 0;
        timeArrowsComp.setLayout(timeArrowLayout);

        GridData gd = new GridData(25, 25);
        Button upTimeBtn = new Button(timeArrowsComp, SWT.ARROW);
        upTimeBtn.setLayoutData(gd);
        upTimeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleTimeChange(Calendar.DAY_OF_MONTH, 1);
            }
        });

        gd = new GridData(25, 25);
        Button dnTimeBtn = new Button(timeArrowsComp, SWT.ARROW | SWT.DOWN);
        dnTimeBtn.setLayoutData(gd);
        dnTimeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleTimeChange(Calendar.DAY_OF_MONTH, -1);
            }
        });

        // Add the time text field
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        timeTF = new Text(timeComp, SWT.BORDER);
        GC gc = new GC(timeTF);
        gd.widthHint = gc.textExtent("0000-00-00 00:00").x;
        gc.dispose();
        timeTF.setLayoutData(gd);
        timeTF.setEditable(false);

        // Add the hours arrows button
        Composite hoursArrowsComp = new Composite(timeComp, SWT.NONE);
        GridLayout hoursArrowLayout = new GridLayout(1, true);
        hoursArrowLayout.verticalSpacing = 0;
        hoursArrowLayout.marginWidth = 0;
        hoursArrowLayout.marginHeight = 0;
        hoursArrowsComp.setLayout(hoursArrowLayout);

        gd = new GridData(25, 25);
        Button upHoursBtn = new Button(hoursArrowsComp, SWT.ARROW);
        upHoursBtn.setLayoutData(gd);
        upHoursBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleTimeChange(Calendar.HOUR_OF_DAY, 1);
            }
        });

        gd = new GridData(25, 25);
        Button dnHoursBtn = new Button(hoursArrowsComp, SWT.ARROW | SWT.DOWN);
        dnHoursBtn.setLayoutData(gd);
        dnHoursBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleTimeChange(Calendar.HOUR_OF_DAY, -1);
            }
        });

        // ------------------------------------
        // Add the hours text field & label
        // ------------------------------------
        gd = new GridData(33, SWT.DEFAULT);
        hoursSpnr = new Spinner(timeComp, SWT.BORDER);
        hoursSpnr.setDigits(0);
        hoursSpnr.setIncrement(1);
        hoursSpnr.setPageIncrement(5);
        hoursSpnr.setSelection(24);
        hoursSpnr.setLayoutData(gd);
        hoursSpnr.setMaximum(1000);

        Label hoursLbl = new Label(timeComp, SWT.NONE);
        hoursLbl.setText("Hrs");

        // -------------------------------------
        // Create the values composite
        // -------------------------------------
        Composite valuesComp = new Composite(valueTimeGroup, SWT.NONE);
        GridLayout valuesGl = new GridLayout(2, false);
        valuesComp.setLayout(valuesGl);

        Label valueLbl = new Label(valuesComp, SWT.NONE);
        valueLbl.setText("Value Is");

        valueIsCbo = new Combo(valuesComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        valueIsCbo.add("Latest Value");
        valueIsCbo.add("Value for Selected Time");
        valueIsCbo.add("Min Value in Window");
        valueIsCbo.add("Max Value in Window");
        valueIsCbo.select(0);
        valueIsCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                PDCOptionData pcOptions = PDCOptionData.getInstance();
                pcOptions.setTimeMode(valueIsCbo.getSelectionIndex());
                shell.setCursor(getWaitCursor());
                drawMap();
                shell.setCursor(null);
            }
        });
    }

    private void createFilteringGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(2, true);
        Group filteringGroup = new Group(shell, SWT.NONE);
        filteringGroup.setText(" Filtering ");
        filteringGroup.setLayout(gl);
        filteringGroup.setLayoutData(gd);

        gl = new GridLayout(2, false);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Composite leftFilterComp = new Composite(filteringGroup, SWT.NONE);
        leftFilterComp.setLayout(gl);
        leftFilterComp.setLayoutData(gd);

        // ---------------------------------------
        // Left section of the filter group
        // ---------------------------------------
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        typeSourceChk = new Button(leftFilterComp, SWT.CHECK);
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
                shell.setCursor(getWaitCursor());
                drawMap();
                shell.setCursor(null);
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button typeSourceBtn = new Button(leftFilterComp, SWT.PUSH);
        typeSourceBtn.setText("Type/Source...");
        typeSourceBtn.setLayoutData(gd);
        typeSourceBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (typeSourceDlg == null) {
                    typeSourceDlg = new FilteringDlg(shell,
                            "Type Source Selection Dialog",
                            FilteringDlg.DialogType.TYPE_SOURCE,
                            physicalElementCbo.getItem(
                                    physicalElementCbo.getSelectionIndex()));
                    typeSourceDlg.addCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            redrawIfTrue(returnValue);
                        }
                    });
                }
                typeSourceDlg.open();
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        serviceAreaChk = new Button(leftFilterComp, SWT.CHECK);
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
                shell.setCursor(getWaitCursor());
                drawMap();
                shell.setCursor(null);
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button serviceAreaBtn = new Button(leftFilterComp, SWT.PUSH);
        serviceAreaBtn.setText("Service Area...");
        serviceAreaBtn.setLayoutData(gd);
        serviceAreaBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (serviceDlg == null) {
                    serviceDlg = new FilteringDlg(shell, "Service Area",
                            FilteringDlg.DialogType.SERVICE_AREA);
                    serviceDlg.addCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            redrawIfTrue(returnValue);
                            serviceDlg = null;
                        }
                    });
                }
                serviceDlg.open();
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        // gd.horizontalAlignment = SWT.END;
        dataSourceChk = new Button(leftFilterComp, SWT.CHECK);
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
                shell.setCursor(getWaitCursor());
                drawMap();
                shell.setCursor(null);
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button dataSourceBtn = new Button(leftFilterComp, SWT.PUSH);
        dataSourceBtn.setText("Data Source...");
        dataSourceBtn.setLayoutData(gd);
        dataSourceBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (dataSourceDlg == null) {
                    dataSourceDlg = new FilteringDlg(shell, "Data Source",
                            FilteringDlg.DialogType.DATA_SOURCE);
                    dataSourceDlg.addCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            redrawIfTrue(returnValue);
                            dataSourceDlg = null;
                        }
                    });
                }
                dataSourceDlg.open();
            }
        });

        // ---------------------------------------
        // Right section of the filter group
        // ---------------------------------------
        gl = new GridLayout(1, false);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite rightFilterComp = new Composite(filteringGroup, SWT.NONE);
        rightFilterComp.setLayout(gl);
        rightFilterComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalAlignment = SWT.CENTER;
        Label suppressLbl = new Label(rightFilterComp, SWT.NONE);
        suppressLbl.setLayoutData(gd);
        suppressLbl.setText("Suppress:");

        int indent = 35;
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalIndent = indent;
        suppressMissingChk = new Button(rightFilterComp, SWT.CHECK);
        suppressMissingChk.setText("Missing");
        suppressMissingChk.setLayoutData(gd);
        suppressMissingChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                PDCOptionData pcOptions = PDCOptionData.getInstance();
                if (suppressMissingChk.getSelection()) {
                    // Hide missing
                    pcOptions.setSupressMissing(0);
                } else {
                    // show missing
                    pcOptions.setSupressMissing(1);
                }
                shell.setCursor(getWaitCursor());
                drawMap();
                shell.setCursor(null);
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalIndent = indent;
        suppressZeroChk = new Button(rightFilterComp, SWT.CHECK);
        suppressZeroChk.setText("Zeros");
        suppressZeroChk.setLayoutData(gd);
        suppressZeroChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                PDCOptionData pcOptions = PDCOptionData.getInstance();
                if (suppressZeroChk.getSelection()) {
                    pcOptions.setValueFilterOperation(2);
                    pcOptions.setValueFilterValue(0);
                } else {
                    pcOptions.setValueFilterOperation(0);
                }
                shell.setCursor(getWaitCursor());
                drawMap();
                shell.setCursor(null);
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalIndent = indent;
        suppressNonFcstPtsChk = new Button(rightFilterComp, SWT.CHECK);
        suppressNonFcstPtsChk.setText("Non-FcstPts");
        suppressNonFcstPtsChk.setLayoutData(gd);
        suppressNonFcstPtsChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                PDCOptionData pcOptions = PDCOptionData.getInstance();
                if (suppressNonFcstPtsChk.getSelection()) {
                    pcOptions.setFcstptsOnly(1);
                } else {
                    pcOptions.setFcstptsOnly(0);
                }
                shell.setCursor(getWaitCursor());
                drawMap();
                shell.setCursor(null);
            }
        });
    }

    /**
     * Update the selectedTime and redraw
     *
     * @param the
     *            field to update
     * @param the
     *            amount to change
     */
    private void handleTimeChange(int field, int changeAmount) {
        selectedTime.add(field, changeAmount);
        timeTF.setText(sdf.get().format(selectedTime.getTime()));
        shell.setCursor(getWaitCursor());
        drawMap();
        shell.setCursor(null);
    }

    private void createDisplayGroup() {
        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group displayGroup = new Group(shell, SWT.NONE);
        displayGroup.setText(" Display ");
        displayGroup.setLayout(gl);
        displayGroup.setLayoutData(gd);

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

        timeChk = new Button(chkBxComp, SWT.CHECK);
        timeChk.setText("Time");
        timeChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                PDCOptionData pcOptions = PDCOptionData.getInstance();
                if (timeChk.getSelection()) {
                    pcOptions.setTime(1);
                }
                fireUpdateEvent();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(2, false);
        Composite colorRiverComp = new Composite(displayGroup, SWT.NONE);
        colorRiverComp.setLayout(gl);
        colorRiverComp.setLayoutData(gd);

        Label colorValLbl = new Label(colorRiverComp, SWT.NONE);
        colorValLbl.setText("River Basis: ");

        riverColorValCbo = new Combo(colorRiverComp,
                SWT.DROP_DOWN | SWT.READ_ONLY);
        riverColorValCbo.add("Observed Value");
        riverColorValCbo.add("Forecast");
        riverColorValCbo.add("Max (Obs, Fcst)");
        riverColorValCbo.select(0);
        riverColorValCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleRiverColorChange();
                shell.setCursor(getWaitCursor());
                // updateData = true;
                drawMap();
                shell.setCursor(null);
            }
        });
    }

    private void createButtons() {
        GridLayout gl = new GridLayout(2, true);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(gl);
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button displayBtn = new Button(buttonComp, SWT.PUSH);
        displayBtn.setText(" Display ");
        displayBtn.setLayoutData(gd);
        displayBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.setCursor(getWaitCursor());
                drawMap();
                shell.setCursor(null);
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button resetBtn = new Button(buttonComp, SWT.PUSH);
        resetBtn.setText("Reset");
        resetBtn.setLayoutData(gd);
        resetBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                resetDisplay();
            }
        });
    }

    private void populateCombosWithAdHoc() {
        PointDataControlManager manager = PointDataControlManager.getInstance();
        PointControlPeTs petsdata = manager.getPCPeTsData();
        elementTypeCbo.removeAll();
        physicalElementCbo.removeAll();

        List<AdHocElements> adHocDataArray = new ArrayList<>(
                adHocData.getDataElementArray());
        if (PointControlPeTs.checkShefProcObs()) {
            adHocDataArray.removeIf(
                    i -> "Processed".equalsIgnoreCase(i.getElementType()));
        }

        for (AdHocElements adHocElements : adHocDataArray) {
            elementTypeCbo.add(adHocElements.getElementType());
        }

        AdHocElements elements;
        elements = adHocData.getDataElement(0);
        List<String> dataElements = elements.getDataElementArray();

        for (String elementStr : dataElements) {
            physicalElementCbo.add(elementStr);
        }

        petsdata.setElementCount(adHocDataArray.size());

        elementTypeCbo.select(0);
        physicalElementCbo.select(0);
    }

    /**
     * Populate the PDCOptionData object to select the appropriate values in the
     * Point Data Control dialog.
     *
     * @param id
     *            The preset id
     */
    @Override
    public void populatePresetData(String id) {
        selPresetCbo.removeAll();

        try {
            presets = dataManager.getPresets();
            if (!presets.isEmpty()) {
                for (PointDataPreset preset : presets) {
                    selPresetCbo.add(preset.getDescription());
                    if ((id != null)
                            && (preset.getPresetId().equalsIgnoreCase(id))) {
                        setPcOptionsUsingPresets(preset);
                    }
                }

                if (id == null) {
                    selPresetCbo.select(0);
                    setPcOptionsUsingPresets(presets.get(0));
                }

            }
            setGUIFromOptionsData();
        } catch (VizException e) {
            statusHandler.error("Failed to load the Point Data Presets.", e);
        }
    }

    private void setPcOptionsUsingPresets(PointDataPreset preset) {
        try {
            Map<String, String[]> presetData = preset.getParsedPresetData();
            PDCOptionData pcOptions = PointDataControlPresets
                    .getPcOptions(presetData);
        } catch (ParseException e) {
            String presetString = preset.getPresetString();
            statusHandler.error(
                    "Failed to parse Point Data Preset: " + presetString + ".",
                    e);
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
        String selection = riverColorValCbo
                .getItem(riverColorValCbo.getSelectionIndex());
        if (selection
                .equals(PDCConstants.StageBasis.BASIS_FCST.getStageBasis())) {
            pcOptions.setStageBasis(1);
        } else if (selection
                .equals(PDCConstants.StageBasis.BASIS_OBS.getStageBasis())) {
            pcOptions.setStageBasis(0);
        } else if (selection
                .equals(PDCConstants.StageBasis.BASIS_MOFO.getStageBasis())) {
            pcOptions.setStageBasis(2);
        }
    }

    private void setGUIFromOptionsData() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        /* Set the element Type */
        int elementType = pcOptions.getElementType();
        elementTypeCbo.select(elementType);

        physicalElementCbo.select(pcOptions.getPeSelection());

        handleAdhocElementComboSelection();

        /* set the time mode */
        valueIsCbo.select(pcOptions.getTimeMode());

        /* set the endtime and the duration */
        Date date = SimulatedTime.getSystemTime().getTime();
        this.selectedTime = TimeUtil.newGmtCalendar(date);
        TimeUtil.minCalendarFields(selectedTime, Calendar.MINUTE,
                Calendar.SECOND, Calendar.MILLISECOND);
        timeTF.setText(sdf.get().format(selectedTime.getTime()));
        hoursSpnr.setSelection(pcOptions.getDurHours());

        /* set toggle button that corresponds to filter by typesource option */
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
        suppressMissingChk.setSelection(pcOptions.getSupressMissing() == 0);
        suppressNonFcstPtsChk.setSelection(pcOptions.getFcstptsOnly() == 1);

        if (pcOptions.getValueFilterOperation() == 2) {
            if (pcOptions.getValueFilterValue() == 0) {
                suppressZeroChk.setSelection(true);
            }
        } else {
            suppressZeroChk.setSelection(false);
        }

        /* set the pc_precipPeOM based on precip_pe_filter */
        pcOptions.getPrecipPeFilter();

        /* set the map display options */
        valueChk.setSelection(pcOptions.getValue() == 1);
        idChk.setSelection(pcOptions.getId() == 1);
        nameChk.setSelection(pcOptions.getName() == 1);
        iconChk.setSelection(pcOptions.getIcon() == 1);
        timeChk.setSelection(pcOptions.getTime() == 1);

        /* set the river options. */
        riverColorValCbo.select(pcOptions.getStageBasis());
    }

    private void handleElementTypeAction() {
        handleAdhocElementComboSelection();
        HydroDisplayManager.getInstance().setDataChanged(true);
        shell.setCursor(getWaitCursor());
        drawMap();
        shell.setCursor(null);
    }

    private void handlePhysicalElementAction() {
        handlePeSelection();
        HydroDisplayManager.getInstance().setDataChanged(true);
        shell.setCursor(getWaitCursor());
        drawMap();
        shell.setCursor(null);
    }

    /**
     * Perform updates for currently selected element type.
     */
    private void handleAdhocElementComboSelection() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        physicalElementCbo.removeAll();

        int index = elementTypeCbo.getSelectionIndex();

        pcOptions.setElementType(index);

        AdHocElements elements = adHocData.getDataElement(index);
        List<String> dataElements = elements.getDataElementArray();

        for (String elementStr : dataElements) {
            physicalElementCbo.add(elementStr);
        }

        physicalElementCbo.select(pcOptions.getPeSelection());

        handlePeSelection();
    }

    /**
     * Update perspective to display the current Selected Preset selection.
     */
    private void handlePresetSelection() {
        if (selPresetCbo.getSelectionIndex() == -1) {
            /* No selection made */
            return;
        }

        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();

        /*
         * Indicate to the pointcontrol manager routines that new data must be
         * retrieved from the IHFS database.
         */
        pdcManager.setRedraw(true);

        PointDataPreset presetData = presets
                .get(selPresetCbo.getSelectionIndex());

        /*
         * Even if there is an option value list to process, first set all of
         * the members of the PDCOptionData to a default value. This needs to be
         * done just in case not all of the options are specified in the
         * predefined option set. We need to make sure that all options have a
         * valid value.
         */
        PDCOptionData.getInstance().reset();
        setPcOptionsUsingPresets(presetData);
        setGUIFromOptionsData();
    }

    /**
     * Update the PDCOptionData based on current Physical Element selection.
     */
    private void handlePeSelection() {
        int index = physicalElementCbo.getSelectionIndex();
        if (index < 0) {
            return;
        }

        PDCOptionData pcOptions = PDCOptionData.getInstance();
        List<String> tsList = new ArrayList<>();
        pcOptions.setPcAndpp(0);
        pcOptions.setPrimary(0);

        /* Store just the pe in the SelectedAdHocElement String */
        String[] parts = physicalElementCbo.getItem(index).split(" ", 2);
        pcOptions.setSelectedAdHocElementString(parts[0]);

        /*
         * Store the whole selection in the SelectedAdHocElementFullString
         */
        pcOptions.setSelectedAdHocElementFullString(
                physicalElementCbo.getItem(index));

        pcOptions.setPeSelection(index);

        /*
         * Toggle the states of the pc_options Primary and PCandPP flags.
         */
        if ((physicalElementCbo.getSelectionIndex() == 0) && (pcOptions
                .getElementType() == HydroConstants.AdHocDataElementType.RIVER_AD_HOC_TYPE
                        .getAdHocDataElementType())) {
            pcOptions.setPrimary(1);
        }

        if ((physicalElementCbo.getSelectionIndex() == 0) && (pcOptions
                .getElementType() == HydroConstants.AdHocDataElementType.RAIN_AD_HOC_TYPE
                        .getAdHocDataElementType())) {
            pcOptions.setPcAndpp(1);
            for (String ts : RAIN_TYPE_SOURCES) {
                tsList.add(ts);
            }
            pcOptions.setTypeSourceChosenList(tsList);
            pcOptions.setTypeSourceChosenCount(tsList.size());
        }
    }

    private void drawMap() {
        StationDisplayData dispData = new StationDisplayData();

        /* Set the map symbol options */
        dispData.setGage(iconChk.getSelection());
        dispData.setId(idChk.getSelection());
        dispData.setValue(valueChk.getSelection());
        dispData.setName(nameChk.getSelection());
        dispData.setTime(timeChk.getSelection());

        /*
         * Enable the plotting of station data. Inform the hv areal routines
         * that they should plot the station data on the screen.
         */
        HydroDisplayManager manager = HydroDisplayManager.getInstance();
        manager.setDrawStation(true);
        manager.setStationData(dispData);

        /* get and set the time fields */
        try {
            setTimeFields();

            /* now map the data */
            retrieveData();
        } catch (ParseException e) {
            statusHandler.error("Failed to parse the time fields.", e);
        }
    }

    /**
     * Schedule a request to retrive map data.
     */
    private void retrieveData() {
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        boolean drawStationFlag = HydroDisplayManager.getInstance()
                .isDrawStation();
        if (drawStationFlag) {
            pdcManager.scheduleRequest(true,
                    PointDataControlManager.REQUEST_TYPE.REQUEST_AD_HOC);
        }
    }

    /**
     * Set the time fields in PDCOptionData base on the current Value/Time
     * selections.
     *
     * @throws ParseException
     */
    @Override
    public void setTimeFields() throws ParseException {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        Date date = selectedTime.getTime();

        SimpleDateFormat fullFormat = new SimpleDateFormat(
                "yyyy-MM-dd HH:mm:ss");
        fullFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        int hours = hoursSpnr.getSelection();
        pcOptions.setValidTime(date);
        pcOptions.setPcTimeStr(fullFormat.format(date));
        pcOptions.setDurHours(hours);

        /*
         * check if this time has changed from the last time in order to
         * determine whether a retrieval is needed
         */
        if ((date.compareTo(previousDate) != 0) || (hours != previousHours)) {
            previousDate = SimulatedTime.getSystemTime().getTime();
            previousHours = hours;
        }
    }

    /**
     * Determine if a redraw needs to be performed.
     *
     * @param returnValue
     *            - return value from the Filtering dialog.
     */
    private void redrawIfTrue(Object returnValue) {
        if (returnValue instanceof Boolean) {
            boolean redraw = (Boolean) returnValue;
            if (redraw) {
                shell.setCursor(getWaitCursor());
                drawMap();
                shell.setCursor(null);
            }
        }
    }

    private Cursor getWaitCursor() {
        return getDisplay().getSystemCursor(SWT.CURSOR_WAIT);
    }

    /**
     * Use the Point data control manager to perform a station display update
     * event.
     */
    private void fireUpdateEvent() {
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        StationDisplayUpdateEvent event = new StationDisplayUpdateEvent(this,
                valueChk.getSelection(), iconChk.getSelection(),
                idChk.getSelection(), nameChk.getSelection(),
                timeChk.getSelection());
        HydroDisplayManager.getInstance()
                .setStationData(event.getDisplayData());
        pdcManager.fireUpdateEvent(event);
    }

    private void resetDisplay() {
        IDisplayPane pane = null;
        IEditorPart part = EditorUtil.getActiveEditor();
        if (part instanceof AbstractEditor) {
            AbstractEditor ae = ((AbstractEditor) part);
            pane = ae.getActiveDisplayPane();
        }

        ResourceList rl = pane.getDescriptor().getResourceList();
        Iterator<ResourcePair> iter = rl.iterator();
        while (iter.hasNext()) {
            ResourcePair rp = iter.next();
            if (!rp.getProperties().isMapLayer()) {
                // mark for removal
                AbstractVizResource<?, ?> rsc = rp.getResource();
                if (rsc instanceof AbstractMultiPointResource) {
                    rl.removeRsc(rsc);
                    rsc.dispose();
                }
            }
        }
    }

    /**
     * Delete the Selected Preset selections' values and update display.
     */
    private void delete() {
        PointDataPreset presetSelection = presets
                .get(selPresetCbo.getSelectionIndex());
        if (presetSelection == null) {
            populatePresetData(null);
            return;
        }

        try {
            dataManager.deletePreset(presetSelection.getPresetId());
        } catch (VizException e) {
            statusHandler.error("Failed to delete Point Data Preset: "
                    + presetSelection.toString() + ".", e);
        }

        populatePresetData(null);
    }

    /**
     * Opens the save dialog.
     */
    private void openSaveDialog() {
        if (saveDlg == null || saveDlg.isDisposed()) {
            saveDlg = new PDC_SaveDlg(shell, selPresetCbo.getSelectionIndex(),
                    this);
            saveDlg.open();
        } else {
            saveDlg.bringToTop();
        }
    }

    @Override
    public List<PointDataPreset> getPresetData() {
        return this.presets;
    }
}
