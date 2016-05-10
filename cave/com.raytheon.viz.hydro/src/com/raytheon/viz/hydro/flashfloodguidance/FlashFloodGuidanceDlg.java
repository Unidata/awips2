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
package com.raytheon.viz.hydro.flashfloodguidance;

import java.io.File;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
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
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.dataplugin.shef.tables.Admin;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.constants.FFGConstants;
import com.raytheon.viz.hydrocommon.constants.FFGConstants.ResolutionLevel;
import com.raytheon.viz.hydrocommon.data.ArealData;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Flash Flood Guidance dialog for Hydroview.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation.
 * 13 Oct 2009  2256       mpduff      Implement the dialog.
 * 07 Feb 2013  1578       rferrel     Changes for non-blocking dialog.
 * Jul 21, 2015 4500       rjpeter     Use Number in blind cast.
 * Aug 05, 2015 4486       rjpeter     Changed Timestamp to Date.
 * Mar 15, 2016 5483       randerso    Fix GUI sizing issues
 * Mar 15, 2016 5483       bkowal      Fix GUI sizing issues
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class FlashFloodGuidanceDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    /** Date format for the dates */
    private static SimpleDateFormat sdf = null;

    private static SimpleDateFormat xmrgDateFormat;

    /** List of RFC names */
    private static final String[] RFC_NAMES = { "ABRFC", "AKRFC", "CBRFC",
            "CNRFC", "LMRFC", "MARFC", "MBRFC", "NCRFC", "NERFC", "NWRFC",
            "OHRFC", "SERFC", "WGRFC", "ALL_RFCS" };

    /** List of durations */
    private static final String[] DURATIONS = { "All", "01hr", "03hr", "06hr",
            "12hr", "24hr" };

    private static final String FFG_AREA_WFO = "WFO";

    private static final String FFG_AREA_RFC = "RFC";

    private static final String[] FFG_AREAS = { FFG_AREA_WFO, FFG_AREA_RFC };

    private static final int NUM_FFG_ROWS = 12;

    private static final String ID_COLUMN_HEADER = "Id";

    private static final String TYPE_COLUMN_HEADER = "Type";

    private static final String FFG_NAME_REGEX = "^(.{3," + Admin.HSA_LENGTH
            + "})(\\d{10})(\\d{2})\\.ffg$";

    private static final Pattern FFG_NAME_PATTERN = Pattern
            .compile(FFG_NAME_REGEX);

    private static final int FFG_WFO_GROUP = 1;

    private static final int FFG_DATE_GROUP = 2;

    private static final int FFG_DURATION_GROUP = 3;

    private enum CONSTANT_FFG_COLUMNS {
        DUR_HR("DurHr"), TIMEZ("Time(Z)");

        private final String text;

        private CONSTANT_FFG_COLUMNS(String text) {
            this.text = text;
        }

        public String getText() {
            return text;
        }
    };

    private Table ffgTable;

    /* Initialize the date format */
    static {
        sdf = new SimpleDateFormat("EEE MM-dd HH");
        sdf.setTimeZone(TimeUtil.GMT_TIME_ZONE);
        xmrgDateFormat = new SimpleDateFormat("yyyyMMddHH");
        xmrgDateFormat.setTimeZone(TimeUtil.GMT_TIME_ZONE);
    }

    /**
     * FFG option string.
     */
    private final String ffgOptionsStr = "Gridded FFG Options";

    /**
     * Areal option string.
     */
    private final String arealOptionsStr = "Areal FFG Options";

    /**
     * Gridded radio button.
     */
    private Button griddedRdo;

    /**
     * Areal radio button.
     */
    private Button arealRdo;

    /**
     * Select button.
     */
    private Button selectBtn;

    /**
     * Clear button.
     */
    private Button clearBtn;

    /**
     * FFG/Areal option group.
     */
    private Group ffgArealOptGroup;

    /**
     * Stack composite that will contain multiple composites.
     */
    private Composite stackComposite;

    /**
     * Stack layout of the stack composite.
     */
    private StackLayout stackLayout;

    /**
     * FFG/Areal combo box.
     */
    private Combo ffgAreaCbo;

    /**
     * FFG ID combo.
     */
    private Combo ffgIdCbo;

    /**
     * FFG duration.
     */
    private Combo ffgDurCbo;

    /**
     * FFG display as combo.
     */
    private Combo ffgDisplayAsCbo;

    /**
     * Areal type.
     */
    private Combo arealTypeCbo;

    /**
     * Areal duration combo.
     */
    private Combo arealDurCbo;

    /**
     * FFG options composite.
     */
    private Composite ffgOptionsComp;

    /**
     * Areal options composite.
     */
    private Composite arealOptionsComp;

    /**
     * Values check box.
     */
    private Button valuesChk;

    /**
     * IDs check box.
     */
    private Button idsChk;

    /**
     * Close button.
     */
    private Button closeBtn;

    /** Bundle variables */
    private final Map<String, String> parameters = new HashMap<>();

    /**
     * The selected RFC.
     */
    private String selectedRFC = null;

    /**
     * The selected duration.
     */
    private String selectedDur = null;

    /**
     * Color legend composite.
     */
    private ColorLegendComp colorLegend;

    /**
     * The duration in millis being displayed.
     */
    private int duration = 3600;

    /**
     * The wait mouse pointer.
     */
    private Cursor waitCursor = null;

    /**
     * The normal arrow mouse pointer.
     */
    private Cursor arrowCursor = null;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public FlashFloodGuidanceDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK);
        setText("Flash Flood Guidance");
        waitCursor = parent.getDisplay().getSystemCursor(SWT.CURSOR_WAIT);
        arrowCursor = parent.getDisplay().getSystemCursor(SWT.CURSOR_ARROW);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        createGridArealControls();
        createFFGDataTable();
        createDataListControlButtons();
        createOptionsGroup();
        createColorLegend();
        createBottomCloseButton();

        populateFFGDataTable();
    }

    /**
     * Create the Grid/Areal radio buttons.
     */
    private void createGridArealControls() {
        Group gridArealGroup = new Group(shell, SWT.NONE);
        gridArealGroup.setText("FFG Mode");
        RowLayout gridArealLayout = new RowLayout();
        gridArealGroup.setLayout(gridArealLayout);
        gridArealGroup.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        griddedRdo = new Button(gridArealGroup, SWT.RADIO);
        griddedRdo.setText("Gridded");
        griddedRdo.setSelection(true);
        griddedRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ffgArealOptGroup.setText(ffgOptionsStr);

                stackLayout.topControl = ffgOptionsComp;
                stackComposite.layout();

                // Clear the data table and reload
                populateFFGDataTable();
            }
        });

        arealRdo = new Button(gridArealGroup, SWT.RADIO);
        arealRdo.setText("Areal");
        arealRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ffgArealOptGroup.setText(arealOptionsStr);

                stackLayout.topControl = arealOptionsComp;
                stackComposite.layout();

                // Clear the data table and reload
                populateFFGDataTable();
            }
        });
    }

    private void createFFGDataTable() {
        Composite tableComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        tableComp.setLayout(gl);
        tableComp.setLayoutData(gd);

        ffgTable = new Table(tableComp, SWT.BORDER | SWT.V_SCROLL | SWT.SINGLE);
        ffgTable.setHeaderVisible(true);
        ffgTable.setLinesVisible(true);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = ffgTable.getItemHeight() * NUM_FFG_ROWS;
        ffgTable.setLayoutData(gd);
        ffgTable.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDoubleClick(MouseEvent e) {
                // Clear the previous data
                clearData();
                // Display the new data
                displayData();
            }
        });
    }

    private void populateFFGDataTable() {
        /*
         * Clear any previous data.
         */
        if (ffgTable.getItemCount() > 0) {
            ffgTable.removeAll();
        }
        if (ffgTable.getColumnCount() > 0) {
            for (TableColumn tc : ffgTable.getColumns()) {
                tc.dispose();
            }
        }

        /*
         * Add the table column headers.
         */
        final String firstColumnTxt = (griddedRdo.getSelection()) ? ID_COLUMN_HEADER
                : TYPE_COLUMN_HEADER;
        GC gc = new GC(ffgTable);
        gc.setFont(ffgTable.getFont());
        TableColumn tc = new TableColumn(ffgTable, SWT.CENTER);
        tc.setText(firstColumnTxt);
        tc.pack();
        for (CONSTANT_FFG_COLUMNS ffgColumn : CONSTANT_FFG_COLUMNS.values()) {
            tc = new TableColumn(ffgTable, SWT.CENTER);
            tc.setText(ffgColumn.getText());
            tc.pack();
        }

        gc.dispose();

        /*
         * Populate the table with data.
         */
        if (griddedRdo.getSelection()) {
            readGriddedFfgProduct();
        } else {
            readArealFfgProduct();
        }
    }

    private void addTableRows(List<FFGGuidanceData> rowDataList) {
        Collections.sort(rowDataList, new FFGGuidanceDataComparator());

        for (FFGGuidanceData rowData : rowDataList) {
            TableItem ti = new TableItem(ffgTable, SWT.NONE);
            ti.setData(rowData);
            final String[] tableItemValues = new String[] {
                    rowData.getIdentifier(), rowData.getFormattedDuration(),
                    rowData.getFormattedDateTime() };
            ti.setText(tableItemValues);
        }
        for (TableColumn tc : ffgTable.getColumns()) {
            tc.pack();
        }
    }

    /**
     * Create the buttons that manipulate the data list control.
     */
    private void createDataListControlButtons() {
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        centeredComp.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        centeredComp.setLayoutData(gd);

        Composite dataControlComp = new Composite(centeredComp, SWT.NONE);
        gl = new GridLayout(2, true);
        dataControlComp.setLayout(gl);

        final int minimumButtonWidth = dataControlComp.getDisplay().getDPI().x;

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        gd.minimumWidth = minimumButtonWidth;
        selectBtn = new Button(dataControlComp, SWT.PUSH);
        selectBtn.setText("Select");
        selectBtn.setLayoutData(gd);
        selectBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (ffgTable.getSelectionCount() <= 0) {
                    MessageBox mb = new MessageBox(getParent(),
                            SWT.ICON_WARNING | SWT.OK);
                    mb.setText("Selection Needed");
                    mb.setMessage("Please select a value in the list.");
                    mb.open();
                } else {
                    // Clear the previous data
                    clearData();
                    // Display the new data
                    displayData();
                }
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        gd.minimumWidth = minimumButtonWidth;
        clearBtn = new Button(dataControlComp, SWT.PUSH);
        clearBtn.setText("Clear");
        clearBtn.setLayoutData(gd);
        clearBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                clearData();
            }
        });
    }

    /**
     * Create the options group (stack layout) composite.
     */
    private void createOptionsGroup() {
        // Set the group layout data so it will take up the width of the
        // shell.
        GridData mainGridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        ffgArealOptGroup = new Group(shell, SWT.NONE);
        ffgArealOptGroup.setText(ffgOptionsStr);
        GridLayout gridLayout = new GridLayout(1, false);
        ffgArealOptGroup.setLayout(gridLayout);
        ffgArealOptGroup.setLayoutData(mainGridData);

        stackComposite = new Composite(ffgArealOptGroup, SWT.NONE);
        stackLayout = new StackLayout();
        stackComposite.setLayout(stackLayout);

        createFfgOptionComposite();
        createArealOptionComposite();

        stackLayout.topControl = ffgOptionsComp;
        stackComposite.layout();
    }

    /**
     * Create the flash flood composite and controls.
     */
    private void createFfgOptionComposite() {
        ffgOptionsComp = new Composite(stackComposite, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        ffgOptionsComp.setLayout(gl);

        Label ffgAreaLbl = new Label(ffgOptionsComp, SWT.RIGHT);
        ffgAreaLbl.setText("FFG Area:");

        GridData gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        ffgAreaCbo = new Combo(ffgOptionsComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        ffgAreaCbo.setItems(FFG_AREAS);
        ffgAreaCbo.select(0);
        ffgAreaCbo.setLayoutData(gd);
        ffgAreaCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (FFG_AREA_WFO.equals(ffgAreaCbo.getText())) {
                    ffgIdCbo.setEnabled(false);
                } else {
                    ffgIdCbo.setEnabled(true);
                }

                populateFFGDataTable();
            }
        });

        Label ffgIdLbl = new Label(ffgOptionsComp, SWT.RIGHT);
        ffgIdLbl.setText("ID:");

        // ------------------------------------------------
        // NOTE: The FFG ID combo box data may be dynamic
        // so the items in the list may change.
        // ------------------------------------------------
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        ffgIdCbo = new Combo(ffgOptionsComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        ffgIdCbo.add("All");
        for (String s : RFC_NAMES) {
            ffgIdCbo.add(s);
        }
        ffgIdCbo.select(0);
        ffgIdCbo.setEnabled(false);
        ffgIdCbo.setLayoutData(gd);
        ffgIdCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                selectedRFC = ffgIdCbo.getItem(ffgIdCbo.getSelectionIndex());
                populateFFGDataTable();
            }

        });

        Label ffgDurLbl = new Label(ffgOptionsComp, SWT.RIGHT);
        ffgDurLbl.setText("Duration:");

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        ffgDurCbo = new Combo(ffgOptionsComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        for (String s : DURATIONS) {
            ffgDurCbo.add(s);
        }
        ffgDurCbo.select(0);
        ffgDurCbo.setLayoutData(gd);
        ffgDurCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int index = ffgDurCbo.getSelectionIndex();
                selectedDur = DURATIONS[index];
                populateFFGDataTable();
            }
        });

        Label ffgDisplayLbl = new Label(ffgOptionsComp, SWT.RIGHT);
        ffgDisplayLbl.setText("Display As:");

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        ffgDisplayAsCbo = new Combo(ffgOptionsComp, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        ffgDisplayAsCbo.add("Grid");
        ffgDisplayAsCbo.add("Basin");
        ffgDisplayAsCbo.select(0);
        ffgDisplayAsCbo.setLayoutData(gd);
    }

    /**
     * Create the areal option composite.
     */
    private void createArealOptionComposite() {
        arealOptionsComp = new Composite(stackComposite, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        arealOptionsComp.setLayout(gl);

        Label arealTypeLbl = new Label(arealOptionsComp, SWT.RIGHT);
        arealTypeLbl.setText("Areal Type:");

        arealTypeCbo = new Combo(arealOptionsComp, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        arealTypeCbo.add(ResolutionLevel.ALL.getResolution());
        arealTypeCbo.add(ResolutionLevel.BASIN.getResolution());
        arealTypeCbo.add(ResolutionLevel.COUNTY.getResolution());
        arealTypeCbo.add(ResolutionLevel.ZONE.getResolution());
        arealTypeCbo.select(0);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        arealTypeCbo.setLayoutData(gd);
        arealTypeCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                populateFFGDataTable();
            }
        });

        Label arealDurLbl = new Label(arealOptionsComp, SWT.RIGHT);
        arealDurLbl.setText("Duration:");

        arealDurCbo = new Combo(arealOptionsComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        arealDurCbo.add("All");
        arealDurCbo.add("1hr");
        arealDurCbo.add("3hr");
        arealDurCbo.add("6hr");
        arealDurCbo.add("12hr");
        arealDurCbo.add("24hr");
        arealDurCbo.select(0);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        arealDurCbo.setLayoutData(gd);
        arealDurCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int index = arealDurCbo.getSelectionIndex();
                selectedDur = DURATIONS[index];
                populateFFGDataTable();
            }
        });

        Label displayLbl = new Label(arealOptionsComp, SWT.NONE);
        displayLbl.setText("Display:");

        valuesChk = new Button(arealOptionsComp, SWT.CHECK);
        valuesChk.setSelection(true);
        valuesChk.setText("Values");
        valuesChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                HydroDisplayManager.getInstance().updateArealFfgDisplay(
                        valuesChk.getSelection(), idsChk.getSelection());
            }
        });

        idsChk = new Button(arealOptionsComp, SWT.CHECK);
        idsChk.setSelection(true);
        idsChk.setText("Ids");
        idsChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                HydroDisplayManager.getInstance().updateArealFfgDisplay(
                        valuesChk.getSelection(), idsChk.getSelection());
            }
        });
    }

    /**
     * Create the color legend composite.
     */
    private void createColorLegend() {
        String user_id = System.getProperty("user.name");

        java.util.List<Colorvalue> colorSet = HydroDisplayManager.getInstance()
                .getFFGColorMap(user_id, "FFG", duration);

        java.util.List<ColorLegendBarData> list = new ArrayList<ColorLegendBarData>();
        list.add(new ColorLegendBarData(colorSet.get(7).getId()
                .getThresholdValue(), colorSet.get(7).getColorname()
                .getColorName()));
        list.add(new ColorLegendBarData(colorSet.get(6).getId()
                .getThresholdValue(), colorSet.get(6).getColorname()
                .getColorName()));
        list.add(new ColorLegendBarData(colorSet.get(5).getId()
                .getThresholdValue(), colorSet.get(5).getColorname()
                .getColorName()));
        list.add(new ColorLegendBarData(colorSet.get(4).getId()
                .getThresholdValue(), colorSet.get(4).getColorname()
                .getColorName()));
        list.add(new ColorLegendBarData(colorSet.get(3).getId()
                .getThresholdValue(), colorSet.get(3).getColorname()
                .getColorName()));
        list.add(new ColorLegendBarData(colorSet.get(2).getId()
                .getThresholdValue(), colorSet.get(2).getColorname()
                .getColorName()));
        list.add(new ColorLegendBarData(colorSet.get(1).getId()
                .getThresholdValue(), colorSet.get(1).getColorname()
                .getColorName()));
        list.add(new ColorLegendBarData(colorSet.get(0).getId()
                .getThresholdValue(), colorSet.get(0).getColorname()
                .getColorName()));

        colorLegend = new ColorLegendComp(shell, "Color Legend", list);
    }

    /**
     * Create the close button.
     */
    private void createBottomCloseButton() {
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        centeredComp.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        centeredComp.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        gd.minimumWidth = centeredComp.getDisplay().getDPI().x;
        closeBtn = new Button(centeredComp, SWT.NONE);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Get the Gridded FFG products for the data list.
     */
    private void readGriddedFfgProduct() {
        FlashFloodGuidanceDataManager dman = FlashFloodGuidanceDataManager
                .getInstance();
        List<FFGGuidanceData> guidanceDataList = new LinkedList<>();

        /* Check the FFG mode. */
        if (FFG_AREA_RFC.equals(ffgAreaCbo.getItem(ffgAreaCbo
                .getSelectionIndex()))) {
            java.util.List<Object[]> rs = dman.getGriddedDataList();

            if ((rs != null) && (rs.size() > 0)) {
                for (Object[] oa : rs) {
                    String id = " ";
                    String durHr = " ";
                    String dateStr = " ";

                    // Get the id
                    String idStr = (String) oa[0];
                    if ((idStr != null) && idStr.contains("-")) {
                        String[] parts = idStr.split("-");
                        id = parts[1];
                        if (dman.rfcSiteLookup(id) != null) {
                            id = dman.rfcSiteLookup(id);
                        }
                    }

                    // Get the duration
                    String dur = (String) oa[1];
                    if ((dur != null) && (dur.length() > 0)) {
                        durHr = dur.substring(3, 5);
                    }

                    // Get the date
                    Date date = (Date) oa[2];
                    dateStr = sdf.format(date);

                    /* Filter the list if needed */
                    if ((selectedRFC != null)
                            && !selectedRFC.equalsIgnoreCase("All")) {
                        if (!id.equalsIgnoreCase(selectedRFC)) {
                            continue;
                        }
                    }

                    // Dur is FFG03, FFG06, etc
                    // selectedDur is 1hr, 3hr, etc
                    if ((selectedDur != null)
                            && !selectedDur.equalsIgnoreCase("All")) {
                        if (!dur.substring(3, 5).equalsIgnoreCase(
                                selectedDur.substring(0,
                                        selectedDur.indexOf("h")))
                                && !dur.substring(3, 5).equals(
                                        "0"
                                                + selectedDur.substring(0,
                                                        selectedDur
                                                                .indexOf("h")))) {
                            continue;

                        }
                    }

                    guidanceDataList.add(new FFGGuidanceData(id, Integer
                            .parseInt(durHr), durHr, date, dateStr));
                }
            }
        } else {
            // WFO Grided data, read xmrg directory
            String ffgDirToken = "gaff_mosaic_dir";

            /*
             * Retrieve the path of the WFO ffg_product file from the
             * "apps defaults" file.
             */
            String ffgDirPath = AppsDefaults.getInstance()
                    .getToken(ffgDirToken);
            if (ffgDirPath == null) {
                statusHandler
                        .error("Error getting WFO FFG directory from token "
                                + ffgDirToken);
                return;
            }

            File ffgDir = new File(ffgDirPath);
            if (ffgDir.exists() && ffgDir.canRead()) {
                File[] xmrgFiles = ffgDir.listFiles();

                for (File xmrg : xmrgFiles) {
                    /*
                     * Check to make sure that this is a FFG file. This is done
                     * by checking the extension on the file.
                     */
                    final String fileName = xmrg.getName();
                    final Matcher matcher = FFG_NAME_PATTERN.matcher(fileName);
                    if (!matcher.matches()) {
                        statusHandler.warn("Discovered unrecognized file: "
                                + xmrg.toString()
                                + " in the WFO FFG directory: "
                                + ffgDirPath.toString() + ". Skipping file.");
                        continue;
                    }

                    String fileWfo = matcher.group(FFG_WFO_GROUP);
                    String xmrgDateString = matcher.group(FFG_DATE_GROUP);
                    /*
                     * Verify that a valid date/time can be parsed from the xmrg
                     * date/time String.
                     */
                    Date xmrgDate = null;
                    try {
                        xmrgDate = xmrgDateFormat.parse(xmrgDateString);
                    } catch (ParseException e) {
                        statusHandler.error(
                                "Failed to parse xmrg date/time: "
                                        + xmrgDateString + " for file: "
                                        + xmrg.toString() + ". Skipping file.",
                                e);
                        continue;
                    }
                    String durationString = matcher.group(FFG_DURATION_GROUP);
                    /*
                     * Verify that the duration String is numeric.
                     */
                    int duration = 0;
                    try {
                        duration = Integer.parseInt(durationString);
                    } catch (NumberFormatException e) {
                        statusHandler.error(
                                "Failed to parse xmrg duration: "
                                        + durationString + " for file: "
                                        + xmrg.toString() + ". Skipping file.",
                                e);
                        continue;
                    }

                    String dateStr = sdf.format(xmrgDate);
                    FFGGuidanceData data = new FFGGuidanceData(fileWfo,
                            duration, durationString, xmrgDate, dateStr);
                    data.setXmrgFile(xmrg);
                    guidanceDataList.add(data);
                }
            }
        }

        addTableRows(guidanceDataList);
    }

    private void readArealFfgProduct() {
        ResolutionLevel res = null;
        String selectedItem = arealTypeCbo.getItem(arealTypeCbo
                .getSelectionIndex());

        if (selectedItem.equals(ResolutionLevel.BASIN.getResolution())) {
            res = ResolutionLevel.BASIN;
        } else if (selectedItem.equals(ResolutionLevel.COUNTY.getResolution())) {
            res = ResolutionLevel.COUNTY;
        } else if (selectedItem.equals(ResolutionLevel.ZONE.getResolution())) {
            res = ResolutionLevel.ZONE;
        } else {
            res = ResolutionLevel.ALL;
        }

        List<FFGGuidanceData> guidanceDataList = new LinkedList<>();
        if (res == ResolutionLevel.ALL) {
            for (ResolutionLevel level : ResolutionLevel.values()) {
                if (level.getResolution().equals("All")
                        || level.getResolution().equals("Grid")) {
                    continue; // Skip "All" and "Grid"
                }

                guidanceDataList.addAll(bldFfgList(level));
            }
        } else {
            guidanceDataList = bldFfgList(res);
        }

        addTableRows(guidanceDataList);
    }

    /**
     * Builds a list of available ffg products for the requested boundary_type
     * (basin, county, zone). Do not use this function for handling gridded FFG
     * requests.
     * 
     * @param level
     */
    private List<FFGGuidanceData> bldFfgList(ResolutionLevel level) {
        List<FFGGuidanceData> guidanceDataList = new LinkedList<>();

        java.util.List<Object[]> rs = FlashFloodGuidanceDataManager
                .getInstance().getContingencyValue(level.getResolution());

        for (Object[] oa : rs) {
            Date validTime = (Date) oa[0];
            int shefDur = ((Number) oa[1]).intValue();
            int dur = shefDur - ((shefDur / 1000) * 1000);

            // Dur is FFG03, FFG06, etc
            // selectedDur is 1hr, 3hr, etc
            if ((selectedDur != null) && !selectedDur.equalsIgnoreCase("All")) {
                String durCheck = StringUtils.leftPad(String.valueOf(dur), 2,
                        "0");
                if (!String.valueOf(durCheck).equalsIgnoreCase(
                        selectedDur.substring(0, selectedDur.indexOf("h")))
                        && !String.valueOf(durCheck).equals(
                                "0"
                                        + selectedDur.substring(0,
                                                selectedDur.indexOf("h")))) {
                    continue;

                }
            }
            String dateStr = sdf.format(validTime);
            String id = level.getResolution();
            String durStr = StringUtils.leftPad(String.valueOf(dur), 2, "0");

            guidanceDataList.add(new FFGGuidanceData(id, dur, durStr,
                    validTime, dateStr));
        }

        return guidanceDataList;
    }

    /**
     * Display the data in CAVE
     */
    private void displayData() {
        FlashFloodGuidanceDataManager dman = FlashFloodGuidanceDataManager
                .getInstance();

        shell.setCursor(waitCursor);

        FFGGuidanceData selectedRowData = (FFGGuidanceData) ffgTable
                .getSelection()[0].getData();

        /* Get the selection from the list and break it up */
        String site = selectedRowData.getIdentifier();
        String durationStr = selectedRowData.getFormattedDuration();
        duration = selectedRowData.getDuration()
                * FFGConstants.SECONDS_PER_HOUR;

        String paramAbr = "FFG" + durationStr + "24hr";

        String rfc = null;
        boolean rfcSelected = false;
        if (ffgAreaCbo.getItem(ffgAreaCbo.getSelectionIndex()).equals("RFC")) {
            rfc = dman.rfcSiteLookup(site);
            rfcSelected = true;
        } else {
            rfc = site;
        }

        // Determine the mode
        if (griddedRdo.getSelection()) {
            String resStr = ffgDisplayAsCbo.getItem(ffgDisplayAsCbo
                    .getSelectionIndex());
            ResolutionLevel res = ResolutionLevel.GRID;
            if (resStr.equals("Basin")) {
                res = ResolutionLevel.BASIN;
            }

            if (ffgDisplayAsCbo.getItem(ffgDisplayAsCbo.getSelectionIndex())
                    .equalsIgnoreCase("GRID")) {
                // Display the grid
                getParameters(selectedRowData);

                if (rfcSelected) {
                    HydroDisplayManager.getInstance().displayGriddedFFG(
                            selectedRowData.getDateTime(), duration, paramAbr,
                            rfc, res);
                } else {
                    HydroDisplayManager.getInstance().displayGriddedFFG(
                            selectedRowData.getXmrgFile(), duration, res);
                }
            } else {
                /* Display the areal basin */
                if (rfcSelected) {
                    HydroDisplayManager.getInstance()
                            .displayRfcGriddedFFGBasin(
                                    selectedRowData.getDateTime(), duration,
                                    paramAbr, rfc, res);
                } else {
                    HydroDisplayManager.getInstance().displayGriddedFFGBasin(
                            selectedRowData.getXmrgFile(), duration, res,
                            selectedRowData.getDateTime());
                }
            }
        } else {
            /* Areal Radio selected */
            java.util.List<ArealData> arealList = buildFfgArea(site,
                    selectedRowData.getDateTime());

            HydroDisplayManager.getInstance().displayArealFfg(arealList,
                    duration, site, selectedRowData.getDateTime(),
                    valuesChk.getSelection(), idsChk.getSelection());
        }
        // Create the legend strings
        // Build the string in the legend
        String line = site + " " + durationStr + " hours "
                + selectedRowData.getFormattedDateTime() + ":00";
        colorLegend.setDisplayText("FFG Grid", line);

        shell.setCursor(arrowCursor);
    }

    /**
     * Get the parameters for the bundle
     */
    private void getParameters(FFGGuidanceData selectedRowData) {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeUtil.GMT_TIME_ZONE);

        String modelname = null;

        String param = "FFG" + selectedRowData.getFormattedDuration() + "24hr";

        // Lookup the name
        String s = FlashFloodGuidanceDataManager.getInstance().rfcSiteLookup(
                selectedRowData.getIdentifier());
        if (s != null) {
            modelname = "FFG-" + s;
        }
        parameters.put("timespan", param);
        parameters.put("model", modelname);
        parameters.put("reftime", sdf.format(selectedRowData.getDateTime()));
    }

    private java.util.List<ArealData> buildFfgArea(String boundaryType,
            Date refTime) {
        FlashFloodGuidanceDataManager dman = FlashFloodGuidanceDataManager
                .getInstance();
        java.util.List<ArealData> arealDataList = new ArrayList<ArealData>();

        /* get the list of areas matching the specified type */
        String where = " where boundary_type = UPPER('" + boundaryType
                + "') order by area_id ";

        java.util.List<Object[]> rs = dman.getGeoArea(where);
        if ((rs != null) && (rs.size() > 0)) {
            for (Object[] oa : rs) {
                String areaId = (String) oa[0];
                double lat = (Double) oa[1];
                double lon = (Double) oa[2];

                java.util.List<Object[]> rs2 = FlashFloodGuidanceDataManager
                        .getInstance().getContingencyValue(areaId, duration,
                                refTime);
                if ((rs2 != null) && (rs2.size() > 0)) {
                    for (Object[] oa2 : rs2) {
                        ArealData ad = new ArealData();
                        ad.setLid((String) oa2[0]);
                        ad.setValidTime((Date) oa2[1]);
                        ad.setValue((Double) oa2[2]);
                        ad.setLat(lat);
                        ad.setLon(lon);
                        arealDataList.add(ad);
                    }
                }
            }
        }

        return arealDataList;
    }

    /**
     * Remove the FFG data from the display.
     */
    private void clearData() {
        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container != null) {
            IDisplayPane pane = container.getActiveDisplayPane();
            ResourceList rl = pane.getDescriptor().getResourceList();
            for (ResourcePair rp : rl) {
                if ((rp.getResource() != null)
                        && (rp.getResource().getName() != null)) {
                    if (rp.getResource().getName().startsWith("FFG")) {
                        rl.remove(rp);
                    }
                }
            }
        }
    }
}