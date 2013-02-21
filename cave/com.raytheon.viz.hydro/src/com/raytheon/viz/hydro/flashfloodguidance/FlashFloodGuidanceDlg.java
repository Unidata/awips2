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
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TimeZone;
import java.util.TreeMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.ohd.AppsDefaults;
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
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class FlashFloodGuidanceDlg extends CaveSWTDialog {
    /** Date format for the dates */
    private static SimpleDateFormat sdf = null;

    /** List of RFC names */
    private static final String[] RFC_NAMES = { "ABRFC", "AKRFC", "CBRFC",
            "CNRFC", "LMRFC", "MARFC", "MBRFC", "NCRFC", "NERFC", "NWRFC",
            "OHRFC", "SERFC", "WGRFC", "ALL_RFCS" };

    /** List of durations */
    private static final String[] DURATIONS = { "All", "01hr", "03hr", "06hr",
            "12hr", "24hr" };

    /* Initialize the date format */
    static {
        sdf = new SimpleDateFormat("EEE MM-dd HH");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /**
     * FFG option string.
     */
    private final String ffgOptionsStr = " Gridded FFG Options: ";

    /**
     * Areal option string.
     */
    private final String arealOptionsStr = " Areal FFG Options: ";

    /**
     * Gridded radio button.
     */
    private Button griddedRdo;

    /**
     * Areal radio button.
     */
    private Button arealRdo;

    /**
     * Font used for list controls.
     */
    private Font font;

    /**
     * FFG data list.
     */
    private List dataList;

    /**
     * ID Type label.
     */
    private Label idTypeLbl;

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
    private Map<String, String> parameters = new HashMap<String, String>();

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
     * Holds the display string and insert time for later use.
     */
    private Map<String, Date> dataMap = new HashMap<String, Date>();

    /**
     * Holds the display string and the xmrg File object.
     */
    private Map<String, File> fileMap = new HashMap<String, File>();

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
        super(parent);
        setText("Flash Flood Guidance");
        waitCursor = parent.getDisplay().getSystemCursor(SWT.CURSOR_WAIT);
        arrowCursor = parent.getDisplay().getSystemCursor(SWT.CURSOR_ARROW);
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
    protected void initializeComponents(Shell shell) {
        font = new Font(shell.getDisplay(), "Monospace", 11, SWT.NORMAL);

        createGridArealControls();
        createListLabels();
        createDataListControl();
        createDataListControlButtons();
        createOptionsGroup();
        createColorLegend();
        createBottomCloseButton();

        populateDataList();
    }

    /**
     * Create the Grid/Areal radio buttons.
     */
    private void createGridArealControls() {
        Group gridArealGroup = new Group(shell, SWT.NONE);
        gridArealGroup.setText(" FFG Mode: ");
        RowLayout gridArealLayout = new RowLayout();
        gridArealLayout.spacing = 5;
        gridArealGroup.setLayout(gridArealLayout);

        griddedRdo = new Button(gridArealGroup, SWT.RADIO);
        griddedRdo.setText("Gridded");
        griddedRdo.setSelection(true);
        griddedRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                idTypeLbl.setText("Id");

                ffgArealOptGroup.setText(ffgOptionsStr);

                stackLayout.topControl = ffgOptionsComp;
                stackComposite.layout();

                // Clear the data list and reload
                dataList.removeAll();
                populateDataList();
            }
        });

        arealRdo = new Button(gridArealGroup, SWT.RADIO);
        arealRdo.setText("Areal");
        arealRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                idTypeLbl.setText("Type");

                ffgArealOptGroup.setText(arealOptionsStr);

                stackLayout.topControl = arealOptionsComp;
                stackComposite.layout();

                // Clear the data list and reload
                dataList.removeAll();
                populateDataList();
            }
        });
    }

    /**
     * Create the labels for the data list.
     */
    private void createListLabels() {
        Composite labelComp = new Composite(shell, SWT.NONE);
        RowLayout layout = new RowLayout();
        labelComp.setLayout(layout);

        RowData rd = new RowData(40, SWT.DEFAULT);
        idTypeLbl = new Label(labelComp, SWT.RIGHT);
        idTypeLbl.setText("Id");
        idTypeLbl.setLayoutData(rd);

        rd = new RowData(65, SWT.DEFAULT);
        Label durHrLbl = new Label(labelComp, SWT.RIGHT);
        durHrLbl.setText("DurHr");
        durHrLbl.setLayoutData(rd);

        rd = new RowData(75, SWT.DEFAULT);
        Label timeLbl = new Label(labelComp, SWT.RIGHT);
        timeLbl.setText("    Time(Z)");
        timeLbl.setLayoutData(rd);
    }

    /**
     * Create the data list control.
     */
    private void createDataListControl() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint = 255;
        gd.heightHint = 250;
        dataList = new List(shell, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        dataList.setLayoutData(gd);

        dataList.setFont(font);
        dataList.addMouseListener(new MouseListener() {
            @Override
            public void mouseDoubleClick(MouseEvent e) {
                // Display data on double click
                // Clear the previous data
                clearData();
                // Display the new data
                displayData();
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
     * Create the buttons that manipulate the data list control.
     */
    private void createDataListControlButtons() {
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        centeredComp.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        centeredComp.setLayoutData(gd);

        Composite dataControlComp = new Composite(centeredComp, SWT.NONE);
        RowLayout layout = new RowLayout();
        layout.spacing = 20;
        dataControlComp.setLayout(layout);

        RowData rd = new RowData(80, SWT.DEFAULT);
        selectBtn = new Button(dataControlComp, SWT.PUSH);
        selectBtn.setText("Select");
        selectBtn.setLayoutData(rd);
        selectBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (dataList.getSelectionIndex() < 0) {
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

        rd = new RowData(80, SWT.DEFAULT);
        clearBtn = new Button(dataControlComp, SWT.PUSH);
        clearBtn.setText("Clear");
        clearBtn.setLayoutData(rd);
        clearBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                clearData();
                // if ( isThereFfgDataToDraw ( ) != 0 )
                // {
                // turnOffFfgData ( ) ;
                // }
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
        ffgAreaLbl.setText("FFG Area: ");

        GridData gd = new GridData(100, SWT.DEFAULT);
        ffgAreaCbo = new Combo(ffgOptionsComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        ffgAreaCbo.add("WFO");
        ffgAreaCbo.add("RFC");
        ffgAreaCbo.select(0);
        ffgAreaCbo.setLayoutData(gd);
        ffgAreaCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (ffgAreaCbo.getText().compareTo("WFO") == 0) {
                    ffgIdCbo.setEnabled(false);
                } else {
                    ffgIdCbo.setEnabled(true);
                }

                populateDataList();
            }
        });

        Label ffgIdLbl = new Label(ffgOptionsComp, SWT.RIGHT);
        ffgIdLbl.setText("Id: ");

        // ------------------------------------------------
        // NOTE: The FFG ID combo box data may be dynamic
        // so the items in the list may change.
        // ------------------------------------------------
        gd = new GridData(100, SWT.DEFAULT);
        ffgIdCbo = new Combo(ffgOptionsComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        ffgIdCbo.add("All");
        for (String s : RFC_NAMES) {
            ffgIdCbo.add(s);
        }
        ffgIdCbo.select(0);
        ffgIdCbo.setEnabled(false);
        ffgIdCbo.setLayoutData(gd);
        ffgIdCbo.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                selectedRFC = ffgIdCbo.getItem(ffgIdCbo.getSelectionIndex());
                populateDataList();
            }

        });

        Label ffgDurLbl = new Label(ffgOptionsComp, SWT.RIGHT);
        ffgDurLbl.setText("Dur: ");

        gd = new GridData(100, SWT.DEFAULT);
        ffgDurCbo = new Combo(ffgOptionsComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        for (String s : DURATIONS) {
            ffgDurCbo.add(s);
        }
        ffgDurCbo.select(0);
        ffgDurCbo.setLayoutData(gd);
        ffgDurCbo.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                int index = ffgDurCbo.getSelectionIndex();
                selectedDur = DURATIONS[index];
                populateDataList();
            }
        });

        Label ffgDisplayLbl = new Label(ffgOptionsComp, SWT.RIGHT);
        ffgDisplayLbl.setText("Display As: ");

        gd = new GridData(100, SWT.DEFAULT);
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
        GridLayout gl = new GridLayout(2, false);
        arealOptionsComp.setLayout(gl);

        Label arealTypeLbl = new Label(arealOptionsComp, SWT.RIGHT);
        arealTypeLbl.setText("Areal Type: ");

        GridData gd = new GridData(100, SWT.DEFAULT);
        arealTypeCbo = new Combo(arealOptionsComp, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        arealTypeCbo.add(ResolutionLevel.ALL.getResolution());
        arealTypeCbo.add(ResolutionLevel.BASIN.getResolution());
        arealTypeCbo.add(ResolutionLevel.COUNTY.getResolution());
        arealTypeCbo.add(ResolutionLevel.ZONE.getResolution());
        arealTypeCbo.select(0);
        arealTypeCbo.setLayoutData(gd);
        arealTypeCbo.addSelectionListener(new SelectionAdapter() {
            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                populateDataList();
            }
        });

        Label arealDurLbl = new Label(arealOptionsComp, SWT.RIGHT);
        arealDurLbl.setText("Dur: ");

        gd = new GridData(100, SWT.DEFAULT);
        arealDurCbo = new Combo(arealOptionsComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        arealDurCbo.add("All");
        arealDurCbo.add("1hr");
        arealDurCbo.add("3hr");
        arealDurCbo.add("6hr");
        arealDurCbo.add("12hr");
        arealDurCbo.add("24hr");
        arealDurCbo.select(0);
        arealDurCbo.setLayoutData(gd);
        arealDurCbo.addSelectionListener(new SelectionAdapter() {
            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                int index = arealDurCbo.getSelectionIndex();
                selectedDur = DURATIONS[index];
                populateDataList();
            }
        });

        // Label filler1Lbl = new Label(arealOptionsComp, SWT.NONE);
        new Label(arealOptionsComp, SWT.NONE);

        // Label filler2Lbl = new Label(arealOptionsComp, SWT.NONE);
        new Label(arealOptionsComp, SWT.NONE);

        gd = new GridData();
        gd.horizontalSpan = 2;
        Composite displayComp = new Composite(arealOptionsComp, SWT.NONE);
        RowLayout rl = new RowLayout();
        rl.spacing = 10;
        displayComp.setLayout(rl);
        displayComp.setLayoutData(gd);

        Label displayLbl = new Label(displayComp, SWT.NONE);
        displayLbl.setText("Display: ");

        valuesChk = new Button(displayComp, SWT.CHECK);
        valuesChk.setSelection(true);
        valuesChk.setText("Values");
        valuesChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                HydroDisplayManager.getInstance().updateArealFfgDisplay(
                        valuesChk.getSelection(), idsChk.getSelection());
            }
        });

        idsChk = new Button(displayComp, SWT.CHECK);
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
        // String app_name = "hydroview";

        java.util.List<Colorvalue> colorSet = HydroDisplayManager.getInstance()
                .getFFGColorMap(user_id, "FFG", duration);

        // NamedColorUseSet pColorUseSet = null;
        // java.util.List<NamedColorUseSet> pColorSetGroup = new
        // ArrayList<NamedColorUseSet>();
        //
        // pColorUseSet = new NamedColorUseSet("FFG1", "Flash Flood Guidance",
        // FFGConstants.FFG_LEVELS, FFGConstants.FFG, "GRAY30", "GRAY10",
        // 3600);
        // pColorSetGroup.add(pColorUseSet);
        //
        // ArrayList<Colorvalue> colorSet = (ArrayList<Colorvalue>)
        // GetColorValues
        // // .get_colorvalues(user_id, app_name, "FFG1", 3600, "E",
        // .get_colorvalues(user_id, app_name, "FFG", 3600, "E",
        // pColorSetGroup);

        ArrayList<ColorLegendBarData> list = new ArrayList<ColorLegendBarData>();
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

        gd = new GridData(70, SWT.DEFAULT);
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
     * Populate the data list.
     */
    private void populateDataList() {
        dataList.removeAll();

        if (griddedRdo.getSelection()) {
            readGriddedFfgProduct();
        } else { // must be areal
            readArealFfgProduct();
        }
    }

    /**
     * Get the Gridded FFG products for the data list.
     */
    private void readGriddedFfgProduct() {
        FlashFloodGuidanceDataManager dman = FlashFloodGuidanceDataManager
                .getInstance();
        ArrayList<String> list = new ArrayList<String>();
        Map<String, String> sortedMap = new HashMap<String, String>();
        dataMap.clear();

        /* Check the FFG mode. */
        if (ffgAreaCbo.getItem(ffgAreaCbo.getSelectionIndex()).equals("RFC")) {
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

                    String line = String.format("%5s    %2s    %s", id, durHr,
                            dateStr);
                    Calendar cal = Calendar.getInstance(TimeZone
                            .getTimeZone("GMT"));
                    cal.setTime(date);
                    int hr = cal.get(Calendar.HOUR_OF_DAY);
                    String hrStr = String.valueOf(hr);
                    if (hr < 10) {
                        hrStr = "0" + hr;
                    }
                    String sortLine = id + " " + durHr + " "
                            + cal.get(Calendar.YEAR) + cal.get(Calendar.MONTH)
                            + cal.get(Calendar.DAY_OF_MONTH) + hrStr;
                    // + cal.get(Calendar.HOUR_OF_DAY);

                    // list.add(line);
                    sortedMap.put(sortLine, line);
                    dataMap.put(line, date);
                }
            }

            // Sort the data, first by id asc, then duration asc
            java.util.List<String> sortList = new ArrayList<String>(
                    sortedMap.keySet());
            String[] data = sortList.toArray(new String[sortList.size()]);

            java.util.Arrays.sort(data);

            String[] sortedData = sort(data);

            for (String s : sortedData) {
                dataList.add(sortedMap.get(s));
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
                // TODO error handling
                System.err
                        .println("Error getting WFO FFG directory from token "
                                + ffgDirToken);
            }

            File ffgDir = new File(ffgDirPath);
            if (ffgDir.exists() && ffgDir.canRead()) {
                File[] xmrgFiles = ffgDir.listFiles();

                for (File xmrg : xmrgFiles) {
                    /*
                     * Check to make sure that this is a FFG file. This is done
                     * by checking the extension on the file.
                     */
                    if (xmrg.getName().endsWith(".ffg")) {
                        /* Parse the filename for the WFO identifier here. */
                        String fileWfo = xmrg.getName().substring(0, 3);
                        int index = xmrg.getName().indexOf("20");
                        /* Parse the filename for duration and time stamp here. */
                        String year = xmrg.getName()
                                .substring(index, index + 4);
                        String month = xmrg.getName().substring(index + 4,
                                index + 6);
                        String day = xmrg.getName().substring(index + 6,
                                index + 8);
                        String hour = xmrg.getName().substring(index + 8,
                                index + 10);
                        String durString = xmrg.getName().substring(index + 10,
                                index + 12);
                        // String year = xmrg.getName().substring(3, 7);
                        // String month = xmrg.getName().substring(7, 9);
                        // String day = xmrg.getName().substring(9, 11);
                        // String hour = xmrg.getName().substring(11, 13);
                        // String durString = xmrg.getName().substring(13, 15);

                        Calendar cal = Calendar.getInstance(TimeZone
                                .getTimeZone("GMT"));
                        cal.set(Calendar.YEAR, Integer.parseInt(year));
                        cal.set(Calendar.MONTH, Integer.parseInt(month) - 1);
                        cal.set(Calendar.DATE, Integer.parseInt(day));
                        cal.set(Calendar.HOUR_OF_DAY, Integer.parseInt(hour));
                        cal.set(Calendar.MINUTE, 0);
                        cal.set(Calendar.SECOND, 0);
                        cal.set(Calendar.MILLISECOND, 0);

                        // Dur is FFG03, FFG06, etc
                        // selectedDur is 1hr, 3hr, etc
                        if ((selectedDur != null)
                                && !selectedDur.equalsIgnoreCase("All")) {
                            if (!durString.equalsIgnoreCase(selectedDur
                                    .substring(0, selectedDur.indexOf("h")))) {
                                continue;

                            }
                        }

                        String dateStr = sdf.format(cal.getTime());
                        String line = String.format("%5s    %2s    %s",
                                fileWfo, durString, dateStr);
                        String sortLine = fileWfo + " " + durString + " "
                                + year + month + day + hour;
                        sortedMap.put(sortLine, line);
                        list.add(line);
                        dataMap.put(line, cal.getTime());
                        fileMap.put(line, xmrg);
                    }
                }

                // Sort the data, first by id asc, then duration asc, date desc
                java.util.List<String> sortList = new ArrayList<String>(
                        sortedMap.keySet());
                String[] data = sortList.toArray(new String[sortList.size()]);

                java.util.Arrays.sort(data);
                String[] sortedData = sort(data);
                for (String s : sortedData) {
                    dataList.add(sortedMap.get(s));
                }
            }
        }
    }

    /**
     * Sort the list of items in reverse by time.
     * 
     * @param strArr
     *            The String[] sorted with time asc
     * @return The String[] sorted with time desc
     */
    private String[] sort(String[] strArr) {
        java.util.List<String> strList = new ArrayList<String>();
        java.util.List<String> holder = new ArrayList<String>();
        String prevDur = "";
        String prevId = "";

        for (String s : strArr) {
            String[] parts = s.split("\\s+", 3);

            if ((prevDur.equals(parts[1]) == false)
                    || (prevId.equals(parts[0]) == false)) {
                for (int i = holder.size() - 1; i >= 0; i--) {
                    strList.add(holder.get(i));
                }
                holder.clear();
                prevDur = parts[1];
                prevId = parts[0];
                holder.add(s);
            } else {
                holder.add(s);
            }
        }

        for (int i = holder.size() - 1; i >= 0; i--) {
            strList.add(holder.get(i));
        }

        return strList.toArray(new String[strList.size()]);
        // return strArr;
    }

    private void readArealFfgProduct() {
        // Clear the map for areal data
        dataMap.clear();
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

        Map<String, String> sortedMap = new TreeMap<String, String>();
        if (res == ResolutionLevel.ALL) {
            for (ResolutionLevel level : ResolutionLevel.values()) {
                if (level.getResolution().equals("All")
                        || level.getResolution().equals("Grid")) {
                    continue; // Skip "All" and "Grid"
                }

                TreeMap<String, String> tmpMap = bldFfgList(level);

                Iterator<String> iter = tmpMap.keySet().iterator();
                while (iter.hasNext()) {
                    String key = iter.next();
                    sortedMap.put(key, tmpMap.get(key));
                }
            }
        } else {
            sortedMap = bldFfgList(res);
        }

        Iterator<String> iter = sortedMap.keySet().iterator();
        java.util.List<String> strList = new ArrayList<String>();
        while (iter.hasNext()) {
            strList.add(iter.next());
        }

        String[] sa = sort(strList.toArray(new String[strList.size()]));

        for (String s : sa) {
            dataList.add(sortedMap.get(s));
        }
    }

    /**
     * Builds a list of available ffg products for the requested boundary_type
     * (basin, county, zone). Do not use this function for handling gridded FFG
     * requests.
     * 
     * @param level
     */
    private TreeMap<String, String> bldFfgList(ResolutionLevel level) {
        TreeMap<String, String> map = new TreeMap<String, String>();

        java.util.List<Object[]> rs = FlashFloodGuidanceDataManager
                .getInstance().getContingencyValue(level.getResolution());

        for (Object[] oa : rs) {
            Timestamp validTime = (Timestamp) oa[0];
            int shefDur = (Integer) oa[1];
            int dur = shefDur - (shefDur / 1000) * 1000;

            // Dur is FFG03, FFG06, etc
            // selectedDur is 1hr, 3hr, etc
            if ((selectedDur != null) && !selectedDur.equalsIgnoreCase("All")) {
                String durCheck = String.valueOf(dur);
                if (dur < 10) {
                    durCheck = "0" + dur;
                }
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
            String durStr = String.valueOf(dur);
            if (dur < 10) {
                durStr = "0" + dur;
            }
            String line = String
                    .format("%-6s   %2s    %s", id, durStr, dateStr);

            Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            cal.setTime(validTime);
            String sortLine = id + " " + dur + " " + cal.get(Calendar.YEAR)
                    + cal.get(Calendar.MONTH) + cal.get(Calendar.DAY_OF_MONTH)
                    + cal.get(Calendar.HOUR_OF_DAY);
            map.put(sortLine, line);
            dataMap.put(line, validTime);
        }

        return map;
    }

    /**
     * Display the data in CAVE
     */
    private void displayData() {
        FlashFloodGuidanceDataManager dman = FlashFloodGuidanceDataManager
                .getInstance();

        shell.setCursor(waitCursor);

        /* Get the selection from the list and break it up */
        String s = dataList.getItem(dataList.getSelectionIndex());
        String key = dataList.getItem(dataList.getSelectionIndex());
        s = s.replaceAll("\\s+", " ").trim();
        String[] parts = s.split(" ");
        String site = parts[0];
        String durationStr = parts[1];
        String day = parts[2];
        String date = parts[3];
        String hour = parts[4];
        duration = Integer.parseInt(durationStr)
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
                getParameters();

                if (rfcSelected) {
                    HydroDisplayManager.getInstance().displayGriddedFFG(
                            dataMap.get(key), duration, paramAbr, rfc, res);
                } else {
                    HydroDisplayManager.getInstance().displayGriddedFFG(
                            fileMap.get(key), duration, res);
                }
            } else {
                /* Display the areal basin */
                if (rfcSelected) {
                    HydroDisplayManager.getInstance()
                            .displayRfcGriddedFFGBasin(dataMap.get(key),
                                    duration, paramAbr, rfc, res);
                } else {
                    HydroDisplayManager.getInstance().displayGriddedFFGBasin(
                            fileMap.get(key), duration, res, dataMap.get(key));
                }
            }
        } else {
            /* Areal Radio selected */
            java.util.List<ArealData> arealList = buildFfgArea(site,
                    dataMap.get(key));

            HydroDisplayManager.getInstance().displayArealFfg(arealList,
                    duration, site, dataMap.get(key), valuesChk.getSelection(),
                    idsChk.getSelection());
        }
        // Create the legend strings
        // Build the string in the legend
        String line = site + " " + durationStr + " hours " + day + " " + date
                + " " + hour + ":00";
        colorLegend.setDisplayText("FFG Grid", line);

        shell.setCursor(arrowCursor);
    }

    /**
     * Get the parameters for the bundle
     */
    private void getParameters() {
        String data = dataList.getItem(dataList.getSelectionIndex());
        String key = dataList.getItem(dataList.getSelectionIndex());

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        data = data.replaceAll("\\s+", " ");
        data = data.trim();
        String[] parts = data.split(" ");
        String modelname = null;

        String param = "FFG" + parts[1] + "24hr";

        // Lookup the name
        String s = FlashFloodGuidanceDataManager.getInstance().rfcSiteLookup(
                parts[0]);
        if (s != null) {
            modelname = "FFG-" + s;
        }
        parameters.put("timespan", param);
        parameters.put("model", modelname);
        parameters.put("reftime", sdf.format(dataMap.get(key)));
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
                        ad.setValidTime(((Timestamp) oa2[1]));
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