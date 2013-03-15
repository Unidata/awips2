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

package com.raytheon.viz.hydro.pointprecipitation;

import static com.raytheon.viz.hydro.pointprecipitation.PointPrecipConstants.FMT_DUR;
import static com.raytheon.viz.hydro.pointprecipitation.PointPrecipConstants.FMT_DUR_HD_TR;
import static com.raytheon.viz.hydro.pointprecipitation.PointPrecipConstants.FMT_DUR_TR;
import static com.raytheon.viz.hydro.pointprecipitation.PointPrecipConstants.FMT_PCT;
import static com.raytheon.viz.hydro.pointprecipitation.PointPrecipConstants.MIN_PERCENT;
import static com.raytheon.viz.hydro.pointprecipitation.PointPrecipConstants.durationValues;
import static com.raytheon.viz.hydrocommon.HydroConstants.DATE_FORMAT;
import static com.raytheon.viz.hydrocommon.whfslib.PrecipUtil.MISSING_PRECIP;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.shef.tables.Rawpc;
import com.raytheon.uf.common.dataplugin.shef.tables.Rawpp;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.viz.hydro.pointprecipitation.PointPrecipConstants.FilterStation;
import com.raytheon.viz.hydro.pointprecipitation.PointPrecipConstants.RawPrecipTable;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDataCache;
import com.raytheon.viz.hydrocommon.data.RawPrecipData;
import com.raytheon.viz.hydrocommon.util.HydroQC;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Point Precipitation Accumulation dialog for
 * HydroView.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- ----------------------------
 * 29 NOV 2007  373        lvenable    Initial creation.
 * 10/24/2008   1617       grichard    Support point precip. accum.
 * 09/29/2010   4384      lbousaidi    Fixed PC/PP display and make Save button 
 * 									   save to tokenized directory
 * 03/14/2013   1790       rferrel     Changes for non-blocking dialog.
 * 
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class PointPrecipAccumDlg extends CaveSWTDialog {

    /**
     * Font used for controls.
     */
    private Font font;

    /**
     * Increment day button.
     */
    private Button upDayBtn;

    /**
     * Decrement day button.
     */
    private Button dnDayBtn;

    /**
     * Increment hours button.
     */
    private Button upHoursBtn;

    /**
     * Decrement hours button.
     */
    private Button dnHoursBtn;

    /**
     * Date/Time text control.
     */
    private Text dateTimeTF;

    /**
     * End time.
     */
    private Calendar endTime = Calendar
            .getInstance(TimeZone.getTimeZone("UTC"));

    /**
     * The begin time based off end time and duration.
     */
    private Calendar beginTime = null;

    /**
     * End query time.
     */
    private Calendar endQueryTime = Calendar.getInstance(TimeZone
            .getTimeZone("GMT"));

    /**
     * Begin query time.
     */
    private Calendar beginQueryTime = Calendar.getInstance(TimeZone
            .getTimeZone("GMT"));

    /**
     * Query end time.
     */
    private String queryEndTime;

    /**
     * Duration list control.
     */
    private List durList;

    /**
     * Selected Durations.
     */
    private int[] selectedDurations = new int[durationValues.length];

    /**
     * Durations.
     */
    private int[] durations;

    /**
     * hrfill.
     */
    private double[] hrfill;

    /**
     * amount.
     */
    private double[] amount;

    /**
     * summed_flag.
     */
    private int[] summedFlag;

    /**
     * Other text control.
     */
    private Text otherTF;

    /**
     * Load Data button.
     */
    private Button loadDataBtn;

    /**
     * Filter by HSA check box.
     */
    private Button byHsaChk;

    /**
     * Filter by location check box.
     */
    private Button byLocationChk;

    /**
     * HSA list control.
     */
    private List hsaList;

    /**
     * Filter location text control.
     */
    private Text locationTF;

    /**
     * Filter by Data Sources check box.
     */
    private Button byDataSourceChk;

    /**
     * PC list control.
     */
    private List pcCtrList;

    /**
     * PP list control.
     */
    private List ppIncList;

    /**
     * List of PTS values.
     */
    private java.util.List<String> pTs = new ArrayList<String>();

    /**
     * Sort combo box.
     */
    private Combo sortCbo;

    /**
     * Show details check box.
     */
    private Button showDetailsChk;

    /**
     * Add PP reports check box.
     */
    private Button addPPChk;

    /**
     * Text display control.
     */
    private StyledText textEditor;

    /**
     * Other time period index.
     */
    private final int OTHER_INDEX = 7;

    /**
     * List of PC (CurPC) values.
     */
    private java.util.List<Rawpc> pcHead = new ArrayList<Rawpc>();

    /**
     * List of PP (CurPP) values.
     */
    private java.util.List<Rawpp> ppHead = new ArrayList<Rawpp>();

    /** System wait cursor. */
    private Cursor waitCursor = null;

    /** Mapping of raw data. */
    private final Map<String, RawPrecipData> dataMap = new LinkedHashMap<String, RawPrecipData>();

    /** Point precipitation options. */
    private final PointPrecipOptions paOptions = new PointPrecipOptions();

    /** Text strings to display. */
    private final java.util.List<String> text = new ArrayList<String>();

    /** Use to format and order PC information for display. */
    private final java.util.List<String> pcDetail = new ArrayList<String>();

    /** Use to format not PC detail information for display. */
    private final java.util.List<String> ppDetail = new ArrayList<String>();

    /* Driver for sending data to printer. */
    private Printer printer;

    /** Printer's line height. */
    private int lineHeight = 0;

    /** Printer's tab width. */
    private int tabWidth = 0;

    /** Printer's left margin. */
    private int leftMargin;

    /** Printer's right margin. */
    private int rightMargin;

    /** Printer's top margin. */
    private int topMargin;

    /** Printer's bottom margin. */
    private int bottomMargin;

    /** Printer's current horizontal position. */
    private int x;

    /** Printer's current vertical position. */
    private int y;

    /** Current character in the text being printed. */
    private int index;

    /** Number of characters in the text being printed. */
    private int end;

    /** Buffer to store line to print. */
    private StringBuffer wordBuffer;

    /** Display driver for the printer. */
    private GC gc;

    /** Flag to indicate the is PC detail data. */
    private boolean pcDisplay = false;

    /** Flag to indicate non-PC detail data to display. */
    private boolean ppDisplay = false;

    /** size and location to display the dialog. */
    private Rectangle bounds;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public PointPrecipAccumDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Point Precipitation Accumulations");

        waitCursor = parent.getDisplay().getSystemCursor(SWT.CURSOR_WAIT);

        // Set the query times to current AWIPS time
        Date d = SimulatedTime.getSystemTime().getTime();
        endQueryTime.setTime(d);
        beginQueryTime.setTime(d);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        if (font != null) {
            font.dispose();
        }
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

        font = new Font(shell.getDisplay(), "Monospace", 11, SWT.NORMAL);

        createTopControls();
        createTextControl();
        createBottomButtons();
    }

    /**
     * Create the control at the top of the dialog.
     */
    private void createTopControls() {
        Composite topComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        topComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        topComp.setLayoutData(gd);

        createSelectTimePeriodGroup(topComp);
        createFilterGroup(topComp);
        createSortGroup(topComp);
    }

    /**
     * Create the Select Time Period controls.
     * 
     * @param parent
     *            Parent composite.
     */
    private void createSelectTimePeriodGroup(Composite parent) {
        Group selTimeGroup = new Group(parent, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        selTimeGroup.setLayout(gl);
        selTimeGroup.setText(" Select Time Period: ");

        // -----------------------------------------------------
        // Create the composite to hold the "end time" controls
        // -----------------------------------------------------
        Composite timeComp = new Composite(selTimeGroup, SWT.NONE);
        GridLayout timeGl = new GridLayout(4, false);
        timeComp.setLayout(timeGl);

        Label endTimeLbl = new Label(timeComp, SWT.CENTER);
        endTimeLbl.setText("Endtime:");

        // Add the time arrow buttons
        Composite timeArrowsComp = new Composite(timeComp, SWT.NONE);
        RowLayout timeArrowRl = new RowLayout(SWT.VERTICAL);
        timeArrowsComp.setLayout(timeArrowRl);

        RowData rd = new RowData(25, 25);
        upDayBtn = new Button(timeArrowsComp, SWT.ARROW);
        upDayBtn.setLayoutData(rd);
        upDayBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                endTime.add(Calendar.DAY_OF_MONTH, 1);
                dateTimeTF.setText(HydroConstants.DATE_FORMAT.format(endTime
                        .getTime()));
                // dateTimeTF
                // .setText(changeTime(endTime, Calendar.DAY_OF_MONTH, 1));
            }
        });

        rd = new RowData(25, 25);
        dnDayBtn = new Button(timeArrowsComp, SWT.ARROW | SWT.DOWN);
        dnDayBtn.setLayoutData(rd);
        dnDayBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                endTime.add(Calendar.DAY_OF_MONTH, -1);
                dateTimeTF.setText(HydroConstants.DATE_FORMAT.format(endTime
                        .getTime()));
                // dateTimeTF.setText(changeTime(endTime, Calendar.DAY_OF_MONTH,
                // -1));
            }
        });

        // Add the time text field
        GridData gd = new GridData(160, SWT.DEFAULT);
        // dateTimeTF has the pa_options.end_timestr value
        dateTimeTF = new Text(timeComp, SWT.BORDER);
        dateTimeTF.setFont(font);
        dateTimeTF.setLayoutData(gd);
        dateTimeTF.setText(initEndTime());

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
                endTime.add(Calendar.HOUR_OF_DAY, 1);
                dateTimeTF.setText(HydroConstants.DATE_FORMAT.format(endTime
                        .getTime()));
                // dateTimeTF
                // .setText(changeTime(endTime, Calendar.HOUR_OF_DAY, 1));
            }
        });

        rd = new RowData(25, 25);
        dnHoursBtn = new Button(hoursArrowsComp, SWT.ARROW | SWT.DOWN);
        dnHoursBtn.setLayoutData(rd);
        dnHoursBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                endTime.add(Calendar.HOUR_OF_DAY, -1);
                dateTimeTF.setText(HydroConstants.DATE_FORMAT.format(endTime
                        .getTime()));
                // dateTimeTF
                // .setText(changeTime(endTime, Calendar.HOUR_OF_DAY, -1));
            }
        });

        // ---------------------------------------------------
        // Create the composite to hold the duration controls
        // ---------------------------------------------------
        Composite durComp = new Composite(selTimeGroup, SWT.NONE);
        GridLayout durGl = new GridLayout(3, false);
        durComp.setLayout(durGl);

        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        Label durationLbl = new Label(durComp, SWT.NONE);
        durationLbl.setText("Duration:");
        durationLbl.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint = 80;
        gd.heightHint = 100;
        // durList has the pa_options.duration_set of values (indirectly)
        durList = new List(durComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        durList.setLayoutData(gd);
        durList.add(((Integer) durationValues[0]).toString() + " Hour");
        durList.add(((Integer) durationValues[1]).toString() + " Hour");
        durList.add(((Integer) durationValues[2]).toString() + " Hour");
        durList.add(((Integer) durationValues[3]).toString() + " Hour");
        durList.add(((Integer) durationValues[4]).toString() + " Hour");
        durList.add(((Integer) durationValues[5]).toString() + " Hour");
        durList.add(((Integer) durationValues[6]).toString() + " Hour");
        durList.add("Other");
        durList.setFont(font);
        durList.setSelection(4);
        durList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (durList.isSelected(OTHER_INDEX) == true) {
                    otherTF.setEnabled(true);
                } else {
                    otherTF.setEnabled(false);
                    otherTF.setText("");
                }
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        Composite durSubComp = new Composite(durComp, SWT.NONE);
        GridLayout durSubGl = new GridLayout(2, false);
        durSubGl.verticalSpacing = 15;
        durSubComp.setLayout(durSubGl);
        durSubComp.setLayoutData(gd);

        Label otherLbl = new Label(durSubComp, SWT.NONE);
        otherLbl.setText("Other:");

        gd = new GridData(80, SWT.DEFAULT);
        // otherTF has the pa_options.other_duration value
        otherTF = new Text(durSubComp, SWT.BORDER);
        otherTF.setLayoutData(gd);
        otherTF.setEnabled(false);
        otherTF.addFocusListener(new FocusAdapter() {
            @Override
            public void focusLost(FocusEvent event) {
                if (otherTF.getText().matches("([0-9])+")) {
                    otherTF.setText(otherTF.getText());
                } else {
                    userInformation("Enter duration in hours as a positive integer value");
                    otherTF.setFocus();
                }
            }
        });

        gd = new GridData(120, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        loadDataBtn = new Button(durSubComp, SWT.PUSH);
        loadDataBtn.setText("Load Data");
        loadDataBtn.setLayoutData(gd);
        loadDataBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if ((durList.isSelected(OTHER_INDEX) == true)
                        && (!otherTF.getText().matches("([0-9])+"))) {
                    userInformation("Enter duration in hours as a positive integer value");
                    otherTF.setFocus();
                } else {
                    boolean valid = true;

                    if (byLocationChk.getSelection()
                            && (locationTF.getText() == null)
                            && locationTF.getText().equals("")) {
                        valid = false;
                        userInformation("Enter a Location");
                    }

                    if (byHsaChk.getSelection()
                            && (hsaList.getSelectionCount() == 0)) {
                        valid = false;
                        userInformation("Select an HSA");
                    }

                    if (valid) {
                        loadData();
                    }
                }
            }
        });
    }

    /**
     * Create the Filter group container and the controls.
     * 
     * @param parent
     *            Parent composite.
     */
    private void createFilterGroup(Composite parent) {
        GridData mainFilterData = new GridData(SWT.DEFAULT, SWT.FILL, false,
                true);
        Group filterGroup = new Group(parent, SWT.NONE);
        GridLayout filterGl = new GridLayout(3, false);
        filterGroup.setLayout(filterGl);
        filterGroup.setLayoutData(mainFilterData);
        filterGroup.setText(" Filter: ");

        // ----------------------------------------------------
        // Create the composite to hold the filter by
        // check box controls
        // ----------------------------------------------------
        Composite filterByComp = new Composite(filterGroup, SWT.NONE);
        GridLayout filterByGl = new GridLayout(2, false);
        filterByComp.setLayout(filterByGl);

        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        // byHsaChk has the pa_options.hsa_switch state
        byHsaChk = new Button(filterByComp, SWT.CHECK);
        byHsaChk.setText("By HSA:");
        byHsaChk.setLayoutData(gd);

        gd = new GridData(80, 100);
        // hsaList has part of the pa_options.HSA_set data
        hsaList = new List(filterByComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        hsaList.setLayoutData(gd);

        // Get real data for the list
        PointPrecipDataManager dman = PointPrecipDataManager.getInstance();
        String[] items = dman.queryHsa();
        hsaList.setFont(font);
        hsaList.setItems(items);

        // byLocationChk has the pa_options.loc_switch state
        byLocationChk = new Button(filterByComp, SWT.CHECK);
        byLocationChk.setText("By\nLocation:");

        gd = new GridData(80, SWT.DEFAULT);
        // locationTF has the pa_options.locid or location ID
        locationTF = new Text(filterByComp, SWT.BORDER);
        locationTF.setLayoutData(gd);
        locationTF.addFocusListener(new FocusAdapter() {
            @Override
            public void focusLost(FocusEvent event) {
                if (locationTF.getText().matches("(\\w)*")) {
                    locationTF.setText(locationTF.getText().toUpperCase());
                } else {
                    userInformation("Enter location ID as a word-character ([a-zA-Z_0-9]*) string");
                    locationTF.setFocus();
                }
            }
        });

        // ----------------------------------------------------
        // Create the composite to hold the filter by
        // data source controls
        // ----------------------------------------------------
        Composite dataSourceComp = new Composite(filterGroup, SWT.NONE);
        GridLayout dataSourceGl = new GridLayout(2, false);
        dataSourceComp.setLayout(dataSourceGl);

        gd = new GridData();
        gd.horizontalSpan = 2;
        // byDataSourceChk has the pa_options.pets_switch state
        byDataSourceChk = new Button(dataSourceComp, SWT.CHECK);
        byDataSourceChk.setText("By Data Sources:");
        byDataSourceChk.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        Label pcLbl = new Label(dataSourceComp, SWT.NONE);
        pcLbl.setText("PC/Ctr");
        pcLbl.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        Label ppLbl = new Label(dataSourceComp, SWT.NONE);
        ppLbl.setText("PP/Inc");
        ppLbl.setLayoutData(gd);

        gd = new GridData(50, 110);
        // pcCtrList has part of the read_PETS_set data and is called ul_PCHead.
        pcCtrList = new List(dataSourceComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL);
        pcCtrList.setLayoutData(gd);
        // Get real data for the list.
        // pa_options.numPC_defined is the number of items (item count) in
        // pcCtrList.
        // pa_options.numPCTS_set is the grouping of indexes selected in
        // pcCtrList.
        // pa_options.numPC_selected is the number of items selected in
        // pcCtrList.
        items = dman.queryIngestfilterPc();
        pcCtrList.setFont(font);
        pcCtrList.setItems(items);

        // select all items in the list
        pcCtrList.setSelection(0, pcCtrList.getItemCount() - 1);

        gd = new GridData(50, 110);
        // ppIncList has part of the read_PETS_set data and is called ul_PPHead.
        ppIncList = new List(dataSourceComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL);
        ppIncList.setLayoutData(gd);
        // Get real data for the list.
        // pa_options.numPP_defined is the number of items (item count) in
        // ppIncList.
        // pa_options.numPPTS_set is the grouping of indexes selected in
        // ppIncList.
        // pa_options.numPP_selected is the number of items selected in
        // ppIncList.
        items = dman.queryIngestfilterPp();
        ppIncList.setFont(font);
        ppIncList.setItems(items);

        // select all items in the list
        ppIncList.setSelection(0, pcCtrList.getItemCount() - 1);
    }

    /**
     * Create the sort group container and its controls.
     * 
     * @param parent
     */
    private void createSortGroup(Composite parent) {
        GridData mainSortData = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group sortGroup = new Group(parent, SWT.NONE);
        GridLayout sortGl = new GridLayout(1, false);
        sortGroup.setLayout(sortGl);
        sortGroup.setLayoutData(mainSortData);
        sortGroup.setText(" Sort: ");

        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.widthHint = 120;
        // sortCbo has the pa_options.sort_option value
        sortCbo = new Combo(sortGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
        sortCbo.setLayoutData(gd);
        sortCbo.add("Location");
        sortCbo.add("Value");
        sortCbo.select(0);

        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        Label optionLbl = new Label(sortGroup, SWT.NONE);
        optionLbl.setText("Options:");
        optionLbl.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        // showDetailsChk has the pa_options.details_switch state
        showDetailsChk = new Button(sortGroup, SWT.CHECK);
        showDetailsChk.setText("Show Details");
        showDetailsChk.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        // addPPChk has the pa_options.PPaccum_switch state
        addPPChk = new Button(sortGroup, SWT.CHECK);
        addPPChk.setText("Add PP reports\nas needed");
        addPPChk.setLayoutData(gd);
        addPPChk.setSelection(true);
    }

    /**
     * Create the main text control.
     */
    private void createTextControl() {
        GridData gd = new GridData(900, 600);
        textEditor = new StyledText(shell, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL | SWT.H_SCROLL);
        textEditor.setLayoutData(gd);
        textEditor.setFont(font);
    }

    /**
     * Create the buttons located at the bottom of the dialog.
     */
    private void createBottomButtons() {
        GridData mainBtnData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite btnComp = new Composite(shell, SWT.NONE);
        GridLayout btnGl = new GridLayout(3, false);
        btnComp.setLayout(btnGl);
        btnComp.setLayoutData(mainBtnData);

        GridData gd = new GridData(SWT.BEGINNING, SWT.DEFAULT, true, false);
        gd.widthHint = 110;
        Button closeBtn = new Button(btnComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                bounds = shell.getBounds();
                close();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 110;
        Button saveBtn = new Button(btnComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                saveFile();
            }
        });

        gd = new GridData(SWT.END, SWT.DEFAULT, true, false);
        gd.widthHint = 110;
        Button printBtn = new Button(btnComp, SWT.PUSH);
        printBtn.setText("Print");
        printBtn.setLayoutData(gd);
        printBtn.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                printData();
            }
        });
    }

    /**
     * Method that loads the data from the curpc or curpp database table.
     */
    private void loadData() {
        Date endDate = null;
        queryEndTime = initQueryTime(endQueryTime);

        try {
            endDate = HydroConstants.DATE_FORMAT.parse(dateTimeTF.getText());
            paOptions.setEndDate(endDate);
        } catch (ParseException e) {
            userInformation("Invalid Endtime format:\nYYYY-MM-DD HH:MM:SS ");
            return;
        }

        /*
         * read the options and load them into the structure for reference
         */
        readAccumOptions();

        // Set the wait cursor
        shell.setCursor(waitCursor);

        // Clear the old data
        dataMap.clear();

        /* Reset the text store */
        text.clear();

        /*
         * only try and load the data if at least one type source was selected
         * for the physical element
         */

        if (pcCtrList.getSelectionCount() > 0) {
            getPrecipData(HydroConstants.PhysicalElement.PC);
            retrievePrecipAcccumulation();
        }

        dataMap.clear();

        if (ppIncList.getSelectionCount() > 0) {
            getPrecipData(HydroConstants.PhysicalElement.PP);
            retrievePrecipAcccumulation();
        }

        displayText();

        // Reset to default cursor
        shell.setCursor(null);
    }

    /**
     * function for managing the retrieval, derivation, and display of the
     * accumulation data for a single lid-pe-ts set of data
     * 
     */
    private void retrievePrecipAcccumulation() {
        FilterStation ignoreStation = FilterStation.UseStation;

        Set<String> keySetPp = dataMap.keySet();
        Iterator<String> iterPp = keySetPp.iterator();
        while (iterPp.hasNext()) {
            RawPrecipData data = dataMap.get(iterPp.next());
            /* Perform the filtering by hsa here, if necessary. */
            if (byHsaChk.getSelection()) {
                /* Hsa filtering is active */
                ignoreStation = filterStationByHsa(data.getLid());
            }
            if (ignoreStation == FilterStation.UseStation) {

                PrecipAccumulation pa = new PrecipAccumulation(data, paOptions);
                PointPrecipData ppd = pa.getPointPrecipData();
                writePaRecord(ppd, data);
            }
        }
    }

    /**
     * Method to read the duration values, consolidate them, and sort them.
     */
    private int[] readDurationSet() {

        /*
         * Fill all indexes in duration list. Also, make sure that the duration
         * array is in the correct order to start with.
         */

        java.util.Arrays.fill(selectedDurations, 9999);

        int[] durationSet = durList.getSelectionIndices();

        for (int i : durationSet) {
            selectedDurations[i] = durationValues[i];
        }

        /*
         * Test to determine if the "Other" option is being used. If it is,
         * then, reorder the contents of the duration_set and durations arrays
         * so that the "Other" value is shown in the correct order.
         */
        if ((otherTF.getEnabled()) && (otherTF.getText().length() > 0)) {
            selectedDurations[durationValues.length - 1] = Integer
                    .parseInt(otherTF.getText());
        }

        // Sort the array
        java.util.Arrays.sort(selectedDurations);

        // Determine the length (i.e., NUMDURATIONS) of the new durations array
        int NUMDURATIONS = 0;
        for (int i = 0; i < selectedDurations.length; i++) {
            if (selectedDurations[i] == 9999) {
                break;
            } else {
                NUMDURATIONS++;
            }

        }

        // Declare the durations array and initialize it (done via copyOf here).
        durations = java.util.Arrays.copyOf(selectedDurations, NUMDURATIONS);

        // Declare the hrfill array and initialize it via fill operation.
        hrfill = new double[NUMDURATIONS];
        java.util.Arrays.fill(hrfill, 0.0);

        // Declare the amount array and initialize it via fill operation.
        amount = new double[NUMDURATIONS];
        java.util.Arrays.fill(amount, MISSING_PRECIP);

        // Declare the summed_flag array and initialize it via fill operation.
        summedFlag = new int[NUMDURATIONS];
        java.util.Arrays.fill(summedFlag, 0);

        return durations;
    }

    /**
     * User Information Message Box
     * 
     * @param information
     *            - text message to display to the operator
     */
    private void userInformation(String information) {
        MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
        mb.setText("Notice");
        mb.setMessage(information);
        mb.open();
    }

    /**
     * Method that loads the data from the curpc or curpp database table.
     * preaccum_show.c line 1733
     */
    private void getPrecipData(HydroConstants.PhysicalElement pe) {
        PointPrecipDataManager dman = PointPrecipDataManager.getInstance();
        boolean notGettingAllTs;
        String lid = null;
        String id = null;

        /*
         * define the data time range based on the longest duration requested.
         */
        int durTime = 0;

        /*
         * Make sure to check whether or not the "Other" duration was selected.
         * If it is set, then there is no guarantee that it is the largest of
         * the durations in the list.
         */
        if (durations.length > 0) {
            durTime = durations[durations.length - 1];
        }

        /* define any location filter */
        if (byLocationChk.getSelection()) {
            lid = locationTF.getText();
        }

        setAccumBeginEnd(durTime);
        switch (pe) {
        case PC:
            /*
             * define any TS filter. only include the TS query filter if not all
             * TS are selected,
             */
            if (byDataSourceChk.getSelection()) {
                notGettingAllTs = false;
                if (pcCtrList.getSelectionCount() < pcCtrList.getItemCount()) {
                    notGettingAllTs = true;
                }
                pTs.clear();
                if (notGettingAllTs) {
                    int[] pcTsSet = pcCtrList.getSelectionIndices();
                    for (int i : pcTsSet) {
                        pTs.add(pcCtrList.getItem(i));
                    }
                }
            }

            /*
             * adjust the begin time, adding some hours for good measure, under
             * certain circumstances
             */
            beginQueryTime.setTimeInMillis(beginTime.getTimeInMillis());
            beginQueryTime.add(Calendar.HOUR_OF_DAY, -1);

            pcHead = dman.loadPcRaw(beginQueryTime.getTime(),
                    endQueryTime.getTime(), lid, pTs,
                    RawPrecipTable.CurRawPrecip);

            String prevId = null;
            RawPrecipData pd = new RawPrecipData();
            if ((pcHead != null) && (pcHead.size() > 0)) {
                // populate the data map
                prevId = pcHead.get(0).getLid();
                for (Rawpc rpc : pcHead) {
                    id = rpc.getLid();
                    if (!prevId.equals(id)) {
                        // Found next site
                        pd.setLid(prevId);
                        dataMap.put(prevId, pd);
                        pd = new RawPrecipData();
                        prevId = id;
                    } else {
                        if (dataMap.containsKey(id)
                                && !pd.equals(dataMap.get(id))) {
                            pd = dataMap.get(id);
                        }
                    }
                    pd.addRawpc(rpc);
                }
                dataMap.put(id, pd);
            }
            break;
        case PP:
            /*
             * define any TS filter. only include the TS query filter if not all
             * TS are selected,
             */
            if (byDataSourceChk.getSelection()) {
                notGettingAllTs = false;
                if (ppIncList.getSelectionCount() < ppIncList.getItemCount()) {
                    notGettingAllTs = true;
                }
                pTs.clear();
                if (notGettingAllTs) {
                    int[] ppTsSet = ppIncList.getSelectionIndices();
                    for (int i : ppTsSet) {
                        pTs.add(ppIncList.getItem(i));
                    }
                }
            }

            /*
             * adjust the begin time, adding some hours for good measure, under
             * certain circumstances
             */
            if (addPPChk.getSelection()) {
                beginQueryTime.setTimeInMillis(beginTime.getTimeInMillis());
                beginQueryTime.add(Calendar.HOUR_OF_DAY, -3);

                endQueryTime.setTimeInMillis(endTime.getTimeInMillis());
                endQueryTime.add(Calendar.HOUR_OF_DAY, 3);
            } else {
                beginQueryTime = beginTime;
                endQueryTime = endTime;
            }

            ppHead = dman.loadPpRaw(beginQueryTime.getTime(),
                    endQueryTime.getTime(), lid, pTs,
                    RawPrecipTable.CurRawPrecip);

            // populate the data map
            pd = new RawPrecipData();
            if ((ppHead != null) && (ppHead.size() > 0)) {
                prevId = ppHead.get(0).getLid();
                for (Rawpp rpp : ppHead) {
                    id = rpp.getLid();
                    if (!prevId.equals(id)) {
                        pd.setLid(prevId);
                        // Found next site
                        dataMap.put(prevId, pd);
                        pd = new RawPrecipData();
                        prevId = id;
                    } else {
                        if (dataMap.containsKey(id)
                                && !pd.equals(dataMap.get(id))) {
                            pd = dataMap.get(id);
                        }
                    }
                    pd.addRawpp(rpp);
                }
                dataMap.put(id, pd);
            }
            break;
        }
    }

    /**
     * Set the beginning and ending times for the query.
     * 
     * @param durTime
     *            The duration of the query.
     */
    private void setAccumBeginEnd(int durTime) {
        Date endDate = null;

        /* define the time range. first convert the endtime */
        try {
            endDate = HydroConstants.DATE_FORMAT.parse(dateTimeTF.getText());
            endTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            endTime.setTime(endDate);

            /* the start time is ALWAYS the endtime minus the duration */
            beginTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            beginTime.setTimeInMillis(endDate.getTime());
            beginTime.add(Calendar.HOUR_OF_DAY, durTime * -1);
        } catch (ParseException e) {
            userInformation("Invalid Endtime format:\nYYYY-MM-DD HH:MM:SS ");
        }
    }

    /**
     * Method that initializes the end time for a hydro query.
     * 
     * @return String representing the time in DATE_TIME format.
     */
    private String initEndTime() {
        Date now = SimulatedTime.getSystemTime().getTime();
        DATE_FORMAT.setTimeZone(TimeZone.getTimeZone("UTC"));
        endTime.setTime(now);

        if ((endTime.get(Calendar.HOUR_OF_DAY)) >= 18) {
            endTime.set(Calendar.HOUR_OF_DAY, 18);
        } else if ((endTime.get(Calendar.HOUR_OF_DAY)) >= 12) {
            endTime.set(Calendar.HOUR_OF_DAY, 12);
        } else if ((endTime.get(Calendar.HOUR_OF_DAY)) >= 6) {
            endTime.set(Calendar.HOUR_OF_DAY, 6);
        } else {
            endTime.set(Calendar.HOUR_OF_DAY, 0);
        }
        endTime.set(Calendar.MINUTE, 0);
        endTime.set(Calendar.SECOND, 0);

        return (DATE_FORMAT.format(endTime.getTime()));
    }

    /**
     * Method for initializing query time.
     * 
     */
    private String initQueryTime(Calendar calendar) {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        calendar.setTime(endTime.getTime());

        return (DATE_FORMAT.format(calendar.getTime()));
    }

    /**
     * Method that gets the current time.
     * 
     * @return String representing the current time in DATE_TIME format.
     */
    private String getTimeNow() {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        Date now = SimulatedTime.getSystemTime().getTime();

        Calendar nowTime = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        nowTime.setTime(now);

        return (sdf.format(nowTime.getTime()));
    }

    /**
     * Write the header information to the text editor.
     */
    private void writeHdrInfo() {
        double percent;
        StringBuilder hdr = new StringBuilder("Precipitation ending: ");
        hdr.append(queryEndTime);
        hdr.append(" for hr duration: ");
        hdr.append("<duration>");
        hdr.append("\n");
        hdr.append("Listing created: ");
        hdr.append(getTimeNow());
        hdr.append("\n");

        int index = hdr.indexOf("<duration>");
        index += 10;

        for (int i = 0; i < durations.length; i++) {
            hdr.insert(index, durations[i]);
            index += Integer.toString(durations[i]).length();
            hdr.insert(index, ",");
            index++;
            hdr.insert(index, " ");
            index++;
        }
        hdr.replace(hdr.indexOf("<duration>"), hdr.indexOf("<duration>") + 10,
                "");
        try {
            hdr.replace(hdr.indexOf(", \n"), hdr.indexOf(", \n") + 3, "\n");
        } catch (IndexOutOfBoundsException e) {
            // done with replacement
        }

        if (showDetailsChk.getSelection()) {
            if (ppIncList.getSelectionCount() > 0) {
                if (addPPChk.getSelection()) {
                    hdr.append("PP values indicated by a 's' are summation of PP reports.\n");
                } else {
                    hdr.append("PP values are direct PP reports; no summing of reports.\n");
                }
            }
            hdr.append("Number of hours found for each duration are shown in parentheses.\n");
        }
        percent = MIN_PERCENT * 100.0;
        if (percent > 0.0) {
            hdr.append("Total considered missing if hours found < ");
            hdr.append(String.format(FMT_PCT, percent));
            hdr.append(" of hours requested.\n");
        }
        hdr.append("\n");
        hdr.append(String.format(FMT_DUR_HD_TR, "Location", "Name", "PE", "TS"));

        for (int i = 0; i < durations.length; i++) {
            hdr.append(String.format(FMT_DUR, durations[i], "hr"));
        }
        hdr.append("\n");
        hdr.append(String.format(FMT_DUR_HD_TR, "========", "====", "==", "=="));

        for (int i = 0; i < durations.length; i++) {
            hdr.append(String.format(FMT_DUR_TR, "===", "=="));
        }

        hdr.append("\n");

        textEditor.setText(hdr.toString());
    }

    /**
     * Method that filters station by HSA.
     * 
     * @param locId
     *            -- the location identifier
     * @return -- the station usage indicator
     */
    private FilterStation filterStationByHsa(String locId) {
        /*
         * If all of the HSA's are selected, then use the station. No further
         * testing is required.
         */
        if (hsaList.getSelectionCount() == hsaList.getItemCount()) {
            return FilterStation.UseStation;
        }

        /* Declare hsa variable. */
        String hsa = HydroDataCache.getInstance().getHsa(locId);
        if (hsa == null) {
            return FilterStation.IgnoreStation;
        }

        /* Check to determine if the hsa is in the list selected by the user. */
        int[] hsaSet = hsaList.getSelectionIndices();
        for (int i : hsaSet) {
            if (hsa.equals((hsaList.getItem(i)))) {
                return FilterStation.UseStation;
            }
        }

        return FilterStation.IgnoreStation;
    }

    /**
     * Write the data for a single lid-pe-ts combination
     * 
     * @param data
     *            The PointPrecipData object
     */
    private void writePaRecord(PointPrecipData data, RawPrecipData rawData) {
        StringBuilder buffer = new StringBuilder();
        StringBuilder dataStr = new StringBuilder();
        String name = null;
        final String pcFormat = "  %8.2f  %s   %s     %-1s     %s\n";
        final String ppFormat = "  %8.2f  %s   %-16s %s     %s     %s\n";
        SimpleDateFormat sdf = new SimpleDateFormat("MM/dd HH:mm");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        /*
         * the max_value is placed here for possible sort purposes only. in all
         * cases, it it stripped off before presenting the output.
         */
        if (data.getMaxValue() != MISSING_PRECIP) {
            buffer.append(String.format("%.2f |", data.getMaxValue()));
        } else {
            buffer.append("-1. |");
        }

        /* write the key information */
        Object[] locInfo = PointPrecipDataManager.getInstance().getLocInfo(
                data.getLid());

        /* return if nothing is returned from location table */

        if (locInfo == null) {
            return;
        }

        name = (String) locInfo[2];

        // shorten the name if needed
        if ((name != null) && (name.length() > 20)) {
            name = name.substring(0, 20);
        } else if (name == null) {
            name = "";
        }
        dataStr.append(String.format("%-8s %-20s %-2s %-2s :", data.getLid(),
                name, data.getPe(), data.getTs()));

        /* write out the info for each selected duration */
        for (int i = 0; i < durations.length; i++) {
            double[] amounts = data.getAmount();
            if (amounts[i] == MISSING_PRECIP) {
                dataStr.append("   MSG");
            } else {
                dataStr.append(String.format("%6.2f", amounts[i]));
            }
        }

        /* show the details at the end of the line */
        if (paOptions.isDetailsSwitch()) {
            boolean first = true;
            dataStr.append(" (");
            int[] summedFlags = data.getSummedFlag();

            for (int i = 0; i < durations.length; i++) {
                if (first) {
                    first = false;
                } else {
                    dataStr.append("/");
                }

                String derivedStr = "";
                if ((summedFlags[i] == 1)
                        && data.getPe().equalsIgnoreCase("PP")) {
                    derivedStr = "s";
                }

                dataStr.append(String.format("%.1f%s", data.getHrfill()[i],
                        derivedStr));
            }
            dataStr.append(")");
        }
        dataStr.append("\n");

        text.add(buffer.toString() + dataStr.toString());

        /*
         * if showing details on only one location, then show the full time
         * series.
         */
        if (paOptions.isLocSwitch() && paOptions.isDetailsSwitch()) {
            /*
             * when listing out the data, always list in descending cron order.
             * the order of the linked list varies depending on whether it is PC
             * or PP, so this needs to be accounted for. PP is provided in the
             * descending time order, so the first pointer must be set special
             * for PC. also display a column header line
             */

            String ts = data.getTs();
            String pe = data.getPe();
            ArrayList<Rawpc> pcList = null;
            ArrayList<Rawpp> ppList = null;

            if (pe.equalsIgnoreCase("PC")) {
                pcList = rawData.getPcList(ts);
            } else {
                ppList = rawData.getPpList(ts);
            }

            text.add("\n");

            String timeStr = null;
            String extremum = null;
            int qualCode = 0;
            String qcStr = null;
            short dur;
            if (pe.equalsIgnoreCase("PC")) {
                if (pcList != null) {
                    pcDisplay = true;
                    for (Rawpc pc : pcList) {
                        if (pc.getTs().equalsIgnoreCase(ts)) {
                            extremum = pc.getExtremum();
                            if (pc.getQualityCode() != null) {
                                qualCode = pc.getQualityCode();
                            }
                            qcStr = HydroQC.buildQcSymbol(qualCode);
                            timeStr = sdf.format(pc.getObstime());
                            pcDetail.add(String.format(pcFormat, pc.getValue(),
                                    timeStr, extremum, pc.getShefQualCode(),
                                    qcStr));
                        }
                    }
                }

                /* Same as AWIPS I PC data is displayed in descending order */
                Collections.reverse(pcDetail);

            } else {
                ppDetail.add("     Value     Time      Duration         Ext Qualif  QC\n");
                if (ppList != null) {
                    ppDisplay = true;
                    for (Rawpp pp : ppList) {
                        extremum = pp.getExtremum();
                        if (pp.getQualityCode() != null) {
                            qualCode = pp.getQualityCode();
                        }
                        qcStr = HydroQC.buildQcSymbol(qualCode);
                        timeStr = sdf.format(pp.getObstime());
                        dur = pp.getDur();
                        String durStr = null;

                        /*
                         * build a presentable string for the duration code
                         * value
                         */
                        if (dur != 0) {
                            durStr = PointPrecipDataManager.getInstance()
                                    .getDur(dur);
                            if (durStr == null) {
                                durStr = dur + " ";
                            }
                        }

                        ppDetail.add(String.format(ppFormat, pp.getValue(),
                                timeStr, durStr, extremum,
                                pp.getShefQualCode(), qcStr));
                    }
                }
            }
        }
    }

    /**
     * Format and display data in the text editor.
     */
    private void displayText() {
        // write the header info to the dialog
        writeHdrInfo();

        String[] lineArr = text.toArray(new String[text.size()]);

        if (lineArr.length < 1) {
            textEditor.append("\n  REQUESTED DATA NOT FOUND.\n");
        } else {
            // sort by location or value, 0 = location,
            // but don't sort it if only displaying a single site
            if (sortCbo.getSelectionIndex() == 0) {
                ArrayList<String> al = new ArrayList<String>();
                for (String s : lineArr) {
                    // remove the leading number
                    String line = s.substring(s.indexOf("|") + 1, s.length());
                    al.add(line);
                }
                lineArr = al.toArray(new String[al.size()]);
                Arrays.sort(lineArr);
            } else {
                // sort by value
                Arrays.sort(lineArr);
                ArrayList<String> al = new ArrayList<String>();
                for (int i = lineArr.length - 1; i >= 0; i--) {
                    // for (String s: lineArr) {
                    // remove the leading number
                    String line = lineArr[i].substring(
                            lineArr[i].indexOf("|") + 1, lineArr[i].length());
                    al.add(line);
                }

                lineArr = al.toArray(new String[al.size()]);
            }

            for (String s : lineArr) {
                if (s.equals("\n")) {
                    continue;
                }

                textEditor.append(s);

                if (paOptions.isLocSwitch() && paOptions.isDetailsSwitch()) {

                    if (pcDisplay) {
                        textEditor
                                .append("\n     Value     Time      Ext Qualif  QC\n");
                        for (String s1 : pcDetail) {
                            pcDisplay = false;
                            textEditor.append(s1);
                        }
                    } else if (ppDisplay) {
                        for (String s2 : ppDetail) {
                            ppDisplay = false;
                            textEditor.append(s2);
                        }
                    }

                }
            }
        }

        // clear these for the next time through
        pcDetail.clear();
        ppDetail.clear();
    }

    /**
     * Read the various options and load them into the object for future
     * reference.
     */
    private void readAccumOptions() {
        PointPrecipDataManager dman = PointPrecipDataManager.getInstance();
        /*
         * get the value of all the settings and switches, starting with the end
         * time
         */
        paOptions.setLocSwitch(byLocationChk.getSelection());
        paOptions.setDetailsSwitch(showDetailsChk.getSelection());
        paOptions.setPpAccumSwitch(addPPChk.getSelection());
        paOptions.setHsaSwitch(byHsaChk.getSelection());
        paOptions.setPetsSwitch(byDataSourceChk.getSelection());
        paOptions.setLid(locationTF.getText());
        if ((otherTF.getText() != null) && (otherTF.getText().length() > 0)) {
            paOptions.setOtherDuration(Integer.parseInt(otherTF.getText()));
        }

        paOptions.setSortOption(sortCbo.getSelectionIndex());

        // Set the TS lists
        ArrayList<String> pcTsList = new ArrayList<String>();
        ArrayList<String> ppTsList = new ArrayList<String>();
        if (byDataSourceChk.getSelection()) {
            int[] indices = pcCtrList.getSelectionIndices();
            for (int i : indices) {
                pcTsList.add(pcCtrList.getItem(i));
            }

            indices = ppIncList.getSelectionIndices();
            for (int i : indices) {
                ppTsList.add(ppIncList.getItem(i));
            }

            paOptions.setPctsSet(pcTsList.toArray(new String[pcTsList.size()]));
            paOptions.setPptsSet(ppTsList.toArray(new String[pcTsList.size()]));
        } else {
            paOptions.setPctsSet(dman.queryIngestfilterPc());
            paOptions.setPptsSet(dman.queryIngestfilterPp());
        }

        /* get the selected durations */
        paOptions.setDurations(readDurationSet());

        /* Get the selected hsa. */
        paOptions.setHsaSet(hsaList.getSelectionIndices());
        paOptions.setNumHsaSelected(hsaList.getSelectionCount());
    }

    /**
     * Display dialog to obtain file for the save.
     */
    private void saveFile() {
        final String tokenizedDir = "whfs_report_dir";
        String saveTable = AppsDefaults.getInstance().getToken(tokenizedDir);
        FileDialog dialog = new FileDialog(shell, SWT.SAVE);
        dialog.setFilterPath(saveTable);
        String filename = dialog.open();
        if (filename == null) {
            return;
        }

        try {
            BufferedWriter out = new BufferedWriter(new FileWriter(filename));
            out.write(textEditor.getText());
            out.close();
        } catch (IOException e) {
            userInformation("Error saving file.");
        }
    }

    /**
     * Send the text to the printer.
     * 
     * @param printer
     *            The printer
     * @param text
     *            The text to print
     */
    public void print(Printer printer, String text) {
        if (printer.startJob("Text")) {
            Rectangle clientArea = printer.getClientArea();
            Rectangle trim = printer.computeTrim(0, 0, 0, 0);
            Point dpi = printer.getDPI();
            leftMargin = dpi.x + trim.x; // one inch from left side of paper
            // one inch from right side of paper
            rightMargin = clientArea.width - dpi.x + trim.x + trim.width;
            // one inch from top edge of paper
            topMargin = dpi.y + trim.y;
            // one inch from bottom edge of paper
            bottomMargin = clientArea.height - dpi.y + trim.y + trim.height;

            /* Create a buffer for computing tab width. */
            int tabSize = 4; // is tab width a user setting in your UI?
            StringBuffer tabBuffer = new StringBuffer(tabSize);
            for (int i = 0; i < tabSize; i++) {
                tabBuffer.append(' ');
            }
            String tabs = tabBuffer.toString();

            /*
             * Create printer GC, and create and set the printer font &
             * foreground color.
             */
            gc = new GC(printer);

            Font printerFont = new Font(printer, "Monospace", 8, SWT.NORMAL);

            Color printerForegroundColor = new Color(printer, new RGB(0, 0, 0));
            Color printerBackgroundColor = new Color(printer, new RGB(255, 255,
                    255));

            gc.setFont(printerFont);
            gc.setForeground(printerForegroundColor);
            gc.setBackground(printerBackgroundColor);
            tabWidth = gc.stringExtent(tabs).x;
            lineHeight = gc.getFontMetrics().getHeight();

            /* Print text to current gc using word wrap */
            printText(text);

            printer.endJob();

            /* Cleanup graphics resources used in printing */
            printerFont.dispose();
            printerForegroundColor.dispose();
            printerBackgroundColor.dispose();
            gc.dispose();
        }
    }

    /**
     * Print the text
     * 
     * @param text
     *            The text to be printed
     */
    private void printText(String text) {
        printer.startPage();
        wordBuffer = new StringBuffer();
        x = leftMargin;
        y = topMargin;
        index = 0;
        end = text.length();
        while (index < end) {
            char c = text.charAt(index);
            index++;
            if (c != 0) {
                if ((c == 0x0a) || (c == 0x0d)) {
                    if ((c == 0x0d) && (index < end)
                            && (text.charAt(index) == 0x0a)) {
                        index++; // if this is cr-lf, skip the lf
                    }
                    printWordBuffer();
                    newline();
                } else {
                    if (c != '\t') {
                        wordBuffer.append(c);
                    }
                    if (Character.isWhitespace(c)) {
                        printWordBuffer();
                        if (c == '\t') {
                            x += tabWidth;
                        }
                    }
                }
            }
        }
        if (y + lineHeight <= bottomMargin) {
            printer.endPage();
        }
    }

    /**
     * Word buffer for formating lines on the printed page
     */
    private void printWordBuffer() {
        if (wordBuffer.length() > 0) {
            String word = wordBuffer.toString();
            int wordWidth = gc.stringExtent(word).x;
            if (x + wordWidth > rightMargin) {
                /* word doesn't fit on current line, so wrap */
                newline();
            }
            gc.drawString(word, x, y, false);
            x += wordWidth;
            wordBuffer.setLength(0);
        }
    }

    /**
     * New line on the printed page
     */
    private void newline() {
        x = leftMargin;
        y += lineHeight;
        if (y + lineHeight > bottomMargin) {
            printer.endPage();
            if (index + 1 < end) {
                y = topMargin;
                printer.startPage();
            }
        }
    }

    /**
     * Performs the action for the print button.
     */
    private void printData() {
        PrintDialog dialog = new PrintDialog(shell, SWT.NONE);
        PrinterData data = dialog.open();
        final String textToPrint = textEditor.getText();

        if (data == null) {
            return;
        }

        if (textToPrint == null) {
            return;
        }

        printer = new Printer(data);

        /*
         * Do the printing in a background thread so that spooling does not
         * freeze the UI.
         */
        Thread printingThread = new Thread("PrintTable") {
            @Override
            public void run() {
                print(printer, textToPrint);
                printer.dispose();
            }
        };
        printingThread.start();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialog#preOpened()
     */
    @Override
    protected void preOpened() {
        super.preOpened();
        shell.addShellListener(new ShellAdapter() {

            @Override
            public void shellClosed(ShellEvent e) {
                bounds = shell.getBounds();
            }
        });
        if (bounds != null) {
            shell.setBounds(bounds);
        }
    }
}
