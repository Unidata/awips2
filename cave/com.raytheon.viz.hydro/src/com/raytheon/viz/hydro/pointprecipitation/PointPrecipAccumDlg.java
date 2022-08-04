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

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.OptionalDouble;
import java.util.OptionalInt;
import java.util.Set;
import java.util.StringJoiner;
import java.util.TimeZone;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
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
import com.raytheon.uf.common.dataplugin.shef.tables.Shefdur;
import com.raytheon.uf.common.hydro.CommonHydroConstants;
import com.raytheon.uf.common.hydro.data.PrecipRecord;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.pointprecipitation.PointPrecipConstants.FilterStation;
import com.raytheon.viz.hydro.pointprecipitation.PointPrecipConstants.RawPrecipTable;
import com.raytheon.viz.hydro.pointprecipitation.PointPrecipConstants.SortOption;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroConstants.PhysicalElement;
import com.raytheon.viz.hydrocommon.HydroDataCache;
import com.raytheon.viz.hydrocommon.util.QualityCodeUtil;
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
 *                                     save to tokenized directory
 * 03/14/2013   1790       rferrel     Changes for non-blocking dialog.
 * Jul 21, 2015 4500       rjpeter     Use Number in blind cast.
 * Feb 21, 2017 6035       njensen     Fix sizing issues
 * Feb 21, 2018 6968       mduff       Refactored to fix data value sorting.
 * Jan 31, 2019 6951       dgilling    Massive refactor/code cleanup to better
 *                                     match A1 behavior.
 * Mar 11, 2020 19533   mgamazaychikov Moved constants from PointPrecipConstants
 *                                     to CommonHydroConstants
 * </pre>
 *
 * @author lvenable
 *
 */
public class PointPrecipAccumDlg extends CaveSWTDialog {

    private static class PointPrecipOptions {

        public LocalDateTime endTime = null;

        public boolean detailsSwitch;

        public boolean hsaSwitch;

        public boolean locSwitch;

        public boolean petsSwitch;

        public boolean ppAccumSwitch;

        public String lid;

        public PointPrecipConstants.SortOption sortOption;

        public int[] durationSet;

        public int otherDuration;

        public String[] hsaSet;

        public String[] pctsSet;

        public String[] pptsSet;

        public boolean tempfile_created = false;

        /**
         * @return the numPcSelected
         */
        public int getNumPcSelected() {
            return pctsSet.length;
        }

        /**
         * @return the numPpSelected
         */
        public int getNumPpSelected() {
            return pptsSet.length;
        }

        /**
         * @return the numHsaSelected
         */
        public int getNumHsaSelected() {
            return hsaSet.length;
        }
    }

    private static class PrecipAccumulationRowData {

        private final float maxValue;

        private final String lid;

        private final String data;

        public PrecipAccumulationRowData(float maxValue, String lid,
                String data) {
            this.maxValue = maxValue;
            this.lid = lid;
            this.data = data;
        }

        public float getMaxValue() {
            return maxValue;
        }

        public String getLid() {
            return lid;
        }

        public String getData() {
            return data;
        }
    }

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    /**
     * Other time period index.
     */
    private static final int OTHER_INDEX = 7;

    private final Comparator<PrecipAccumulationRowData> LOCATION_COMPARATOR = Comparator
            .comparing(PrecipAccumulationRowData::getLid);

    private final Comparator<PrecipAccumulationRowData> VALUE_COMPARATOR = Comparator
            .comparing(PrecipAccumulationRowData::getMaxValue);

    private final DateFormat timeSeriesObsTimeFormat = new SimpleDateFormat(
            "MM/dd HH:mm");

    private final Map<Integer, String> shefDurCache = new HashMap<>();

    private OptionalDouble minPercent = OptionalDouble.empty();

    /**
     * Font used for controls.
     */
    private Font font;

    /**
     * Date/Time text control.
     */
    private Text dateTimeTF;

    /**
     * Duration list control.
     */
    private List durList;

    /**
     * Other text control.
     */
    private Text otherTF;

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

    /** Point precipitation options. */
    private final PointPrecipOptions paOptions = new PointPrecipOptions();

    /** List of all the row data objects */
    private final java.util.List<PrecipAccumulationRowData> allRows = new ArrayList<>();

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

        timeSeriesObsTimeFormat.setTimeZone(TimeUtil.GMT_TIME_ZONE);
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
        if (font != null) {
            font.dispose();
        }
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        font = new Font(shell.getDisplay(), "Monospace", 11, SWT.NORMAL);

        createTopControls();
        createTextControl();
        createBottomButtons();

        paOptions.hsaSet = new String[0];
        paOptions.pctsSet = new String[0];
        paOptions.pptsSet = new String[0];

        /*
         * set the list of pe/ts combinations. do this only once so that
         * subsequent entries will be quick. however, new items in the list
         * become available, they will not be recognized until the program
         * restarts
         */
        initPETSlist();

        /* initialize info related to the duration list */
        initDurationList();

        /* initialize the hsa list */
        initHSAlist();

        initEndTime();
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
        selTimeGroup.setLayout(new GridLayout(1, false));
        GridData gd = new GridData();
        gd.verticalAlignment = SWT.FILL;
        gd.grabExcessVerticalSpace = true;
        gd.verticalSpan = 2;
        selTimeGroup.setLayoutData(gd);
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

        Button upDayBtn = new Button(timeArrowsComp, SWT.ARROW);
        upDayBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                try {
                    readEndtimeStr();
                    paOptions.endTime = paOptions.endTime.plusDays(1);
                    dateTimeTF.setText(PointPrecipConstants.DATE_TIME_FORMATTER
                            .format(paOptions.endTime));
                } catch (DateTimeParseException e) {
                    statusHandler.warn(
                            "Could not read endtime string. Format must be YYYY-MM-DD HH:mm:ss.");
                }
            }
        });

        Button dnDayBtn = new Button(timeArrowsComp, SWT.ARROW | SWT.DOWN);
        dnDayBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                try {
                    readEndtimeStr();
                    paOptions.endTime = paOptions.endTime.minusDays(1);
                    dateTimeTF.setText(PointPrecipConstants.DATE_TIME_FORMATTER
                            .format(paOptions.endTime));
                } catch (DateTimeParseException e) {
                    statusHandler.warn(
                            "Could not read endtime string. Format must be YYYY-MM-DD HH:mm:ss.");
                }
            }
        });

        // Add the time text field
        // dateTimeTF has the pa_options.end_timestr value
        dateTimeTF = new Text(timeComp, SWT.BORDER);
        dateTimeTF.setFont(font);
        GC gc = new GC(dateTimeTF);
        int dtWidth = gc.textExtent(CommonHydroConstants.IHFS_DATE_FORMAT).x;
        gc.dispose();
        gd = new GridData(dtWidth, SWT.DEFAULT);
        dateTimeTF.setLayoutData(gd);

        // Add the hours arrows button
        Composite hoursArrowsComp = new Composite(timeComp, SWT.NONE);
        RowLayout hoursArrowRl = new RowLayout(SWT.VERTICAL);
        hoursArrowsComp.setLayout(hoursArrowRl);

        Button upHoursBtn = new Button(hoursArrowsComp, SWT.ARROW);
        upHoursBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                try {
                    readEndtimeStr();
                    paOptions.endTime = paOptions.endTime.plusHours(1);
                    dateTimeTF.setText(PointPrecipConstants.DATE_TIME_FORMATTER
                            .format(paOptions.endTime));
                } catch (DateTimeParseException e) {
                    statusHandler.warn(
                            "Could not read endtime string. Format must be YYYY-MM-DD HH:mm:ss.");
                }
            }
        });

        Button dnHoursBtn = new Button(hoursArrowsComp, SWT.ARROW | SWT.DOWN);
        dnHoursBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                try {
                    readEndtimeStr();
                    paOptions.endTime = paOptions.endTime.minusHours(1);
                    dateTimeTF.setText(PointPrecipConstants.DATE_TIME_FORMATTER
                            .format(paOptions.endTime));
                } catch (DateTimeParseException e) {
                    statusHandler.warn(
                            "Could not read endtime string. Format must be YYYY-MM-DD HH:mm:ss.");
                }
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

        // durList has the pa_options.duration_set of values (indirectly)
        durList = new List(durComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        durList.setItems(PointPrecipConstants.DURATION_NAMES);

        // set up duration list layout
        String longestString = PointPrecipConstants.DURATION_NAMES[0];
        for (String dur : PointPrecipConstants.DURATION_NAMES) {
            if (dur.length() > longestString.length()) {
                longestString = dur;
            }
        }
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gc = new GC(durList);
        Rectangle bounds = durList.computeTrim(0, 0,
                gc.stringExtent(longestString).x, durList.getItemHeight() * 5);
        gc.dispose();
        gd.widthHint = bounds.width;
        gd.heightHint = bounds.height;
        durList.setLayoutData(gd);

        durList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (durList.isSelected(OTHER_INDEX)) {
                    otherTF.setEnabled(true);
                } else {
                    otherTF.setEnabled(false);
                    otherTF.setText(StringUtils.EMPTY);
                }
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        Composite durSubComp = new Composite(durComp, SWT.NONE);
        GridLayout durSubGl = new GridLayout(2, false);
        durSubGl.verticalSpacing = 15;
        durSubGl.marginHeight = 0;
        durSubComp.setLayout(durSubGl);
        durSubComp.setLayoutData(gd);

        Label otherLbl = new Label(durSubComp, SWT.NONE);
        otherLbl.setText("Other:");
        otherLbl.setLayoutData(new GridData(SWT.DEFAULT, SWT.TOP, false, true));

        // otherTF has the pa_options.other_duration value
        otherTF = new Text(durSubComp, SWT.BORDER);
        gc = new GC(otherTF);
        int textWidth = gc.stringExtent("999999").x;
        gc.dispose();
        gd = new GridData();
        gd.widthHint = textWidth;
        otherTF.setLayoutData(gd);
        otherTF.setEnabled(false);
        otherTF.addVerifyListener((e) -> {
            e.doit = e.text.matches("[0-9]*");
        });

        gd = new GridData();
        gd.horizontalAlignment = SWT.FILL;
        gd.grabExcessHorizontalSpace = true;
        gd.horizontalSpan = 2;
        Button loadDataBtn = new Button(durSubComp, SWT.PUSH);
        loadDataBtn.setText("Load Data");
        loadDataBtn.setLayoutData(gd);
        loadDataBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (byLocationChk.getSelection()
                        && (StringUtils.isEmpty(locationTF.getText()))) {
                    userInformation("Enter a Location");
                    return;
                }

                if (byHsaChk.getSelection()
                        && (hsaList.getSelectionCount() == 0)) {
                    userInformation("Select an HSA");
                    return;
                }

                loadData();
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
        mainFilterData.verticalSpan = 2;
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
        filterByComp.setLayout(new GridLayout(2, false));

        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        // byHsaChk has the pa_options.hsa_switch state
        byHsaChk = new Button(filterByComp, SWT.CHECK);
        byHsaChk.setText("By HSA:");
        byHsaChk.setLayoutData(gd);

        // hsaList has part of the pa_options.HSA_set data
        hsaList = new List(filterByComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        gd = new GridData();
        gd.horizontalAlignment = SWT.FILL;
        Rectangle bounds = hsaList.computeTrim(0, 0, 1,
                hsaList.getItemHeight() * 5);
        gd.heightHint = bounds.height;
        hsaList.setLayoutData(gd);

        // byLocationChk has the pa_options.loc_switch state
        byLocationChk = new Button(filterByComp, SWT.CHECK);
        byLocationChk.setText("By\nLocation:");

        // locationTF has the pa_options.locid or location ID
        locationTF = new Text(filterByComp, SWT.BORDER);
        GC gc = new GC(locationTF);
        int textWidth = gc.stringExtent("XXXXXX99").x;
        gc.dispose();
        gd = new GridData();
        gd.widthHint = locationTF.computeTrim(0, 0, textWidth, 1).width;
        locationTF.setLayoutData(gd);
        locationTF.addVerifyListener((e) -> {
            e.doit = e.text.matches("\\w*");
        });

        // ----------------------------------------------------
        // Create the composite to hold the filter by
        // data source controls
        // ----------------------------------------------------
        Composite dataSourceComp = new Composite(filterGroup, SWT.NONE);
        GridLayout dataSourceGl = new GridLayout(2, true);
        dataSourceComp.setLayout(dataSourceGl);

        gd = new GridData();
        gd.horizontalSpan = 2;
        // byDataSourceChk has the pa_options.pets_switch state
        byDataSourceChk = new Button(dataSourceComp, SWT.CHECK);
        byDataSourceChk.setText("By Data Sources:");
        byDataSourceChk.setLayoutData(gd);

        Label pcLbl = new Label(dataSourceComp, SWT.NONE);
        pcLbl.setText("PC/Ctr");
        pcLbl.setLayoutData(new GridData());

        Label ppLbl = new Label(dataSourceComp, SWT.NONE);
        ppLbl.setText("PP/Inc");
        ppLbl.setLayoutData(new GridData());

        // pcCtrList has part of the read_PETS_set data and is called ul_PCHead.
        pcCtrList = new List(dataSourceComp,
                SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        gd = new GridData();
        gd.horizontalAlignment = SWT.FILL;
        bounds = pcCtrList.computeTrim(0, 0, 1, pcCtrList.getItemHeight() * 5);
        gd.heightHint = bounds.height;
        pcCtrList.setLayoutData(gd);

        // ppIncList has part of the read_PETS_set data and is called ul_PPHead.
        ppIncList = new List(dataSourceComp,
                SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        gd = new GridData();
        gd.horizontalAlignment = SWT.FILL;
        bounds = ppIncList.computeTrim(0, 0, 1, ppIncList.getItemHeight() * 5);
        gd.heightHint = bounds.height;
        ppIncList.setLayoutData(gd);
    }

    /**
     * Create the sort group container and its controls.
     *
     * @param parent
     */
    private void createSortGroup(Composite parent) {
        Group sortGroup = new Group(parent, SWT.NONE);
        GridLayout sortGl = new GridLayout(1, false);
        sortGroup.setLayout(sortGl);
        sortGroup.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        sortGroup.setText(" Sort: ");

        // sortCbo has the pa_options.sort_option value
        sortCbo = new Combo(sortGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
        sortCbo.setLayoutData(new GridData(SWT.DEFAULT, SWT.TOP, false, true));
        sortCbo.add("Location");
        sortCbo.add("Value");
        sortCbo.select(0);
        sortCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                readSortOption();

                if (paOptions.tempfile_created) {
                    textEditor.setText(StringUtils.EMPTY);
                    displayText();
                }
            }
        });

        Group optionsGroup = new Group(parent, SWT.NONE);
        optionsGroup.setText(" Options: ");
        optionsGroup.setLayout(new GridLayout(1, false));
        optionsGroup
                .setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        // showDetailsChk has the pa_options.details_switch state
        showDetailsChk = new Button(optionsGroup, SWT.CHECK);
        showDetailsChk.setText("Show Details");
        showDetailsChk
                .setLayoutData(
                        new GridData(SWT.DEFAULT, SWT.TOP, false, false));

        // addPPChk has the pa_options.PPaccum_switch state
        addPPChk = new Button(optionsGroup, SWT.CHECK);
        addPPChk.setText("Add PP reports\nas needed");
        addPPChk.setLayoutData(
                new GridData(SWT.DEFAULT, SWT.TOP, false, false));
        addPPChk.setSelection(true);
    }

    /**
     * Create the main text control.
     */
    private void createTextControl() {
        textEditor = new StyledText(shell,
                SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL);
        textEditor.setEditable(false);
        GC gc = new GC(textEditor);
        gc.setFont(font);
        int charWidth = gc.getFontMetrics().getAverageCharWidth();
        gc.dispose();
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Rectangle bounds = textEditor.computeTrim(0, 0, charWidth * 100,
                textEditor.getLineHeight() * 32);
        gd.heightHint = bounds.height;
        gd.widthHint = bounds.width;
        textEditor.setLayoutData(gd);
        textEditor.setFont(font);
    }

    /**
     * Create the buttons located at the bottom of the dialog.
     */
    private void createBottomButtons() {
        Composite btnComp = new Composite(shell, SWT.NONE);
        btnComp.setLayout(new GridLayout(3, true));
        btnComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        Button closeBtn = new Button(btnComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(
                new GridData(SWT.BEGINNING, SWT.DEFAULT, true, false));
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                bounds = shell.getBounds();
                close();
            }
        });

        Button saveBtn = new Button(btnComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(
                new GridData(SWT.CENTER, SWT.DEFAULT, true, false));
        saveBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                saveFile();
            }
        });

        Button printBtn = new Button(btnComp, SWT.PUSH);
        printBtn.setText("Print");
        printBtn.setLayoutData(new GridData(SWT.END, SWT.DEFAULT, true, false));
        printBtn.addSelectionListener(new SelectionAdapter() {

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
        try {
            /*
             * read the options and load them into the structure for reference
             */
            readAccumOptions();

            allRows.clear();

            /*
             * set the cursor to a watch. do this here, after the possible
             * return above due to file open error, so that cursor isn't set
             * wrong
             */
            shell.setCursor(
                    shell.getDisplay().getSystemCursor(SWT.CURSOR_WAIT));

            /*
             * only try and load the data if at least one type source was
             * selected for the physical element
             */
            Map<String, Map<String, java.util.List<PrecipRecord>>> pcHead = Collections
                    .emptyMap();
            if (paOptions.getNumPcSelected() > 0) {
                pcHead = getPrecipData(HydroConstants.PhysicalElement.PC);
            }

            Map<String, Map<String, java.util.List<PrecipRecord>>> ppHead = Collections
                    .emptyMap();
            if (paOptions.getNumPpSelected() > 0) {
                ppHead = getPrecipData(HydroConstants.PhysicalElement.PP);
            }

            /*
             * HSA filtering was handled in getPrecipData to limit number of
             * records to manage
             */

            statusHandler.debug(String.format("pc, pp cnt=%d %d\n",
                    pcHead.size(), ppHead.size()));

            for (Entry<String, Map<String, java.util.List<PrecipRecord>>> entry : pcHead
                    .entrySet()) {
                /*
                 * Determine the value and load into the global structure, write
                 * the data to the output file
                 */
                Map<String, PointPrecipData> paData = loadPrecipValues(
                        entry.getValue());
                writePaRecord(paData, entry.getValue());
            }

            for (Entry<String, Map<String, java.util.List<PrecipRecord>>> entry : ppHead
                    .entrySet()) {
                Map<String, PointPrecipData> paData = loadPrecipValues(
                        entry.getValue());
                writePaRecord(paData, entry.getValue());
            }

            displayText();
        } finally {
            /* reset the cursor */
            shell.setCursor(null);
        }
    }

    /**
     * Primary driver function for managing the retrieval, derivation, and
     * display of the accumulation data for a single lid-pe-ts set of data
     * within the linked list.
     *
     * @param lidPeTsRecords
     * @return
     */
    private Map<String, PointPrecipData> loadPrecipValues(
            Map<String, java.util.List<PrecipRecord>> lidPeTsRecords) {
        /* Initialize the precipitation processing settings. */
        if (!minPercent.isPresent()) {
            double newValue = AppsDefaults.getInstance()
                    .getDouble(PointPrecipConstants.HV_MIN_DUR_FILLED, 0.0);
            minPercent = OptionalDouble.of(newValue);
        }

        Map<String, PointPrecipData> sitePrecipByType = new HashMap<>();

        String pe = lidPeTsRecords
                .get(lidPeTsRecords.keySet().iterator().next()).get(0).getPe();

        PrecipAccumulation precipCalc = new PrecipAccumulation(
                PointPrecipConstants.EXACT_ENDINGTIME_MATCH,
                minPercent.getAsDouble(), true, !paOptions.ppAccumSwitch);
        for (int duration : paOptions.durationSet) {
            /*
             * for the current duration, determine the begin and end time
             */
            Pair<LocalDateTime, LocalDateTime> timeRange = setAccumBeginEnd(
                    duration);
            LocalDateTime beginTime = timeRange.getFirst();
            LocalDateTime endTime = timeRange.getSecond();

            Collection<TotalPrecip> totalPrecip = Collections.emptyList();
            if ("PC".equals(pe)) {
                /*
                 * use the PC function if processing PC data. PC data is always
                 * derived, so fix the indicator flag to 1
                 */
                totalPrecip = precipCalc
                        .getTotalRawPrecip(lidPeTsRecords, PhysicalElement.PC,
                                beginTime, endTime);
            } else {
                /*
                 * for PP data, specify whether the user wants the function to
                 * derive the amount as needed. also, get a flag back indicating
                 * whether the data was derived or not.
                 */
                totalPrecip = precipCalc
                        .getTotalRawPrecip(lidPeTsRecords, PhysicalElement.PP,
                                beginTime, endTime);
            }

            for (TotalPrecip precipRecord : totalPrecip) {
                String ts = precipRecord.getTS();
                PointPrecipData paData = sitePrecipByType.get(ts);
                if (paData == null) {
                    /*
                     * initialize the temporary structure to hold the data for
                     * all the durations for this lid-pe-ts
                     */
                    paData = new PointPrecipData(precipRecord.getLid(),
                            precipRecord.getPE(), ts);
                    sitePrecipByType.put(ts, paData);
                }
                boolean summedFlag = ("PC".equals(pe)) ? true
                        : precipRecord.isSummed_flag();
                paData.addData(duration, precipRecord.getHours_covered(),
                        precipRecord.getValue(), summedFlag);
            }
        }

        return sitePrecipByType;
    }

    private void readSortOption() {
        switch (sortCbo.getSelectionIndex()) {
        case 0:
            paOptions.sortOption = SortOption.SortByLocation;
            break;
        case 1:
            paOptions.sortOption = SortOption.SortByValue;
            break;
        }
    }

    /**
     * Method to read the duration values, consolidate them, and sort them.
     */
    private void readDurationSet() {
        Set<Integer> durations = new HashSet<>();

        for (int i : durList.getSelectionIndices()) {
            if (i < PointPrecipConstants.durationValues.length) {
                durations.add(PointPrecipConstants.durationValues[i]);
            } else {
                durations.add(paOptions.otherDuration);
            }
        }

        int[] sortedDurations = durations.stream().mapToInt(Integer::intValue)
                .sorted().toArray();
        paOptions.durationSet = sortedDurations;
    }

    /**
     * This tracks which of the rows of the list of pe-ts combinations are
     * currently selected.
     */
    private void readPETSSet() {
        /* find out which rows are selected */
        paOptions.pctsSet = (paOptions.petsSwitch) ? pcCtrList.getSelection()
                : pcCtrList.getItems();

        paOptions.pptsSet = (paOptions.petsSwitch) ? ppIncList.getSelection()
                : ppIncList.getItems();
    }

    private void readHSASet() {
        paOptions.hsaSet = (paOptions.hsaSwitch) ? hsaList.getSelection()
                : hsaList.getItems();
    }

    private void readEndtimeStr() {
        String str = dateTimeTF.getText().trim();
        paOptions.endTime = LocalDateTime.parse(str,
                PointPrecipConstants.DATE_TIME_FORMATTER);
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
    private Map<String, Map<String, java.util.List<PrecipRecord>>> getPrecipData(
            HydroConstants.PhysicalElement pe) {
        /*
         * define the data time range based on the longest duration requested.
         * this is not necessarily the time range of the data query
         */
        OptionalInt maxDurationSelected = Arrays.stream(paOptions.durationSet)
                .max();
        int durTime = maxDurationSelected.orElse(0);

        Pair<LocalDateTime, LocalDateTime> timeRange = setAccumBeginEnd(
                durTime);
        LocalDateTime beginTime = timeRange.getFirst();
        LocalDateTime endTime = timeRange.getSecond();

        /*
         * adjust the begin time, adding some hours for good measure, under
         * certain circumstances
         */
        if (pe == PhysicalElement.PC) {
            beginTime = beginTime.minusHours(1);
        } else if (pe == PhysicalElement.PP) {
            if (paOptions.ppAccumSwitch) {
                beginTime = beginTime.minusHours(3);
            }

            endTime = endTime.plusHours(3);
        }

        /* define any location filter */
        String lid = null;
        if (paOptions.locSwitch) {
            lid = locationTF.getText();
        }

        java.util.List<String> pTs = (pe == PhysicalElement.PC)
                ? Arrays.asList(paOptions.pctsSet)
                : Arrays.asList(paOptions.pptsSet);

        Map<String, Map<String, java.util.List<PrecipRecord>>> ppHead = Collections
                .emptyMap();
        if (pe == PhysicalElement.PC) {
            Collection<Rawpc> rawPcRecords = PointPrecipDataManager.loadPCRaw(
                    beginTime, endTime, lid,
                    pTs, RawPrecipTable.CurRawPrecip);
            String where = PointPrecipDataManager.getPCPPQuery();
            statusHandler
                    .debug(String.format("SELECT * from CurPC %s\n", where));
            ppHead = rawPcRecords.stream().filter((r) -> {
                if (paOptions.hsaSwitch) {
                    return filterStationByHsa(
                            r.getLid()) == FilterStation.UseStation;
                }
                return true;
            }).map(PrecipRecord::new).collect(Collectors.groupingBy(
                    r -> r.getLid(), Collectors.groupingBy(r -> r.getTs())));
        } else {
            Collection<Rawpp> rawPpRecords = PointPrecipDataManager.loadPPRaw(
                    beginTime, endTime, lid,
                    pTs, RawPrecipTable.CurRawPrecip);
            String where = PointPrecipDataManager.getPCPPQuery();
            statusHandler
                    .debug(String.format("SELECT * from CurPP %s\n", where));
            ppHead = rawPpRecords.stream().filter((r) -> {
                if (paOptions.hsaSwitch) {
                    return filterStationByHsa(
                            r.getLid()) == FilterStation.UseStation;
                }
                return true;
            }).map(PrecipRecord::new).collect(Collectors.groupingBy(
                    r -> r.getLid(), Collectors.groupingBy(r -> r.getTs())));
        }

        return ppHead;
    }

    /**
     * Set the beginning and ending times for the query.
     *
     * @param durTime
     *            The duration of the query.
     */
    private Pair<LocalDateTime, LocalDateTime> setAccumBeginEnd(int durTime) {
        LocalDateTime endTime = paOptions.endTime;
        LocalDateTime startTime = endTime.minusHours(durTime);
        return new Pair<>(startTime, endTime);
    }

    private void initPETSlist() {
        // Get real data for the list.
        // pa_options.numPC_defined is the number of items (item count) in
        // pcCtrList.
        // pa_options.numPCTS_set is the grouping of indexes selected in
        // pcCtrList.
        // pa_options.numPC_selected is the number of items selected in
        // pcCtrList.
        try {
            String[] pcItems = PointPrecipDataManager.queryIngestfilterPc();
            pcCtrList.setItems(pcItems);
            // select all items in the list
            pcCtrList.setSelection(0, pcCtrList.getItemCount() - 1);
        } catch (VizException e) {
            statusHandler.error(
                    "Unable to retrieve type sources for physical element PC.",
                    e);
        }

        // Get real data for the list.
        // pa_options.numPP_defined is the number of items (item count) in
        // ppIncList.
        // pa_options.numPPTS_set is the grouping of indexes selected in
        // ppIncList.
        // pa_options.numPP_selected is the number of items selected in
        // ppIncList.
        try {
            String[] ppItems = PointPrecipDataManager.queryIngestfilterPp();
            ppIncList.setItems(ppItems);

            // select all items in the list
            ppIncList.setSelection(0, pcCtrList.getItemCount() - 1);
        } catch (VizException e) {
            statusHandler.error(
                    "Unable to retrieve type sources for physical element PP.",
                    e);
        }
    }

    private void initHSAlist() {
        try {
            String[] items = PointPrecipDataManager.queryHsa();
            hsaList.setItems(items);
        } catch (VizException e) {
            statusHandler.error("Failed to retrieve hsa list.", e);
        }
    }

    /**
     * Method that initializes the end time for a hydro query.
     *
     * @return String representing the time in DATE_TIME format.
     */
    private void initEndTime() {
        Date dateNow = SimulatedTime.getSystemTime().getTime();
        LocalDateTime utime = LocalDateTime.ofInstant(dateNow.toInstant(),
                ZoneOffset.UTC);

        int synopHour = (utime.getHour() / 6) * 6;

        paOptions.endTime = LocalDateTime.of(utime.toLocalDate(),
                LocalTime.of(synopHour, 0));

        dateTimeTF.setText(PointPrecipConstants.DATE_TIME_FORMATTER
                .format(paOptions.endTime));
    }

    private void initDurationList() {
        durList.setSelection(4);
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
        /*
         * set a flag indicating that the file has been created. this is helpful
         * to know when the callback on the sort button should attempt to
         * redisplay the sorted data.
         */
        paOptions.tempfile_created = true;

        StringBuilder hdr = new StringBuilder("Precipitation ending: ");
        hdr.append(PointPrecipConstants.DATE_TIME_FORMATTER
                .format(paOptions.endTime));
        hdr.append(" for hr duration: ");
        hdr.append(Arrays.stream(paOptions.durationSet)
                .mapToObj(Integer::toString).collect(Collectors.joining(" ")));
        hdr.append("\n");
        hdr.append("Listing created: ");
        hdr.append(getTimeNow());
        hdr.append("\n");

        if (paOptions.detailsSwitch) {
            if (paOptions.getNumPpSelected() > 0) {
                if (paOptions.ppAccumSwitch) {
                    hdr.append(
                            "PP values indicated by a 's' are summation of PP reports.\n");
                } else {
                    hdr.append(
                            "PP values are direct PP reports; no summing of reports.\n");
                }
            }
            hdr.append(
                    "Number of hours found for each duration are shown in parentheses.\n");
        }
        double percent = minPercent.orElse(-1) * 100.0;
        if (percent > 0.0) {
            hdr.append("Total considered missing if hours found < ");
            hdr.append(String.format(PointPrecipConstants.FMT_PCT, percent));
            hdr.append("% of hours requested.\n");
        }
        hdr.append("\n");
        hdr.append(String.format(PointPrecipConstants.FMT_DUR_HD_TR, "Location",
                "Name", "PE", "TS"));

        for (int duration : paOptions.durationSet) {
            hdr.append(String.format(PointPrecipConstants.FMT_DUR, duration,
                    "hr"));
        }
        hdr.append("\n");
        hdr.append(String.format(PointPrecipConstants.FMT_DUR_HD_TR, "========",
                "====", "==", "=="));
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
        if (paOptions.getNumHsaSelected() == hsaList.getItemCount()) {
            return FilterStation.UseStation;
        }

        /* Declare hsa variable. */
        String hsa = HydroDataCache.getInstance().getHsa(locId);
        if (hsa == null) {
            return FilterStation.IgnoreStation;
        }

        /* Check to determine if the hsa is in the list selected by the user. */
        for (String selectedHsa : paOptions.hsaSet) {
            if (selectedHsa.equals(hsa)) {
                return FilterStation.UseStation;
            }
        }

        return FilterStation.IgnoreStation;
    }

    /**
     * Write the data for a single lid-pe-ts combination
     *
     * @param paData
     *            The PointPrecipData object
     */
    private void writePaRecord(Map<String, PointPrecipData> paData,
            Map<String, java.util.List<PrecipRecord>> rawData) {
        String lid = paData.values().iterator().next().getLid();
        String name = StringUtils.left(HydroDataCache.getInstance()
                .getLocationName(lid).orElse(StringUtils.EMPTY), 20);

        for (String ts : new TreeSet<>(paData.keySet())) {
            PointPrecipData paDataRec = paData.get(ts);
            java.util.List<PrecipRecord> rawDataRecs = rawData.get(ts);

            StringBuilder sb = new StringBuilder();

            /* write the key information */
            sb.append(
                    String.format("%-8s %-20s %-2s %-2s :", paDataRec.getLid(),
                            name, paDataRec.getPe(), paDataRec.getTs()));

            /* write out the info for each selected duration */
            for (int dur : paOptions.durationSet) {
                float amount = paDataRec.getValue(dur);
                if (PointPrecipConstants.MISSING_PRECIP == amount) {
                    sb.append("   MSG ");
                } else {
                    sb.append(String.format("  %5.2f", amount));
                }
            }

            /* show the details at the end of the line */
            if (paOptions.detailsSwitch) {
                StringJoiner joiner = new StringJoiner("/", " (", ")");
                for (int dur : paOptions.durationSet) {
                    String derivedStr = ((paDataRec.isSummedFlag(dur))
                            && (CommonHydroConstants.PP
                                    .equals(paDataRec.getPe()))) ? "s"
                                            : StringUtils.EMPTY;
                    joiner.add(String.format("%.1f%s", paDataRec.getHrFill(dur),
                            derivedStr));
                }
                sb.append(joiner.toString());
            }
            sb.append('\n');

            /*
             * if showing details on only one location, then show the full time
             * series.
             */
            if (paOptions.locSwitch && paOptions.detailsSwitch) {
                sb.append('\n');

                if (CommonHydroConstants.PC.equals(paDataRec.getPe())) {
                    sb.append("     Value     Time      Ext Qualif  QC\n");
                } else {
                    sb.append(
                            "     Value     Time      Duration         Ext Qualif  QC\n");
                }

                for (PrecipRecord rawDataRec : rawDataRecs) {
                    String timestr = timeSeriesObsTimeFormat
                            .format(rawDataRec.getDate());

                    /* build a presentable string for the quality code value */
                    String qcStr = QualityCodeUtil
                            .buildQcSymbol(rawDataRec.getQualityCode());

                    if (CommonHydroConstants.PC.equals(paDataRec.getPe())) {
                        sb.append(
                                String.format("  %8.2f  %s  %s     %s     %s\n",
                                        rawDataRec.getValue(), timestr,
                                        rawDataRec.getExtremum(),
                                        rawDataRec.getShefQualCode(), qcStr));
                    } else {
                        /*
                         * build a presentable string for the duration code
                         * value
                         */
                        String durStr = StringUtils.EMPTY;
                        if (rawDataRec.getDuration() != 0) {
                            durStr = shefDurCache.get(rawDataRec.getDuration());
                            if (durStr == null) {
                                String where = String.format(" WHERE dur=%d ",
                                        rawDataRec.getDuration());
                                try {
                                    java.util.List<Shefdur> durPtr = PointPrecipDataManager
                                            .getShefDur(where);
                                    if (durPtr.isEmpty()) {
                                        durStr = String.format("%d ",
                                                rawDataRec.getDuration());
                                    } else {
                                        durStr = durPtr.get(0).getName() + " ";
                                    }
                                } catch (VizException e) {
                                    statusHandler
                                            .warn("Failed to query for duration ["
                                                    + rawDataRec.getDuration()
                                                    + "].", e);
                                    durStr = String.format("%d ",
                                            rawDataRec.getDuration());
                                }
                            }

                            shefDurCache.putIfAbsent(rawDataRec.getDuration(),
                                    durStr);
                        }

                        /* now finally print out the real data */
                        sb.append(String.format(
                                "  %8.2f  %s  %-16s %s     %s     %s\n",
                                rawDataRec.getValue(), timestr, durStr,
                                rawDataRec.getExtremum(),
                                rawDataRec.getShefQualCode(), qcStr));
                    }
                }

                sb.append('\n');
            }

            allRows.add(new PrecipAccumulationRowData(
                    paDataRec.getMaxValue(), paDataRec.getLid(),
                    sb.toString()));
        }
    }

    /**
     * Format and display data in the text editor.
     */
    private void displayText() {
        // write the header info to the dialog
        writeHdrInfo();

        if (allRows.isEmpty()) {
            textEditor.append("\n  REQUESTED DATA NOT FOUND.\n");
        } else {
            Comparator<PrecipAccumulationRowData> comparator = (paOptions.sortOption == SortOption.SortByLocation)
                    ? LOCATION_COMPARATOR : VALUE_COMPARATOR.reversed();
            Collections.sort(allRows, comparator);
            for (PrecipAccumulationRowData row : allRows) {
                textEditor.append(row.getData());
            }
        }
    }

    /**
     * Read the various options and load them into the object for future
     * reference.
     */
    private void readAccumOptions() {
        /*
         * get the value of all the settings and switches, starting with the end
         * time
         */
        try {
            readEndtimeStr();
        } catch (DateTimeParseException e) {
            statusHandler.warn(
                    "Could not read endtime string. Format must be YYYY-MM-DD HH:mm:ss.");
        }

        /* get the settings of the switches */
        paOptions.locSwitch = byLocationChk.getSelection();
        paOptions.detailsSwitch = showDetailsChk.getSelection();
        paOptions.ppAccumSwitch = addPPChk.getSelection();
        paOptions.hsaSwitch = byHsaChk.getSelection();
        paOptions.petsSwitch = byDataSourceChk.getSelection();

        /* get the location string */
        paOptions.lid = locationTF.getText().trim();

        /* Get the other duration. */
        paOptions.otherDuration = 0;
        if (StringUtils.isNotBlank(otherTF.getText())) {
            try {
                paOptions.otherDuration = Integer
                        .parseInt(otherTF.getText().trim());
            } catch (NumberFormatException e) {
                statusHandler.warn("Invalid duration value ["
                        + otherTF.getText().trim()
                        + "] entered. Duration value must be numbers only.");
            }
        }

        /* get the sort option */
        readSortOption();

        /*
         * Get the list of pe-ts combinations. Do this only if the filter by
         * data sources toggle button is "on".
         */
        readPETSSet();

        /* get the selected durations */
        readDurationSet();

        /* Get the selected hsa. */
        readHSASet();

        /* now dump for debug */
        statusHandler.debug(String.format(
                "switch for loc,detail, accum=%b %b %b", paOptions.locSwitch,
                paOptions.detailsSwitch, paOptions.ppAccumSwitch));
        statusHandler.debug(String.format("loc=%s; sort=%s; time= %s",
                paOptions.lid, paOptions.sortOption,
                PointPrecipConstants.DATE_TIME_FORMATTER
                        .format(paOptions.endTime)));
        statusHandler.debug("dur " + Arrays.toString(paOptions.durationSet));

        statusHandler.debug(String.format(
                "numPC PP defined/selected = %d/%d %d/%d",
                pcCtrList.getItemCount(), paOptions.getNumPcSelected(),
                ppIncList.getItemCount(), paOptions.getNumPpSelected()));
        statusHandler.debug("PCts " + Arrays.toString(paOptions.pctsSet));
        statusHandler.debug("PPts " + Arrays.toString(paOptions.pptsSet));
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

        try (BufferedWriter out = Files
                .newBufferedWriter(Paths.get(filename))) {
            out.write(textEditor.getText());
            out.close();
        } catch (IOException e) {
            statusHandler.error("Error saving file [" + filename + "].", e);
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
            // one inch from left side of paper
            leftMargin = dpi.x + trim.x;
            // one inch from right side of paper
            rightMargin = (clientArea.width - dpi.x) + trim.x + trim.width;
            // one inch from top edge of paper
            topMargin = dpi.y + trim.y;
            // one inch from bottom edge of paper
            bottomMargin = (clientArea.height - dpi.y) + trim.y + trim.height;

            /* Create a buffer for computing tab width. */
            // is tab width a user setting in your UI?
            int tabSize = 4;
            StringBuilder tabBuffer = new StringBuilder(tabSize);
            for (int i = 0; i < tabSize; i++) {
                tabBuffer.append(' ');
            }
            String tabs = tabBuffer.toString();

            /*
             * Create printer GC, and create and set the printer font &
             * foreground color.
             */
            GC gc = new GC(printer);

            Font printerFont = new Font(printer, "Monospace", 8, SWT.NORMAL);

            Color printerForegroundColor = new Color(printer, new RGB(0, 0, 0));
            Color printerBackgroundColor = new Color(printer,
                    new RGB(255, 255, 255));

            gc.setFont(printerFont);
            gc.setForeground(printerForegroundColor);
            gc.setBackground(printerBackgroundColor);
            tabWidth = gc.stringExtent(tabs).x;
            lineHeight = gc.getFontMetrics().getHeight();

            /* Print text to current gc using word wrap */
            printText(text, gc);

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
    private void printText(String text, GC gc) {
        printer.startPage();
        StringBuilder wordBuffer = new StringBuilder();
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
                        // if this is cr-lf, skip the lf
                        index++;
                    }
                    printWordBuffer(wordBuffer, gc);
                    newline();
                } else {
                    if (c != '\t') {
                        wordBuffer.append(c);
                    }
                    if (Character.isWhitespace(c)) {
                        printWordBuffer(wordBuffer, gc);
                        if (c == '\t') {
                            x += tabWidth;
                        }
                    }
                }
            }
        }
        if ((y + lineHeight) <= bottomMargin) {
            printer.endPage();
        }
    }

    /**
     * Word buffer for formating lines on the printed page
     */
    private void printWordBuffer(StringBuilder wordBuffer, GC gc) {
        if (wordBuffer.length() > 0) {
            String word = wordBuffer.toString();
            int wordWidth = gc.stringExtent(word).x;
            if ((x + wordWidth) > rightMargin) {
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
        if ((y + lineHeight) > bottomMargin) {
            printer.endPage();
            if ((index + 1) < end) {
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
