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
package com.raytheon.viz.hydrobase.dialogs;

import java.time.MonthDay;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.DateTimeParseException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Optional;
import java.util.StringJoiner;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.TypedEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrobase.data.ShefDur;
import com.raytheon.viz.hydrobase.data.ShefPE;
import com.raytheon.viz.hydrobase.data.ShefTypeSource;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.DataLimitData;
import com.raytheon.viz.hydrocommon.data.LocationDataLimitData;
import com.raytheon.viz.hydrocommon.datamanager.QcAlertAlarmLimitsDataManager;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the QC Alert and Alarm dialog.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 05, 2008            lvenable     Initial creation.
 * Dec 08, 2008  1744      askripsk     Connect to DB.
 * May 05, 2009  2181      mpduff       Keep selection upon submit.
 * Jun 16, 2010  5526      lbousaidi    Start/End date not correct
 * Oct 27, 2011  11305     lbousaidi    change some logic to have physical
 *                                      elements matches the selection of default limits
 * Apr 19, 2013  1790      rferrel      Make dialog non-blocking.
 * Nov 26, 2013  15800     wkwock       Fix unhandled event loop
 * Jan 07, 2013  16643     snaples      Fixed changeFormat to use string formatting instead of converting to Date.
 * Mar 31, 2014  #2970     lvenable     Put dispose checks in the runAsync calls.
 * Mar 10, 2017  28419     gvalenzuela  Fixed UELE when entering search text that dosn't match an existing station.
 * Jul 20, 2018  #7211     dgilling     Re-port dialog behavior from A1.
 *
 * </pre>
 *
 * @author lvenable
 */

public class QcAlertAlarmLimitsDlg extends CaveSWTDialog {

    /**
     * States for the dialog.
     */
    private static enum DataEntryMode {
        NORMAL_ENTRY, NEW_ENTRY
    };

    private static enum LimitType {
        DEFAULT_RANGES("Default Limits"),
        LOCATION_RANGES("Location Limits");

        private final String displayString;

        LimitType(String displayString) {
            this.displayString = displayString;
        }

        private String getDisplayString() {
            return displayString;
        }

        private static LimitType fromDisplayString(final String string) {
            for (LimitType type : LimitType.values()) {
                if (type.displayString.equals(string)) {
                    return type;
                }
            }

            throw new IllegalArgumentException(
                    "Invalid displayString specified for enum LimitType: "
                            + string);
        }
    };

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private static final String NOTES_TEXT = "Notes:\n"
            + "1) Individual check is not performed if the limit value is not defined.\n"
            + "2) If the limits defined for location, default limits not considered\n"
            + "   even if location limits are undefined.";

    private static final String TOP_TABLE_HEADER_TEXT = String.format(
            "                                                    %S           %S    %S"
                    + "       %S              %S",
            "Gross", "Reasonable", "Rate of", "Alert Limit", "Alarm Limit");

    private static final String BOTTOM_TABLE_HEADER_TEXT = String.format(
            "%S   %S    %S   %S  %S     %S        %S        %S     %S      %S   "
                    + "%S  %S   %S   %S  %S  %S  %S   %S   %S",
            "Location", "PE", "Dur", "TS", "Start", "End", "Min", "Max", "Min",
            "Max", "Change", "Upper", "Lower", "ROC", "Diff", "Upper", "Lower",
            "ROC", "Diff");

    private static final DateTimeFormatter DB_DATE_FORMAT = new DateTimeFormatterBuilder()
            .appendPattern("MM-dd").toFormatter();

    private static final DateTimeFormatter UI_DATE_FORMAT = new DateTimeFormatterBuilder()
            .appendPattern("MM/dd").toFormatter();

    private final Cursor waitCursor;

    private final Font controlFont;

    private Combo limitCbo;

    private Button locationChk;

    private Text locationLimitTF;

    private Button physElemChk;

    private List filterPhysElemList;

    private Button filterTsCheck;

    private List filterTsList;

    private List limitsList;

    private Text locationSelItemTF;

    private Group limitSelectedGroup;

    private Combo durationCbo;

    private Text startDateTF;

    private Text endDateTF;

    private List physElemSelItemList;

    private List selItemTsList;

    private Text grossRangeMinTF;

    private Text grossRangeMaxTF;

    private Text reasonableRangeMinTF;

    private Text reasonableRangeMaxTF;

    private Text rateOfChangeTF;

    private Text alertUpperTF;

    private Text alertLowerTF;

    private Text alertRocTF;

    private Text alertDiffTF;

    private Text alarmUpperTF;

    private Text alarmLowerTF;

    private Text alarmRocTF;

    private Text alarmDiffTF;

    private Button deleteBtn;

    private LimitType menuPos;

    private DataEntryMode dataEntryMode;

    private java.util.List<ShefDur> shefDurList;

    private java.util.List<ShefPE> shefPeList;

    private java.util.List<ShefTypeSource> shefTsList;

    private String queryOrderByPhrase;

    private String queryLocPhrase;

    private String queryPePhrase;

    private String queryTsPhrase;

    private String queryWhereOrderByPhrase;

    private int[] filterPePosList;

    private boolean isOkayToClearPePosList;

    private String queryWhereItemPhrase;

    private java.util.List<? extends DataLimitData> limitData;

    private DataLimitData workingItem;

    private boolean deleteFlag;

    private int limitsListSelectedPos;


    /**
     * @param parent
     */
    public QcAlertAlarmLimitsDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Quality Control and Alert/Alarm Limits");

        waitCursor = getDisplay().getSystemCursor(SWT.CURSOR_WAIT);

        // Initialize all of the controls and layouts
        controlFont = new Font(getDisplay(), "Monospace", 10, SWT.NORMAL);

        limitData = Collections.emptyList();
        workingItem = null;
        queryWhereItemPhrase = StringUtils.EMPTY;
        shefPeList = Collections.emptyList();
        shefDurList = Collections.emptyList();
        filterPePosList = new int[0];
        isOkayToClearPePosList = false;
        dataEntryMode = DataEntryMode.NORMAL_ENTRY;
        deleteFlag = false;
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
    protected void disposed() {
        controlFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        createLimitsGroup(shell);
        createLimitsSelectedGroup(shell);
        createBottomButtons(shell);

        createShefDurItems();

        shefPeList = getShefPEs();
        loadPeList(filterPhysElemList, shefPeList);
        loadPeList(physElemSelItemList, shefPeList);

        shefTsList = getShefTSs();
        loadTsList(filterTsList, shefTsList);
        loadTsList(selItemTsList, shefTsList);

        menuPos = LimitType.DEFAULT_RANGES;
        loadDisplay();
    }

    /**
     * Create Limits group and controls.
     */
    private Composite createLimitsGroup(Shell shell) {
        Group limitGroup = new Group(shell, SWT.NONE);
        limitGroup.setLayout(new GridLayout(1, false));
        limitGroup.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        limitGroup.setText("Limits");

        Composite topComp = new Composite(limitGroup, SWT.NONE);
        topComp.setLayout(new GridLayout(9, false));
        topComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        Label listLabel = new Label(topComp, SWT.NONE);
        listLabel.setText("List:");

        limitCbo = new Combo(topComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        for (LimitType type : LimitType.values()) {
            limitCbo.add(type.getDisplayString());
        }
        limitCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent event) {
                menuPos = LimitType.fromDisplayString(
                        limitCbo.getItem(limitCbo.getSelectionIndex()));
                loadDisplay();
            }
        });
        limitCbo.select(0);

        Label filterByLabel = new Label(topComp, SWT.RIGHT);
        filterByLabel.setText("Filter By:");
        filterByLabel
                .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true));

        locationChk = new Button(topComp, SWT.CHECK);
        locationChk.setText("Location");
        locationChk.setEnabled(false);
        locationChk.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent event) {
                handleLocationSelection(event);
                useFilter(event);
            }
        });
        GridData gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.horizontalIndent = 5;
        locationChk.setLayoutData(gd);

        locationLimitTF = new Text(topComp, SWT.BORDER);
        locationLimitTF.setEnabled(false);
        locationLimitTF.addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(ModifyEvent e) {
                useFilter(e);
            }
        });
        GC gc = new GC(locationLimitTF);
        int charWidth = gc.getFontMetrics().getAverageCharWidth();
        gc.dispose();
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.widthHint = charWidth * 12;
        locationLimitTF.setLayoutData(gd);

        physElemChk = new Button(topComp, SWT.CHECK);
        physElemChk.setText("PhysElem");
        physElemChk.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent event) {
                handlePeSelection(event);
                useFilter(event);
            }
        });
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.horizontalIndent = 5;
        physElemChk.setLayoutData(gd);

        filterPhysElemList = new List(topComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        filterPhysElemList.setFont(controlFont);
        filterPhysElemList.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent event) {
                useFilter(event);
            }
        });
        gc = new GC(filterPhysElemList);
        gc.setFont(controlFont);
        charWidth = gc.getFontMetrics().getAverageCharWidth();
        gc.dispose();
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        Rectangle trim = filterPhysElemList.computeTrim(0, 0, charWidth * 25,
                filterPhysElemList.getItemHeight() * 6);
        gd.heightHint = trim.height;
        gd.verticalSpan = 2;
        filterPhysElemList.setLayoutData(gd);

        filterTsCheck = new Button(topComp, SWT.CHECK);
        filterTsCheck.setText("Type Source:");
        filterTsCheck.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                handleTsSelection(e);
                useFilter(e);
            }
        });
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.horizontalIndent = 5;
        filterTsCheck.setLayoutData(gd);

        filterTsList = new List(topComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        filterTsList.setEnabled(false);
        filterTsList.setFont(controlFont);
        filterTsList.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                useFilter(e);
            }
        });
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        gd.heightHint = trim.height;
        gd.verticalSpan = 2;
        filterTsList.setLayoutData(gd);

        Label notesLbl = new Label(topComp, SWT.NONE);
        notesLbl.setText(NOTES_TEXT);
        notesLbl.setFont(controlFont);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, true);
        gd.horizontalSpan = 6;
        notesLbl.setLayoutData(gd);

        Label topListLbl = new Label(limitGroup, SWT.NONE);
        topListLbl.setText(TOP_TABLE_HEADER_TEXT);
        topListLbl.setFont(controlFont);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        gd.horizontalIndent = 4;
        topListLbl.setLayoutData(gd);

        Label bottomListLbl = new Label(limitGroup, SWT.NONE);
        bottomListLbl.setText(BOTTOM_TABLE_HEADER_TEXT);
        bottomListLbl.setFont(controlFont);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        gd.horizontalIndent = 4;
        bottomListLbl.setLayoutData(gd);

        limitsList = new List(limitGroup,
                SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        limitsList.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent event) {
                setItemData(event);
            }
        });
        limitsList.setFont(controlFont);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        trim = limitsList.computeTrim(0, 0, charWidth * 135,
                limitsList.getItemHeight() * 6);
        gd.heightHint = trim.height;
        limitsList.setLayoutData(gd);

        return limitGroup;
    }

    /**
     * Create Limits Selected group and controls.
     */
    private Composite createLimitsSelectedGroup(Shell shell) {
        limitSelectedGroup = new Group(shell, SWT.NONE);
        limitSelectedGroup.setText("Limits For Selected Item");
        limitSelectedGroup.setLayout(new GridLayout(4, false));
        limitSelectedGroup.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        // -----------------------------------------------------
        // Create the location, duration, and date controls
        // -----------------------------------------------------
        Composite leftComp = new Composite(limitSelectedGroup, SWT.NONE);
        leftComp.setLayout(new GridLayout(2, false));
        leftComp.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, true));

        Label locationLbl = new Label(leftComp, SWT.RIGHT);
        locationLbl.setText("Location:");
        locationLbl
                .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, true));

        locationSelItemTF = new Text(leftComp, SWT.BORDER);
        locationSelItemTF.setTextLimit(8);
        GC gc = new GC(locationLimitTF);
        int propCharWidth = gc.getFontMetrics().getAverageCharWidth();
        gc.dispose();
        GridData gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = propCharWidth * 12;
        locationSelItemTF.setLayoutData(gd);

        Label durationLbl = new Label(leftComp, SWT.RIGHT);
        durationLbl.setText("Duration:");
        durationLbl
                .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, true));

        durationCbo = new Combo(leftComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        durationCbo.setLayoutData(
                new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false));

        Label startDateLbl = new Label(leftComp, SWT.RIGHT);
        startDateLbl.setText("Start MM/DD:");
        startDateLbl
                .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, true));

        startDateTF = new Text(leftComp, SWT.BORDER);
        startDateTF.setTextLimit(5);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = propCharWidth * 12;
        startDateTF.setLayoutData(gd);

        Label endDateLbl = new Label(leftComp, SWT.RIGHT);
        endDateLbl.setText("End MM/DD:");
        endDateLbl
                .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, true));

        endDateTF = new Text(leftComp, SWT.BORDER);
        endDateTF.setTextLimit(5);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = propCharWidth * 12;
        endDateTF.setLayoutData(gd);

        // -----------------------------------------------------
        // Create the physical element controls
        // -----------------------------------------------------
        Composite centerComp = new Composite(limitSelectedGroup, SWT.NONE);
        centerComp.setLayout(new GridLayout(1, false));
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalAlignment = SWT.CENTER;
        centerComp.setLayoutData(gd);

        Label peLbl = new Label(centerComp, SWT.NONE);
        peLbl.setText("Physical Element:");

        physElemSelItemList = new List(centerComp,
                SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        physElemSelItemList.setFont(controlFont);
        gc = new GC(physElemSelItemList);
        gc.setFont(controlFont);
        int monoCharWidth = gc.getFontMetrics().getAverageCharWidth();
        gc.dispose();
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        Rectangle trim = physElemSelItemList.computeTrim(0, 0,
                monoCharWidth * 25, physElemSelItemList.getItemHeight() * 10);
        gd.heightHint = trim.height;
        physElemSelItemList.setLayoutData(gd);

        // -----------------------------------------------------
        // Create the type source controls
        // -----------------------------------------------------
        Composite tsComp = new Composite(limitSelectedGroup, SWT.NONE);
        tsComp.setLayout(new GridLayout(1, false));
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalAlignment = SWT.CENTER;
        tsComp.setLayoutData(gd);

        Label tsLbl = new Label(tsComp, SWT.NONE);
        tsLbl.setText("Type Source:");

        selItemTsList = new List(tsComp,
                SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        selItemTsList.setFont(controlFont);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.heightHint = trim.height;
        selItemTsList.setLayoutData(gd);

        // -----------------------------------------------------
        // Create the QC and Alarm/Alert controls
        // -----------------------------------------------------
        Composite rightComp = new Composite(limitSelectedGroup, SWT.NONE);
        rightComp.setLayout(new GridLayout(1, false));

        Group qcGroup = new Group(rightComp, SWT.NONE);
        qcGroup.setLayout(new GridLayout(3, false));
        qcGroup.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        qcGroup.setText("Quality Control Limits");

        // Filler Label
        new Label(qcGroup, SWT.NONE);

        Label minLbl = new Label(qcGroup, SWT.CENTER);
        minLbl.setText("Min");
        minLbl.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, false, false));

        Label maxLbl = new Label(qcGroup, SWT.CENTER);
        maxLbl.setText("Max");
        maxLbl.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, false, false));

        Label grossRangeLbl = new Label(qcGroup, SWT.RIGHT);
        grossRangeLbl.setText("Gross Range:");
        grossRangeLbl
                .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, true));

        grossRangeMinTF = new Text(qcGroup, SWT.BORDER);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = propCharWidth * 15;
        grossRangeMinTF.setLayoutData(gd);

        grossRangeMaxTF = new Text(qcGroup, SWT.BORDER);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = propCharWidth * 15;
        grossRangeMaxTF.setLayoutData(gd);

        Label reasonableLbl = new Label(qcGroup, SWT.RIGHT);
        reasonableLbl.setText("Reasonable Range:");
        reasonableLbl
                .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, true));

        reasonableRangeMinTF = new Text(qcGroup, SWT.BORDER);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = propCharWidth * 15;
        reasonableRangeMinTF.setLayoutData(gd);

        reasonableRangeMaxTF = new Text(qcGroup, SWT.BORDER);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = propCharWidth * 15;
        reasonableRangeMaxTF.setLayoutData(gd);

        Label rateOfChangeLbl = new Label(qcGroup, SWT.RIGHT);
        rateOfChangeLbl.setText("Rate of Change:");
        rateOfChangeLbl
                .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, true));

        rateOfChangeTF = new Text(qcGroup, SWT.BORDER);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = propCharWidth * 15;
        rateOfChangeTF.setLayoutData(gd);

        Label unitHourLbl = new Label(qcGroup, SWT.RIGHT);
        unitHourLbl.setText("Units/Hour");

        Group aaGroup = new Group(rightComp, SWT.NONE);
        aaGroup.setText("Alert/Alarm Limits");
        aaGroup.setLayout(new GridLayout(5, false));
        aaGroup.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        // Filler Label
        new Label(aaGroup, SWT.NONE);

        Label upperLbl = new Label(aaGroup, SWT.CENTER);
        upperLbl.setText("Upper");
        upperLbl.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, false, false));

        Label lowerLbl = new Label(aaGroup, SWT.CENTER);
        lowerLbl.setText("Lower");
        lowerLbl.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, false, false));

        Label rocLbl = new Label(aaGroup, SWT.CENTER);
        rocLbl.setText("ROC");
        rocLbl.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, false, false));

        Label diffLbl = new Label(aaGroup, SWT.CENTER);
        diffLbl.setText("Diff");
        diffLbl.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, false, false));

        Label alertLbl = new Label(aaGroup, SWT.RIGHT);
        alertLbl.setText("Alert:");
        alertLbl.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, true));

        alertUpperTF = new Text(aaGroup, SWT.BORDER);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = propCharWidth * 10;
        alertUpperTF.setLayoutData(gd);

        alertLowerTF = new Text(aaGroup, SWT.BORDER);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = propCharWidth * 10;
        alertLowerTF.setLayoutData(gd);

        alertRocTF = new Text(aaGroup, SWT.BORDER);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = propCharWidth * 10;
        alertRocTF.setLayoutData(gd);

        alertDiffTF = new Text(aaGroup, SWT.BORDER);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = propCharWidth * 10;
        alertDiffTF.setLayoutData(gd);

        Label alarmLbl = new Label(aaGroup, SWT.RIGHT);
        alarmLbl.setText("Alarm:");
        alarmLbl.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, true));

        alarmUpperTF = new Text(aaGroup, SWT.BORDER);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = propCharWidth * 10;
        alarmUpperTF.setLayoutData(gd);

        alarmLowerTF = new Text(aaGroup, SWT.BORDER);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = propCharWidth * 10;
        alarmLowerTF.setLayoutData(gd);

        alarmRocTF = new Text(aaGroup, SWT.BORDER);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = propCharWidth * 10;
        alarmRocTF.setLayoutData(gd);

        alarmDiffTF = new Text(aaGroup, SWT.BORDER);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = propCharWidth * 10;
        alarmDiffTF.setLayoutData(gd);

        return limitSelectedGroup;
    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private Composite createBottomButtons(Shell shell) {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(5, true));
        buttonComp.setLayoutData(
                new GridData(SWT.CENTER, SWT.DEFAULT, false, false));

        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent event) {
                if (handleApplyClick(event)) {
                    close();
                }
            }
        });
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        okBtn.setLayoutData(gd);

        Button applyBtn = new Button(buttonComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent event) {
                handleApplyClick(event);
            }
        });
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        applyBtn.setLayoutData(gd);

        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        cancelBtn.setLayoutData(gd);

        Button newBtn = new Button(buttonComp, SWT.PUSH);
        newBtn.setText("New");
        newBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent event) {
                handleNewClick(event);
            }
        });
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        newBtn.setLayoutData(gd);

        deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent event) {
                handleDeleteClick(event);
            }
        });
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        deleteBtn.setLayoutData(gd);

        return buttonComp;
    }

    private void loadDisplay() {
        if (menuPos == LimitType.DEFAULT_RANGES) {
            queryOrderByPhrase = " ORDER BY pe, dur, monthdaystart, "
                    + "monthdayend, gross_range_min, gross_range_max ";

            setLocSensitivity(false);
            handlePeSelection(null);
        } else if (menuPos == LimitType.LOCATION_RANGES) {
            queryOrderByPhrase = " ORDER BY lid, pe, dur, ts, monthdaystart, "
                    + "monthdayend, gross_range_min, gross_range_max ";

            setLocSensitivity(true);
            handleLocationSelection(null);
            handlePeSelection(null);
            handleTsSelection(null);
        }

        // determine what data to load
        filterGrabLocPeTs();
        loadDataIntoDisplay(menuPos);
    }

    private void loadDataIntoDisplay(LimitType menuPos) {
        filterBuildPhrases(queryLocPhrase, queryPePhrase, queryTsPhrase,
                queryOrderByPhrase);

        limitsList.removeAll();
        clearForm();

        queryWhereItemPhrase = StringUtils.EMPTY;
        limitData = Collections.emptyList();

        if (LimitType.DEFAULT_RANGES == menuPos) {
            try {
                limitData = QcAlertAlarmLimitsDataManager
                        .getDataLimits(queryWhereOrderByPhrase);
            } catch (VizException e) {
                statusHandler.error("Unable to retrieve default limit data", e);
            }
            loadDefaultOrLocation();
        } else if (LimitType.LOCATION_RANGES == menuPos) {
            try {
                limitData = QcAlertAlarmLimitsDataManager
                        .getLocDataLimits(queryWhereOrderByPhrase);
            } catch (VizException e) {
                statusHandler.error("Unable to retrieve location limit data",
                        e);
            }
            loadDefaultOrLocation();
        }
    }

    private void createShefDurItems() {
        try {
            shefDurList = ShefDur.getShefDur(" ORDER BY dur ");
            String[] listItems = shefDurList.stream().map(
                    s -> String.format("%s (%d)", s.getName(), s.getDuration()))
                    .toArray(String[]::new);
            durationCbo.setItems(listItems);
        } catch (VizException e) {
            statusHandler.error("GetShefDur() failed to retrieve data.", e);
        }
    }

    private java.util.List<ShefPE> getShefPEs() {
        try {
            java.util.List<ShefPE> shefpe = ShefPE.getShefPe(" ORDER BY pe ");
            return shefpe;
        } catch (VizException e) {
            statusHandler.error("GetShefPe() failed to retrieve data.", e);
        }

        return Collections.emptyList();
    }

    private java.util.List<ShefTypeSource> getShefTSs() {
        try {
            java.util.List<ShefTypeSource> shefts = ShefTypeSource
                    .getShefTs(" ORDER BY ts ");
            return shefts;
        } catch (VizException e) {
            statusHandler.error("GetShefTS() failed to retrieve data.", e);
        }

        return Collections.emptyList();
    }

    private void loadPeList(List peLI, Collection<ShefPE> shefPeList) {
        String[] items = shefPeList.stream()
                .map(pe -> String.format("%s %-20s", pe.getPe(), pe.getName()))
                .toArray(String[]::new);
        peLI.setItems(items);
    }

    private void loadTsList(List tsLI, Collection<ShefTypeSource> shefTsList) {
        String[] items = shefTsList.stream()
                .map(ts -> String.format("%s", ts.getCode()))
                .toArray(String[]::new);
        tsLI.setItems(items);
    }

    private void recomputeLimitsListSelection() {
        int selPos = limitsListSelectedPos;

        if (selPos == (limitsList.getItemCount() - 1)) {
            selPos -= 1;
        }

        if (selPos < 0) {
            selPos = 0;
        }

        limitsListSelectedPos = selPos;
        deleteFlag = true;
    }

    private boolean handleApplyClick(SelectionEvent event) {
        Optional<? extends DataLimitData> item = getItemData();
        boolean okay = item.isPresent();
        if (!okay) {
            return false;
        }

        boolean error = applyItemData(item.get());
        if (!error) {
            if (DataEntryMode.NEW_ENTRY == dataEntryMode) {
                clearForm();

                if (limitsListSelectedPos > 0) {
                    limitsList.setSelection(limitsListSelectedPos);
                    limitsList.showSelection();
                }

                limitSelectedGroup.setText("Selected Item");
                dataEntryMode = DataEntryMode.NORMAL_ENTRY;
            }

            loadDataIntoDisplay(menuPos);

            return true;
        } else {
            MessageDialog.openError(getShell(), "Database Error",
                    "Failed to save record to database. See AlertViz for more information.");
        }

        return false;
    }

    private boolean applyItemData(DataLimitData data) {
        if (DataEntryMode.NEW_ENTRY == dataEntryMode) {
            try {
                QcAlertAlarmLimitsDataManager.putDataLimits(data);
            } catch (VizException e) {
                statusHandler
                        .error("PutDataLimits() failed to save new record.",
                        e);
                return true;
            }
        } else if (DataEntryMode.NORMAL_ENTRY == dataEntryMode) {
            try {
                QcAlertAlarmLimitsDataManager.updateDataLimits(data,
                        queryWhereItemPhrase);
            } catch (VizException e) {
                statusHandler
                        .error("UpdateDataLimits() failed to save new record.",
                                e);
                return true;
            }
        }

        return false;
    }

    private void handleNewClick(SelectionEvent event) {
        limitsList.deselectAll();

        if (DataEntryMode.NORMAL_ENTRY == dataEntryMode) {
            clearForm();
            limitSelectedGroup.setText("NEW Item");

            durationCbo.select(0);
            physElemSelItemList.select(0);
            physElemSelItemList.showSelection();
            physElemSelItemList.deselectAll();

            selItemTsList.select(0);
            selItemTsList.showSelection();
            selItemTsList.deselectAll();

            dataEntryMode = DataEntryMode.NEW_ENTRY;
        }

        setDataSensitivity(true);
    }

    private void handleDeleteClick(SelectionEvent event) {
        getShell().setCursor(waitCursor);

        int selectedPos = limitsList.getSelectionIndex();
        if (selectedPos > -1) {
            DataLimitData selectedData = limitData.get(selectedPos);

            String where = buildSelectedItemWhere(selectedData);

            boolean error = true;
            try {
                QcAlertAlarmLimitsDataManager.deleteDataLimits(selectedData,
                        where);
                error = false;
            } catch (VizException e) {
                statusHandler.error(
                        "DeleteDataLimits() failed to delete record.", e);
                error = true;
            }

            if (!error) {
                recomputeLimitsListSelection();
            } else {
                getShell().setCursor(null);
                MessageDialog.openError(getShell(), "Database Error",
                        "Failed to delete record from database. See AlertViz for more information.");
            }
        }

        filterGrabLocPeTs();
        loadDataIntoDisplay(menuPos);

        getShell().setCursor(null);
    }

    private void handleLocationSelection(SelectionEvent event) {
        if (locationChk.getSelection()) {
            locationLimitTF.setEnabled(true);
        } else {
            locationLimitTF.setEnabled(false);
        }
    }

    private void handlePeSelection(SelectionEvent event) {
        if (physElemChk.getSelection()) {
            filterPhysElemList.setEnabled(true);
        } else {
            filterPhysElemList.setEnabled(false);
        }
    }

    private void handleTsSelection(SelectionEvent event) {
        if (filterTsCheck.getSelection()) {
            filterTsList.setEnabled(true);
        } else {
            filterTsList.setEnabled(false);
        }
    }

    private void setLocSensitivity(boolean set) {
        if (set) {
            locationChk.setEnabled(true);
            locationLimitTF.setEnabled(true);
            locationSelItemTF.setEnabled(true);
            filterTsCheck.setEnabled(true);
            filterTsList.setEnabled(true);
            selItemTsList.setEnabled(true);
        } else {
            locationChk.setEnabled(false);
            locationChk.setSelection(false);
            locationLimitTF.setEnabled(false);
            locationSelItemTF.setEnabled(false);
            filterTsList.setEnabled(false);
            filterTsCheck.setEnabled(false);
            filterTsCheck.setSelection(false);
            selItemTsList.setEnabled(false);
            locationLimitTF.setText(StringUtils.EMPTY);
            locationSelItemTF.setText(StringUtils.EMPTY);
            filterTsList.deselectAll();
        }
    }

    private void setDataSensitivity(boolean set) {
        if (set) {
            limitSelectedGroup.setEnabled(true);
            physElemSelItemList.setEnabled(true);

            if (DataEntryMode.NORMAL_ENTRY == dataEntryMode) {
                deleteBtn.setEnabled(true);
            } else {
                deleteBtn.setEnabled(false);
            }
        } else {
            limitSelectedGroup.setEnabled(false);
            physElemSelItemList.setEnabled(false);
            deleteBtn.setEnabled(false);
        }
    }

    private void useFilter(TypedEvent event) {
        if (physElemChk == event.widget) {
            if (physElemChk.getSelection()) {
                filterPhysElemList.select(filterPePosList);
                isOkayToClearPePosList = true;
            } else {
                filterPhysElemList.deselectAll();
                isOkayToClearPePosList = false;
            }
        }

        // Always filter on Loc & Pe.
        // Load the display with data.
        filterGrabLocPeTs();
        loadDataIntoDisplay(menuPos);
    }

    private void filterGrabLocPeTs() {
        queryLocPhrase = StringUtils.EMPTY;
        if (locationChk.getSelection()) {
            queryLocPhrase = obtainLocPhrase();
        }

        queryPePhrase = StringUtils.EMPTY;
        if (physElemChk.getSelection()) {
            queryPePhrase = obtainPePhrase();
        }

        queryTsPhrase = StringUtils.EMPTY;
        if (filterTsCheck.getSelection()) {
            queryTsPhrase = obtainTsPhrase();
        }
    }

    private void filterBuildPhrases(String locPhrase, String pePhrase,
            String tsPhrase, String orderByPhrase) {
        queryWhereOrderByPhrase = StringUtils.EMPTY;

        StringJoiner queryBuilder = new StringJoiner(") AND (", " WHERE (",
                ") ");
        queryBuilder.setEmptyValue(StringUtils.EMPTY);
        for (String phrase : new String[] { locPhrase, pePhrase, tsPhrase }) {
            if (StringUtils.isNotBlank(phrase)) {
                queryBuilder.add(phrase);
            }
        }
        String queryWherePhrase = queryBuilder.toString();

        if (!queryWherePhrase.isEmpty()) {
            queryWhereOrderByPhrase = queryWherePhrase;
        }

        queryWhereOrderByPhrase += orderByPhrase;
    }

    private String obtainLocPhrase() {
        String text = locationLimitTF.getText();
        return String.format("lid = '%s'", text);
    }

    private String obtainPePhrase() {
        if (isOkayToClearPePosList && filterPePosList.length > 0) {
            filterPePosList = new int[0];
        }

        int[] selectedIdxs = filterPhysElemList.getSelectionIndices();
        if (selectedIdxs.length > 0) {
            filterPePosList = selectedIdxs;
        }

        if (selectedIdxs.length == 0) {
            return "pe in (NULL)";
        }

        String pePhrase = Arrays.stream(selectedIdxs)
                .mapToObj(i -> shefPeList.get(i).getPe())
                .collect(Collectors.joining("','", "pe in ('", "')"));
        return pePhrase;
    }

    private String obtainTsPhrase() {
        int[] selectedIdxs = filterTsList.getSelectionIndices();

        if (selectedIdxs.length == 0) {
            return "ts in (NULL)";
        }

        String tsPhrase = Arrays.stream(selectedIdxs)
                .mapToObj(i -> shefTsList.get(i).getCode())
                .collect(Collectors.joining("','", "ts in ('", "')"));
        return tsPhrase;
    }

    private Optional<? extends DataLimitData> getItemData() {
        DataLimitData item = (LimitType.DEFAULT_RANGES == menuPos)
                ? new DataLimitData() : new LocationDataLimitData();

        if (item instanceof LocationDataLimitData) {
            LocationDataLimitData casted = (LocationDataLimitData) item;
            casted.setLid(locationSelItemTF.getText());

            int idx = selItemTsList.getSelectionIndex();
            if (idx >= 0) {
                casted.setTs(shefTsList.get(idx).getCode());
            } else {
                MessageDialog.openError(getShell(), "Select a Type Source",
                        "You must select a Type Source.");
                return Optional.empty();
            }
        }

        int idx = durationCbo.getSelectionIndex();
        item.setDur(shefDurList.get(idx).getDuration());

        idx = physElemSelItemList.getSelectionIndex();
        if (idx >= 0) {
            item.setPe(shefPeList.get(idx).getPe());
        } else {
            MessageDialog.openError(getShell(), "Select a Physical Element",
                    "You must select a Physical Element.");
            return Optional.empty();
        }

        item.setGrossRangeMin(getDoubleValue(grossRangeMinTF.getText()));
        item.setGrossRangeMax(getDoubleValue(grossRangeMaxTF.getText()));

        if (item.getGrossRangeMax() <= item.getGrossRangeMin()) {
            MessageDialog.openError(getShell(),
                    "Enter Valid Gross Range Values",
                    "The min value must be < the max value.\n"
                            + "Please make the required changes and re-Apply them.");
            return Optional.empty();
        }

        item.setReasonRangeMin(getDoubleValue(reasonableRangeMinTF.getText()));

        item.setReasonRangeMax(getDoubleValue(reasonableRangeMaxTF.getText()));

        item.setRocMax(getDoubleValue(rateOfChangeTF.getText()));

        item.setAlertUpperLimit(getDoubleValue(alertUpperTF.getText()));

        item.setAlertLowerLimit(getDoubleValue(alertLowerTF.getText()));

        item.setAlarmUpperLimit(getDoubleValue(alarmUpperTF.getText()));

        item.setAlarmLowerLimit(getDoubleValue(alarmLowerTF.getText()));

        item.setAlertDiffLimit(getDoubleValue(alertDiffTF.getText()));

        item.setAlarmDiffLimit(getDoubleValue(alarmDiffTF.getText()));

        item.setAlertRocLimit(getDoubleValue(alertRocTF.getText()));

        item.setAlarmRocLimit(getDoubleValue(alarmRocTF.getText()));

        MonthDay startDate = MonthDay.of(12, 31);
        try {
            startDate = MonthDay.parse(startDateTF.getText(), UI_DATE_FORMAT);
        } catch (DateTimeParseException e) {
            MessageDialog.openError(getShell(), "Invalid Start Date",
                    "Invalid start date...Please try again.");
            return Optional.empty();
        }

        MonthDay endDate = MonthDay.of(1, 1);
        try {
            endDate = MonthDay.parse(endDateTF.getText(), UI_DATE_FORMAT);
        } catch (DateTimeParseException e) {
            MessageDialog.openError(getShell(), "Invalid End Date",
                    "Invalid end date...Please try again.");
            return Optional.empty();
        }

        if (endDate.isBefore(startDate)) {
            MessageDialog.openError(getShell(), "Invalid Date Range",
                    "The end date must be >= the start date, up to\n"
                            + "a maximum end date of 12/31.  Please make the\n"
                            + "required changes and re-Apply them.");
            return Optional.empty();
        }

        item.setMonthDayStart(DB_DATE_FORMAT.format(startDate));
        item.setMonthDayEnd(DB_DATE_FORMAT.format(endDate));

        workingItem = item;
        return Optional.of(item);
    }

    private double getDoubleValue(String text) {
        if (StringUtils.isNotBlank(text)) {
            try{
                return Double.parseDouble(text);
            } catch (NumberFormatException e) {
                statusHandler.debug("Invalid double value entered: " + text);
            }
        } else {
            return HydroConstants.MISSING_VALUE;
        }

        return 0;
    }

    private void setItemData(SelectionEvent event) {
        dataEntryMode = DataEntryMode.NORMAL_ENTRY;
        limitSelectedGroup.setText("Limits For Selected Item");

        int selectedIdx = limitsList.getSelectionIndex();

        if (selectedIdx >= 0) {
            deleteBtn.setEnabled(true);

            limitsListSelectedPos = selectedIdx;

            DataLimitData selectedData = limitData.get(selectedIdx);

            if (selectedData instanceof LocationDataLimitData) {
                LocationDataLimitData casted = (LocationDataLimitData) selectedData;
                locationSelItemTF.setText(casted.getLid());

                for (int i = 0; i < shefTsList.size(); i++) {
                    if (casted.getTs().equals(shefTsList.get(i).getCode())) {
                        selItemTsList.setSelection(i);
                        selItemTsList.showSelection();
                        break;
                    }
                }
            }

            for (int i = 0; i < shefDurList.size(); i++) {
                if (selectedData.getDur() == shefDurList.get(i).getDuration()) {
                    durationCbo.select(i);
                    break;
                }
            }

            String start = selectedData.getMonthDayStart().replace('-', '/');
            startDateTF.setText(start);

            String end = selectedData.getMonthDayEnd().replace('-', '/');
            endDateTF.setText(end);

            for (int i = 0; i < shefPeList.size(); i++) {
                if (selectedData.getPe().equals(shefPeList.get(i).getPe())) {
                    physElemSelItemList.setSelection(i);
                    physElemSelItemList.showSelection();
                    break;
                }
            }

            grossRangeMinTF.setText(formatValue(selectedData.getGrossRangeMin(),
                    "%.2f", StringUtils.EMPTY));

            grossRangeMaxTF.setText(formatValue(selectedData.getGrossRangeMax(),
                    "%.2f", StringUtils.EMPTY));

            reasonableRangeMinTF
                    .setText(formatValue(selectedData.getReasonRangeMin(),
                            "%.2f", StringUtils.EMPTY));

            reasonableRangeMaxTF
                    .setText(formatValue(selectedData.getReasonRangeMax(),
                            "%.2f", StringUtils.EMPTY));

            rateOfChangeTF.setText(formatValue(selectedData.getRocMax(), "%.2f",
                    StringUtils.EMPTY));

            alertUpperTF.setText(formatValue(selectedData.getAlertUpperLimit(),
                    "%.2f", StringUtils.EMPTY));

            alarmUpperTF.setText(formatValue(selectedData.getAlarmUpperLimit(),
                    "%.2f", StringUtils.EMPTY));

            alertLowerTF.setText(formatValue(selectedData.getAlertLowerLimit(),
                    "%.2f", StringUtils.EMPTY));

            alarmLowerTF.setText(formatValue(selectedData.getAlarmLowerLimit(),
                    "%.2f", StringUtils.EMPTY));

            alertDiffTF.setText(formatValue(selectedData.getAlertDiffLimit(),
                    "%.2f", StringUtils.EMPTY));

            alarmDiffTF.setText(formatValue(selectedData.getAlarmDiffLimit(),
                    "%.2f", StringUtils.EMPTY));

            alertRocTF.setText(formatValue(selectedData.getAlarmRocLimit(),
                    "%.2f", StringUtils.EMPTY));

            alarmRocTF.setText(formatValue(selectedData.getAlarmRocLimit(),
                    "%.2f", StringUtils.EMPTY));

            queryWhereItemPhrase = buildSelectedItemWhere(selectedData);
        }
    }

    private String buildSelectedItemWhere(DataLimitData selection) {
        StringBuilder where = new StringBuilder(" WHERE ");

        if (selection instanceof LocationDataLimitData) {
            where.append(
                    String.format("(lid = '%s') AND ",
                            ((LocationDataLimitData) selection).getLid()));
            where.append(String.format("(ts = '%s') AND ",
                    ((LocationDataLimitData) selection).getTs()));
        }

        where.append(String.format(
                "(pe = '%s') AND (dur = %d) AND (monthdaystart = '%s')",
                selection.getPe(), selection.getDur(),
                selection.getMonthDayStart()));

        return where.toString();
    }

    private void loadDefaultOrLocation() {
        setDataSensitivity(!limitData.isEmpty());

        String[] tableItems = limitData.stream()
                .map(item -> decodeDataLimitString(item))
                .toArray(String[]::new);

        limitsList.setItems(tableItems);

        if (deleteFlag) {
            int pos = limitsListSelectedPos;
            deleteFlag = false;
            limitsList.setSelection(pos);
            limitsList.showSelection();
        } else {
            if (workingItem != null) {
                selectNewItem(workingItem);
            } else {
                limitsList.select(0);
                limitsList.showSelection();
            }
        }

        setItemData(null);
    }

    private void selectNewItem(DataLimitData data) {
        String item = decodeDataLimitString(data);

        int pos = limitsList.indexOf(item);
        if (pos > -1) {
            limitsList.select(pos);
        } else {
            limitsList.select(0);
        }
        limitsList.showSelection();
    }

    private String decodeDataLimitString(DataLimitData data) {
        StringBuilder buf = new StringBuilder();

        if (data instanceof LocationDataLimitData) {
            buf.append(String.format("%-10s%3s%7s%5s%7s%8s  ",
                    ((LocationDataLimitData) data).getLid(),
                    data.getPe(), data.getDur(),
                    ((LocationDataLimitData) data).getTs(),
                    data.getMonthDayStart(),
                    data.getMonthDayEnd()));
        } else {
            buf.append(String.format("%13s%7s%5s%7s%8s  ", data.getPe(),
                    data.getDur(), StringUtils.EMPTY, data.getMonthDayStart(),
                    data.getMonthDayEnd()));
        }

        // Gross Min/Max
        buf.append(HydroDataUtils.getDisplayString("%9s", "%9.1f",
                data.getGrossRangeMin()));
        buf.append(HydroDataUtils.getDisplayString("%11s", "%10.1f",
                data.getGrossRangeMax()));

        // Reason Min/Max
        buf.append(HydroDataUtils.getDisplayString("%8s", "%8.1f",
                data.getReasonRangeMin()));
        buf.append(HydroDataUtils.getDisplayString("%9s", "%8.1f",
                data.getReasonRangeMax()));

        // ROC
        buf.append(HydroDataUtils.getDisplayString("%7s", "%6.1f",
                data.getRocMax()));

        // ALERT LIMIT
        buf.append(HydroDataUtils.getDisplayString("%8s", "%7.1f",
                data.getAlertUpperLimit()));
        buf.append(HydroDataUtils.getDisplayString("%7s", "%7.1f",
                data.getAlertLowerLimit()));

        // Alert ROC Limit
        buf.append(HydroDataUtils.getDisplayString("%8s", "%6.1f",
                data.getAlertRocLimit()));

        // Alert Diff Limit
        buf.append(HydroDataUtils.getDisplayString("%5s", "%5.1f",
                data.getAlertDiffLimit()));

        // Alert Upper Limit
        buf.append(HydroDataUtils.getDisplayString("%7s", "%6.1f",
                data.getAlarmUpperLimit()));

        // Alarm Lower Limit
        buf.append(HydroDataUtils.getDisplayString("%7s", "%5.1f",
                data.getAlarmLowerLimit()));

        // Alarm ROC limit
        buf.append(HydroDataUtils.getDisplayString("%7s", "%6.1f",
                data.getAlarmRocLimit()));

        // Alarm Diff Limit
        buf.append(HydroDataUtils.getDisplayString("%7s", "%6.1f",
                data.getAlarmDiffLimit()));

        return buf.toString();
    }

    private void clearForm() {
        locationSelItemTF.setText(StringUtils.EMPTY);
        durationCbo.clearSelection();
        startDateTF.setText(StringUtils.EMPTY);
        endDateTF.setText(StringUtils.EMPTY);
        physElemSelItemList.deselectAll();
        selItemTsList.deselectAll();
        grossRangeMinTF.setText(StringUtils.EMPTY);
        grossRangeMaxTF.setText(StringUtils.EMPTY);
        reasonableRangeMinTF.setText(StringUtils.EMPTY);
        reasonableRangeMaxTF.setText(StringUtils.EMPTY);
        rateOfChangeTF.setText(StringUtils.EMPTY);
        alertUpperTF.setText(StringUtils.EMPTY);
        alertLowerTF.setText(StringUtils.EMPTY);
        alertRocTF.setText(StringUtils.EMPTY);
        alertDiffTF.setText(StringUtils.EMPTY);
        alarmUpperTF.setText(StringUtils.EMPTY);
        alarmLowerTF.setText(StringUtils.EMPTY);
        alarmRocTF.setText(StringUtils.EMPTY);
        alarmDiffTF.setText(StringUtils.EMPTY);
    }

    private String formatValue(double value, String formatString,
            String defaultValue) {
        if (value != HydroConstants.MISSING_VALUE) {
            return String.format(formatString, value);
        }

        return defaultValue;
    }
}
