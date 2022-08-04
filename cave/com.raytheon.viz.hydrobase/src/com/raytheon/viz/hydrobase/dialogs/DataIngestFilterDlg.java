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

import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.DataIngestFilterData;
import com.raytheon.viz.hydrocommon.datamanager.DataIngestFilterDataManager;
import com.raytheon.viz.hydrocommon.datamanager.DataTrashCanDataManager;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.hydrocommon.datamanager.LocationDataManager;
import com.raytheon.viz.hydrocommon.util.StnClassSyncUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displayed the Data Ingest Filter dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Sep 4,  2008             lvenable    Initial creation
 * Dec 11, 2008 1787        askripsk    Connect to DB
 * Apr 18, 2013 1790        rferrel     Make dialog non-blocking.
 * Mar 31, 2014 #2970       lvenable    Put dispose checks in the runAsync calls.
 * May 1,  2014 17096       xwei        By default the first item of the data 
 *                                      list is selected
 * Feb 16, 2016 5354        bkowal      Only update the stnclass table after an ingest
 *                                      filter removal if there is an associated location.                                     
 * Feb 17, 2016 14607       amoore      Add WFO filter
 * Oct 6,  2017 19998       jdeng       Adjust ingest list control window
 * Jan 03, 2018  6806       mduff       Optimized filter action handlers.
 * Aug 9, 2018  19998       jdeng       Adjust ingest List control window to line up labels with List data
 * 
 * </pre>
 * 
 * @author lvenable
 */
public class DataIngestFilterDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataIngestFilterDlg.class);

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Ingest data list control.
     */
    private List ingestDataList;

    /**
     * WFO filter list control.
     */
    private List wfoFilterList;

    /**
     * Physical element filter list control.
     */
    private List physElemFilterList;

    /**
     * Physical element selected list control.
     */
    private List physElemSelectedList;

    /**
     * Master check box.
     */
    private Button masterChk;

    /**
     * OFS check box.
     */
    private Button ofsChk;

    /**
     * MPE check box.
     */
    private Button mpeChk;

    /**
     * Location check box.
     */
    private Button locationChk;

    /**
     * WFO check box.
     */
    private Button wfoChk;

    /**
     * Type source check box.
     */
    private Button typeSrcChk;

    /**
     * Physical Element check box.
     */
    private Button physElemChk;

    /**
     * Switches check box.
     */
    private Button switchesChk;

    /**
     * Location label.
     */
    private Label locationLbl;

    /**
     * WFO label.
     */
    private Label wfoLbl;

    /**
     * Physical element label.
     */
    private Label physElemLbl;

    /**
     * Type source label.
     */
    private Label typeSrcLbl;

    /**
     * Switches label.
     */
    private Label switchesLbl;

    /**
     * Location filter text control.
     */
    private Text locationFilterTF;

    /**
     * Type source filter combo box.
     */
    private Combo typeSrcFilterCbo;

    /**
     * Master filter check box.
     */
    private Button masterFilterChk;

    /**
     * OPS filter check box.
     */
    private Button ofsFilterChk;

    /**
     * MPE filter check box.
     */
    private Button mpeFilterChk;

    /**
     * Selected Group container.
     */
    private Group selectedGroup;

    /**
     * Location selected text control.
     */
    private Text locationSelectedTF;

    /**
     * WFO selected text control.
     */
    private Label wfoSelectedLbl;

    /**
     * Duration selected combo box.
     */
    private Combo durationSelectedCbo;

    /**
     * Type source selected combo box.
     */
    private Combo typeSrcSelectedCbo;

    /**
     * Extremum selected combo box.
     */
    private Combo extremumSelectedCbo;

    /**
     * Type source rank combo box.
     */
    private Combo typeSrcRankCbo;

    /**
     * Master switch check box.
     */
    private Button masterSwitchChk;

    /**
     * OFS switch check box.
     */
    private Button ofsSwitchChk;

    /**
     * MPE switch check box.
     */
    private Button mpeSwitchChk;

    /**
     * Delete button
     */
    private Button deleteBtn;

    /**
     * Array of controls in the selected item group container.
     */
    private ArrayList<Control> selectedItemControls;

    /**
     * Possible states for the dialog
     */
    private enum DialogStates {
        ADD_RECORD, NO_DATA, DATA_AVAILABLE
    }

    /**
     * Set to display's wait cursor. No need to dispose.
     */
    private Cursor waitCursor;

    /**
     * Used by setBusy to determine which cursor to display.
     */
    private AtomicInteger busyCnt = new AtomicInteger(0);

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public DataIngestFilterDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Data Ingest Filter");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        controlFont.dispose();
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

        waitCursor = shell.getDisplay().getSystemCursor(SWT.CURSOR_WAIT);

        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);
        selectedItemControls = new ArrayList<>();

        createIngestFilterGroup();

        createSelectedItemGroup();

        createBottomButtons();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialog#preOpened()
     */
    @Override
    protected void preOpened() {
        super.preOpened();
        populateStaticData();
        populateLists(true);
    }

    /**
     * Create the Ingest Filter group.
     */
    private void createIngestFilterGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group ingestGroup = new Group(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        ingestGroup.setLayout(gl);
        ingestGroup.setLayoutData(gd);
        ingestGroup.setText(" Ingest Filter Contents for Locations ");

        createIngestListControls(ingestGroup);

        createFilterParamsGroup(ingestGroup);
    }

    /**
     * Create the Ingest List controls.
     * 
     * @param parentGroup
     *            Parent group.
     */
    private void createIngestListControls(Group parentGroup) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite listComp = new Composite(parentGroup, SWT.NONE);
        listComp.setLayout(new GridLayout(10, false));
        listComp.setLayoutData(gd);

        GC gc = new GC(listComp);

        // ---------------------------------------------------
        // Create the labels above the Ingest list control
        // ---------------------------------------------------

        Label locationLbl = new Label(listComp, SWT.NONE);
        locationLbl.setFont(controlFont);
        String locationText = "Location      ";
        int locationWidth = gc.textExtent(locationText).x;
        gd = new GridData(locationWidth, SWT.DEFAULT);
        locationLbl.setLayoutData(gd);
        locationLbl.setText(locationText);
        int listWidth = locationWidth;

        wfoLbl = new Label(listComp, SWT.NONE);
        wfoLbl.setFont(controlFont);
        String wfoText = "WFO  ";
        int wfoWidth = gc.textExtent(wfoText).x;
        gd = new GridData(wfoWidth, SWT.DEFAULT);
        wfoLbl.setLayoutData(gd);
        wfoLbl.setText(wfoText);
        listWidth = listWidth + wfoWidth;

        Label peLbl = new Label(listComp, SWT.NONE);
        peLbl.setFont(controlFont);
        String peText = "PE  ";
        int peWidth = gc.textExtent(peText).x;
        gd = new GridData(peWidth, SWT.DEFAULT);
        peLbl.setLayoutData(gd);
        peLbl.setText(peText);
        listWidth = listWidth + peWidth;

        Label durLbl = new Label(listComp, SWT.NONE);
        durLbl.setFont(controlFont);
        String durText = "Dur   ";
        int durWidth = gc.textExtent(durText).x;
        gd = new GridData(durWidth, SWT.DEFAULT);
        durLbl.setLayoutData(gd);
        durLbl.setText(durText);
        listWidth = listWidth + durWidth;

        Label typSrcLbl = new Label(listComp, SWT.NONE);
        typSrcLbl.setFont(controlFont);
        String typSrcText = "TypeSrc      ";
        int typSrcWidth = gc.textExtent(typSrcText).x;
        gd = new GridData(typSrcWidth, SWT.DEFAULT);
        typSrcLbl.setLayoutData(gd);
        typSrcLbl.setText(typSrcText);
        listWidth = listWidth + typSrcWidth;

        Label extLbl = new Label(listComp, SWT.NONE);
        extLbl.setFont(controlFont);
        String extText = "Ext      ";
        int extWidth = gc.textExtent(extText).x;
        gd = new GridData(extWidth, SWT.DEFAULT);
        extLbl.setLayoutData(gd);
        extLbl.setText(extText);
        listWidth = listWidth + extWidth;

        Label rankLbl = new Label(listComp, SWT.NONE);
        rankLbl.setFont(controlFont);
        String rankText = "Rank    ";
        int rankWidth = gc.textExtent(rankText).x;
        gd = new GridData(rankWidth, SWT.DEFAULT);
        rankLbl.setLayoutData(gd);
        rankLbl.setText(rankText);
        listWidth = listWidth + rankWidth;

        Label masterLbl = new Label(listComp, SWT.NONE);
        masterLbl.setFont(controlFont);
        String masterText = "Master   ";
        int masterWidth = gc.textExtent(masterText).x;
        gd = new GridData(masterWidth, SWT.DEFAULT);
        masterLbl.setLayoutData(gd);
        masterLbl.setText(masterText);
        listWidth = listWidth + masterWidth;

        Label ofsLbl = new Label(listComp, SWT.NONE);
        ofsLbl.setFont(controlFont);
        String ofsText = "OFS     ";
        int ofsWidth = gc.textExtent(ofsText).x;
        gd = new GridData(ofsWidth, SWT.DEFAULT);
        ofsLbl.setLayoutData(gd);
        ofsLbl.setText(ofsText);
        listWidth = listWidth + ofsWidth;

        Label mpeLbl = new Label(listComp, SWT.NONE);
        mpeLbl.setFont(controlFont);
        String mpeText = "MPE      ";
        int mpeWidth = gc.textExtent(mpeText).x;
        gd = new GridData(mpeWidth, SWT.DEFAULT);
        mpeLbl.setLayoutData(gd);
        mpeLbl.setText(mpeText);
        listWidth = listWidth + mpeWidth;

        gc.dispose();

        // ---------------------------------------------------
        // Create the Ingest list control
        // ---------------------------------------------------

        ingestDataList = new List(listComp,
                SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = listWidth;
        gd.heightHint = ingestDataList.getItemHeight() * 16;
        gd.horizontalSpan = 10;

        ingestDataList.setLayoutData(gd);
        ingestDataList.setFont(controlFont);

        ingestDataList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateSelectedInformation();
            }
        });

        // ---------------------------------------------------
        // Create the buttons below the Ingest list control
        // ---------------------------------------------------
        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 7;

        Button setSwitchesBtn = new Button(listComp, SWT.PUSH);
        setSwitchesBtn.setText("Set Switches for All Listed Above");
        setSwitchesBtn.setLayoutData(gd);
        setSwitchesBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setSwitches();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        masterChk = new Button(listComp, SWT.CHECK);
        masterChk.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        gd.horizontalIndent = -10;
        ofsChk = new Button(listComp, SWT.CHECK);
        ofsChk.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        gd.horizontalIndent = -20;
        mpeChk = new Button(listComp, SWT.CHECK);
        mpeChk.setLayoutData(gd);

    }

    /**
     * Create Filter Parameters group and controls.
     * 
     * @param parentGroup
     */
    private void createFilterParamsGroup(Group parentGroup) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group filterGroup = new Group(parentGroup, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        gl.verticalSpacing = 10;
        filterGroup.setLayout(gl);
        filterGroup.setLayoutData(gd);
        filterGroup.setText(" Filter Parameters ");

        // ----------------------------------------------------
        // Add the filter check boxes
        // ----------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label filterLbl = new Label(filterGroup, SWT.RIGHT);
        filterLbl.setText("Filter By: ");
        filterLbl.setLayoutData(gd);

        locationChk = new Button(filterGroup, SWT.CHECK);
        locationChk.setText("Location");
        locationChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleLocationCheckAction();
            }
        });

        // Filler
        new Label(filterGroup, SWT.NONE);

        wfoChk = new Button(filterGroup, SWT.CHECK);
        wfoChk.setText("WFO");
        wfoChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleWfoCheckAction();
            }
        });

        // Filler
        new Label(filterGroup, SWT.NONE);

        typeSrcChk = new Button(filterGroup, SWT.CHECK);
        typeSrcChk.setText("TypeSrc");
        typeSrcChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleTsCheckAction();
            }
        });

        // Filler
        new Label(filterGroup, SWT.NONE);

        physElemChk = new Button(filterGroup, SWT.CHECK);
        physElemChk.setText("PhysElem");
        physElemChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handlePeCheckAction();
            }
        });

        // Filler
        new Label(filterGroup, SWT.NONE);

        switchesChk = new Button(filterGroup, SWT.CHECK);
        switchesChk.setText("Switches");
        switchesChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleSwitchesCheckAction();
            }
        });

        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 4;
        Label sepLbl = new Label(filterGroup, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        // ----------------------------------------------------
        // Add the filter controls
        // ----------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        locationLbl = new Label(filterGroup, SWT.RIGHT);
        locationLbl.setText("Location: ");
        locationLbl.setEnabled(false);
        locationLbl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        gd.horizontalSpan = 3;
        locationFilterTF = new Text(filterGroup, SWT.BORDER);
        locationFilterTF.setEnabled(false);
        locationFilterTF.setLayoutData(gd);
        locationFilterTF.addKeyListener(new KeyListener() {
            @Override
            public void keyPressed(KeyEvent e) {
            }

            @Override
            public void keyReleased(KeyEvent e) {
                populateLists(false);
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        wfoLbl = new Label(filterGroup, SWT.RIGHT);
        wfoLbl.setText("WFO: ");
        wfoLbl.setEnabled(false);
        wfoLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 250;
        gd.heightHint = 100;
        gd.horizontalSpan = 3;
        wfoFilterList = new List(filterGroup,
                SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        wfoFilterList.setLayoutData(gd);
        wfoFilterList.setEnabled(false);
        wfoFilterList.setFont(controlFont);
        wfoFilterList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                populateLists(false);
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        physElemLbl = new Label(filterGroup, SWT.RIGHT);
        physElemLbl.setText("PhysElem: ");
        physElemLbl.setEnabled(false);
        physElemLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 250;
        gd.heightHint = 100;
        gd.horizontalSpan = 3;
        physElemFilterList = new List(filterGroup,
                SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        physElemFilterList.setLayoutData(gd);
        physElemFilterList.setEnabled(false);
        physElemFilterList.setFont(controlFont);
        physElemFilterList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                populateLists(false);
            }
        });

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        typeSrcLbl = new Label(filterGroup, SWT.RIGHT);
        typeSrcLbl.setText("TypeSrc: ");
        typeSrcLbl.setEnabled(false);
        typeSrcLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.horizontalSpan = 3;
        typeSrcFilterCbo = new Combo(filterGroup,
                SWT.DROP_DOWN | SWT.READ_ONLY);
        typeSrcFilterCbo.select(0);
        typeSrcFilterCbo.setEnabled(false);
        typeSrcFilterCbo.setLayoutData(gd);
        typeSrcFilterCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                populateLists(false);
            }
        });

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        switchesLbl = new Label(filterGroup, SWT.RIGHT);
        switchesLbl.setText("Switches: ");
        switchesLbl.setEnabled(false);
        switchesLbl.setLayoutData(gd);

        masterFilterChk = new Button(filterGroup, SWT.CHECK);
        masterFilterChk.setText("Master");
        masterFilterChk.setEnabled(false);
        masterFilterChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                populateLists(false);
            }
        });

        gd = new GridData(70, SWT.DEFAULT);
        ofsFilterChk = new Button(filterGroup, SWT.CHECK);
        ofsFilterChk.setText("OFS");
        ofsFilterChk.setEnabled(false);
        ofsFilterChk.setLayoutData(gd);
        ofsFilterChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                populateLists(false);
            }
        });

        mpeFilterChk = new Button(filterGroup, SWT.CHECK);
        mpeFilterChk.setText("MPE");
        mpeFilterChk.setEnabled(false);
        mpeFilterChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                populateLists(false);
            }
        });
    }

    /**
     * Create Selected Item group and controls.
     */
    private void createSelectedItemGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        selectedGroup = new Group(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        selectedGroup.setLayout(gl);
        selectedGroup.setLayoutData(gd);
        selectedGroup.setText("Selected Item");

        selectedItemControls.add(selectedGroup);

        // --------------------------------------------
        // Add left side composite and controls
        // --------------------------------------------
        Composite leftComp = new Composite(selectedGroup, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.verticalSpacing = 10;
        leftComp.setLayout(gl);
        leftComp.setLayoutData(new GridData(SWT.DEFAULT, SWT.TOP, false, true));

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label locationSelLbl = new Label(leftComp, SWT.RIGHT);
        locationSelLbl.setText("Location: ");
        locationSelLbl.setLayoutData(gd);
        selectedItemControls.add(locationSelLbl);

        gd = new GridData(80, SWT.DEFAULT);
        locationSelectedTF = new Text(leftComp, SWT.BORDER);
        locationSelectedTF.setLayoutData(gd);
        selectedItemControls.add(locationSelectedTF);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label wfoSelLbl = new Label(leftComp, SWT.RIGHT);
        wfoSelLbl.setText("WFO: ");
        wfoSelLbl.setLayoutData(gd);
        selectedItemControls.add(wfoSelLbl);

        gd = new GridData(80, SWT.DEFAULT);
        wfoSelectedLbl = new Label(leftComp, SWT.BORDER);
        wfoSelectedLbl.setLayoutData(gd);
        selectedItemControls.add(wfoSelectedLbl);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label durationSelLbl = new Label(leftComp, SWT.RIGHT);
        durationSelLbl.setText("Duration: ");
        durationSelLbl.setLayoutData(gd);
        selectedItemControls.add(durationSelLbl);

        durationSelectedCbo = new Combo(leftComp,
                SWT.DROP_DOWN | SWT.READ_ONLY);
        durationSelectedCbo.add("Sample Duration Data 1");
        durationSelectedCbo.add("Sample Duration Data 2");
        durationSelectedCbo.add("Sample Duration Data 3");
        durationSelectedCbo.select(0);
        selectedItemControls.add(durationSelectedCbo);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label typeSrcSelLbl = new Label(leftComp, SWT.RIGHT);
        typeSrcSelLbl.setText("TypeSource: ");
        typeSrcSelLbl.setLayoutData(gd);
        selectedItemControls.add(typeSrcSelLbl);

        typeSrcSelectedCbo = new Combo(leftComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        typeSrcSelectedCbo.add("Sample TypeSource Data 1");
        typeSrcSelectedCbo.add("Sample TypeSource Data 2");
        typeSrcSelectedCbo.add("Sample TypeSource Data 3");
        typeSrcSelectedCbo.select(0);
        selectedItemControls.add(typeSrcSelectedCbo);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label extremumSelLbl = new Label(leftComp, SWT.RIGHT);
        extremumSelLbl.setText("Extremum: ");
        extremumSelLbl.setLayoutData(gd);
        selectedItemControls.add(extremumSelLbl);

        extremumSelectedCbo = new Combo(leftComp,
                SWT.DROP_DOWN | SWT.READ_ONLY);
        extremumSelectedCbo.add("Sample Extremum Data 1");
        extremumSelectedCbo.add("Sample Extremum Data 2");
        extremumSelectedCbo.add("Sample Extremum Data 3");
        extremumSelectedCbo.select(0);
        selectedItemControls.add(extremumSelectedCbo);

        // --------------------------------------------
        // Add center composite and controls
        // --------------------------------------------
        Composite centerComp = new Composite(selectedGroup, SWT.NONE);
        centerComp.setLayout(new GridLayout(1, false));
        centerComp.setLayoutData(
                new GridData(SWT.CENTER, SWT.DEFAULT, true, false));

        Label peSelLbl = new Label(centerComp, SWT.RIGHT);
        peSelLbl.setText("Physical Element: ");
        selectedItemControls.add(peSelLbl);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 225;
        gd.heightHint = 125;
        physElemSelectedList = new List(centerComp,
                SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        physElemSelectedList.setLayoutData(gd);
        physElemSelectedList.setFont(controlFont);
        selectedItemControls.add(physElemSelectedList);

        // --------------------------------------------
        // Add right side composite and controls
        // --------------------------------------------
        Composite rightComp = new Composite(selectedGroup, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.verticalSpacing = 10;
        rightComp.setLayout(gl);
        rightComp
                .setLayoutData(new GridData(SWT.DEFAULT, SWT.TOP, false, true));

        Label typeSrcRankSelLbl = new Label(rightComp, SWT.RIGHT);
        typeSrcRankSelLbl.setText("TypeSource Rank: ");
        selectedItemControls.add(typeSrcRankSelLbl);

        typeSrcRankCbo = new Combo(rightComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        typeSrcRankCbo.add("1st");
        typeSrcRankCbo.add("2nd");
        typeSrcRankCbo.add("3rd");
        typeSrcRankCbo.add("4th");
        typeSrcRankCbo.add("5th");
        typeSrcRankCbo.select(0);
        selectedItemControls.add(typeSrcRankCbo);

        gd = new GridData();
        gd.horizontalSpan = 2;
        masterSwitchChk = new Button(rightComp, SWT.CHECK);
        masterSwitchChk.setText("Master Switch");
        masterSwitchChk.setLayoutData(gd);
        selectedItemControls.add(masterSwitchChk);

        gd = new GridData();
        gd.horizontalSpan = 2;
        gd.horizontalIndent = 15;
        ofsSwitchChk = new Button(rightComp, SWT.CHECK);
        ofsSwitchChk.setText("OFS Input Switch");
        ofsSwitchChk.setLayoutData(gd);
        selectedItemControls.add(ofsSwitchChk);

        gd = new GridData();
        gd.horizontalSpan = 2;
        gd.horizontalIndent = 15;
        mpeSwitchChk = new Button(rightComp, SWT.CHECK);
        mpeSwitchChk.setText("MPE Input Switch");
        mpeSwitchChk.setLayoutData(gd);
        selectedItemControls.add(mpeSwitchChk);
    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(5, true));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 90;

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (saveRecord()) {
                    close();
                }
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button applyBtn = new Button(buttonComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button newBtn = new Button(buttonComp, SWT.PUSH);
        newBtn.setText("New");
        newBtn.setLayoutData(gd);
        newBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                newRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteRecord();
            }
        });
    }

    private void handleLocationCheckAction() {
        // Make the filter happen on check
        locationLbl.setEnabled(locationChk.getSelection());
        locationFilterTF.setEnabled(locationChk.getSelection());
        if (locationChk.getSelection()) {
            // clear and disable wfo
            wfoChk.setEnabled(false);
            wfoChk.setSelection(false);
            wfoFilterList.setEnabled(false);
            wfoLbl.setEnabled(false);
            if (locationFilterTF.getText().isEmpty()) {
                return;
            }
        } else {
            // enable ability to filter with wfo
            wfoChk.setEnabled(true);
        }
        populateLists(false);
    }

    private void handleWfoCheckAction() {
        wfoLbl.setEnabled(wfoChk.getSelection());
        wfoFilterList.setEnabled(wfoChk.getSelection());
        if (wfoFilterList.getSelectionIndex() != -1) {
            populateLists(false);
        }
    }

    private void handleTsCheckAction() {
        typeSrcLbl.setEnabled(typeSrcChk.getSelection());
        typeSrcFilterCbo.setEnabled(typeSrcChk.getSelection());
        populateLists(false);
    }

    private void handlePeCheckAction() {
        physElemLbl.setEnabled(physElemChk.getSelection());
        physElemFilterList.setEnabled(physElemChk.getSelection());
        if (physElemFilterList.getSelectionIndex() != -1) {
            populateLists(false);
        }
    }

    private void handleSwitchesCheckAction() {
        switchesLbl.setEnabled(switchesChk.getSelection());
        masterFilterChk.setEnabled(switchesChk.getSelection());
        ofsFilterChk.setEnabled(switchesChk.getSelection());
        mpeFilterChk.setEnabled(switchesChk.getSelection());
        populateLists(false);
    }

    /**
     * Populates lists with data that cannot be edited from the database
     */
    private void populateStaticData() {
        DataIngestFilterDataManager man = DataIngestFilterDataManager
                .getInstance();

        try {
            // Load WFO
            wfoFilterList.removeAll();
            for (String currWFO : man.getWFOs()) {
                wfoFilterList.add(currWFO);
            }

            // Load Duration
            durationSelectedCbo.removeAll();
            for (String currDur : man.getShefDur()) {
                durationSelectedCbo.add(currDur);
            }
            durationSelectedCbo.select(0);

            // Load Type Source
            typeSrcFilterCbo.removeAll();
            typeSrcSelectedCbo.removeAll();
            for (String currTs : man.getShefTs()) {
                typeSrcFilterCbo.add(currTs);
                typeSrcSelectedCbo.add(currTs);
            }
            typeSrcFilterCbo.select(0);
            typeSrcSelectedCbo.select(0);

            // Load Extremum
            extremumSelectedCbo.removeAll();
            for (String currExt : man.getShefExtremum()) {
                extremumSelectedCbo.add(currExt);
            }
            extremumSelectedCbo.select(0);

            // Load Physical Element Lists
            physElemFilterList.removeAll();
            physElemSelectedList.removeAll();
            for (String currPE : DataTrashCanDataManager.getInstance()
                    .getPEList()) {
                physElemFilterList.add(currPE);
                physElemSelectedList.add(currPE);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Problem populating static data ", e);
        }

    }

    /**
     * Populates the main ingest filter list with data from the database.
     * 
     * @param forceLoad
     *            True will force the cache to be refreshes, else the data
     *            returned will be from cache.
     */
    private void populateLists(boolean forceLoad) {
        ingestDataList.removeAll();
        setBusy(true);

        getIngestFilter(physElemChk.getSelection(), getSelectedPEs(),
                locationChk.getSelection(),
                locationFilterTF.getText().toUpperCase(), wfoChk.getSelection(),
                getSelectedWFOs(), switchesChk.getSelection(),
                masterFilterChk.getSelection(), ofsFilterChk.getSelection(),
                mpeFilterChk.getSelection(), typeSrcChk.getSelection(),
                getSelectedStringValue(typeSrcFilterCbo), forceLoad);
    }

    /**
     * Queries manager for data to populate the ingest data list off the UI
     * thread.
     * 
     * @param physElemChkSelection
     * @param selectedPEs
     * @param locationChkSelection
     * @param locationFilterText
     * @param wfoChkSelection
     * @param selectedWFOs
     * @param switchesChkSelection
     * @param masterFilterChkSelection
     * @param ofsFilterChkSelection
     * @param mpeFilterChkSelection
     * @param typeSrcChkSelection
     * @param typeSrcFilterCboValue
     * @param forceLoad
     */
    private void getIngestFilter(final boolean physElemChkSelection,
            final java.util.List<String> selectedPEs,
            final boolean locationChkSelection, final String locationFilterText,
            final boolean wfoChkSelection,
            final java.util.List<String> selectedWFOs,
            final boolean switchesChkSelection,
            final boolean masterFilterChkSelection,
            final boolean ofsFilterChkSelection,
            final boolean mpeFilterChkSelection,
            final boolean typeSrcChkSelection,
            final String typeSrcFilterCboValue, final boolean forceLoad) {

        Job job = new Job("") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {

                DataIngestFilterDataManager man = DataIngestFilterDataManager
                        .getInstance();
                java.util.List<DataIngestFilterData> temp = null;
                try {
                    temp = man.getIngestFilter(physElemChkSelection,
                            selectedPEs, locationChkSelection,
                            locationFilterText, wfoChkSelection, selectedWFOs,
                            switchesChkSelection, masterFilterChkSelection,
                            ofsFilterChkSelection, mpeFilterChkSelection,
                            typeSrcChkSelection, typeSrcFilterCboValue,
                            forceLoad);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Problem filter list ", e);
                } finally {
                    final java.util.List<DataIngestFilterData> t = temp;
                    VizApp.runAsync(new Runnable() {

                        @Override
                        public void run() {
                            if (isDisposed()) {
                                return;
                            }
                            updatePopulateList(t);
                        }
                    });

                }
                return Status.OK_STATUS;
            }
        };
        job.schedule();
    }

    /**
     * Update the ingest data list.
     * 
     * @param difData
     */
    private void updatePopulateList(
            java.util.List<DataIngestFilterData> difData) {
        if (difData != null) {
            for (DataIngestFilterData currData : difData) {
                ingestDataList.add(currData.getIngestFilterString());
            }

            if (ingestDataList.getItemCount() > 0) {
                updateDialogState(DialogStates.DATA_AVAILABLE);
                ingestDataList.select(0);
                updateSelectedInformation();
            } else {
                updateDialogState(DialogStates.NO_DATA);
            }
        }
        setBusy(false);
    }

    /**
     * Returns the selected PEs to filter by
     * 
     * @return
     */
    private java.util.List<String> getSelectedPEs() {
        int[] selectedInd = physElemFilterList.getSelectionIndices();
        java.util.List<String> peFilter = new ArrayList<>();

        for (int i : selectedInd) {
            peFilter.add(
                    physElemFilterList.getItem(i).split(" ")[0].toUpperCase());
        }

        return peFilter;
    }

    /**
     * Returns the selected WFOs to filter by
     * 
     * @return
     */
    private java.util.List<String> getSelectedWFOs() {
        int[] selectedInd = wfoFilterList.getSelectionIndices();
        java.util.List<String> wfoFilter = new ArrayList<>(selectedInd.length);

        for (int i : selectedInd) {
            wfoFilter.add(wfoFilterList.getItem(i));
        }

        return wfoFilter;
    }

    /**
     * Updates the display information in the bottom group to reflect the data
     * from the currently selected record.
     */
    private void updateSelectedInformation() {
        DataIngestFilterData selectedData = DataIngestFilterDataManager
                .getInstance()
                .getSelectedFilterData(ingestDataList.getSelectionIndex());
        // Location
        locationSelectedTF.setText(selectedData.getLid());

        // WFO
        wfoSelectedLbl.setText(selectedData.getWfo());

        // Duration
        for (int i = 0; i < durationSelectedCbo.getItemCount(); i++) {
            if (durationSelectedCbo.getItem(i)
                    .contains("(" + selectedData.getDuration() + ")")) {
                durationSelectedCbo.select(i);
                break;
            }
        }

        // TypeSrc
        for (int i = 0; i < typeSrcSelectedCbo.getItemCount(); i++) {
            if (typeSrcSelectedCbo.getItem(i)
                    .contains("(" + selectedData.getTypeSource() + ")")) {
                typeSrcSelectedCbo.select(i);
                break;
            }
        }

        // Extremum
        for (int i = 0; i < extremumSelectedCbo.getItemCount(); i++) {
            if (extremumSelectedCbo.getItem(i)
                    .contains("(" + selectedData.getExtremum() + ")")) {
                extremumSelectedCbo.select(i);
                break;
            }
        }

        // Physical Element
        for (int i = 0; i < physElemSelectedList.getItemCount(); i++) {
            if (physElemSelectedList.getItem(i).split(" ")[0]
                    .equals(selectedData.getPe())) {
                physElemSelectedList.select(i);
                break;
            }
        }

        // TS Rank
        for (int i = 0; i < typeSrcRankCbo.getItemCount(); i++) {
            if (typeSrcRankCbo.getItem(i)
                    .contains("" + selectedData.getTsRank())) {
                typeSrcRankCbo.select(i);
                break;
            }
        }

        // Master Ingest Switch, set if "T"
        masterSwitchChk.setSelection("T".equals(selectedData.getIngest()));

        // OFS Ingest Switch, set if "T"
        ofsSwitchChk.setSelection("T".equals(selectedData.getOfsInput()));

        // MPE Ingest Switch, set if "T"
        mpeSwitchChk.setSelection("T".equals(selectedData.getStg2Input()));

        // Update the dialog state
        updateDialogState(DialogStates.DATA_AVAILABLE);
    }

    /**
     * Saves the currently displayed information to the database.
     * 
     * @return True if the save was successful, else False
     */
    private boolean saveRecord() {
        boolean successful = false;
        DataIngestFilterData dataToSave = new DataIngestFilterData();

        // LID
        dataToSave.setLid(locationSelectedTF.getText());

        // WFO not retrieved from text field because it is not part of the
        // ingestfilter table, and is derived from the location table by LID

        // Duration
        dataToSave.setDuration(getSelectedIntValue(durationSelectedCbo));

        // Type Source
        dataToSave.setTypeSource(getSelectedStringValue(typeSrcSelectedCbo));

        // Extremum
        dataToSave.setExtremum(getSelectedStringValue(extremumSelectedCbo));

        // PE
        dataToSave.setPe(getSelectedInformationPE());

        // Type Src Rank
        dataToSave.setTsRank(typeSrcRankCbo.getSelectionIndex() + 1);

        // Master Ingest
        dataToSave.setIngest((masterSwitchChk.getSelection()) ? "T" : "F");

        // OFS Ingest
        dataToSave.setOfsInput((ofsSwitchChk.getSelection()) ? "T" : "F");

        // MPE Ingest
        dataToSave.setStg2Input((mpeSwitchChk.getSelection()) ? "T" : "F");

        try {
            if (checkFKConstraintsMet(dataToSave.getLid())) {
                HydroDBDataManager.getInstance().putData(dataToSave);

                // Synchronize StnClass table
                StnClassSyncUtil.setStnClass(dataToSave.getLid());

                successful = true;
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Problem saving record ", e);
        }

        populateLists(true);

        return successful;
    }

    /**
     * Checks the foreign key constraints for the record.
     * 
     * @return True if the foreign key constraints are met.
     */
    private boolean checkFKConstraintsMet(String lid) {
        boolean rval = false;

        // Lid must exist in riverStat table
        String query = "Select lid FROM location WHERE lid='" + lid + "'";

        try {
            QueryResult data = HydroDBDataManager.getInstance()
                    .runMappedQuery(query);

            if (data.getResultCount() > 0) {
                rval = true;
            } else {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                mb.setMessage("Specified location does not exist.");
                mb.open();
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Problem checking constraints ", e);
        }

        return rval;
    }

    /**
     * Parses out the DB value out of the display value.
     * 
     * @return The DB value for the display string.
     */
    private int getSelectedIntValue(Combo currCombo) {
        int rval = HydroConstants.MISSING_VALUE;

        if (currCombo.getSelectionIndex() >= 0) {
            // Build regex for Mileage part of detail
            String durRegex = "\\((\\d*)\\)";
            Pattern durPattern = Pattern.compile(durRegex);

            Matcher durMatcher = durPattern
                    .matcher(currCombo.getItem(currCombo.getSelectionIndex()));

            // Find the Duration
            if (durMatcher.find()) {
                rval = Integer.parseInt(durMatcher.group(1));
            }
        }

        return rval;
    }

    /**
     * Parses out the DB value out of the display value.
     * 
     * @return The DB value for the display string.
     */
    private String getSelectedStringValue(Combo currCombo) {
        String rval = "";

        if (currCombo.getSelectionIndex() >= 0) {
            // Build regex for Mileage part of detail
            String durRegex = "\\((\\w+)\\)";
            Pattern durPattern = Pattern.compile(durRegex);

            Matcher durMatcher = durPattern
                    .matcher(currCombo.getItem(currCombo.getSelectionIndex()));

            // Find the Duration
            if (durMatcher.find()) {
                rval = durMatcher.group(1);
            }
        }

        return rval;
    }

    /**
     * Returns the selected Physical Element
     * 
     * @return
     */
    private String getSelectedInformationPE() {
        String rval = "";

        String selectedPE = physElemSelectedList.getSelection()[0];

        rval = selectedPE.split(" ")[0];

        return rval;
    }

    /**
     * Sets up the dialog for a new record
     */
    private void newRecord() {
        clearInformation();
        updateDialogState(DialogStates.ADD_RECORD);
    }

    /**
     * Prompts the users and then sets the ingest switches for all of the data
     * currently displayed.
     */
    private void setSwitches() {
        StringBuilder msg = new StringBuilder(
                "Do you wish to update all displayed entries to\n\n");
        msg.append("Master = ");
        msg.append(masterChk.getSelection() ? "T" : "F");
        msg.append(", OFS = ");
        msg.append(ofsChk.getSelection() ? "T" : "F");
        msg.append(", and MPE = ");
        msg.append(mpeChk.getSelection() ? "T" : "F");
        msg.append(" ?");

        MessageBox mb = new MessageBox(shell,
                SWT.ICON_QUESTION | SWT.OK | SWT.CANCEL);
        mb.setText("Set Switches Confirmation");
        mb.setMessage(msg.toString());
        int result = mb.open();

        if (result == SWT.OK) {
            try {
                DataIngestFilterDataManager.getInstance().setSwitches(
                        masterChk.getSelection(), ofsChk.getSelection(),
                        mpeChk.getSelection());

                populateLists(true);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Problem savings switches ", e);
            }
        }
    }

    /**
     * Clears the Selected Item section of the dialog.
     */
    private void clearInformation() {
        locationSelectedTF.setText("");
        wfoSelectedLbl.setText("");
        durationSelectedCbo.select(0);
        typeSrcSelectedCbo.select(0);
        extremumSelectedCbo.select(0);
        physElemSelectedList.select(0);
        typeSrcFilterCbo.select(0);
        typeSrcRankCbo.select(0);
        masterSwitchChk.setSelection(false);
        ofsSwitchChk.setSelection(false);
        mpeSwitchChk.setSelection(false);
    }

    /**
     * Prompts the user to delete the current record
     */
    private void deleteRecord() {
        if (ingestDataList.getSelectionCount() > 0) {
            MessageBox mb = new MessageBox(shell,
                    SWT.ICON_QUESTION | SWT.OK | SWT.CANCEL);
            mb.setText("Delete Confirmation");
            mb.setMessage("Do you wish to delete this entry?");

            int result = mb.open();

            if (result == SWT.OK) {
                DataIngestFilterData selectedData = DataIngestFilterDataManager
                        .getInstance().getSelectedFilterData(
                                ingestDataList.getSelectionIndex());

                try {
                    HydroDBDataManager.getInstance().deleteRecord(selectedData);

                    /*
                     * If there is not a location associated with the lid of the
                     * ingest filter record that was just deleted, there is no
                     * reason to sync the changes to the stnclass table because
                     * the stnclass table defines a foreign key in which the lid
                     * must map to a location in the location table. The ingest
                     * filter table does not define a similar foreign key. So,
                     * if there is not a location associated with the lid, an
                     * associated record could never exist in the stnclass table
                     * and any attempt to insert a record into the stnclass
                     * table will fail.
                     */

                    if (!LocationDataManager.getInstance()
                            .getLocationName(selectedData.getLid()).isEmpty()) {
                        /*
                         * an associated location exists, so the stnclass table
                         * can be updated.
                         */
                        // Synchronize StnClass table
                        StnClassSyncUtil.setStnClass(selectedData.getLid());
                    }
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Problem deleting record ", e);
                }

                populateLists(true);
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_WARNING | SWT.OK);
            mb.setText("Invalid Selection");
            mb.setMessage("Please select an ingest filter first.");
            mb.open();
        }
    }

    /**
     * Updates the dialog to reflect the current state.
     * 
     * @param currState
     *            The state of the dialog
     */
    private void updateDialogState(DialogStates currState) {
        switch (currState) {
        case NO_DATA:
            // Disable bottom form and PE list
            selectedGroup.setEnabled(false);
            physElemSelectedList.setEnabled(false);

            // Disable Delete Button
            deleteBtn.setEnabled(false);

            selectedGroup.setText("Selected Item");

            clearInformation();

            break;
        case ADD_RECORD:
            // Disable bottom form and PE list
            selectedGroup.setEnabled(true);
            physElemSelectedList.setEnabled(true);

            selectedGroup.setText("NEW item");

            break;
        case DATA_AVAILABLE:
            // Enable bottom form and PE list
            selectedGroup.setEnabled(true);
            physElemSelectedList.setEnabled(true);

            // Enable Delete Button
            deleteBtn.setEnabled(true);

            selectedGroup.setText("Selected Item");

            break;
        default:
            break;
        }
    }

    /**
     * Determine what cursor to display.
     * 
     * @param busy
     */
    private void setBusy(boolean busy) {
        if (busy) {
            busyCnt.incrementAndGet();
            shell.setCursor(waitCursor);
        } else if (busyCnt.decrementAndGet() == 0) {
            shell.setCursor(null);
        }
    }
}
