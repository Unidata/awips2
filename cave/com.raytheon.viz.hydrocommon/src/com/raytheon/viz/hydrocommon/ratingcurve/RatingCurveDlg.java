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
package com.raytheon.viz.hydrocommon.ratingcurve;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.TimeZone;

import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.RiverDataPoint;
import com.raytheon.viz.hydrocommon.datamanager.RiverDataManager;
import com.raytheon.viz.ui.UiPlugin;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Rating Curve dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 8, 2008				lvenable	Initial creation.
 * 24 Nov 2008  1682        dhladky     Made interactive.
 * 15 Dec 2009  2422        mpduff      Fixed bad label.
 * 21 Feb 2010  4167        mpduff      Added TimeZone to SimpleDateFormat.
 * 29 Apr 2010  4993        mpduff      Fixed date format in error message.
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class RatingCurveDlg extends CaveSWTDialog implements IRatingCurveSort {

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Stage data label.
     */
    private Label stageDataLbl;

    /**
     * KCFS data label.
     */
    private Label kcfsDataLbl;

    /**
     * Flood data label.
     */
    private Label floodDataLbl;

    /**
     * Record data label.
     */
    private Label recordDataLbl;

    /**
     * Rating curve canvas.
     */
    private RatingCurveCanvasComp ratingCurveCanvas = null;

    /**
     * Shift data list control.
     */
    private List shiftDataList = null;

    /**
     * Shift date text control.
     */
    private Text shiftDateTF;

    /**
     * Shift value text control.
     */
    private Text shiftValueTF;

    /**
     * Shift Active check box.
     */
    private Button shiftActiveChk;

    /**
     * Array of rating curve data (no shift curve).
     */
    private ArrayList<RatingCurveData> noShiftCurveArray;

    /**
     * Array of rating curve data (with shift curve).
     */
    private ArrayList<RatingCurveData> shiftCurveArray;

    /**
     * Shift Data if available
     */
    private ArrayList<RatingCurveShiftData> shiftData;

    /**
     * Shift curve data list control.
     */
    private List shiftCurveDataList;

    /**
     * No shift data list control.
     */
    private List noShiftCurveDataList;

    /**
     * Rating label.
     */
    private Label ratingLbl;

    /**
     * Stage text control.
     */
    private Text stageTF;

    /**
     * Discharge text control.
     */
    private Text dischargeTF;

    /**
     * Flood value.
     */
    private double floodDbl = 0.0;

    /**
     * Record value.
     */
    private double recordDbl = 0.0;

    /**
     * Flag indicating if all of the controls should be displayed.
     */
    private boolean fullControls;

    /**
     * Rating label text.
     */
    private String ratingLblText = "Date of Rating:\nUSGS Rating No.:";

    /**
     * Shift remove button.
     */
    private Button shftRemoveBtn;

    /**
     * Shift update/insert button.
     */
    private Button shftUpdateInsBtn;

    /**
     * Curve import button.
     */
    private Button curveImportBtn;

    /**
     * Curve clear all button.
     */
    private Button curveClearAllBtn;

    /**
     * Curve remove button.
     */
    private Button curveRemoveBtn;

    /**
     * Curve update/insert button.
     */
    private Button curveUpdateInsBtn;

    /**
     * Save & Exit button.
     */
    private Button saveExitBtn;

    /**
     * Save button.
     */
    private Button saveBtn;

    /**
     * Sort by enumeration.
     */
    private enum sortBy {
        Stage, Discharge
    };

    /**
     * Sort by
     */
    private sortBy sortKey;

    /**
     * Dialog lid information.
     */
    private String lid;

    /**
     * Decimal Formatter
     */
    private DecimalFormat df = new DecimalFormat();

    /**
     * Date Formatter
     */
    private SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy");

    /**
     * rating curve
     */
    private static String extension = "rating";

    /**
     * Import curve boolean
     */
    private boolean newRatingCurve = false;

    /**
     * rating curve points
     */
    private ArrayList<RatingCurveData> removedPoints = null;

    private ArrayList<RatingCurveData> addedPoints = null;

    /**
     * rating curve shift points
     */
    private ArrayList<RatingCurveShiftData> removedCurveShifts = null;

    private ArrayList<RatingCurveShiftData> addedCurveShifts = null;

    /**
     * The current selected shift curve
     */
    private RatingCurveShiftData selectedRatingShift = null;

    /**
     * The current selected rating point
     */
    private RatingCurveData selectedRatingPoint = null;

    /**
     * Shift amount
     */
    private double shiftAmount = 0;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param GageData
     *            gage chosen on screen
     * @param fullControls
     *            Flag indicating if all of the controls should be displayed.
     */
    public RatingCurveDlg(Shell parent, String lid, String titleInfo,
            boolean fullControls) {
        super(parent);
        setText("Rating Curve" + titleInfo);

        this.lid = lid;
        this.fullControls = fullControls;

        // set defaults
        newRatingCurve = false;

        // delete/add/update objects ~ RatingCurveData Points
        removedPoints = new ArrayList<RatingCurveData>();
        addedPoints = new ArrayList<RatingCurveData>();

        // delete/add/update objects ~ RatingCurveShift Points
        removedCurveShifts = new ArrayList<RatingCurveShiftData>();
        addedCurveShifts = new ArrayList<RatingCurveShiftData>();

        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(2, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        controlFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        // Initialize all of the controls and layouts
        createCurveData();
        createLeftSideControls();
        createRightSideControls();
        addSeparator();
        createBottomButtons();
        populateControls();
    }

    /**
     * Create the controls on the left side of the dialog.
     */
    private void createLeftSideControls() {
        Composite leftComp = new Composite(shell, SWT.NONE);
        GridLayout leftGl = new GridLayout(1, false);
        leftGl.marginHeight = 0;
        leftComp.setLayout(leftGl);

        createRatingCurveLabels(leftComp);
        createRatingCurveCanvas(leftComp);
        createShiftControls(leftComp);
    }

    /**
     * Create the rating curve labels.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createRatingCurveLabels(Composite parentComp) {
        Composite labelComp = new Composite(parentComp, SWT.NONE);
        GridLayout labelGl = new GridLayout(8, false);
        labelComp.setLayout(labelGl);

        Label stageLbl = new Label(labelComp, SWT.NONE);
        stageLbl.setText("Stage:");

        GridData gd = new GridData(100, SWT.DEFAULT);
        stageDataLbl = new Label(labelComp, SWT.NONE);
        stageDataLbl.setText("");
        stageDataLbl.setLayoutData(gd);

        Label kcfsLbl = new Label(labelComp, SWT.NONE);
        kcfsLbl.setText("KCFS:");

        gd = new GridData(100, SWT.DEFAULT);
        kcfsDataLbl = new Label(labelComp, SWT.NONE);
        kcfsDataLbl.setText("");
        kcfsDataLbl.setLayoutData(gd);

        Label floodLbl = new Label(labelComp, SWT.NONE);
        floodLbl.setText("Flood:");

        gd = new GridData(100, SWT.DEFAULT);
        floodDataLbl = new Label(labelComp, SWT.NONE);
        floodDataLbl.setText("");
        floodDataLbl.setLayoutData(gd);

        Label recordLbl = new Label(labelComp, SWT.NONE);
        recordLbl.setText("Record:");

        gd = new GridData(100, SWT.DEFAULT);
        recordDataLbl = new Label(labelComp, SWT.NONE);
        recordDataLbl.setText("");
        recordDataLbl.setLayoutData(gd);
    }

    /**
     * Create the rating curve canvas.
     * 
     * @param parentComp
     */
    private void createRatingCurveCanvas(Composite parentComp) {

        ratingCurveCanvas = new RatingCurveCanvasComp(parentComp, this,
                noShiftCurveArray, floodDbl, recordDbl, shiftAmount);

        if (noShiftCurveArray != null) {
            ratingCurveCanvas.setEnabled(true);
        } else {
            ratingCurveCanvas.setEnabled(false);
        }

    }

    /**
     * Create the shift controls.
     * 
     * @param parentComp
     */
    private void createShiftControls(Composite parentComp) {
        Composite mainShiftComp = new Composite(parentComp, SWT.NONE);
        GridLayout labelGl = new GridLayout(3, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        mainShiftComp.setLayout(labelGl);
        mainShiftComp.setLayoutData(gd);

        int buttonWidth = 120;

        // --------------------------------------------------------
        // Create the shift data list and text controls
        // --------------------------------------------------------
        Composite shiftComp = new Composite(mainShiftComp, SWT.NONE);
        labelGl = new GridLayout(3, false);
        shiftComp.setLayout(labelGl);

        // Create Shift Data List label
        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, true);
        gd.horizontalSpan = 3;
        gd.horizontalIndent = 4;
        Label shiftListLbl = new Label(shiftComp, SWT.NONE);
        shiftListLbl.setText(getShiftListLabel());
        shiftListLbl.setFont(controlFont);
        shiftListLbl.setLayoutData(gd);

        // Create Shift Data List
        gd = new GridData(270, 90);
        gd.horizontalSpan = 3;
        shiftDataList = new List(shiftComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        shiftDataList.setFont(controlFont);
        shiftDataList.setLayoutData(gd);

        // Create Shift Data Controls
        gd = new GridData(110, SWT.DEFAULT);
        shiftDateTF = new Text(shiftComp, SWT.BORDER);
        shiftDateTF.setLayoutData(gd);
        shiftDateTF.setEditable(false);

        gd = new GridData(60, SWT.DEFAULT);
        shiftValueTF = new Text(shiftComp, SWT.BORDER);
        shiftValueTF.setLayoutData(gd);
        shiftValueTF.setEditable(false);

        shiftActiveChk = new Button(shiftComp, SWT.CHECK);
        shiftActiveChk.setText("Active");

        df.setGroupingUsed(false);
        df.setMaximumFractionDigits(2);
        df.setMaximumIntegerDigits(3);

        shiftDataList.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                int index = 0; // default
                RatingCurveShiftData rcsd = shiftData.get(index);

                shiftValueTF.setText(df.format(rcsd.getValue()));
                shiftDateTF.setText(sdf.format(rcsd.getDate().getTime()));
                shiftActiveChk.setSelection(rcsd.isActive());
                generateShiftList(rcsd);
                setSelectedShift(rcsd);

                if (shiftActiveChk.getSelection()) {
                    // redraw the canvas with shifted data
                    ratingCurveCanvas.updateCurveData(shiftCurveArray,
                            floodDbl, recordDbl, shiftAmount);
                } else {
                    ratingCurveCanvas.updateCurveData(noShiftCurveArray,
                            floodDbl, recordDbl, shiftAmount);
                }
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                int index = shiftDataList.getSelectionIndex();
                setSelectedShift(shiftData.get(index));
                RatingCurveShiftData rcsd = shiftData.get(index);

                shiftValueTF.setText(df.format(rcsd.getValue()));
                shiftDateTF.setText(sdf.format(rcsd.getDate().getTime()));
                shiftActiveChk.setSelection(rcsd.isActive());

                if (shiftActiveChk.getSelection()) {
                    // redraw the canvas with shifted data
                    generateShiftList(getEditingShiftData());
                    ratingCurveCanvas.updateCurveData(shiftCurveArray,
                            floodDbl, recordDbl, shiftAmount);
                } else {
                    ratingCurveCanvas.updateCurveData(noShiftCurveArray,
                            floodDbl, recordDbl, shiftAmount);
                }
            }

        });

        shiftActiveChk.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                // redraw the canvas with shifted data
                if ((shiftData.size() > 0) && (shiftDataList != null)) {
                    int index = 0;
                    RatingCurveShiftData rcsd = shiftData.get(index);
                    if (rcsd.isActive()) {
                        ratingCurveCanvas.updateCurveData(shiftCurveArray,
                                floodDbl, recordDbl, shiftAmount);
                        shiftActiveChk.setSelection(rcsd.isActive());
                    } else {
                        ratingCurveCanvas.updateCurveData(noShiftCurveArray,
                                floodDbl, recordDbl, shiftAmount);
                    }
                }
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                // redraw the canvas with shifted data
                if ((shiftData.size() > 0)
                        && (shiftDataList.getSelectionIndex() != -1)) {
                    RatingCurveShiftData rcsd = shiftData.get(shiftDataList
                            .getSelectionIndex());
                    generateShiftList(rcsd);
                    if (rcsd.isActive()) {
                        ratingCurveCanvas.updateCurveData(shiftCurveArray,
                                floodDbl, recordDbl, shiftAmount);
                    } else {
                        ratingCurveCanvas.updateCurveData(noShiftCurveArray,
                                floodDbl, recordDbl, shiftAmount);
                    }
                }
            }

        });

        // --------------------------------------------------------
        // Create the Shift Remove & Update/Insert buttons
        // --------------------------------------------------------
        Composite shiftBtnComp = new Composite(mainShiftComp, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        labelGl = new GridLayout(1, false);
        shiftBtnComp.setLayout(labelGl);
        shiftBtnComp.setLayoutData(gd);

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        shftRemoveBtn = new Button(shiftBtnComp, SWT.PUSH);
        shftRemoveBtn.setText("Remove Shift");
        shftRemoveBtn.setLayoutData(gd);
        shftRemoveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (getEditingShiftData() != null) {
                    int index = shiftDataList.getSelectionIndex();
                    removedCurveShifts.add(getEditingShiftData());
                    shiftData.remove(index);
                    selectedRatingShift = null;

                    shiftDataList.removeAll();
                    for (RatingCurveShiftData rcsd : shiftData) {
                        shiftDataList.add(getShiftListString(rcsd));
                    }
                    shiftDataList.redraw();

                    shiftValueTF.setText("");
                    shiftDateTF.setText("");
                    shiftActiveChk.setSelection(false);
                    // default without shifting
                    ratingCurveCanvas.updateCurveData(noShiftCurveArray,
                            floodDbl, recordDbl, shiftAmount);
                }
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        shftUpdateInsBtn = new Button(shiftBtnComp, SWT.PUSH);
        shftUpdateInsBtn.setText("Update/Insert");
        shftUpdateInsBtn.setLayoutData(gd);
        shftUpdateInsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateInsert();
            }
        });

        // --------------------------------------------------------
        // Create the Curve control buttons
        // --------------------------------------------------------
        Composite curveBtnComp = new Composite(mainShiftComp, SWT.NONE);
        labelGl = new GridLayout(1, false);
        curveBtnComp.setLayout(labelGl);

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        curveImportBtn = new Button(curveBtnComp, SWT.PUSH);
        curveImportBtn.setText("Import Curve");
        curveImportBtn.setLayoutData(gd);
        curveImportBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                FileDialog fd = new FileDialog(shell, SWT.OPEN);
                fd.setText("Open");
                fd.setFilterPath("$HOME");
                String[] filterExt = { "*." + extension };
                fd.setFilterExtensions(filterExt);
                
                String file = fd.open();
                if (file != null) {
                    importCurveData(importRatingCurve(fd.open()));
                }
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        curveClearAllBtn = new Button(curveBtnComp, SWT.PUSH);
        curveClearAllBtn.setText("Clear All");
        curveClearAllBtn.setLayoutData(gd);
        curveClearAllBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // get rid of every point
                removedPoints = noShiftCurveArray;
                noShiftCurveArray.clear();
                noShiftCurveDataList.removeAll();
                noShiftCurveDataList.redraw();

                if (shiftCurveArray != null) {
                    shiftCurveArray.clear();
                    shiftCurveDataList.removeAll();
                    shiftCurveDataList.redraw();
                }

                stageTF.setText("");
                dischargeTF.setText("");
                selectedRatingShift = null;
                selectedRatingPoint = null;

                ratingCurveCanvas.updateCurveData(noShiftCurveArray, floodDbl,
                        recordDbl, shiftAmount);
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        curveRemoveBtn = new Button(curveBtnComp, SWT.PUSH);
        curveRemoveBtn.setText("Remove Point");
        curveRemoveBtn.setLayoutData(gd);
        curveRemoveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (noShiftCurveDataList.getSelectionIndex() != -1) {
                    // get rid of this point
                    int index = noShiftCurveDataList.getSelectionIndex();
                    removedPoints.add(noShiftCurveArray.remove(index));
                    remakeRatingCurveDataList();

                    stageTF.setText("");
                    dischargeTF.setText("");

                    if (getEditingShiftData() != null) {
                        generateShiftList(getEditingShiftData());
                    }

                    ratingCurveCanvas.updateCurveData(noShiftCurveArray,
                            floodDbl, recordDbl, shiftAmount);
                }
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        curveUpdateInsBtn = new Button(curveBtnComp, SWT.PUSH);
        curveUpdateInsBtn.setText("Update/Insert");
        curveUpdateInsBtn.setLayoutData(gd);
        curveUpdateInsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (verifyDouble(stageTF) && verifyInt(dischargeTF)) {

                    RatingCurveData rcd = new RatingCurveData(new Double(
                            stageTF.getText().trim()), new Double(dischargeTF
                            .getText().trim()));

                    if (noShiftCurveDataList != null) {
                        if (getEditingCurveData() != null) {
                            int index = noShiftCurveDataList
                                    .getSelectionIndex();
                            if (index > -1) {
                                noShiftCurveArray.remove(index);
                                noShiftCurveDataList.remove(index);
                            }
                        }
                    }

                    if (!addedPoints.contains(rcd)) {
                        addedPoints.add(rcd);
                    } else {
                        addedPoints.remove(rcd);
                        addedPoints.add(rcd);
                    }

                    noShiftCurveArray.add(rcd);
                    remakeRatingCurveDataList();

                    if (getEditingShiftData() != null) {
                        if (getEditingShiftData().isActive()) {
                            int index = shiftDataList.getSelectionIndex();
                            generateShiftList(shiftData.get(index));
                            ratingCurveCanvas.updateCurveData(shiftCurveArray,
                                    floodDbl, recordDbl, shiftAmount);
                        } else {
                            ratingCurveCanvas.updateCurveData(
                                    noShiftCurveArray, floodDbl, recordDbl,
                                    shiftAmount);
                        }
                    } else {
                        ratingCurveCanvas.updateCurveData(noShiftCurveArray,
                                floodDbl, recordDbl, shiftAmount);
                    }
                }
            }
        });

        if (fullControls == false) {
            shftRemoveBtn.setVisible(false);
            shftUpdateInsBtn.setVisible(false);
            curveImportBtn.setVisible(false);
            curveClearAllBtn.setVisible(false);
            curveRemoveBtn.setVisible(false);
            curveUpdateInsBtn.setVisible(false);
        } else {
            shiftDateTF.setEditable(true);
            shiftValueTF.setEditable(true);
        }
    }

    /**
     * Create the controls on the right side of the dialog.
     */
    private void createRightSideControls() {
        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        Composite rightComp = new Composite(shell, SWT.NONE);
        GridLayout rightGl = new GridLayout(1, false);
        rightGl.marginTop = 10;
        rightComp.setLayout(rightGl);
        rightComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Label activeLbl = new Label(rightComp, SWT.CENTER);
        activeLbl.setText("Active Rating Curve\nincluding Shift");
        activeLbl.setLayoutData(gd);

        createStageDischargeLabels(rightComp);

        gd = new GridData(220, 400);
        shiftCurveDataList = new List(rightComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        shiftCurveDataList.setFont(controlFont);
        shiftCurveDataList.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        ratingLbl = new Label(rightComp, SWT.CENTER);
        ratingLbl.setText(ratingLblText);
        ratingLbl.setLayoutData(gd);

        gd = new GridData(220, 130);
        noShiftCurveDataList = new List(rightComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        noShiftCurveDataList.setFont(controlFont);
        noShiftCurveDataList.setLayoutData(gd);

        if (noShiftCurveArray != null) {
            // populate the list
            for (RatingCurveData curve : noShiftCurveArray) {
                noShiftCurveDataList.add(curve.toString());
            }
            noShiftCurveDataList.setEnabled(true);
        } else {
            noShiftCurveDataList.setEnabled(false);
        }

        noShiftCurveDataList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                RatingCurveData data = noShiftCurveArray
                        .get(noShiftCurveDataList.getSelectionIndex());
                stageTF.setText(String.format("%7.2f", data.getStage()));
                dischargeTF
                        .setText(String.format("%7.1f", data.getDischarge()));
                setSelectedCurveData(data);
            }
        });

        createStageDischargeTextFields(rightComp);
    }

    /**
     * Create the stage and discharge labels.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createStageDischargeLabels(Composite parentComp) {
        Composite labelComp = new Composite(parentComp, SWT.NONE);
        GridLayout rightGl = new GridLayout(2, false);
        rightGl.marginHeight = 0;
        labelComp.setLayout(rightGl);

        GridData gd = new GridData(150, SWT.DEFAULT);
        Label stageLbl = new Label(labelComp, SWT.NONE);
        stageLbl.setText("Stage");
        stageLbl.setLayoutData(gd);

        gd = new GridData(70, SWT.DEFAULT);
        Label dischargeLbl = new Label(labelComp, SWT.NONE);
        dischargeLbl.setText("Discharge");
        dischargeLbl.setLayoutData(gd);
        dischargeLbl.setLayoutData(gd);
    }

    /**
     * Create stage and discharge text fields.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createStageDischargeTextFields(Composite parentComp) {
        Composite labelComp = new Composite(parentComp, SWT.NONE);
        GridLayout rightGl = new GridLayout(2, false);
        rightGl.marginHeight = 0;
        labelComp.setLayout(rightGl);

        GridData gd = new GridData(100, SWT.DEFAULT);
        stageTF = new Text(labelComp, SWT.BORDER);
        stageTF.setEditable(false);
        stageTF.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        dischargeTF = new Text(labelComp, SWT.BORDER);
        dischargeTF.setEditable(false);
        dischargeTF.setLayoutData(gd);

        if (fullControls) {
            stageTF.setEditable(true);
            dischargeTF.setEditable(true);
        }
    }

    /**
     * Add a horizontal separator bar to the dialog.
     */
    private void addSeparator() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 2;
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, true);
        buttonComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        buttonComp.setLayoutData(gd);

        int buttonWidth = 120;

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        saveExitBtn = new Button(buttonComp, SWT.NONE);
        saveExitBtn.setText("Save && Exit");
        saveExitBtn.setLayoutData(gd);
        saveExitBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                save();
                shell.dispose();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        saveBtn = new Button(buttonComp, SWT.NONE);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                save();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button closeBtn = new Button(buttonComp, SWT.NONE);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        if (fullControls == false) {
            saveExitBtn.setVisible(false);
            saveBtn.setVisible(false);
        }
    }

    /**
     * Get the label text for the shift list control.
     * 
     * @return Label text.
     */
    private String getShiftListLabel() {
        String format = "%S       %S       %S";

        String labelStr = String
                .format(format, "Shift Date", "Value", "Active");

        return labelStr;
    }

    /**
     * Sets up the initial control config
     */
    private void populateControls() {
        floodDataLbl.setText(String.format("%7.2f", floodDbl));
        recordDataLbl.setText(String.format("%7.2f", recordDbl));
        // ------------------------------
        // Shift data list and controls
        // ------------------------------
        if (shiftData != null) {
            for (RatingCurveShiftData rcsd : shiftData) {
                String fmtStr = "%10S %10S %10S";
                String tmpStr = String.format(fmtStr, sdf.format(rcsd.getDate()
                        .getTime()), df.format(rcsd.getValue()), rcsd
                        .isActive());
                shiftDataList.add(tmpStr);
            }
        }

        // Get the date of rating and usgs rating number
        RatingCurveDataManager rcdm = new RatingCurveDataManager();
        String label = rcdm.getRatingInfo(lid);
        if (label != null) {
            ratingLbl.setText(label);
        }

    }

    /**
     * Create the rating curve data
     */
    private void createCurveData() {
        // ------------------------------
        // Flood and Record labels
        // ------------------------------
        RiverDataManager rdm = RiverDataManager.getInstance();
        RiverDataPoint rdp = rdm.getRiverDataPoint(lid);
        rdp = rdm.getRiverCrest(rdp);
        RatingCurveDataManager rcdm = new RatingCurveDataManager();

        shiftData = rcdm.getRatingCurveShift(lid);
        noShiftCurveArray = rcdm.getRatingCurve(lid);

        floodDbl = rdp.getFloodStage();
        recordDbl = rdp.getCrestValue();
    }

    /**
     * Update the rating curve with an imported one
     */
    private boolean importCurveData(
            ArrayList<RatingCurveData> newNoShiftCurveArray) {
        newRatingCurve = false;

        try {
            RiverDataManager rdm = RiverDataManager.getInstance();
            RiverDataPoint rdp = rdm.getRiverDataPoint(lid);
            rdp = rdm.getRiverCrest(rdp);

            noShiftCurveArray = newNoShiftCurveArray;

            floodDbl = rdp.getFloodStage();
            recordDbl = rdp.getCrestValue();

            if (noShiftCurveArray != null) {
                // delete and readd the list data
                noShiftCurveDataList.removeAll();
                // re add
                for (RatingCurveData curve : noShiftCurveArray) {
                    noShiftCurveDataList.add(curve.toString());
                }
                noShiftCurveDataList.redraw();
            }

            ratingCurveCanvas.updateCurveData(noShiftCurveArray, floodDbl,
                    recordDbl, shiftAmount);
            newRatingCurve = true;
        } catch (Exception e) {
            e.printStackTrace();
        }

        return newRatingCurve;
    }

    /**
     * Shift the data for the rating curve
     * 
     * @param rcsd
     */
    public void generateShiftList(RatingCurveShiftData rcsd) {

        if (rcsd != null) {
            shiftAmount = rcsd.getValue();
            shiftCurveDataList.removeAll();
            shiftCurveArray = new ArrayList<RatingCurveData>();

            // remake the rating curve with shift data
            for (RatingCurveData curve : noShiftCurveArray) {
                RatingCurveData curve2 = new RatingCurveData(curve.getStage()
                        + rcsd.getValue(), curve.getDischarge());
                shiftCurveArray.add(curve2);
                shiftCurveDataList.add(curve2.toString());
            }
            // redraw for the full effect
            shiftCurveDataList.redraw();
        }
    }

    @Override
    public String getSortType() {
        // TODO Auto-generated method stub
        return "Stage";
    }

    /**
     * Sort the crest data by stage value.
     */
    public void sortByStage() {
        sortKey = sortBy.Stage;
        sortCurveData();
    }

    /**
     * Sort the crest data by flow value.
     */
    public void sortByDischarge() {
        sortKey = sortBy.Discharge;
        sortCurveData();
    }

    /**
     * Sort the curve data.
     */
    private void sortCurveData() {
        Collections.sort(noShiftCurveArray);
    }

    /**
     * Imports a rating curve
     * 
     * @param fileName
     * @return ArrayList<RatingCurveImport>
     */
    private RatingCurveImport importRatingCurve(String fileName) {

        File file = new File(fileName);
        RatingCurveImport curve = null;

        if (file.exists()) {
            // if they choose to import entire directory
            if (file.isDirectory()) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION
                        | SWT.OK | SWT.CANCEL);
                mb.setText("Directory Chosen!");
                mb
                        .setMessage("Cannot open a directory, choose a rating curve. EX: 'NBD1.rating'");
                mb.open();
            }
            // if they choose just the file to import
            else if (file.isFile()) {
                if (fileName.endsWith(extension)) {
                    try {
                        curve = readFile(fileName);
                    } catch (VizException ve) {
                        VizApp.logAndAlert(Status.ERROR, ve,
                                "Error reading in file: " + fileName,
                                "Error reading in file: " + fileName, UiPlugin
                                        .getDefault(), UiPlugin.PLUGIN_ID);
                    }
                } else {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION
                            | SWT.OK | SWT.CANCEL);
                    mb.setText("Invalid File format Chosen!");
                    mb
                            .setMessage("Cannot open this file format, choose rating curve. EX: 'NBD1.rating'");
                    mb.open();
                }
            }
        }

        return curve;
    }

    /**
     * Import a rating curve when you need to.
     * 
     * @param fileName
     * @return
     * @throws VizException
     */
    private RatingCurveImport readFile(String fileName) throws VizException {
        RatingCurveImport rci = new RatingCurveImport(fileName, lid);

        try {
            BufferedReader br = new BufferedReader(new InputStreamReader(
                    new DataInputStream(new FileInputStream(fileName))));
            String strLine;

            // Read File Line By Line
            while ((strLine = br.readLine()) != null) {
                String[] line = strLine.split(" ");
                // should be ordered stage, flow separated by a space
                if (line.length == 2) {
                    rci.add(new Double(line[0]), new Double(line[1]));
                }
            }
            br.close();
        } catch (FileNotFoundException e) {
            throw new VizException();
        } catch (IOException e) {
            throw new VizException();
        }
        return rci;
    }

    /**
     * Save the changes you've made
     */
    private void save() {
        RatingCurveDataManager rcdm = new RatingCurveDataManager();
        // save everything you have done
        if (newRatingCurve) {
            // replace with new rating curve
            rcdm.deleteRatingCurve(lid);
            rcdm.insertRatingCurve((RatingCurveImport) noShiftCurveArray);
            newRatingCurve = false;
        }

        if (removedPoints.size() != 0) {
            for (RatingCurveData rcd : removedPoints) {
                rcdm.deleteRatingCurveData(rcd, lid);
            }
            removedPoints = new ArrayList<RatingCurveData>();
        }
        if (addedPoints.size() != 0) {
            for (RatingCurveData rcd : addedPoints) {
                rcdm.insertRatingCurveData(rcd, lid);
            }
            addedPoints = new ArrayList<RatingCurveData>();
        }
        if (removedCurveShifts.size() != 0) {
            for (RatingCurveShiftData rcsd : removedCurveShifts) {
                rcdm.deleteRatingCurveShift(rcsd);
            }
            removedCurveShifts = new ArrayList<RatingCurveShiftData>();
        }
        if (addedCurveShifts.size() != 0) {
            for (RatingCurveShiftData rcsd : addedCurveShifts) {
                rcdm.insertRatingCurveShift(rcsd);
            }
            addedCurveShifts = new ArrayList<RatingCurveShiftData>();
        }
    }

    /**
     * Verify validity of input
     * 
     * @param field
     */
    private boolean verifyDouble(Text field) {
        // verify input parameters in text fields.
        try {
            df.setMinimumIntegerDigits(1);
            df.setMaximumFractionDigits(2);
            df.setMinimumFractionDigits(1);
            df.setGroupingUsed(false);
            df.parse(field.getText().trim()).doubleValue();
        } catch (ParseException pe) {
            // fire a dialog here
            error("Invalid Stage Format, numbers or decimals required", field);
            return false;
        }
        return true;
    }

    /**
     * Verify validity of input
     * 
     * @param field
     */
    private boolean verifyInt(Text field) {
        // verify input parameters in text fields.
        try {
            df.setMinimumIntegerDigits(1);
            df.setMaximumFractionDigits(1);
            df.setMinimumFractionDigits(1);
            df.setGroupingUsed(false);
            df.parse(field.getText().trim()).intValue();
        } catch (ParseException pe) {
            // fire a dialog here
            error("Invalid Flow Format, whole numbers required", field);
            return false;
        }
        return true;
    }

    private void updateInsert() {
        try {
            if (verifyDate(shiftDateTF) && verifyDouble(shiftValueTF)) {

                Calendar cal = Calendar
                        .getInstance(TimeZone.getTimeZone("GMT"));
                cal.setTime(sdf.parse(shiftDateTF.getText()));
                RatingCurveShiftData rcsd = new RatingCurveShiftData(lid, cal,
                        new Double(shiftValueTF.getText()), shiftActiveChk
                                .getSelection());
                // remove old
                if (shiftDataList != null) {
                    int index = shiftDataList.getSelectionIndex();
                    if (index != -1) {
                        if (rcsd.getDate().getTime().equals(
                                shiftData.get(index).getDate().getTime())) {
                            shiftDataList.remove(index);
                            shiftData.remove(index);
                        }
                    } else {
                        for (int i = 0; i < shiftData.size(); i++) {
                            RatingCurveShiftData data = shiftData.get(i);
                            if (data.getDate().getTime().equals(cal.getTime())) {
                                shiftData.remove(i);
                                shiftDataList.remove(i);
                                i--;
                            }
                        }
                    }
                }

                if (!addedCurveShifts.contains(rcsd)) {
                    addedCurveShifts.add(rcsd);
                } else {
                    addedCurveShifts.remove(rcsd);
                    addedCurveShifts.add(rcsd);
                }

                shiftData.add(rcsd);
                shiftDataList.add(getShiftListString(rcsd));
                shiftDataList.redraw();

                if (shiftActiveChk.getSelection()) {
                    generateShiftList(rcsd);
                    ratingCurveCanvas.updateCurveData(shiftCurveArray,
                            floodDbl, recordDbl, shiftAmount);
                } else {
                    ratingCurveCanvas.updateCurveData(noShiftCurveArray,
                            floodDbl, recordDbl, shiftAmount);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Verify validity of input
     * 
     * @param field
     */
    private boolean verifyDate(Text field) {
        // verify input parameters in date field.
        try {
            sdf.parse(field.getText().trim());
        } catch (ParseException pe) {
            // fire a dialog here
            error("Invalid Date Format, required: dd/mm/yyyy", field);
            return false;
        }
        return true;
    }

    /**
     * Error message box
     * 
     * @param message
     */
    private void error(String message, Text field) {

        MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                | SWT.CANCEL);
        mb.setText("Failed Validation");
        mb.setMessage(message);

        mb.open();
        field.setFocus();
    }

    /**
     * Gets the clean string for the active curve shifted list
     * 
     * @param rcsd
     * @return
     */
    private String getShiftListString(RatingCurveShiftData rcsd) {

        String fmtStr = "%10S %10S %10S";
        String tmpStr = String.format(fmtStr, sdf.format(rcsd.getDate()
                .getTime()), df.format(rcsd.getValue()), rcsd.isActive());

        return tmpStr;
    }

    /**
     * get the currently edited curve shift
     * 
     * @return
     */
    private RatingCurveShiftData getEditingShiftData() {
        return selectedRatingShift;
    }

    /**
     * set the current curve shift
     * 
     * @param selectedRatingShift
     */
    private void setSelectedShift(RatingCurveShiftData selectedRatingShift) {
        this.selectedRatingShift = selectedRatingShift;
    }

    /**
     * Get the editing curve data
     * 
     * @return
     */
    private RatingCurveData getEditingCurveData() {
        return selectedRatingPoint;
    }

    /**
     * Sets the selected curve data
     * 
     * @param selectedRatingPoint
     */
    private void setSelectedCurveData(RatingCurveData selectedRatingPoint) {
        this.selectedRatingPoint = selectedRatingPoint;
    }

    /**
     * update the noShiftCurveDataList
     */
    private void remakeRatingCurveDataList() {

        sortCurveData();
        noShiftCurveDataList.removeAll();
        for (RatingCurveData rcd : noShiftCurveArray) {
            noShiftCurveDataList.add(rcd.toString());
        }
        noShiftCurveDataList.redraw();
    }

    /**
     * Get an intance of the KCFS data label
     * 
     * @return the label
     */
    protected Label getKcfsDataLbl() {
        return kcfsDataLbl;
    }

    /**
     * Get an intance of the Stage data label
     * 
     * @return the label
     */
    protected Label getStageDataLbl() {
        return stageDataLbl;
    }
}
