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
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.ohd.AppsDefaults;
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
 * 26 Jul 2012  14711/963   mpduff      Fix problems adding/removing shift points
 * 22 Jan 2013  15682       lbousaidi   fix openfile problem and changed the path to
 *                                      whfs_import_dir for "Import Curve" button.
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class RatingCurveDlg extends CaveSWTDialog {

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
            	removeShift();
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
            	final String tokenizedDir = "whfs_import_dir";                
                String importRating= AppsDefaults.getInstance().getToken(tokenizedDir);
                FileDialog fd = new FileDialog(shell, SWT.OPEN);
                fd.setFilterPath(importRating);
                String[] filterExt = { "*." + extension };
                fd.setFilterExtensions(filterExt);
                String filename = fd.open();
                if (filename == null) {
                    return;
                } else {                   
                    importCurveData(importRatingCurve(filename));
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
                MessageBox messageDialog = new MessageBox(shell, SWT.OK | SWT.CANCEL);
                messageDialog.setText("Clear Confirmation");
                messageDialog.setMessage("This will clear the list for " + lid + ".");
                int response = messageDialog.open();

                if (response == SWT.OK) {
	                // get rid of every point
	                removedPoints = noShiftCurveArray;
	                noShiftCurveArray.clear();
	                noShiftCurveDataList.removeAll();
	                noShiftCurveDataList.redraw();
	                
	                if (shiftCurveArray != null) {
	                    shiftCurveArray.clear();
	                }
	                shiftCurveDataList.removeAll();
	                shiftCurveDataList.redraw();
	
	                stageTF.setText("");
	                dischargeTF.setText("");
	                selectedRatingShift = null;
	
	                ratingCurveCanvas.updateCurveData(noShiftCurveArray, floodDbl,
	                        recordDbl, shiftAmount);
                }
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
					MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION
							| SWT.OK | SWT.CANCEL);
					mb.setText("Remove Base Rating Point Confirmation");
					mb.setMessage("This will remove the highlighted pair.");
					int response = mb.open();

					if (response == SWT.OK) {
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
                	insertBaseCurvePoint(rcd);
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
    
    private void insertBaseCurvePoint(RatingCurveData rcd) {
    	if (!noShiftCurveArray.contains(rcd)) {
    		// Check for a matching stage value
    		RatingCurveData data = null;
    		for (RatingCurveData d: noShiftCurveArray) {
    			if (d.getStage() == rcd.getStage()) {
    				data = d;
    				break;
    			}
    		}
    			
    		if (data != null) {
    			noShiftCurveArray.remove(data);
    		}

    		noShiftCurveArray.add(rcd);
            if (!addedPoints.contains(rcd)) {
                addedPoints.add(rcd);
            } else {
                addedPoints.remove(rcd);
                addedPoints.add(rcd);
            }

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
        shiftCurveDataList = new List(rightComp, SWT.BORDER | SWT.V_SCROLL);
        shiftCurveDataList.setFont(controlFont);
        shiftCurveDataList.setLayoutData(gd);
        shiftCurveDataList.deselectAll();
        shiftCurveDataList.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_GRAY));
        shiftCurveDataList.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				shiftCurveDataList.deselectAll();
			}        	
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        ratingLbl = new Label(rightComp, SWT.CENTER);
        ratingLbl.setText(ratingLblText);
        ratingLbl.setLayoutData(gd);

        gd = new GridData(220, 130);
        noShiftCurveDataList = new List(rightComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        noShiftCurveDataList.setFont(controlFont);
        noShiftCurveDataList.setLayoutData(gd);
        noShiftCurveDataList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                RatingCurveData data = noShiftCurveArray
                        .get(noShiftCurveDataList.getSelectionIndex());
                stageTF.setText(String.format("%7.2f", data.getStage()));
                dischargeTF
                        .setText(String.format("%7.1f", data.getDischarge()));
            }
        });

        if (noShiftCurveArray != null) {
            // populate the list
        	RatingCurveShiftData currentShift = null;
        	if (shiftData != null && shiftData.size() > 0) {
	            if (shiftData.get(0).isActive()) {
		        	currentShift = shiftData.get(0);
	            }
        	}
        	
            for (RatingCurveData curve : noShiftCurveArray) {
                noShiftCurveDataList.add(curve.toString());
            }
            if (noShiftCurveDataList.getItemCount() > 0) {
            	noShiftCurveDataList.select(0);
            	generateShiftList(currentShift);
            }
        }

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
        
        if (noShiftCurveArray.size() > 0) {
	        RatingCurveData rcd = noShiftCurveArray.get(0);
	        this.stageTF.setText(String.valueOf(rcd.getStage()));
	        this.dischargeTF.setText(String.valueOf(rcd.getDischarge()));
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
        shiftCurveDataList.removeAll();

        if (rcsd != null) {
            shiftAmount = rcsd.getValue();
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
        } else {
        	// make the rating curve with no shift data
            for (RatingCurveData curve : noShiftCurveArray) {
                shiftCurveDataList.add(curve.toString());
            }
        }
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
                
                if (shiftData.size() > 0 && shiftData.contains(rcsd)) {
	                for (RatingCurveShiftData sd: shiftData) {
	                	if (rcsd.toString().equals(sd.toString())) {
	                		sd.setActive(rcsd.isActive());
	                		sd.setDate(rcsd.getDate());
	                		sd.setLid(rcsd.getLid());
	                		sd.setValue(rcsd.getValue());
	                		break;
	                	}
	                }
                } else {
                	shiftData.add(rcsd);
                }
                
                if (!addedCurveShifts.contains(rcsd)) {
                	addedCurveShifts.add(rcsd);
                } else {
                	addedCurveShifts.remove(rcsd);
                	addedCurveShifts.add(rcsd);
                }

                shiftDataList.removeAll();
                Collections.sort(shiftData);

                for (RatingCurveShiftData sd: shiftData) {
                	shiftDataList.add(getShiftListString(sd));
                }

                // Display the latest shift
                RatingCurveShiftData currentShift = shiftData.get(0);
                if (currentShift.isActive()) {
                	generateShiftList(currentShift);
                	ratingCurveCanvas.updateCurveData(shiftCurveArray,
                			floodDbl, recordDbl, currentShift.getValue());
                } else {
                	ratingCurveCanvas.updateCurveData(noShiftCurveArray,
                			floodDbl, recordDbl, 0);
                }
            }
            
            shiftValueTF.setText("");
            shiftDateTF.setText("");
            shiftActiveChk.setSelection(false);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    private void removeShift() {
    	if (shiftDataList.getItemCount() > 0 && shiftDataList.getSelectionCount() > 0) {
            MessageBox messageDialog = new MessageBox(shell, SWT.OK | SWT.CANCEL);
            messageDialog.setText("Shift Remove Confirmation");
            messageDialog.setMessage("This will remove the highlighted shift.");
            int response = messageDialog.open();

            if (response == SWT.OK) {
            	String selection = shiftDataList.getItem(shiftDataList.getSelectionIndex());
            	for (RatingCurveShiftData sd: shiftData) {
            		if (getShiftListString(sd).equals(selection)) {
            			removedCurveShifts.add(sd);
            			break;
            		}
            	}
            	
            	shiftData.removeAll(removedCurveShifts);
                shiftDataList.removeAll();
                Collections.sort(shiftData);
                for (RatingCurveShiftData rcsd : shiftData) {
                    shiftDataList.add(getShiftListString(rcsd));
                }
                shiftDataList.redraw();

                if (shiftData.size() > 0) {
                	shiftAmount = shiftData.get(0).getValue();
                } else {
                	shiftAmount = 0;
                }
                
            	ratingCurveCanvas.updateCurveData(noShiftCurveArray,
            			floodDbl, recordDbl, shiftAmount);
            	
            	if (shiftData.size() > 0) {
            		RatingCurveShiftData currentShift = shiftData.get(0);
            		if (currentShift.isActive()) {
            			generateShiftList(currentShift);
            		} else {
            			generateShiftList(null);
            		}
            	} else {
            		generateShiftList(null);
            	}
            }
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
     * update the noShiftCurveDataList
     */
    private void remakeRatingCurveDataList() {
    	Collections.sort(noShiftCurveArray);
    	int index = noShiftCurveDataList.getSelectionIndex();
        noShiftCurveDataList.removeAll();
        shiftCurveDataList.removeAll();
        for (RatingCurveData rcd : noShiftCurveArray) {
            noShiftCurveDataList.add(rcd.toString());
        }
        
        if (shiftData.size() > 0) {
	        RatingCurveShiftData currentShift = shiftData.get(0);
	        if (currentShift.isActive()) {
	        	generateShiftList(currentShift);
	        } else {
	            generateShiftList(null);
	        }
        } else {
            generateShiftList(null);
        }
        
        if (noShiftCurveDataList.getItemCount() > 0) {
        	if (index >= noShiftCurveDataList.getItemCount()) {
        		noShiftCurveDataList.select(noShiftCurveDataList.getItemCount() - 1);
        	} else if (index >= 0 && index < noShiftCurveArray.size()) {
        		noShiftCurveDataList.select(index);
        	} else {
        		noShiftCurveDataList.select(0);
        	}
    		noShiftCurveDataList.showSelection();

        }
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
