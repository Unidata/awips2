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

package com.raytheon.viz.hydro.staffgage;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.data.RiverDataPoint;
import com.raytheon.viz.hydrocommon.datamanager.RiverDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the About dialog for Hydroview.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation
 * 09 FEB 2011  4383	   lbousaidi   changed createStaffGageData
 * 15 MAR 2013  1790       rferrel     Made dialog non-blocking.
 * 
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class StaffGageDlg extends CaveSWTDialog {

    /**
     * Font used with text controls.
     */
    private Font textFont;

    /**
     * City name text control.
     */
    private Text nameTF;

    /**
     * Latitude/Longitude text control.
     */
    private Text latLonTF;

    /**
     * Basin text control.
     */
    private Text basinTF;

    /**
     * Elevation text control.
     */
    private Text elevationTF;

    /**
     * Stream text control.
     */
    private Text streamTF;

    /**
     * Tidal elevation text control.
     */
    private Text tidalTF;

    /**
     * County text control.
     */
    private Text countyTF;

    /**
     * State text control.
     */
    private Text stateTF;

    /**
     * Record level label.
     */
    private Label recordLbl;

    /**
     * Record level text control.
     */
    private Text recordTF;

    /**
     * Major category text control.
     */
    private Text majorCatTF;

    /**
     * Moderate category text control.
     */
    private Text modCatTF;

    /**
     * Minor category text control.
     */
    private Text minorCatTF;

    /**
     * Flood text control.
     */
    private Text floodTF;

    /**
     * Action text control.
     */
    private Text actionTF;

    /**
     * Bank full text control.
     */
    private Text bankfullTF;

    /**
     * Zero datum text control.
     */
    private Text zeroDatumTF;

    /**
     * Record date string.
     */
    private String recordDate = "";

    /**
     * Gage data class.
     */
    private StaffGageData gageData;

    /** Date Formatter. */
    private SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy");

    /** Value for missing staff gage data. */
    public static String MISSING = "0";

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param titleName
     *            Dialog title information.
     */
    public StaffGageDlg(Shell parent, String titleName) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Staff Gage for (" + titleName + ")");
        setReturnValue(titleName);
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
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        mainLayout.verticalSpacing = 10;
        mainLayout.marginTop = 5;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        textFont.dispose();
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
        textFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createStaffGageData();

        // Initialize all of the controls and layouts
        createReferenceGroup();
        createSigStagesGroup();
        createBottomButton();

        updateTextAndLabelControls();
    }

    /**
     * Create the reference group and controls.
     */
    private void createReferenceGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group refGroup = new Group(shell, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        gl.horizontalSpacing = 10;
        refGroup.setLayout(gl);
        refGroup.setLayoutData(gd);
        refGroup.setText(" Reference ");

        int leftLblWidth = 80;
        int leftTfWidth = 260;
        int rightLblWidth = 160;
        int rightTfWidth = 270;

        gd = new GridData(leftLblWidth, SWT.DEFAULT);
        Label nameLbl = new Label(refGroup, SWT.RIGHT);
        nameLbl.setText("Name");
        nameLbl.setLayoutData(gd);

        gd = new GridData(leftTfWidth, SWT.DEFAULT);
        nameTF = new Text(refGroup, SWT.BORDER);
        nameTF.setEditable(false);
        nameTF.setLayoutData(gd);
        nameTF.setFont(textFont);

        gd = new GridData(rightLblWidth, SWT.DEFAULT);
        Label latLonLbl = new Label(refGroup, SWT.RIGHT);
        latLonLbl.setText("Lat/Lon");
        latLonLbl.setLayoutData(gd);

        gd = new GridData(rightTfWidth, SWT.DEFAULT);
        latLonTF = new Text(refGroup, SWT.BORDER);
        latLonTF.setEditable(false);
        latLonTF.setLayoutData(gd);
        latLonTF.setFont(textFont);

        gd = new GridData(leftLblWidth, SWT.DEFAULT);
        Label basinLbl = new Label(refGroup, SWT.RIGHT);
        basinLbl.setText("Basin");
        basinLbl.setLayoutData(gd);

        gd = new GridData(leftTfWidth, SWT.DEFAULT);
        basinTF = new Text(refGroup, SWT.BORDER);
        basinTF.setEditable(false);
        basinTF.setLayoutData(gd);
        basinTF.setFont(textFont);

        gd = new GridData(rightLblWidth, SWT.DEFAULT);
        Label elevationLbl = new Label(refGroup, SWT.RIGHT);
        elevationLbl.setText("Elevation");
        elevationLbl.setLayoutData(gd);

        gd = new GridData(rightTfWidth, SWT.DEFAULT);
        elevationTF = new Text(refGroup, SWT.BORDER);
        elevationTF.setEditable(false);
        elevationTF.setLayoutData(gd);
        elevationTF.setFont(textFont);

        gd = new GridData(leftLblWidth, SWT.DEFAULT);
        Label streamLbl = new Label(refGroup, SWT.RIGHT);
        streamLbl.setText("Stream");
        streamLbl.setLayoutData(gd);

        gd = new GridData(leftTfWidth, SWT.DEFAULT);
        streamTF = new Text(refGroup, SWT.BORDER);
        streamTF.setEditable(false);
        streamTF.setLayoutData(gd);
        streamTF.setFont(textFont);

        gd = new GridData(rightLblWidth, SWT.DEFAULT);
        Label tidalLbl = new Label(refGroup, SWT.RIGHT);
        tidalLbl.setText("Tidal Elevation");
        tidalLbl.setLayoutData(gd);

        gd = new GridData(rightTfWidth, SWT.DEFAULT);
        tidalTF = new Text(refGroup, SWT.BORDER);
        tidalTF.setEditable(false);
        tidalTF.setLayoutData(gd);
        tidalTF.setFont(textFont);

        gd = new GridData(leftLblWidth, SWT.DEFAULT);
        Label countyLbl = new Label(refGroup, SWT.RIGHT);
        countyLbl.setText("County");
        countyLbl.setLayoutData(gd);

        gd = new GridData(leftTfWidth, SWT.DEFAULT);
        countyTF = new Text(refGroup, SWT.BORDER);
        countyTF.setEditable(false);
        countyTF.setLayoutData(gd);
        countyTF.setFont(textFont);

        // Filler/Spacer for cell 3 & 4 of the GridLayout
        // on the same row of the county information.
        new Label(refGroup, SWT.RIGHT).setText("");
        new Label(refGroup, SWT.RIGHT).setText("");

        gd = new GridData(leftLblWidth, SWT.DEFAULT);
        Label stateLbl = new Label(refGroup, SWT.RIGHT);
        stateLbl.setText("State");
        stateLbl.setLayoutData(gd);

        gd = new GridData(leftTfWidth, SWT.DEFAULT);
        stateTF = new Text(refGroup, SWT.BORDER);
        stateTF.setEditable(false);
        stateTF.setLayoutData(gd);
        stateTF.setFont(textFont);
    }

    /**
     * Create the significant stage group.
     */
    private void createSigStagesGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group refGroup = new Group(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 10;
        refGroup.setLayout(gl);
        refGroup.setLayoutData(gd);
        refGroup.setText(" Significant Stages ");

        setupStageDrawingArea(refGroup);
        createStageDataArea(refGroup);
    }

    /**
     * Setup the canvas drawing area composite.
     * 
     * @param parent
     *            Parent composite.
     */
    private void setupStageDrawingArea(Composite parent) {
        // ***************************************************************
        // NOTE:
        // The code on how to draw the staff gage is located in the
        // staffgage_show.c file. It has all of the calculations, etc.
        // ***************************************************************

        Composite canvasComp = new Composite(parent, SWT.NONE);
        canvasComp.setLayout(new GridLayout(1, false));

        // staffGageCanvas = new StaffGageCanvasComp(parent, gageData);
        new StaffGageCanvasComp(parent, gageData);
    }

    /**
     * Create the container to hold the stage data.
     * 
     * @param parent
     *            Parent composite.
     */
    private void createStageDataArea(Composite parent) {
        GridData mainGD = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        Composite stageDataComp = new Composite(parent, SWT.NONE);
        GridLayout stageDataGl = new GridLayout(2, false);
        stageDataComp.setLayout(stageDataGl);
        stageDataComp.setLayoutData(mainGD);

        int leftSideWidth = 200;
        int rightSideLrgWidth = 180;
        int rightSideSmWidth = 80;

        // Filler/Spacer for 1st cell top row
        new Label(stageDataComp, SWT.RIGHT).setText("");

        GridData gd = new GridData(rightSideLrgWidth, SWT.DEFAULT);
        Label stageFlowLbl = new Label(stageDataComp, SWT.CENTER);
        stageFlowLbl.setText("Stage               Flow");
        stageFlowLbl.setLayoutData(gd);

        gd = new GridData(leftSideWidth, SWT.DEFAULT);
        recordLbl = new Label(stageDataComp, SWT.RIGHT);
        recordLbl.setText("Record " + recordDate + ":");
        recordLbl.setLayoutData(gd);

        gd = new GridData(rightSideLrgWidth, SWT.DEFAULT);
        recordTF = new Text(stageDataComp, SWT.BORDER);
        recordTF.setFont(textFont);
        recordTF.setEditable(false);
        recordTF.setLayoutData(gd);

        gd = new GridData(leftSideWidth, SWT.DEFAULT);
        Label majorCatLbl = new Label(stageDataComp, SWT.RIGHT);
        majorCatLbl.setText("Major Category:");
        majorCatLbl.setLayoutData(gd);

        gd = new GridData(rightSideLrgWidth, SWT.DEFAULT);
        majorCatTF = new Text(stageDataComp, SWT.BORDER);
        majorCatTF.setFont(textFont);
        majorCatTF.setEditable(false);
        majorCatTF.setLayoutData(gd);

        gd = new GridData(leftSideWidth, SWT.DEFAULT);
        Label modCatLbl = new Label(stageDataComp, SWT.RIGHT);
        modCatLbl.setText("Moderate Category:");
        modCatLbl.setLayoutData(gd);

        gd = new GridData(rightSideLrgWidth, SWT.DEFAULT);
        modCatTF = new Text(stageDataComp, SWT.BORDER);
        modCatTF.setFont(textFont);
        modCatTF.setEditable(false);
        modCatTF.setLayoutData(gd);

        gd = new GridData(leftSideWidth, SWT.DEFAULT);
        Label minorCatLbl = new Label(stageDataComp, SWT.RIGHT);
        minorCatLbl.setText("Minor Category:");
        minorCatLbl.setLayoutData(gd);

        gd = new GridData(rightSideLrgWidth, SWT.DEFAULT);
        minorCatTF = new Text(stageDataComp, SWT.BORDER);
        minorCatTF.setFont(textFont);
        minorCatTF.setEditable(false);
        minorCatTF.setLayoutData(gd);

        gd = new GridData(leftSideWidth, SWT.DEFAULT);
        Label floodLbl = new Label(stageDataComp, SWT.RIGHT);
        floodLbl.setText("Flood:");
        floodLbl.setLayoutData(gd);

        gd = new GridData(rightSideLrgWidth, SWT.DEFAULT);
        floodTF = new Text(stageDataComp, SWT.BORDER);
        floodTF.setFont(textFont);
        floodTF.setEditable(false);
        floodTF.setLayoutData(gd);

        gd = new GridData(leftSideWidth, SWT.DEFAULT);
        Label actionLbl = new Label(stageDataComp, SWT.RIGHT);
        actionLbl.setText("Action:");
        actionLbl.setLayoutData(gd);

        gd = new GridData(rightSideLrgWidth, SWT.DEFAULT);
        actionTF = new Text(stageDataComp, SWT.BORDER);
        actionTF.setFont(textFont);
        actionTF.setEditable(false);
        actionTF.setLayoutData(gd);

        gd = new GridData(leftSideWidth, 20);
        gd.horizontalSpan = 2;
        Label filler = new Label(stageDataComp, SWT.RIGHT);
        filler.setLayoutData(gd);

        gd = new GridData(leftSideWidth, SWT.DEFAULT);
        Label bankfullLbl = new Label(stageDataComp, SWT.RIGHT);
        bankfullLbl.setText("Bankfull Stage:");
        bankfullLbl.setLayoutData(gd);

        gd = new GridData(rightSideSmWidth, SWT.DEFAULT);
        bankfullTF = new Text(stageDataComp, SWT.BORDER | SWT.RIGHT);
        bankfullTF.setFont(textFont);
        bankfullTF.setEditable(false);
        bankfullTF.setLayoutData(gd);

        gd = new GridData(leftSideWidth, SWT.DEFAULT);
        Label zeroDatumLbl = new Label(stageDataComp, SWT.RIGHT);
        zeroDatumLbl.setText("Zero Datum:");
        zeroDatumLbl.setLayoutData(gd);

        gd = new GridData(rightSideSmWidth, SWT.DEFAULT);
        zeroDatumTF = new Text(stageDataComp, SWT.BORDER | SWT.RIGHT);
        zeroDatumTF.setFont(textFont);
        zeroDatumTF.setEditable(false);
        zeroDatumTF.setLayoutData(gd);
    }

    /**
     * Create the Close button located at the bottom of the dialog.
     */
    private void createBottomButton() {
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        centeredComp.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        centeredComp.setLayoutData(gd);

        gd = new GridData(70, SWT.DEFAULT);
        Button closeBtn = new Button(centeredComp, SWT.NONE);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Update the record label with the proper date.
     * 
     * @param date
     *            Record date.
     */
    private void updateRecordLabel(String date) {
        recordLbl.setText("Record " + date + ":");
    }

    /**
     * Update the text and label control with the staff gage data.
     */
    private void updateTextAndLabelControls() {
        updateRecordLabel(gageData.getRecordDate());

        nameTF.setText(gageData.getName());
        latLonTF.setText(gageData.getLatLon());
        basinTF.setText(gageData.getBasin());
        elevationTF.setText(gageData.getElevation());
        if (gageData.getStream() != null) {
            streamTF.setText(gageData.getStream());
        }
        if (gageData.getTidal() != null) {
            tidalTF.setText(gageData.getTidal());
        }
        if (gageData.getCounty() != null) {
            countyTF.setText(gageData.getCounty());
        }
        if (gageData.getState() != null) {
            stateTF.setText(gageData.getState());
        }

        String fmtStr = "%8S %11S";

        recordTF.setText(String.format(fmtStr, gageData.getRecordStage(),
                gageData.getRecordFlow()));
        majorCatTF.setText(String.format(fmtStr, gageData.getMajorCatStage(),
                gageData.getMajorCatFlow()));
        modCatTF.setText(String.format(fmtStr, gageData.getModCatStage(),
                gageData.getModCatFlow()));
        minorCatTF.setText(String.format(fmtStr, gageData.getMinorCatStage(),
                gageData.getMinorCatFlow()));
        floodTF.setText(String.format(fmtStr, gageData.getFloodStage(),
                gageData.getFloodFlow()));
        actionTF.setText(String.format(fmtStr, gageData.getActionStage(),
                gageData.getActionFlow()));
        bankfullTF.setText(gageData.getBankfullStage());
        zeroDatumTF.setText(gageData.getZeroDatum());
    }

    /**
     * Gets the Staff Gage data
     */
    private void createStaffGageData() {

        RiverDataManager rdm = RiverDataManager.getInstance();
        DecimalFormat df = new DecimalFormat();

        df.setMinimumIntegerDigits(1);
        df.setMinimumFractionDigits(2);
        df.setGroupingUsed(false);
        df.setDecimalSeparatorAlwaysShown(false);

        gageData = new StaffGageData();

        // get Location information
        String lid = HydroDisplayManager.getInstance().getCurrentLid();
        RiverDataPoint rdp = rdm.getRiverDataPoint(lid);

        if (rdp != null) {
            gageData.setName(rdp.getLocName());
            if (rdp.getlat() != HydroConstants.MISSING_VALUE
                    && rdp.getlon() != HydroConstants.MISSING_VALUE) {
                gageData.setLatLon(df.format(rdp.getlat()) + " / "
                        + df.format(rdp.getlon()));
            }
            if (rdp.getRiverName() != null) {
                gageData.setBasin(rdp.getRiverName());
            }
            if (rdp.getElevation() != HydroConstants.MISSING_VALUE) {
                gageData.setElevation(df.format(rdp.getElevation()));
            }
            gageData.setStream(rdp.getStreamName());
            gageData.setTidal(rdp.getTide());
            gageData.setCounty(rdp.getCounty());
            gageData.setState(rdp.getState());
            if (rdp.getCrestTime() != null) {
                gageData.setRecordDate(sdf.format(rdp.getCrestTime().getTime()));
            }
            if (rdp.getCrestValue() != HydroConstants.MISSING_VALUE) {
                gageData.setRecordStage(df.format(rdp.getCrestValue()));
            }
            if (rdp.getCrestFlow() != HydroConstants.MISSING_VALUE) {
                gageData.setRecordFlow(df.format(rdp.getCrestFlow()));
            }
            if (rdp.getMajorStage() != HydroConstants.MISSING_VALUE) {
                gageData.setMajorCatStage(df.format(rdp.getMajorStage()));
            }
            if (rdp.getMajorFlow() != HydroConstants.MISSING_VALUE) {
                gageData.setMajorCatFlow(df.format(rdp.getMajorFlow()));
            }
            if (rdp.getModerateStage() != HydroConstants.MISSING_VALUE) {
                gageData.setModCatStage(df.format(rdp.getModerateStage()));
            }
            if (rdp.getModerateFlow() != HydroConstants.MISSING_VALUE) {
                gageData.setModCatFlow(df.format(rdp.getModerateFlow()));
            }
            if (rdp.getMinorStage() != HydroConstants.MISSING_VALUE) {
                gageData.setMinorCatStage(df.format(rdp.getMinorStage()));
            }
            if (rdp.getMinorFlow() != HydroConstants.MISSING_VALUE) {
                gageData.setMinorCatFlow(df.format(rdp.getMinorFlow()));
            }
            if (rdp.getFloodStage() != HydroConstants.MISSING_VALUE) {
                gageData.setFloodStage(df.format(rdp.getFloodStage()));
            }
            if (rdp.getFloodFlow() != HydroConstants.MISSING_VALUE) {
                gageData.setFloodFlow(df.format(rdp.getFloodFlow()));
            }
            if (rdp.getActionStage() != HydroConstants.MISSING_VALUE) {
                gageData.setActionStage(df.format(rdp.getActionStage()));
            }
            if (rdp.getActionFlow() != HydroConstants.MISSING_VALUE) {
                gageData.setActionFlow(df.format(rdp.getActionFlow()));
            }
            if (rdp.getBankFull() != HydroConstants.MISSING_VALUE) {
                gageData.setBankfullStage(df.format(rdp.getBankFull()));
            }
            if (rdp.getZeroDatum() != HydroConstants.MISSING_VALUE) {
                gageData.setZeroDatum(df.format(rdp.getZeroDatum()));
            }
        }
    }
}
