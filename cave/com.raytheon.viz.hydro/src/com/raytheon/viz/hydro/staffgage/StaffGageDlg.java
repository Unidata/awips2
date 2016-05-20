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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
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
 * 09 FEB 2011  4383       lbousaidi   changed createStaffGageData
 * 15 MAR 2013  1790       rferrel     Made dialog non-blocking.
 * 05 May 2016  5483       bkowal      Fix GUI sizing issues.
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

    /*
     * Record ? labels
     */
    private Label recordStageLbl;

    private Label recordFlowLbl;

    /*
     * Major Category ? labels
     */
    private Label majorCategoryStageLbl;

    private Label majorCategoryFlowLbl;

    /*
     * Minor Category ? labels
     */
    private Label minorCategoryStageLbl;

    private Label minorCategoryFlowLbl;

    /*
     * Flood ? labels
     */
    private Label floodStageLbl;

    private Label floodFlowLbl;

    /*
     * Action ? labels
     */
    private Label actionStageLbl;

    private Label actionFlowLbl;

    /*
     * Bankfull stage label
     */
    private Label bankfullStageLbl;

    /*
     * Zero datum label
     */
    private Label zeroDatumLbl;

    /**
     * Record level label.
     */
    private Label recordLbl;

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

    @Override
    protected void initializeComponents(Shell shell) {
        createStaffGageData();

        // Initialize all of the controls and layouts
        createReferenceGroup();
        createSigStagesGroup();
        createBottomButton();

        updateTextAndLabelControls();
    }

    private void createReferenceGroup() {
        Group refGroup = new Group(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, true);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        refGroup.setLayout(gl);
        refGroup.setLayoutData(gd);
        refGroup.setText("Reference");

        Composite leftComp = new Composite(refGroup, SWT.NONE);
        gl = new GridLayout(2, false);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        leftComp.setLayout(gl);
        leftComp.setLayoutData(gd);

        /*
         * Leftmost Content.
         */
        // Name
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        Label nameLbl = new Label(leftComp, SWT.RIGHT);
        nameLbl.setText("Name");
        nameLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        nameTF = new Text(leftComp, SWT.BORDER);
        nameTF.setEditable(false);
        nameTF.setLayoutData(gd);

        // Basin
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        Label basinLbl = new Label(leftComp, SWT.RIGHT);
        basinLbl.setText("Basin");
        basinLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        basinTF = new Text(leftComp, SWT.BORDER);
        basinTF.setEditable(false);
        basinTF.setLayoutData(gd);

        // Stream
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        Label streamLbl = new Label(leftComp, SWT.RIGHT);
        streamLbl.setText("Stream");
        streamLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        streamTF = new Text(leftComp, SWT.BORDER);
        streamTF.setEditable(false);
        streamTF.setLayoutData(gd);

        // County
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        Label countyLbl = new Label(leftComp, SWT.RIGHT);
        countyLbl.setText("County");
        countyLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        countyTF = new Text(leftComp, SWT.BORDER);
        countyTF.setEditable(false);
        countyTF.setLayoutData(gd);

        // State
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        Label stateLbl = new Label(leftComp, SWT.RIGHT);
        stateLbl.setText("State");
        stateLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        stateTF = new Text(leftComp, SWT.BORDER);
        stateTF.setEditable(false);
        stateTF.setLayoutData(gd);

        Composite rightComp = new Composite(refGroup, SWT.NONE);
        gl = new GridLayout(2, false);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        rightComp.setLayout(gl);
        rightComp.setLayoutData(gd);

        /*
         * Rightmost Content.
         */
        // Lat/Lon
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        Label latLonLbl = new Label(rightComp, SWT.RIGHT);
        latLonLbl.setText("Lat/Lon");
        latLonLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        latLonTF = new Text(rightComp, SWT.BORDER);
        latLonTF.setEditable(false);
        latLonTF.setLayoutData(gd);

        // Elevation
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        Label elevationLbl = new Label(rightComp, SWT.RIGHT);
        elevationLbl.setText("Elevation");
        elevationLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        elevationTF = new Text(rightComp, SWT.BORDER);
        elevationTF.setEditable(false);
        elevationTF.setLayoutData(gd);

        // Tidal Elevation
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        Label tidalLbl = new Label(rightComp, SWT.RIGHT);
        tidalLbl.setText("Tidal Elevation");
        tidalLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        tidalTF = new Text(rightComp, SWT.BORDER);
        tidalTF.setEditable(false);
        tidalTF.setLayoutData(gd);
    }

    /**
     * Create the significant stage group.
     */
    private void createSigStagesGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group refGroup = new Group(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, true);
        refGroup.setLayout(gl);
        refGroup.setLayoutData(gd);
        refGroup.setText("Significant Stages");

        setupStageDrawingArea(refGroup);
        createStageDataAreaWireframeTable(refGroup);
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

        new StaffGageCanvasComp(parent, gageData);
    }

    /**
     * Create the container to hold the stage data.
     * 
     * @param parent
     *            Parent composite.
     */
    private void createStageDataAreaWireframeTable(Composite parent) {
        Composite stageDataComp = new Composite(parent, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.TOP, true, true);
        stageDataComp.setLayout(gl);
        stageDataComp.setLayoutData(gd);

        // Empty Row

        Composite rowsComp = new Composite(stageDataComp, SWT.NONE);
        gl = new GridLayout(3, true);
        gl.marginHeight = 0;
        gd = new GridData(SWT.FILL, SWT.TOP, true, false);
        rowsComp.setLayout(gl);
        rowsComp.setLayoutData(gd);

        // Row Headers
        Label emptyCell = new Label(rowsComp, SWT.NONE);
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        emptyCell.setLayoutData(gd);
        emptyCell.setText("");

        Label stageColumnHeaderLbl = new Label(rowsComp, SWT.NONE);
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, false);
        stageColumnHeaderLbl.setLayoutData(gd);
        stageColumnHeaderLbl.setText("Stage");

        Label flowColumnHeaderLbl = new Label(rowsComp, SWT.NONE);
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, false);
        flowColumnHeaderLbl.setLayoutData(gd);
        flowColumnHeaderLbl.setText("Flow");

        // Record
        recordLbl = new Label(rowsComp, SWT.NONE);
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        recordLbl.setLayoutData(gd);
        recordLbl.setText("Record:");

        recordStageLbl = new Label(rowsComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        recordStageLbl.setLayoutData(gd);
        recordStageLbl.setAlignment(SWT.CENTER);

        recordFlowLbl = new Label(rowsComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        recordFlowLbl.setLayoutData(gd);
        recordFlowLbl.setAlignment(SWT.CENTER);

        // Major Category
        Label majorCategoryHeaderLbl = new Label(rowsComp, SWT.NONE);
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        majorCategoryHeaderLbl.setLayoutData(gd);
        majorCategoryHeaderLbl.setText("Major Category:");

        majorCategoryStageLbl = new Label(rowsComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        majorCategoryStageLbl.setLayoutData(gd);
        majorCategoryStageLbl.setAlignment(SWT.CENTER);

        majorCategoryFlowLbl = new Label(rowsComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        majorCategoryFlowLbl.setLayoutData(gd);
        majorCategoryFlowLbl.setAlignment(SWT.CENTER);

        // Minor Category
        Label minorCategoryHeaderLbl = new Label(rowsComp, SWT.NONE);
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        minorCategoryHeaderLbl.setLayoutData(gd);
        minorCategoryHeaderLbl.setText("Minor Category:");

        minorCategoryStageLbl = new Label(rowsComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        minorCategoryStageLbl.setLayoutData(gd);
        minorCategoryStageLbl.setAlignment(SWT.CENTER);

        minorCategoryFlowLbl = new Label(rowsComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        minorCategoryFlowLbl.setLayoutData(gd);
        minorCategoryFlowLbl.setAlignment(SWT.CENTER);

        // Flood
        Label floodHeaderLbl = new Label(rowsComp, SWT.NONE);
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        floodHeaderLbl.setLayoutData(gd);
        floodHeaderLbl.setText("Flood:");

        floodStageLbl = new Label(rowsComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        floodStageLbl.setLayoutData(gd);
        floodStageLbl.setAlignment(SWT.CENTER);

        floodFlowLbl = new Label(rowsComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        floodFlowLbl.setLayoutData(gd);
        floodFlowLbl.setAlignment(SWT.CENTER);

        // Action
        Label actionHeaderLbl = new Label(rowsComp, SWT.NONE);
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        actionHeaderLbl.setLayoutData(gd);
        actionHeaderLbl.setText("Action:");

        actionStageLbl = new Label(rowsComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        actionStageLbl.setLayoutData(gd);
        actionStageLbl.setAlignment(SWT.CENTER);

        actionFlowLbl = new Label(rowsComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        actionFlowLbl.setLayoutData(gd);
        actionFlowLbl.setAlignment(SWT.CENTER);

        // Empty Row
        new Label(rowsComp, SWT.NONE);
        new Label(rowsComp, SWT.NONE);
        new Label(rowsComp, SWT.NONE);

        // Bankfull Stage
        Label bankfullStageHeaderLbl = new Label(rowsComp, SWT.NONE);
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        bankfullStageHeaderLbl.setLayoutData(gd);
        bankfullStageHeaderLbl.setText("Bankfull Stage:");

        bankfullStageLbl = new Label(rowsComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        bankfullStageLbl.setLayoutData(gd);
        bankfullStageLbl.setAlignment(SWT.CENTER);

        /* Empty Cell */
        new Label(rowsComp, SWT.NONE);

        // Zero Datum
        Label zeroDatumHeaderLbl = new Label(rowsComp, SWT.NONE);
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        zeroDatumHeaderLbl.setLayoutData(gd);
        zeroDatumHeaderLbl.setText("Zero Datum:");

        zeroDatumLbl = new Label(rowsComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        zeroDatumLbl.setLayoutData(gd);
        zeroDatumLbl.setAlignment(SWT.CENTER);

        /* Empty Cell */
        new Label(rowsComp, SWT.NONE);
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

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        gd.minimumWidth = centeredComp.getDisplay().getDPI().x;
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

        recordStageLbl.setText(gageData.getRecordStage());
        recordFlowLbl.setText(gageData.getRecordFlow());
        majorCategoryStageLbl.setText(gageData.getMajorCatStage());
        majorCategoryFlowLbl.setText(gageData.getMajorCatFlow());
        minorCategoryStageLbl.setText(gageData.getMinorCatStage());
        minorCategoryFlowLbl.setText(gageData.getMinorCatFlow());
        floodStageLbl.setText(gageData.getFloodStage());
        floodFlowLbl.setText(gageData.getFloodFlow());
        actionStageLbl.setText(gageData.getActionStage());
        actionFlowLbl.setText(gageData.getActionFlow());

        bankfullStageLbl.setText(gageData.getBankfullStage());
        zeroDatumLbl.setText(gageData.getZeroDatum());
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
