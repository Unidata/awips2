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

package com.raytheon.viz.hydro.damdisplay;

import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.pointdatacontrol.PointDataControlManager;
import com.raytheon.viz.hydro.resource.DamLocationResource;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.data.DamMaster;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Dam Display Control dialog for Hydroview.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class DamDisplayControlDlg extends CaveSWTDialog {
    private final String GREATER_THAN = "greater than";

    private final String EQUAL_TO = "equal to";

    private final String LESS_THAN = "less than";

    private final String ALL = "All";

    private final String CENTER_LAT_TOKEN = "hv_center_lat";

    private final String CENTER_LON_TOKEN = "hv_center_lon";

    /**
     * Dam volume combo box.
     */
    private Combo damVolumeCbo;

    /**
     * Dam volume text control.
     */
    private Text damVolumeTF;

    /**
     * Center point latitude text control.
     */
    private Text centerPtLatTF;

    /**
     * Center point longitude text control.
     */
    private Text centerPtLonTF;

    /**
     * Offset latitude text control.
     */
    private Text offsetLatTF;

    /**
     * Offset longitude text control.
     */
    private Text offsetLonTF;

    /**
     * Enable check box.
     */
    private Button enableChk;

    /**
     * ID check box.
     */
    private Button idChk;

    /**
     * Name check box.
     */
    private Button nameChk;

    /**
     * Icon check box.
     */
    private Button iconChk;

    /**
     * Map Data button.
     */
    private Button mapDataBtn;

    /**
     * Close button.
     */
    private Button closeBtn;

    /**
     * Clear Data button.
     */
    private Button clearDataBtn;

    /** The previous where clause used */
    private String previousWhereClause = "123";

    /** The wait mouse cursor */
    private Cursor waitCursor = null;

    /** The arrow mouse cursor */
    private Cursor arrowCursor = null;

    /** Error dialog flag */
    private boolean displayErrorDialog = false;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public DamDisplayControlDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Dam Display Control");

        waitCursor = new Cursor(parent.getDisplay(), SWT.CURSOR_WAIT);
        arrowCursor = new Cursor(parent.getDisplay(), SWT.CURSOR_ARROW);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
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

        createTopLabel();
        createDamVolumeControls();
        createPointOffsetControl();
        addSeparator();
        createDisplayControls();
        addSeparator();
        createBottomButtons();
    }

    /**
     * Create the top filter by label.
     */
    private void createTopLabel() {
        Composite topLabelComp = new Composite(shell, SWT.NONE);
        RowLayout topLabelCompRl = new RowLayout();
        topLabelComp.setLayout(topLabelCompRl);

        Label filterByLbl = new Label(topLabelComp, SWT.NONE);
        filterByLbl.setText("Filter by:");
    }

    /**
     * Create the dam volume control.
     */
    private void createDamVolumeControls() {
        Composite damVolumeComp = new Composite(shell, SWT.NONE);
        GridLayout damVolumeCompGl = new GridLayout(3, false);
        damVolumeCompGl.horizontalSpacing = 10;
        damVolumeComp.setLayout(damVolumeCompGl);

        GridData gd = new GridData(110, SWT.DEFAULT);
        Label damVolumeLbl = new Label(damVolumeComp, SWT.CENTER);
        damVolumeLbl.setText("Dam Volume\n(acre-ft.)");
        damVolumeLbl.setLayoutData(gd);

        damVolumeCbo = new Combo(damVolumeComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        damVolumeCbo.add(GREATER_THAN);
        damVolumeCbo.add(EQUAL_TO);
        damVolumeCbo.add(LESS_THAN);
        damVolumeCbo.add(ALL);
        damVolumeCbo.select(0);

        gd = new GridData(80, SWT.DEFAULT);
        damVolumeTF = new Text(damVolumeComp, SWT.BORDER);
        damVolumeTF.setLayoutData(gd);
        damVolumeTF.setText("100000");
    }

    /**
     * Create the point and offset controls.
     */
    private void createPointOffsetControl() {
        Composite mainPtOffsetComp = new Composite(shell, SWT.NONE);
        GridLayout mainLayout = new GridLayout(4, false);
        mainPtOffsetComp.setLayout(mainLayout);

        // ------------------------------------------
        // Create center point controls
        // ------------------------------------------
        Composite centerPtComp = new Composite(mainPtOffsetComp, SWT.NONE);
        RowLayout centerPtLayout = new RowLayout(SWT.VERTICAL);
        centerPtComp.setLayout(centerPtLayout);

        RowData rd = new RowData(100, SWT.DEFAULT);
        Label centerPtLbl = new Label(centerPtComp, SWT.CENTER);
        centerPtLbl.setText("Center Point");
        centerPtLbl.setLayoutData(rd);

        AppsDefaults ad = AppsDefaults.getInstance();
        String centerLatTokenValue = ad.getToken(CENTER_LAT_TOKEN);
        String centerLonTokenValue = ad.getToken(CENTER_LON_TOKEN);

        rd = new RowData(90, SWT.DEFAULT);
        centerPtLatTF = new Text(centerPtComp, SWT.BORDER);
        centerPtLatTF.setLayoutData(rd);
        centerPtLatTF.setText(centerLatTokenValue);

        rd = new RowData(90, SWT.DEFAULT);
        centerPtLonTF = new Text(centerPtComp, SWT.BORDER);
        centerPtLonTF.setLayoutData(rd);
        centerPtLonTF.setText(centerLonTokenValue);

        // ------------------------------------------
        // Create Lat/Lon labels
        // ------------------------------------------
        Composite latLonComp = new Composite(mainPtOffsetComp, SWT.NONE);
        RowLayout latLonLayout = new RowLayout(SWT.VERTICAL);
        latLonLayout.spacing = 16;
        latLonLayout.marginTop = 24;
        latLonComp.setLayout(latLonLayout);

        Label latitudeLbl = new Label(latLonComp, SWT.NONE);
        latitudeLbl.setText("Latitude");

        Label longitudeLbl = new Label(latLonComp, SWT.NONE);
        longitudeLbl.setText("Longitude");

        // ------------------------------------------
        // Create offset controls
        // ------------------------------------------
        Composite offsetComp = new Composite(mainPtOffsetComp, SWT.NONE);
        RowLayout offsetLayout = new RowLayout(SWT.VERTICAL);
        offsetComp.setLayout(offsetLayout);

        rd = new RowData(70, SWT.DEFAULT);
        Label offsetLbl = new Label(offsetComp, SWT.CENTER);
        offsetLbl.setText("Offset");
        offsetLbl.setLayoutData(rd);

        rd = new RowData(70, SWT.DEFAULT);
        offsetLatTF = new Text(offsetComp, SWT.BORDER);
        offsetLatTF.setLayoutData(rd);
        offsetLatTF.setText("2.0");

        rd = new RowData(70, SWT.DEFAULT);
        offsetLonTF = new Text(offsetComp, SWT.BORDER);
        offsetLonTF.setLayoutData(rd);
        offsetLonTF.setText("2.0");

        // ------------------------------------------
        // Create enable control
        // ------------------------------------------
        Composite enableComp = new Composite(mainPtOffsetComp, SWT.NONE);
        RowLayout enableLayout = new RowLayout(SWT.VERTICAL);
        enableLayout.marginTop = 29;
        enableComp.setLayout(enableLayout);

        enableChk = new Button(enableComp, SWT.CHECK);
        enableChk.setText("Enable");
    }

    /**
     * Create the display check boxes.
     */
    private void createDisplayControls() {
        Composite displayComp = new Composite(shell, SWT.NONE);
        GridLayout displayLayout = new GridLayout(4, false);
        displayLayout.horizontalSpacing = 20;
        displayComp.setLayout(displayLayout);

        Label displayLbl = new Label(displayComp, SWT.NONE);
        displayLbl.setText("Display:");

        idChk = new Button(displayComp, SWT.CHECK);
        idChk.setText("Id");

        nameChk = new Button(displayComp, SWT.CHECK);
        nameChk.setText("Name");
        nameChk.setSelection(true);

        iconChk = new Button(displayComp, SWT.CHECK);
        iconChk.setText("Icon");
        iconChk.setSelection(true);
    }

    /**
     * Create the bottom buttons.
     */
    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        RowLayout layout = new RowLayout();
        layout.spacing = 20;
        layout.marginLeft = 28;
        buttonComp.setLayout(layout);

        RowData rd = new RowData(90, SWT.DEFAULT);
        mapDataBtn = new Button(buttonComp, SWT.PUSH);
        mapDataBtn.setText("Map Data");
        mapDataBtn.setLayoutData(rd);
        mapDataBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.setCursor(waitCursor);
                handleMapCall();
                shell.setCursor(arrowCursor);
            }
        });

        rd = new RowData(90, SWT.DEFAULT);
        closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(rd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        rd = new RowData(90, SWT.DEFAULT);
        clearDataBtn = new Button(buttonComp, SWT.PUSH);
        clearDataBtn.setText("Clear Data");
        clearDataBtn.setLayoutData(rd);
        clearDataBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                /* Remove the dam data from the map */
                HydroDisplayManager manager = HydroDisplayManager.getInstance();
                manager.setDamList(null);

                /* Notify the system that the map needs to be redrawn */
                PointDataControlManager pdcManager = PointDataControlManager
                        .getInstance();
                DamLocationResource dlr = pdcManager.getDamLocationResource();
                if (dlr != null) {
                    dlr.unload();
                    pdcManager.setRedraw(true);
                }
            }

        });
    }

    /**
     * Method to add a horizontal separator line on the display.
     */
    private void addSeparator() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 4;
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    private void handleMapCall() {
        List<DamMaster> damList;
        String where = null;
        displayErrorDialog = false;

        /* set the cursor to a watch while the request is being processed */
        shell.setCursor(waitCursor);

        /* Process the user request by reading gui options and retrieving data. */
        where = buildWhereClause();
        if (!displayErrorDialog) {
            /* if the where clause has changed then query the database */
            // if (!where.equals(previousWhereClause)) {
            previousWhereClause = where;
            DamDisplayDataManager dataManager = new DamDisplayDataManager();

            try {
                damList = dataManager.getDamMaster(where);

                HydroDisplayManager displayManager = HydroDisplayManager
                        .getInstance();
                displayManager.setDamList(damList);
                displayManager.setDisplayDamId(idChk.getSelection());
                displayManager.setDisplayDamName(nameChk.getSelection());
                displayManager.setDisplayDamIcon(iconChk.getSelection());
            } catch (VizException ve) {
                ve.printStackTrace();
                MessageDialog
                        .openError(shell, "Error Occurred",
                                "An error occurred while querying for dam catalog data.\n");
            }
            // }
        }

        shell.setCursor(arrowCursor);

        /* Notify the system that the map needs to be redrawn */
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        pdcManager.setRedraw(true);
    }

    /**
     * Build the sql where clause.
     * 
     * @return The where clause
     */
    private String buildWhereClause() {
        StringBuilder where = new StringBuilder();

        String volumeSelection = damVolumeCbo.getItem(damVolumeCbo
                .getSelectionIndex());
        String volumeStr = damVolumeTF.getText();
        if (((volumeStr == null) || (volumeStr.length() == 0))
                && !volumeSelection.equals(ALL)) {
            displayErrorDialog = true;
            MessageBox mb = new MessageBox(getParent(), SWT.ICON_WARNING
                    | SWT.OK);
            mb.setText("Error");
            mb.setMessage("A Dam Volume must be entered");
            mb.open();
            return null;
        }

        if (volumeSelection.equals(GREATER_THAN)) {
            where.append("where max_storage > " + volumeStr);
        } else if (volumeSelection.equals(EQUAL_TO)) {
            where.append("where max_storage = " + volumeStr);
        } else if (volumeSelection.equals(LESS_THAN)) {
            where.append("where max_storage < " + volumeStr);
        }

        if (enableChk.getSelection()) {
            String filterLatLon = buildLatLonFilter();
            if (filterLatLon != null) {
                if (where.toString().contains("max_storage")) {
                    where.append(" and " + filterLatLon);
                } else {
                    where.append(" where " + filterLatLon);
                }
            }
        }

        return where.toString();
    }

    /**
     * Create the lat/lon filter for the where clause.
     * 
     * @return the formated string to add to the where clause
     */
    private String buildLatLonFilter() {
        String filter = null;
        String latCenterText = centerPtLatTF.getText();
        String latOffsetText = offsetLatTF.getText();
        String lonCenterText = centerPtLonTF.getText();
        String lonOffsetText = offsetLonTF.getText();

        double latOffset = HydroConstants.MISSING_VALUE;
        double latCenter = HydroConstants.MISSING_VALUE;
        double lonOffset = HydroConstants.MISSING_VALUE;
        double lonCenter = HydroConstants.MISSING_VALUE;
        double southernLat = 0;
        double northernLat = 0;
        double easternLon = 0;
        double westernLon = 0;

        /* Make sure we have valid values entered in the text boxes */
        try {
            StringBuilder sb = new StringBuilder(
                    "Invalid value entered for Latitude Offset");
            latOffset = Double.parseDouble(latOffsetText);
            sb.setLength(0);
            sb.append("Invalid value entered for Latitude Center Point");
            latCenter = Double.parseDouble(latCenterText);
            sb.setLength(0);
            sb.append("Invalid value entered for Longitude Offset");
            lonOffset = Double.parseDouble(lonOffsetText);
            sb.setLength(0);
            sb.append("Invalid value entered for Longitude Center Point");
            lonCenter = Double.parseDouble(lonCenterText);
        } catch (NumberFormatException pe) {
            displayErrorDialog = true;
            MessageBox mb = new MessageBox(getParent(), SWT.ICON_WARNING
                    | SWT.OK);
            mb.setText("Error");
            mb.setMessage("An invalid value is entered in the ");
            mb.open();
            return null;
        }

        southernLat = latCenter - latOffset;
        northernLat = latCenter + latOffset;
        easternLon = lonCenter + lonOffset;
        westernLon = lonCenter - lonOffset;

        filter = String.format(
                " latitude_dam  >= %.2f AND latitude_dam  <= %.2f AND "
                        + " longitude_dam >= %.2f AND longitude_dam <= %.2f ",
                southernLat, northernLat, westernLon, easternLon);

        return filter;
    }
}
