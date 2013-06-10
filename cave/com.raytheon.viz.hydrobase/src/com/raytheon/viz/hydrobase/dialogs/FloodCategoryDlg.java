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

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.FloodCategoryData;
import com.raytheon.viz.hydrocommon.datamanager.FloodCategoryDataManager;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Flood Category dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 4, 2008				lvenable	Initial creation
 * Nov 10, 2008 1661        askripsky   Connected to DB
 * March 3, 2011            JingtaoD    Mondification - display blank on Flood category GUI 
 * 										if the value is null in database
 * Apr 19, 2013 1790        rferrel     Make dialog non-blocking.
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class FloodCategoryDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FloodCategoryDlg.class);

    /**
     * Location whose data is displayed.
     */
    private String lid;

    /**
     * Major stage text control.
     */
    private Text majorStageTF;

    /**
     * Major discharge text control.
     */
    private Text majorDischargeTF;

    /**
     * Moderate stage text control.
     */
    private Text modStageTF;

    /**
     * Moderate discharge text control.
     */
    private Text modDischargeTF;

    /**
     * Minor stage text control.
     */
    private Text minorStageTF;

    /**
     * Minor discharge text control.
     */
    private Text minorDischargeTF;

    /**
     * constant for missing value
     */
    private static final double missingVal = -9999.0;

    /**
     * delta factor to consider 2 double values equal.
     */
    private static final double diffAllowed = 0.0001;

    /**
     * Non-blocking Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param titleInfo
     *            Dialog title information.
     */
    public FloodCategoryDlg(Shell parent, String titleInfo, String lid) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Flood Category" + titleInfo);

        this.lid = lid;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
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
        setReturnValue(lid);
        // Initialize all of the controls and layouts
        initializeComponents();

        // Load Data
        loadFloodCategoryData();
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        createCategoryGroup();
        createBottomButtons();
    }

    /**
     * Create the category group and controls.
     */
    private void createCategoryGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group categoriesGroup = new Group(shell, SWT.NONE);
        categoriesGroup.setLayout(new GridLayout(3, false));
        categoriesGroup.setLayoutData(gd);
        categoriesGroup.setText(" Categories ");

        int stageTextWidth = 80;
        int dischargeTextWidth = 160;

        // Filler label
        new Label(categoriesGroup, SWT.NONE);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label stageLbl = new Label(categoriesGroup, SWT.CENTER);
        stageLbl.setText("Stage");
        stageLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label dischargeLbl = new Label(categoriesGroup, SWT.CENTER);
        dischargeLbl.setText("Discharge");
        dischargeLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label majorLbl = new Label(categoriesGroup, SWT.RIGHT);
        majorLbl.setText("Major: ");
        majorLbl.setLayoutData(gd);

        gd = new GridData(stageTextWidth, SWT.DEFAULT);
        majorStageTF = new Text(categoriesGroup, SWT.BORDER);
        majorStageTF.setLayoutData(gd);

        gd = new GridData(dischargeTextWidth, SWT.DEFAULT);
        majorDischargeTF = new Text(categoriesGroup, SWT.BORDER);
        majorDischargeTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label moderateLbl = new Label(categoriesGroup, SWT.RIGHT);
        moderateLbl.setText("Moderate: ");
        moderateLbl.setLayoutData(gd);

        gd = new GridData(stageTextWidth, SWT.DEFAULT);
        modStageTF = new Text(categoriesGroup, SWT.BORDER);
        modStageTF.setLayoutData(gd);

        gd = new GridData(dischargeTextWidth, SWT.DEFAULT);
        modDischargeTF = new Text(categoriesGroup, SWT.BORDER);
        modDischargeTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label minorLbl = new Label(categoriesGroup, SWT.RIGHT);
        minorLbl.setText("Minor: ");
        minorLbl.setLayoutData(gd);

        gd = new GridData(stageTextWidth, SWT.DEFAULT);
        minorStageTF = new Text(categoriesGroup, SWT.BORDER);
        minorStageTF.setLayoutData(gd);

        gd = new GridData(dischargeTextWidth, SWT.DEFAULT);
        minorDischargeTF = new Text(categoriesGroup, SWT.BORDER);
        minorDischargeTF.setLayoutData(gd);
    }

    /**
     * Create the bottom buttons on the dialog.
     */
    private void createBottomButtons() {
        Composite mainButtonComp = new Composite(shell, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        Composite buttonComp = new Composite(mainButtonComp, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, false));
        buttonComp.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(90, SWT.DEFAULT);
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (saveChanges()) {
                    close();
                }
            }
        });

        gd = new GridData(90, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });

        gd = new GridData(90, SWT.DEFAULT);
        Button deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (MessageDialog.openConfirm(null, "Delete Confirmation",
                        "Do you wish to delete this entry?")) {
                    removeRecord();
                    close();
                }
            }
        });
    }

    /**
     * Verify and save changes.
     * 
     * @return true if save successful
     */
    private boolean saveChanges() {
        boolean successful = false;

        if (checkFKConstraintsMet(lid)) {
            try {
                successful = FloodCategoryDataManager.getInstance()
                        .putFloodCategoryData(lid, majorStageTF.getText(),
                                modStageTF.getText(), minorStageTF.getText(),
                                majorDischargeTF.getText(),
                                modDischargeTF.getText(),
                                minorDischargeTF.getText(), shell);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, "Save failed. ", e);
            }
        }

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
        String query = "Select lid FROM riverstat WHERE lid='" + lid + "'";

        try {
            QueryResult data = HydroDBDataManager.getInstance().runMappedQuery(
                    query);

            if (data.getResultCount() > 0) {
                rval = true;
            } else {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                mb.setMessage("Data for the location must be add via the River Gauge dialog first.");
                mb.open();
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Check constraints failed. ", e);
        }

        return rval;
    }

    private void removeRecord() {
        try {
            FloodCategoryDataManager.getInstance().deleteRecord(lid);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Delete record failed. ", e);
        }
    }

    /**
     * Get flood catagory data and update display.
     */
    private void loadFloodCategoryData() {
        try {
            FloodCategoryData data = FloodCategoryDataManager.getInstance()
                    .getFloodCategoryData(lid);

            if (Math.abs(data.getMajorStage() - missingVal) < diffAllowed) {
                majorStageTF.setText("");
            } else {
                majorStageTF.setText(Double.toString(data.getMajorStage()));
            }

            if (Math.abs(data.getModerateStage() - missingVal) < diffAllowed) {
                modStageTF.setText("");
            } else {
                modStageTF.setText(Double.toString(data.getModerateStage()));
            }

            if (Math.abs(data.getMinorStage() - missingVal) < diffAllowed) {
                minorStageTF.setText("");
            } else {
                minorStageTF.setText(Double.toString(data.getMinorStage()));
            }

            if (Math.abs(data.getMajorDischarge() - missingVal) < diffAllowed) {
                majorDischargeTF.setText("");
            } else {
                majorDischargeTF.setText(Double.toString(data
                        .getMajorDischarge()));
            }

            if (Math.abs(data.getModerateDischarge() - missingVal) < diffAllowed) {
                modDischargeTF.setText("");
            } else {
                modDischargeTF.setText(Double.toString(data
                        .getModerateDischarge()));
            }

            if (Math.abs(data.getMinorDischarge() - missingVal) < diffAllowed) {
                minorDischargeTF.setText("");
            } else {
                minorDischargeTF.setText(Double.toString(data
                        .getMinorDischarge()));
            }

        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Retrieve failed. ", e);
        }
    }
}
