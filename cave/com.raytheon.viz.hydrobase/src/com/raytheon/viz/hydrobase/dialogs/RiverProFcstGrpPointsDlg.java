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
import java.util.HashMap;

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
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.RPFFcstGroupData;
import com.raytheon.viz.hydrocommon.data.RPFFcstPointData;
import com.raytheon.viz.hydrocommon.datamanager.AddModifyLocationDataManager;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the River Forecast Group Points dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 8, 2008				lvenable	Initial creation
 * Dec 20, 2008 1802        askripsk    Connect to database
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class RiverProFcstGrpPointsDlg extends CaveSWTDialog {

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Group list control.
     */
    private List groupList;

    /**
     * Group ID text control.
     */
    private Text groupIdTF;

    /**
     * Name text control.
     */
    private Text nameTF;

    /**
     * Order text control.
     */
    private Text orderTF;

    /**
     * Recommend all check box.
     */
    private Button recommAllChk;

    /**
     * Apply forecast group button.
     */
    private Button applyfcstGrpBtn;

    /**
     * Forecast point list control.
     */
    private List fcstPointList;

    /**
     * Order button at the bottom of the display.
     */
    private Text bottomOrderTF;

    /**
     * Description of forecast points
     */
    private Label fcstPntLbl;

    /**
     * Column headings for forecast points
     */
    private Label fcstListLbl;

    /**
     * Primary button.
     */
    private Button primaryBtn;

    /**
     * Non-primary button.
     */
    private Button nonPrimaryBtn;

    /**
     * Stage/Flow text control.
     */
    private Text stageFlowTF;

    /**
     * Primary HSA list control.
     */
    private List primaryHsaList;

    /**
     * Secondary HSA list control.
     */
    private List secondaryHsaList;

    /**
     * Back hours text control.
     */
    private Text backHrsTF;

    /**
     * Forward hours text control.
     */
    private Text fwdHrsTF;

    /**
     * VTEC Adjust end hours text control.
     */
    private Text vtecAdjEndHrsTF;

    /**
     * Apply forecast point button.
     */
    private Button applyFcstPntBtn;

    /**
     * Add group button.
     */
    private Button addGroupBtn;

    /**
     * Delete group button.
     */
    private Button deleteGroupBtn;

    /**
     * The group info
     */
    private ArrayList<RPFFcstGroupData> groupData;

    /**
     * The point info for each group
     */
    private HashMap<String, ArrayList<RPFFcstPointData>> pointData;

    /**
     * States for the dialog
     */
    private enum DialogStates {
        NEW_GROUP, NO_POINTS, NO_GROUPS, GROUPS_AVAILABLE, POINTS_AVAILABLE
    }

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public RiverProFcstGrpPointsDlg(Shell parent) {
        super(parent);
        setText("RiverPro Forecast Groups/Points");
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
        setReturnValue(false);

        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createGroupListControls();

        createSelectedForecastGroup();

        // Add a separator bar
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        createBottomButtons();

        loadStaticData();

        getDialogData();
    }

    /**
     * Create the Group label and list control.
     */
    private void createGroupListControls() {
        Composite listComp = new Composite(shell, SWT.NONE);
        listComp.setLayout(new GridLayout(1, false));

        GridData gd = new GridData();
        gd.horizontalIndent = 4;
        Label groupListLbl = new Label(listComp, SWT.NONE);
        groupListLbl.setText(getGroupListLblText());
        groupListLbl.setFont(controlFont);
        groupListLbl.setLayoutData(gd);

        gd = new GridData(850, 150);
        groupList = new List(listComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        groupList.setLayoutData(gd);
        groupList.setFont(controlFont);
        groupList.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                updateGroupInformation();
            }
        });
    }

    /**
     * Create the selected forecast group and controls.
     */
    private void createSelectedForecastGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group selectedFcstGroup = new Group(shell, SWT.NONE);
        selectedFcstGroup.setLayout(new GridLayout(1, false));
        selectedFcstGroup.setLayoutData(gd);
        selectedFcstGroup.setText(" Selected Forecast Group ");

        // ------------------------------------------------------
        // Group ID controls (top)
        // ------------------------------------------------------
        Composite topComp = new Composite(selectedFcstGroup, SWT.NONE);
        topComp.setLayout(new GridLayout(5, false));

        Label groupIdLbl = new Label(topComp, SWT.NONE);
        groupIdLbl.setText("Group Id:");

        Label nameLbl = new Label(topComp, SWT.NONE);
        nameLbl.setText("Name:");

        gd = new GridData();
        gd.horizontalSpan = 3;
        Label orderLbl = new Label(topComp, SWT.NONE);
        orderLbl.setText("Order:");
        orderLbl.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        groupIdTF = new Text(topComp, SWT.BORDER);
        groupIdTF.setLayoutData(gd);
        groupIdTF.setTextLimit(8);

        gd = new GridData(300, SWT.DEFAULT);
        nameTF = new Text(topComp, SWT.BORDER);
        nameTF.setLayoutData(gd);
        nameTF.setTextLimit(32);

        gd = new GridData(80, SWT.DEFAULT);
        orderTF = new Text(topComp, SWT.BORDER);
        orderTF.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalIndent = 20;
        recommAllChk = new Button(topComp, SWT.CHECK);
        recommAllChk.setText("Recomm All");
        recommAllChk.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalIndent = 20;
        applyfcstGrpBtn = new Button(topComp, SWT.PUSH);
        applyfcstGrpBtn.setText("Apply FcstGroup");
        applyfcstGrpBtn.setLayoutData(gd);
        applyfcstGrpBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                saveGroupRecord();
            }
        });

        // ------------------------------------------------------
        // Add a separator bar
        // ------------------------------------------------------
        gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(selectedFcstGroup, SWT.SEPARATOR
                | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        // ------------------------------------------------------
        // Forecast Points controls (middle)
        // ------------------------------------------------------
        Composite middleComp = new Composite(selectedFcstGroup, SWT.NONE);
        middleComp.setLayout(new GridLayout(1, false));

        fcstPntLbl = new Label(middleComp, SWT.NONE);
        fcstPntLbl.setText("Forecast Points within Forecast Group");

        gd = new GridData();
        gd.horizontalIndent = 4;
        fcstListLbl = new Label(middleComp, SWT.NONE);
        fcstListLbl.setText(getFcstListLblText());
        fcstListLbl.setFont(controlFont);

        gd = new GridData(850, 150);
        fcstPointList = new List(middleComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        fcstPointList.setLayoutData(gd);
        fcstPointList.setFont(controlFont);
        fcstPointList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updatePointInformation();
            }
        });

        // ------------------------------------------------------
        // Remaining controls (bottom)
        // ------------------------------------------------------
        Composite bottomComp = new Composite(selectedFcstGroup, SWT.NONE);
        GridLayout gl = new GridLayout(8, false);
        gl.horizontalSpacing = 15;
        bottomComp.setLayout(gl);

        Label bottomOrderLbl = new Label(bottomComp, SWT.NONE);
        bottomOrderLbl.setText("Order:");

        Label impactLbl = new Label(bottomComp, SWT.NONE);
        impactLbl.setText("Impact/Crest\nbased on:");

        Label stageFlowLbl = new Label(bottomComp, SWT.NONE);
        stageFlowLbl.setText("Stage/Flow\nChg Window:");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label hsaPrimaryLbl = new Label(bottomComp, SWT.CENTER);
        hsaPrimaryLbl.setText("Primary\nHSA Backup:");
        hsaPrimaryLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label hsaSecondaryLbl = new Label(bottomComp, SWT.CENTER);
        hsaSecondaryLbl.setText("Secondary\nHSA Backup:");
        hsaSecondaryLbl.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 3;
        Label dataRetrievalLbl = new Label(bottomComp, SWT.NONE);
        dataRetrievalLbl.setText("Data Retrieval Info:");
        dataRetrievalLbl.setLayoutData(gd);

        gd = new GridData(35, SWT.DEFAULT);
        bottomOrderTF = new Text(bottomComp, SWT.BORDER);
        bottomOrderTF.setLayoutData(gd);

        primaryBtn = new Button(bottomComp, SWT.CHECK);
        primaryBtn.setText("Primary PE");

        gd = new GridData(50, SWT.DEFAULT);
        stageFlowTF = new Text(bottomComp, SWT.BORDER);
        stageFlowTF.setLayoutData(gd);

        gd = new GridData(100, 100);
        gd.verticalSpan = 3;
        primaryHsaList = new List(bottomComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        primaryHsaList.setLayoutData(gd);
        primaryHsaList.setFont(controlFont);

        gd = new GridData(100, 100);
        gd.verticalSpan = 3;
        secondaryHsaList = new List(bottomComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        secondaryHsaList.setLayoutData(gd);
        secondaryHsaList.setFont(controlFont);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label obsBackHrsLbl = new Label(bottomComp, SWT.CENTER);
        obsBackHrsLbl.setText("Observed\nBackHrs:");
        obsBackHrsLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label forecastFwdHrsLbl = new Label(bottomComp, SWT.CENTER);
        forecastFwdHrsLbl.setText("Forecast\nFwdHrs:");
        forecastFwdHrsLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label vtecAdjustHrsLbl = new Label(bottomComp, SWT.CENTER);
        vtecAdjustHrsLbl.setText("VTEC Adjust\nEnd Hrs:");
        vtecAdjustHrsLbl.setLayoutData(gd);

        // Filler
        new Label(bottomComp, SWT.NONE);

        nonPrimaryBtn = new Button(bottomComp, SWT.CHECK);
        nonPrimaryBtn.setText("Non-primary PE");

        // Filler
        new Label(bottomComp, SWT.NONE);

        gd = new GridData(80, SWT.DEFAULT);
        backHrsTF = new Text(bottomComp, SWT.BORDER);
        backHrsTF.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        fwdHrsTF = new Text(bottomComp, SWT.BORDER);
        fwdHrsTF.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        vtecAdjEndHrsTF = new Text(bottomComp, SWT.BORDER);
        vtecAdjEndHrsTF.setLayoutData(gd);

        // Filler
        gd = new GridData();
        gd.horizontalSpan = 3;
        Label filler = new Label(bottomComp, SWT.NONE);
        filler.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.horizontalSpan = 3;
        applyFcstPntBtn = new Button(bottomComp, SWT.PUSH);
        applyFcstPntBtn.setText("Apply FcstPoint");
        applyFcstPntBtn.setLayoutData(gd);
        applyFcstPntBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                savePointRecord();
            }
        });
    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, true));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 120;

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        addGroupBtn = new Button(buttonComp, SWT.PUSH);
        addGroupBtn.setText("Add Group");
        addGroupBtn.setLayoutData(gd);
        addGroupBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                addGroupRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        deleteGroupBtn = new Button(buttonComp, SWT.PUSH);
        deleteGroupBtn.setText("Delete Group");
        deleteGroupBtn.setLayoutData(gd);
        deleteGroupBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                deleteGroupRecord();
            }
        });
    }

    /**
     * Get the text for the group list control label.
     * 
     * @return Label text.
     */
    private String getGroupListLblText() {
        String format = "%S           %S                                    %S            %S";

        String str = String.format(format, "Group Id", "Name", "Order",
                "Recomm");

        return str;
    }

    /**
     * Get the text for the forecast list control label.
     * 
     * @return
     */
    private String getFcstListLblText() {
        String format = "%S   %S                           %S %S   %S    %S   %S   %S   %S   %S";

        String str = String.format(format, "Point Id", "Name", "Order", "Type",
                "Chg", "Primary", "Second", "BkHrs", "FwdHrs", "AdjEndHrs");

        return str;
    }

    /**
     * Retrieves static data from the database and populates the dialog with it.
     */
    private void loadStaticData() {
        primaryHsaList.removeAll();
        secondaryHsaList.removeAll();

        try {
            ArrayList<String> hsa = AddModifyLocationDataManager.getInstance()
                    .getHSAs();

            for (String currHSA : hsa) {
                primaryHsaList.add(currHSA);
                secondaryHsaList.add(currHSA);
            }
        } catch (VizException e) {
            e.printStackTrace();
        }

    }

    /**
     * Retrieves the forecast groups/points from the database
     */
    private void getDialogData() {
        // Get the group info
        try {
            groupData = HydroDBDataManager.getInstance().getData(
                    RPFFcstGroupData.class);

            // Initialize the Point map
            pointData = new HashMap<String, ArrayList<RPFFcstPointData>>();

            // Get the point info
            ArrayList<RPFFcstPointData> temp = HydroDBDataManager.getInstance()
                    .getData(RPFFcstPointData.class);

            ArrayList<RPFFcstPointData> tempArr;
            for (RPFFcstPointData currPoint : temp) {
                // Get the ArrayList for the Group ID
                if (pointData.containsKey(currPoint.getGroupID())) {
                    tempArr = pointData.get(currPoint.getGroupID());
                } else {
                    tempArr = new ArrayList<RPFFcstPointData>();
                }

                // Add the point to the ArrayList
                tempArr.add(currPoint);

                // Store the ArrayList in the point data
                pointData.put(currPoint.getGroupID(), tempArr);
            }

            updateDialogDisplay();
        } catch (VizException e) {
            e.printStackTrace();
        }
    }

    /**
     * Loads the groups and points into their respective lists.
     */
    private void updateDialogDisplay() {
        groupList.removeAll();
        fcstPointList.removeAll();

        if (groupData.size() > 0) {
            for (RPFFcstGroupData currGroup : groupData) {
                groupList.add(getDisplayString(currGroup));
            }

            updateDialogState(DialogStates.GROUPS_AVAILABLE);
        } else {
            updateDialogState(DialogStates.NO_GROUPS);
        }
    }

    /**
     * Generates a display string for the current forecast group.
     * 
     * @param currGroup
     *            The current forecast group.
     * @return The display string for the group.
     */
    private String getDisplayString(RPFFcstGroupData currGroup) {
        String rval = "%-8.8s    %-32.32s                %-4.4s               %-1s";

        return String.format(rval, currGroup.getGroupID(), currGroup
                .getGroupName(), HydroDataUtils.getDisplayString(currGroup
                .getOrdinal()), currGroup.getRecommendAll());
    }

    /**
     * Generates a display string for the current forecast point.
     * 
     * @param currPoint
     *            The current forecast point.
     * @return The display string for the point.
     */
    private String getDisplayString(RPFFcstPointData currPoint) {
        String rval = "%-8s  %-30.30s  %3.3s   %-3s   %s     %-3.3s       %-3.3s      %-6.6s  %-6.6s  %6.6s";

        return String.format(rval, currPoint.getLid(), currPoint.getLidName(),
                HydroDataUtils.getDisplayString(currPoint.getOrdinal()),
                currPoint.getRecordType(), HydroDataUtils.getDisplayString(
                        "%s", "%.2f", currPoint.getChangeThreshold()),
                currPoint.getPrimaryBackup(), currPoint.getSecondaryBackup(),
                HydroDataUtils.getDisplayString(currPoint.getBackHours()),
                HydroDataUtils.getDisplayString(currPoint.getForwardHours()),
                HydroDataUtils.getDisplayString("%s", "%6.1f", currPoint
                        .getAdjustEndHours()));
    }

    /**
     * Updates the group information to display the currently selected group's
     * information.
     */
    private void updateGroupInformation() {
        RPFFcstGroupData currGroup = getSelectedGroup();

        if (currGroup != null) {
            updateDialogState(DialogStates.GROUPS_AVAILABLE);

            groupIdTF.setText(currGroup.getGroupID());
            nameTF.setText(currGroup.getGroupName());
            orderTF.setText(HydroDataUtils.getDisplayString(currGroup
                    .getOrdinal()));
            recommAllChk.setSelection(currGroup.getRecommendAll().equals("Y"));

            // Populate the points
            updatePointList(currGroup.getGroupID());
        } else {
            updateDialogState(DialogStates.NO_GROUPS);

            clearGroupInformation();
        }
    }

    /**
     * Populates the point list with the points for the current group.
     * 
     * @param groupID
     *            The current group's id.
     */
    private void updatePointList(String groupID) {
        fcstPointList.removeAll();

        if (pointData.containsKey(groupID)) {
            for (RPFFcstPointData currPoint : pointData.get(groupID)) {
                fcstPointList.add(getDisplayString(currPoint));
            }

            updateDialogState(DialogStates.POINTS_AVAILABLE);
            
            if (fcstPointList.getItemCount() > 0 )
            {
            	fcstPointList.setSelection(0);            	
            }
        } else {
            updateDialogState(DialogStates.NO_POINTS);
        }

        clearPointInformation();
    }

    /**
     * Updates the point information to display the currently selected point's
     * information.
     */
    private void updatePointInformation() {
        RPFFcstPointData currPoint = getSelectedPoint();

        if (currPoint != null) {
            bottomOrderTF.setText(HydroDataUtils.getDisplayString(currPoint
                    .getOrdinal()));

            if (currPoint.getRecordType().equals("NPE")) {
                primaryBtn.setSelection(false);
                nonPrimaryBtn.setSelection(true);
            } else {
                primaryBtn.setSelection(true);
                nonPrimaryBtn.setSelection(false);
            }

            stageFlowTF.setText(HydroDataUtils.getDisplayString(currPoint
                    .getChangeThreshold()));

            String temp;

            // Primary HSA
            temp = currPoint.getPrimaryBackup();
            for (int i = 0; i < primaryHsaList.getItemCount(); i++) {
                if (primaryHsaList.getItem(i).equals(temp)) {
                    primaryHsaList.select(i);
                }
            }

            // Backup HSA
            temp = currPoint.getSecondaryBackup();
            for (int i = 0; i < secondaryHsaList.getItemCount(); i++) {
                if (secondaryHsaList.getItem(i).equals(temp)) {
                    secondaryHsaList.select(i);
                }
            }

            backHrsTF.setText(HydroDataUtils.getDisplayString(currPoint
                    .getBackHours()));
            fwdHrsTF.setText(HydroDataUtils.getDisplayString(currPoint
                    .getForwardHours()));
            vtecAdjEndHrsTF.setText(HydroDataUtils.getDisplayString(currPoint
                    .getAdjustEndHours()));
        } else {
            clearPointInformation();
        }
    }

    /**
     * Retrieves the currently selected forecast group
     * 
     * @return
     */
    private RPFFcstGroupData getSelectedGroup() {
        RPFFcstGroupData currGroup = null;

        if (groupList.getSelectionCount() > 0) {
            currGroup = groupData.get(groupList.getSelectionIndex());
        }

        return currGroup;
    }

    /**
     * Retrieves the currently selected forecast point
     * 
     * @return
     */
    private RPFFcstPointData getSelectedPoint() {
        RPFFcstPointData currPoint = null;

        if (fcstPointList.getSelectionCount() > 0) {
            RPFFcstGroupData currGroup = getSelectedGroup();

            if (currGroup != null) {
                currPoint = pointData.get(currGroup.getGroupID()).get(
                        fcstPointList.getSelectionIndex());
            }
        }

        return currPoint;
    }

    /**
     * Clears the group information display.
     */
    private void clearGroupInformation() {
        groupIdTF.setText("");
        nameTF.setText("");
        orderTF.setText("");
        recommAllChk.setSelection(false);

        clearPointInformation();
    }

    /**
     * Clears the point information display.
     */
    private void clearPointInformation() {
        bottomOrderTF.setText("");
        primaryBtn.setSelection(false);
        nonPrimaryBtn.setSelection(false);
        stageFlowTF.setText("");
        primaryHsaList.select(0);
        secondaryHsaList.select(0);
        backHrsTF.setText("");
        fwdHrsTF.setText("");
        vtecAdjEndHrsTF.setText("");
    }

    /**
     * Saves the group's information.
     */
    private void saveGroupRecord() {
        if (!groupIdTF.getText().equals("")) {
            RPFFcstGroupData newData = new RPFFcstGroupData();

            newData.setGroupID(groupIdTF.getText());
            newData.setGroupName(nameTF.getText());

            Integer temp = HydroDataUtils.getIntegerFromTF(shell, orderTF,
                    "Order");
            if (temp == null) {
                return;
            }
            newData.setOrdinal(temp);

            newData.setRecommendAll(recommAllChk.getSelection() ? "Y" : "N");

            try {
                HydroDBDataManager.getInstance().putData(newData);

                getDialogData();
            } catch (VizException e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                mb.setMessage("An error occurred while saving.");
                mb.open();

                e.printStackTrace();
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Value");
            mb.setMessage("The Group Id can't be blank");
            mb.open();
        }
    }

    /**
     * Saves the current forecast point information.
     */
    private void savePointRecord() {
        RPFFcstPointData newData = getSelectedPoint();

        if (newData != null) {
            newData.setGroupID(getSelectedGroup().getGroupID());

            Integer temp = HydroDataUtils.getIntegerFromTF(shell,
                    bottomOrderTF, "Order");
            if (temp == null) {
                return;
            }
            newData.setOrdinal(temp);

            newData.setRecordType(nonPrimaryBtn.getSelection() ? "NPE" : "PE");

            Double temp2 = HydroDataUtils.getDoubleFromTF(shell, stageFlowTF,
                    "Stage/Flow Chg Window");
            if (temp2 == null) {
                return;
            }
            newData.setChangeThreshold(temp2);

            newData.setPrimaryBackup(primaryHsaList.getSelection()[0]);
            newData.setSecondaryBackup(secondaryHsaList.getSelection()[0]);

            // Back Hours
            temp = HydroDataUtils.getIntegerFromTF(shell, backHrsTF,
                    "Observed BackHrs");
            if (temp == null) {
                return;
            }
            newData.setBackHours(temp);

            // Forward Hours
            temp = HydroDataUtils.getIntegerFromTF(shell, fwdHrsTF,
                    "Forecast FwdHrs");
            if (temp == null) {
                return;
            }
            newData.setForwardHours(temp);

            // Adjusted End Hours
            temp2 = HydroDataUtils.getDoubleFromTF(shell, vtecAdjEndHrsTF,
                    "VTEC Adjust End Hrs");
            if (temp2 == null) {
                return;
            }
            newData.setAdjustEndHours(temp2);

            try {
                HydroDBDataManager.getInstance().putData(newData);

                updatePointList(newData.getGroupID());
            } catch (VizException e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                mb.setMessage("An error occurred while saving.");
                mb.open();

                e.printStackTrace();
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Selection");
            mb.setMessage("Please select a point first");
            mb.open();
        }
    }

    /**
     * Prompts and deletes the currently selected group.
     */
    private void deleteGroupRecord() {
        RPFFcstGroupData currGroup = getSelectedGroup();

        if (currGroup != null) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                    | SWT.CANCEL);
            mb.setText("Delete Confirmation");
            mb
                    .setMessage("Do you want to delete this Forecast Group?\n"
                            + "(This will remove all Forecast Point associations to this Group.)");

            int result = mb.open();

            if (result == SWT.OK) {
                try {
                    // Delete all points under group
                    if (pointData.containsKey(currGroup.getGroupID())) {
                        for (RPFFcstPointData currPoint : pointData
                                .get(currGroup.getGroupID())) {
                            HydroDBDataManager.getInstance().deleteRecord(
                                    currPoint);
                        }
                    }

                    // Delete Group
                    HydroDBDataManager.getInstance().deleteRecord(currGroup);

                    getDialogData();
                } catch (VizException e) {
                    mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                    mb.setText("Unable to Delete");
                    mb.setMessage("An error occurred while trying to delete");
                    mb.open();

                    e.printStackTrace();
                }
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Selection");
            mb.setMessage("Please select a group first");
            mb.open();
        }
    }

    /**
     * Sets up the dialog so a group can be added.
     */
    private void addGroupRecord() {
        clearGroupInformation();
        updateDialogState(DialogStates.NEW_GROUP);
    }

    /**
     * Updates the dialog state.
     * 
     * @param currState
     *            The current state of the dialog.
     */
    private void updateDialogState(DialogStates currState) {
        switch (currState) {
        case NEW_GROUP:
            clearGroupInformation();

            // Clear selections
            fcstPointList.deselectAll();
            groupList.deselectAll();

            // FcstPoint Columns/Header
            fcstListLbl.setEnabled(false);
            fcstPntLbl.setEnabled(false);
            fcstPointList.setEnabled(false);

            // Disable FcstPoint Order
            bottomOrderTF.setEnabled(false);

            // Disable (Non-)Primary PE
            primaryBtn.setEnabled(false);
            nonPrimaryBtn.setEnabled(false);

            // Disable Stage/Flow
            stageFlowTF.setEnabled(false);

            // Disable hours to go back
            backHrsTF.setEnabled(false);

            // Disable hours to go forward
            fwdHrsTF.setEnabled(false);

            // Disable adjusted hours
            vtecAdjEndHrsTF.setEnabled(false);

            // Disable Apply FcstPoint and Delete
            applyFcstPntBtn.setEnabled(false);
            deleteGroupBtn.setEnabled(false);

            // Enable Apply FcstGroup
            applyfcstGrpBtn.setEnabled(true);
            break;
        case NO_GROUPS:
            // Disable Group Apply/Delete
            applyfcstGrpBtn.setEnabled(false);
            deleteGroupBtn.setEnabled(false);
            break;
        case NO_POINTS:
            clearPointInformation();

            // FcstPoint Columns/Header
            fcstListLbl.setEnabled(false);
            fcstPntLbl.setEnabled(false);
            fcstPointList.setEnabled(false);

            // Disable FcstPoint Order
            bottomOrderTF.setEnabled(false);

            // Disable (Non-)Primary PE
            primaryBtn.setEnabled(false);
            nonPrimaryBtn.setEnabled(false);

            // Disable Stage/Flow
            stageFlowTF.setEnabled(false);

            // Disable hours to go back
            backHrsTF.setEnabled(false);

            // Disable hours to go forward
            fwdHrsTF.setEnabled(false);

            // Disable adjusted hours
            vtecAdjEndHrsTF.setEnabled(false);

            // Disable Apply FcstPoint
            applyFcstPntBtn.setEnabled(false);
            break;
        case GROUPS_AVAILABLE:
            // Enable Group Apply/Delete
            applyfcstGrpBtn.setEnabled(true);
            deleteGroupBtn.setEnabled(true);
            break;
        case POINTS_AVAILABLE:
            // FcstPoint Columns/Header
            fcstListLbl.setEnabled(true);
            fcstPntLbl.setEnabled(true);
            fcstPointList.setEnabled(true);

            // Enable FcstPoint Order
            bottomOrderTF.setEnabled(true);

            // Enable (Non-)Primary PE
            primaryBtn.setEnabled(true);
            nonPrimaryBtn.setEnabled(true);

            // Enable Stage/Flow
            stageFlowTF.setEnabled(true);

            // Enable hours to go back
            backHrsTF.setEnabled(true);

            // Enable hours to go forward
            fwdHrsTF.setEnabled(true);

            // Enable adjusted hours
            vtecAdjEndHrsTF.setEnabled(true);

            // Enable Apply FcstPoint
            applyFcstPntBtn.setEnabled(true);
            break;
        default:
            break;
        }
    }
}
