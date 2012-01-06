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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.LowWaterData;
import com.raytheon.viz.hydrocommon.datamanager.LowWaterDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Low Water dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 5, 2008				lvenable	Initial creation
 * Nov 12, 2008 1697        askripsky   Connect to DB
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class LowWaterDlg extends CaveSWTDialog {

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Data list control.
     */
    private List dataList;

    /**
     * Stage text control.
     */
    private Text stageTF;

    /**
     * Flow text control.
     */
    private Text flowTF;

    /**
     * Date text control.
     */
    private Text dateTF;

    /**
     * Notes text control.
     */
    private Text notesTF;

    private Button okBtn;

    private Button applyBtn;

    private Button cancelBtn;

    private Button newBtn;

    private Button deleteBtn;

    /**
     * Current location.
     */
    private String lid;

    /**
     * Low water Data for the current location
     */
    private ArrayList<LowWaterData> lwData;

    /**
     * Formats date.
     */
    private SimpleDateFormat dateFormat;

    private enum DialogStates {
        NEW, STATEMENTS_AVAILABLE, STATEMENTS_NOT_AVAILABLE
    }

    /**
     * Keep track of what buttons should be enabled
     */
    private DialogStates buttonState;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param titleInfo
     *            Dialog title information.
     */
    public LowWaterDlg(Shell parent, String titleInfo, String lid) {
        super(parent);
        setText("Low Water" + titleInfo);

        this.lid = lid;
        dateFormat = new SimpleDateFormat("MM/dd/yyyy");
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
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
        // Initialize all of the controls and layouts
        initializeComponents();

        // Get the Low water data
        getLowWaterData();
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createListControl();

        createInformationGroup();

        createBottomButtons();
    }

    /**
     * Create the data label and list control.
     */
    private void createListControl() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite listComp = new Composite(shell, SWT.NONE);
        listComp.setLayout(new GridLayout(1, false));
        listComp.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalIndent = 4;
        Label listLbl = new Label(listComp, SWT.NONE);
        listLbl.setText(getDataListLabelText());
        listLbl.setFont(controlFont);
        listLbl.setLayoutData(gd);

        // --------------------------------------
        // Create the data list control
        // --------------------------------------
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 700;
        gd.heightHint = 125;
        dataList = new List(listComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        dataList.setLayoutData(gd);
        dataList.setFont(controlFont);
        dataList.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                getLowWaterInformation();
            }
        });
    }

    /**
     * Create the Information group and controls.
     */
    private void createInformationGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group infoGroup = new Group(shell, SWT.NONE);
        infoGroup.setLayout(new GridLayout(3, false));
        infoGroup.setLayoutData(gd);
        infoGroup.setText(" Information ");

        gd = new GridData(300, SWT.DEFAULT);
        Label stageLbl = new Label(infoGroup, SWT.NONE);
        stageLbl.setText("Stage:");
        stageLbl.setLayoutData(gd);

        gd = new GridData(250, SWT.DEFAULT);
        Label flowLbl = new Label(infoGroup, SWT.NONE);
        flowLbl.setText("Flow:");
        flowLbl.setLayoutData(gd);

        Label dateLbl = new Label(infoGroup, SWT.NONE);
        dateLbl.setText("Date:");

        gd = new GridData(150, SWT.DEFAULT);
        stageTF = new Text(infoGroup, SWT.BORDER);
        stageTF.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        flowTF = new Text(infoGroup, SWT.BORDER);
        flowTF.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        dateTF = new Text(infoGroup, SWT.BORDER);
        dateTF.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 3;
        Label notesLbl = new Label(infoGroup, SWT.NONE);
        notesLbl.setText("Notes:");
        notesLbl.setLayoutData(gd);

        gd = new GridData(450, SWT.DEFAULT);
        gd.horizontalSpan = 3;
        notesTF = new Text(infoGroup, SWT.BORDER);
        notesTF.setLayoutData(gd);
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
        okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                saveRecord();
                shell.dispose();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        applyBtn = new Button(buttonComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                saveRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        newBtn = new Button(buttonComp, SWT.PUSH);
        newBtn.setText("New");
        newBtn.setLayoutData(gd);
        newBtn.addSelectionListener(new SelectionAdapter() {
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
            public void widgetSelected(SelectionEvent event) {
                deleteRecord();
            }
        });
    }

    /**
     * Get the data list control label text.
     * 
     * @return Label text.
     */
    private String getDataListLabelText() {
        String format = "%S                                 %S                                     %S";

        String labelStr = String.format(format, "Stage", "Flow", "Date");

        return labelStr;
    }

    /**
     * Loads the Stage/Display statement data for a location
     */
    private void getLowWaterData() {
        if (lwData != null) {
            lwData.clear();
        }

        try {
            lwData = LowWaterDataManager.getInstance().getLowWaterData(lid);
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        updateFloodDamageData();
    }

    private void updateFloodDamageData() {
        // Clear current Stages
        dataList.removeAll();

        for (LowWaterData currData : lwData) {
            dataList.add(currData.toString());
        }

        // Set the button states according to whether or not data is available
        if (dataList.getItemCount() > 0) {
            buttonState = DialogStates.STATEMENTS_AVAILABLE;
        } else {
            buttonState = DialogStates.STATEMENTS_NOT_AVAILABLE;
        }

        setButtonStates();
    }

    private void getLowWaterInformation() {
        updateInformationDisplay(getCurrentlySelectedStage());
    }

    private void updateInformationDisplay(LowWaterData data) {
        stageTF
                .setText((data.getStage() != LowWaterData.MISSING_VALUE_D) ? Double
                        .toString(data.getStage())
                        : "");
        flowTF.setText((data.getFlow() != LowWaterData.MISSING_VALUE) ? Integer
                .toString(data.getFlow()) : "");
        dateTF.setText(dateFormat.format(data.getDate()));
        notesTF.setText(data.getRemark());
    }

    private LowWaterData getCurrentlySelectedStage() {
        return lwData.get(dataList.getSelectionIndex());
    }

    private void deleteRecord() {
        if (MessageDialog.openConfirm(null, "Delete Confirmation",
                "Do you wish to delete this entry?")) {
            try {
                LowWaterDataManager.getInstance().deleteRecord(
                        getCurrentlySelectedStage());
            } catch (VizException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

            getLowWaterData();
        }

        clearInformation();
    }

    private void newRecord() {
        clearInformation();

        buttonState = DialogStates.NEW;
        setButtonStates();
    }

    private void clearInformation() {
        stageTF.setText("");
        flowTF.setText("");
        dateTF.setText("");
        notesTF.setText("");
    }

    private void saveRecord() {
        int flowVal = LowWaterData.MISSING_VALUE;
        double stageVal = LowWaterData.MISSING_VALUE_D;

        try {
            flowVal = (flowTF.getText().compareTo("") != 0) ? Integer
                    .parseInt(flowTF.getText()) : LowWaterData.MISSING_VALUE;

            try {
                stageVal = (stageTF.getText().compareTo("") != 0) ? Double
                        .parseDouble(stageTF.getText())
                        : LowWaterData.MISSING_VALUE_D;

                LowWaterDataManager.getInstance().putLowWaterData(lid,
                        dateFormat.parse(dateTF.getText()), flowVal,
                        notesTF.getText(), stageVal);
            } catch (NumberFormatException e) {
                MessageDialog.openConfirm(null, "Invalid Stage value",
                        "Please enter a valid numeric value for Stage");
            } catch (VizException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (ParseException e) {
                MessageDialog.openConfirm(null, "Invalid Date value",
                        "Please enter a date in the form: MM/DD/YYYY");
            }
        } catch (NumberFormatException e) {
            MessageDialog.openConfirm(null, "Invalid Flow value",
                    "Please enter a valid numeric value for Flow");
        }

        getLowWaterData();
    }

    /**
     * Handles the states (enable/disable)
     */
    private void setButtonStates() {
        switch (buttonState) {
        case NEW:
            okBtn.setEnabled(true);
            applyBtn.setEnabled(true);
            break;
        case STATEMENTS_AVAILABLE:
            okBtn.setEnabled(true);
            applyBtn.setEnabled(true);
            deleteBtn.setEnabled(true);
            break;
        case STATEMENTS_NOT_AVAILABLE:
            okBtn.setEnabled(false);
            applyBtn.setEnabled(false);
            deleteBtn.setEnabled(false);
            break;
        default:
            break;
        }
    }
}
