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
package com.raytheon.viz.hydrocommon.lowwaterstatment;

import java.util.ArrayList;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
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
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.LowWaterStatementData;
import com.raytheon.viz.hydrocommon.datamanager.LowWaterStatementDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Low Water Statement dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 5, 2008				lvenable	Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class LowWaterStatementDlg extends CaveSWTDialog {

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Data list control.
     */
    private List dataList;

    /**
     * Lower limit text control.
     */
    private Text lowerLimitTF;

    /**
     * Upper limit text control.
     */
    private Text upperLimitTF;

    /**
     * Physical element text control.
     */
    private Text physicalElemTF;

    /**
     * Order text control.
     */
    private Text orderTF;

    /**
     * Criteria text control.
     */
    private Text criteriaTF;

    /**
     * Source text control.
     */
    private Text sourceTF;

    /**
     * Statement text control.
     */
    private Text statementTF;

    /**
     * OK button.
     */
    private Button okBtn;

    /**
     * Apply button.
     */
    private Button applyBtn;

    /**
     * Delete button.
     */
    private Button deleteBtn;

    /**
     * New button.
     */
    private Button newBtn;

    /**
     * The current location
     */
    private String lid;

    /**
     * Low water statement Data for the current location
     */
    private ArrayList<LowWaterStatementData> lwStatements;

    /**
     * Flag indicating if all the controls should be displayed.
     */
    private boolean fullControls = false;

    private enum DialogStates {
        HYDRO_VIEW, NEW, STATEMENTS_AVAILABLE, STATEMENTS_NOT_AVAILABLE
    }    
    
    /**
     * text from the remark text box
     */
    private String currentCriteriaText=null;
    private String currentSourceText=null;
    private String currentStatementText=null;
    
    private DialogStates buttonState;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param titleInfo
     *            Dialog title information.
     * @param fullControls
     *            Flag indicating if all the controls should be displayed.
     */
    public LowWaterStatementDlg(Shell parent, String titleInfo,
            boolean fullControls, String lid) {
        super(parent);
        setText("Low Water Statement " + titleInfo);

        this.lid = lid;
        this.fullControls = fullControls;
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
        initializeComponents();

        // Load the data
        getLowWaterStatementData();
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        createDataListControl();
        createInformationGroup();
        addSeparator();
        createBottomButtons();
    }

    /**
     * Create the data label and list control.
     */
    private void createDataListControl() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Composite listComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        listComp.setLayout(gl);
        listComp.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalIndent = 4;
        Label listLbl = new Label(listComp, SWT.NONE);
        listLbl.setText(getDataListControlLabelText());
        listLbl.setFont(controlFont);
        listLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        gd.widthHint = 700;
        gd.heightHint = 150;
        dataList = new List(listComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        dataList.setLayoutData(gd);
        dataList.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                getLowWaterStatementInformation();
            }
        });

        dataList.setFont(controlFont);
    }

    /**
     * Create the Information group and controls.
     */
    private void createInformationGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group infoGroup = new Group(shell, SWT.NONE);
        infoGroup.setText("Information");
        GridLayout gl = new GridLayout(8, false);
        infoGroup.setLayout(gl);
        infoGroup.setLayoutData(gd);

        Label lowerLimitLbl = new Label(infoGroup, SWT.RIGHT);
        lowerLimitLbl.setText("Lower Limit:");

        gd = new GridData(95, SWT.DEFAULT);
        lowerLimitTF = new Text(infoGroup, SWT.BORDER);
        lowerLimitTF.setLayoutData(gd);

        Label upperLimitLbl = new Label(infoGroup, SWT.RIGHT);
        upperLimitLbl.setText("  Upper Limit:");

        gd = new GridData(95, SWT.DEFAULT);
        upperLimitTF = new Text(infoGroup, SWT.BORDER);
        upperLimitTF.setLayoutData(gd);

        Label physicalElemLbl = new Label(infoGroup, SWT.RIGHT);
        physicalElemLbl.setText("  Physical Element:");

        gd = new GridData(50, SWT.DEFAULT);
        physicalElemTF = new Text(infoGroup, SWT.BORDER);
        physicalElemTF.setLayoutData(gd);

        Label orderLbl = new Label(infoGroup, SWT.RIGHT);
        orderLbl.setText("  Order:");

        gd = new GridData(50, SWT.DEFAULT);
        orderTF = new Text(infoGroup, SWT.BORDER);
        orderTF.setLayoutData(gd);

        gd = new GridData(SWT.RIGHT, SWT.TOP, true, true);
        Label criteriaLbl = new Label(infoGroup, SWT.RIGHT);
        criteriaLbl.setText("Criteria:");
        criteriaLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = 75;
        gd.horizontalSpan = 7;
        criteriaTF = new Text(infoGroup, SWT.BORDER | SWT.MULTI);
        criteriaTF.setLayoutData(gd);
        currentCriteriaText=criteriaTF.getText();
        ModifyListener listenerC = new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
        		if (criteriaTF.getText().length()>255){
        			criteriaTF.setText(currentCriteriaText);
        			shell.getDisplay().beep();
        		}
        		else
        			currentCriteriaText=criteriaTF.getText();
        	}
        };

        criteriaTF.addModifyListener(listenerC);

        gd = new GridData(SWT.RIGHT, SWT.TOP, true, true);
        Label sourceLbl = new Label(infoGroup, SWT.RIGHT);
        sourceLbl.setText("Source:");
        sourceLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = 75;
        gd.horizontalSpan = 7;
        sourceTF = new Text(infoGroup, SWT.BORDER | SWT.MULTI);
        sourceTF.setLayoutData(gd);
        currentSourceText=sourceTF.getText();
        ModifyListener listenerS = new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
        		if (sourceTF.getText().length()>255){
        			sourceTF.setText(currentSourceText);
        			shell.getDisplay().beep();
        		}
        		else
        			currentSourceText=sourceTF.getText();
        	}
        };

        sourceTF.addModifyListener(listenerS);

        gd = new GridData(SWT.RIGHT, SWT.TOP, true, true);
        Label statementLbl = new Label(infoGroup, SWT.RIGHT);
        statementLbl.setText("Statement:");
        statementLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = 75;
        gd.horizontalSpan = 7;
        statementTF = new Text(infoGroup, SWT.BORDER | SWT.MULTI);
        statementTF.setLayoutData(gd);
        currentStatementText=statementTF.getText();
        ModifyListener listenerT = new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
        		if (statementTF.getText().length()>200){
        			statementTF.setText(currentStatementText);
        			shell.getDisplay().beep();
        		}
        		else
        			currentStatementText=statementTF.getText();
        	}
        };

        statementTF.addModifyListener(listenerT);

    }

    /**
     * Add a horizontal separator to the main display.
     */
    private void addSeparator() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 4;
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
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
        okBtn.setEnabled(false);
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
        applyBtn.setEnabled(false);
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                saveRecord();
            }
        });

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
        deleteBtn.setEnabled(false);
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                deleteRecord();
            }
        });

        if (fullControls == false) {
            buttonState = DialogStates.HYDRO_VIEW;
            setButtonStates();
        }
    }

    /**
     * Get the data list control label text.
     * 
     * @return Label text.
     */
    private String getDataListControlLabelText() {
        String format = "%S     %S     %S      %S";

        String labelStr = String.format(format, "Lower Limit", "Upper Limit",
                "Physical Element", "Order");

        return labelStr;
    }

    /**
     * Loads the statement data for a location
     */
    private void getLowWaterStatementData() {
        try {
            lwStatements = LowWaterStatementDataManager.getInstance()
                    .getLowWaterStatementData(lid);
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        // Check if any data was returned
        if (lwStatements != null && lwStatements.size() > 0) {
            buttonState = DialogStates.STATEMENTS_AVAILABLE;
        } else {
            buttonState = DialogStates.STATEMENTS_NOT_AVAILABLE;
        }

        updateLowWaterStatementData();
    }

    private void updateLowWaterStatementData() {
        // Clear current Stages
        dataList.removeAll();

        for (LowWaterStatementData currData : lwStatements) {
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

    private void getLowWaterStatementInformation() {
        updateInformationDisplay(getCurrentlySelectedStatement());
    }

    private void updateInformationDisplay(LowWaterStatementData data) {
        lowerLimitTF.setText(data.getLowerValueString());
        upperLimitTF.setText(data.getUpperValueString());
        physicalElemTF.setText(data.getPe());
        orderTF.setText(Integer.toString(data.getCriteriaRank()));
        criteriaTF.setText(data.getLowWaterCriteria());
        sourceTF.setText(data.getLowWaterSource());
        statementTF.setText(data.getStatement());
    }

    private LowWaterStatementData getCurrentlySelectedStatement() {
        LowWaterStatementData currData = null;

        if (dataList.getSelectionCount() > 0) {
            currData = lwStatements.get(dataList.getSelectionIndex());
        }

        return currData;
    }

    private void deleteRecord() {
        LowWaterStatementData currData = getCurrentlySelectedStatement();

        if (currData != null) {
            if (MessageDialog.openConfirm(null, "Delete Confirmation",
                    "Do you wish to delete this entry?")) {
                try {
                    LowWaterStatementDataManager.getInstance().deleteRecord(
                            currData);
                } catch (VizException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }

                getLowWaterStatementData();
            }

            clearInformation();
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Selection");
            mb.setMessage("Please select an entry first.");
            mb.open();
        }
    }

    private void newRecord() {
        clearInformation();

        buttonState = DialogStates.NEW;
        setButtonStates();
    }

    private void clearInformation() {
        lowerLimitTF.setText("");
        upperLimitTF.setText("");
        physicalElemTF.setText("");
        orderTF.setText("");
        criteriaTF.setText("");
        sourceTF.setText("");
        statementTF.setText("");
    }

    /**
     * valid and stuff values from text fields into a new data object and save
     * via data manager. Automatically handles insert vs. update.
     */
    private void saveRecord() {
        double upperVal = HydroConstants.MISSING_VALUE;
        double lowerVal = HydroConstants.MISSING_VALUE;
        int rankVal = HydroConstants.MISSING_VALUE;

        try {
            if (lowerLimitTF.getText().compareTo("") != 0) {
                lowerVal = Double.parseDouble(lowerLimitTF.getText().trim());
            } else {
                lowerVal = HydroConstants.MISSING_VALUE;
                MessageDialog.openError(null, "Invalid Lower Limit value",
                        "Please enter a valid numeric value for Lower Limit");
                return;
            }
        } catch (NumberFormatException e) {
            MessageDialog.openError(null, "Invalid Lower Limit value",
                    "Please enter a valid numeric value for Lower Limit");
            return;
        }

        try {
            upperVal = (upperLimitTF.getText().compareTo("") != 0) ? Double
                    .parseDouble(upperLimitTF.getText().trim())
                    : HydroConstants.MISSING_VALUE;
        } catch (NumberFormatException e) {
            MessageDialog.openError(null, "Invalid Upper Limit value",
                    "Please enter a valid numeric value for Upper Limit");
            return;
        }

        try {
            if (orderTF.getText().compareTo("") != 0) {
                rankVal = Integer.parseInt(orderTF.getText().trim());
            } else {
                rankVal = HydroConstants.MISSING_VALUE;
                MessageDialog.openError(null, "Invalid Order value",
                        "Please enter a valid numeric value for Order");
                return;
            }
        } catch (NumberFormatException e) {
            MessageDialog.openError(null, "Invalid Order value",
                    "Please enter a valid numeric value for Order");
            return;
        }

        if (physicalElemTF.getText().compareTo("") == 0) {
            MessageDialog.openError(null, "Invalid Physical Element value",
                    "Please enter a valid value for Physical Element");
            return;
        }

        LowWaterStatementData dataToSave = new LowWaterStatementData();

        // Stuff into new object
        dataToSave.setCriteriaRank(rankVal);
        dataToSave.setLid(lid);
        dataToSave.setLowerValue(lowerVal);
        dataToSave.setLowWaterCriteria(criteriaTF.getText());
        dataToSave.setLowWaterSource(sourceTF.getText());
        dataToSave.setPe(physicalElemTF.getText());
        dataToSave.setStatement(statementTF.getText());
        dataToSave.setUpperValue(upperVal);

        try {
            // Save to DB
            LowWaterStatementDataManager.getInstance()
                    .putLowWaterStatementData(dataToSave);
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        getLowWaterStatementData();
    }

    /**
     * Handles the states (enable/disable)
     */
    private void setButtonStates() {
        switch (buttonState) {
        case HYDRO_VIEW:
            okBtn.setVisible(false);
            applyBtn.setVisible(false);
            newBtn.setVisible(false);
            deleteBtn.setVisible(false);
            break;
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
