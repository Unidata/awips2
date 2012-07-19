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

package com.raytheon.viz.hydrocommon.impactstatement;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.FloodStatementData;
import com.raytheon.viz.hydrocommon.data.LocationData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Impact Statement dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 5, 2008				lvenable	Initial creation.
 * 10/20/2008   1617        grichard    Support impact statement.
 * Jan 5, 2008  1802        askripsk    Complete HydroBase version.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ImpactStatementDlg extends CaveSWTDialog {

    /** The rising element of the impact statement */
    public static final String RISING = "RISING";

    /** The falling element of the impact statement */
    public static final String FALLING = "FALLING";

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Data list control.
     */
    private List dataList;

    /**
     * Impact value text control.
     */
    private Text impactValTF;

    /**
     * Impact physical element text control.
     */
    private Text impactPeTF;

    /**
     * Beginning month combo box.
     */
    private Combo beginMonthCbo;

    /**
     * Ending month combo box.
     */
    private Combo endMonthCbo;

    /**
     * Beginning day text control.
     */
    private Text beginDayTF;

    /**
     * Ending day text control.
     */
    private Text endDayTF;

    /**
     * Rising radio button.
     */
    private Button risingRdo;

    /**
     * Falling radio button.
     */
    private Button fallingRdo;

    /**
     * Text editor control.
     */
    private StyledText textEditor;
    /**
     * text from the remark text box
     */
    private String currentImpactText=null;

    /**
     * OK button.
     */
    private Button okBtn;

    /**
     * Apply button.
     */
    private Button applyBtn;

    /**
     * Cancel button.
     */
    private Button cancelBtn;

    /**
     * New button.
     */
    private Button newBtn;

    /**
     * Delete button.
     */
    private Button deleteBtn;

    /**
     * Print all button.
     */
    private Button printAllBtn;

    /**
     * Save to file button.
     */
    private Button saveToFileBtn;

    /**
     * Flag indicating if all the controls should be displayed.
     */
    private boolean fullControl = false;

    /**
     * Location
     */
    private String lid;

    /**
     * Cache of data for location
     */
    private ArrayList<FloodStatementData> statementData;

    /**
     * Used by the print methods
     */
    private Printer printer;

    private int lineHeight = 0;

    private int tabWidth = 0;

    private int leftMargin;

    private int rightMargin;

    private int topMargin;

    private int bottomMargin;

    private int x, y;

    private int index, end;

    private String tabs;

    private StringBuffer wordBuffer;

    private GC gc;

    /**
     * Months enumeration.
     */
    private enum months {
        January, February, March, April, May, June, July, August, September, October, November, December
    };

    /**
     * Possible States for the dialog
     */
    private enum DialogStates {
        NEW_RECORD, NO_RECORDS, RECORDS_AVAILABLE
    }

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param titleInfo
     *            Dialog title information.
     * @param fullControl
     *            Flag indicating if all the controls should be displayed.
     */
    public ImpactStatementDlg(Shell parent, String titleInfo, String lid,
            boolean fullControl) {
        super(parent);
        setText("Impact Statement" + titleInfo);

        this.fullControl = fullControl;
        this.lid = lid;
    }

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

    @Override
    protected void disposed() {
        controlFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        // Initialize all of the controls and layouts
        createListControl();
        createCharacteristicsGroup();
        createImpactGroup();
        createBottomButtons();

        // Query floodstmt table in IHFS database using SQL.
        getDialogData();

        populateCharacteristics(0);
    }

    /**
     * Create the data label and list control.
     */
    private void createListControl() {
        Composite listComp = new Composite(shell, SWT.NONE);
        listComp.setLayout(new GridLayout(1, false));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);

        gd = new GridData();
        gd.horizontalIndent = 4;
        Label listLbl = new Label(listComp, SWT.NONE);
        listLbl.setText(getDataListLabelText());
        listLbl.setFont(controlFont);
        listLbl.setLayoutData(gd);

        gd = new GridData(875, 160);
        dataList = new List(listComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        dataList.setLayoutData(gd);

        dataList.setFont(controlFont);

        dataList.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                populateCharacteristics(dataList.getSelectionIndex());
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent event) {
            }
        });
    }

    /**
     * Create the characteristics group and controls.
     */
    private void createCharacteristicsGroup() {
        GridData mainGridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group characteristicsGroup = new Group(shell, SWT.NONE);
        characteristicsGroup.setText("Characteristics");
        GridLayout layout = new GridLayout(8, false);
        characteristicsGroup.setLayout(layout);
        characteristicsGroup.setLayoutData(mainGridData);

        // ----------------------------------------------
        // First row of controls
        // ----------------------------------------------

        GridData gd = new GridData(100, SWT.DEFAULT);
        Label impactValLbl = new Label(characteristicsGroup, SWT.RIGHT);
        impactValLbl.setText("Impact Value:");
        impactValLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        impactValTF = new Text(characteristicsGroup, SWT.BORDER | SWT.CENTER);
        impactValTF.setLayoutData(gd);
        impactValTF.setEditable(fullControl);

        gd = new GridData(110, SWT.DEFAULT);
        Label impactPeLbl = new Label(characteristicsGroup, SWT.RIGHT);
        impactPeLbl.setText("Impact PE:");
        impactPeLbl.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        impactPeTF = new Text(characteristicsGroup, SWT.BORDER);
        impactPeTF.setLayoutData(gd);
        impactPeTF.setEditable(fullControl);
        impactPeTF.setTextLimit(2);

        gd = new GridData(140, SWT.DEFAULT);
        Label beginLbl = new Label(characteristicsGroup, SWT.RIGHT);
        beginLbl.setText("Begin (Seasonal):");
        beginLbl.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        beginMonthCbo = new Combo(characteristicsGroup, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        beginMonthCbo.setLayoutData(gd);
        populateMonthCombo(beginMonthCbo);
        beginMonthCbo.select(0);

        gd = new GridData(30, SWT.DEFAULT);
        Label filler1 = new Label(characteristicsGroup, SWT.NONE);
        filler1.setLayoutData(gd);

        gd = new GridData(40, SWT.DEFAULT);
        beginDayTF = new Text(characteristicsGroup, SWT.BORDER);
        beginDayTF.setLayoutData(gd);
        beginDayTF.setEditable(false);
        beginDayTF.setEditable(fullControl);
        beginDayTF.setTextLimit(2);

        // ----------------------------------------------
        // Second row of controls
        // ----------------------------------------------

        gd = new GridData(100, SWT.DEFAULT);
        Label tendencyLbl = new Label(characteristicsGroup, SWT.RIGHT);
        tendencyLbl.setText("Tendency:");
        tendencyLbl.setLayoutData(gd);

        risingRdo = new Button(characteristicsGroup, SWT.RADIO);
        risingRdo.setText("Rising");
        risingRdo.setSelection(true);

        gd = new GridData(110, SWT.DEFAULT);
        Label filler2 = new Label(characteristicsGroup, SWT.RIGHT);
        filler2.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        Label filler3 = new Label(characteristicsGroup, SWT.RIGHT);
        filler3.setLayoutData(gd);

        gd = new GridData(140, SWT.DEFAULT);
        Label endLbl = new Label(characteristicsGroup, SWT.RIGHT);
        endLbl.setText("End (Seasonal):");
        endLbl.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        endMonthCbo = new Combo(characteristicsGroup, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        endMonthCbo.setLayoutData(gd);
        populateMonthCombo(endMonthCbo);
        endMonthCbo.select(months.values().length - 1);

        gd = new GridData(30, SWT.DEFAULT);
        Label filler4 = new Label(characteristicsGroup, SWT.NONE);
        filler4.setLayoutData(gd);
        beginDayTF.setText("01");
        gd = new GridData(40, SWT.DEFAULT);
        endDayTF = new Text(characteristicsGroup, SWT.BORDER);
        endDayTF.setLayoutData(gd);
        endDayTF.setEditable(false);
        endDayTF.setEditable(fullControl);
        endDayTF.setTextLimit(2);

        // ----------------------------------------------
        // Third row of controls
        // ----------------------------------------------

        gd = new GridData(100, SWT.DEFAULT);
        Label filler5 = new Label(characteristicsGroup, SWT.RIGHT);
        filler5.setLayoutData(gd);

        fallingRdo = new Button(characteristicsGroup, SWT.RADIO);
        fallingRdo.setText("Falling");
    }

    /**
     * Create the impact group and controls.
     */
    private void createImpactGroup() {
        GridData mainGridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group impactGroup = new Group(shell, SWT.NONE);
        impactGroup.setText("Impact");
        GridLayout layout = new GridLayout(1, false);
        impactGroup.setLayout(layout);
        impactGroup.setLayoutData(mainGridData);

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = 200;
        gd.widthHint = 700;
        textEditor = new StyledText(impactGroup, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL | SWT.H_SCROLL);
        textEditor.setLayoutData(gd);
        textEditor.setFont(controlFont);
        textEditor.setEditable(fullControl);
        textEditor.setWordWrap(true);
        textEditor.setTextLimit(512);
        currentImpactText=textEditor.getText();
        ModifyListener listener = new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
        		if (textEditor.getText().length()>512){
        			textEditor.setText(currentImpactText);
        			shell.getDisplay().beep();
        		}
        		else
        			currentImpactText=textEditor.getText();
        	}
        };

        textEditor.addModifyListener(listener);

    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(8, fullControl));
        buttonComp.setLayoutData(gd);

        boolean takeAvailableSpace = true;
        int actionButtonWidth = 85;
        int printSaveButtonWidth = 110;

        if (fullControl == false) {
            actionButtonWidth = 0;
            printSaveButtonWidth = 0;
            takeAvailableSpace = false;
        }
        int cancelBtnWidth = 85;

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, takeAvailableSpace, false);
        gd.widthHint = actionButtonWidth;
        okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (saveRecord()) {
                    shell.dispose();
                }
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, takeAvailableSpace, false);
        gd.widthHint = actionButtonWidth;
        applyBtn = new Button(buttonComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = cancelBtnWidth;
        cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, takeAvailableSpace, false);
        gd.widthHint = actionButtonWidth;
        newBtn = new Button(buttonComp, SWT.PUSH);
        newBtn.setText("New");
        newBtn.setLayoutData(gd);
        newBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                newRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, takeAvailableSpace, false);
        gd.widthHint = actionButtonWidth;
        deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteRecord();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, takeAvailableSpace, false);
        Label filler = new Label(buttonComp, SWT.NONE);
        filler.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, takeAvailableSpace, false);
        gd.widthHint = printSaveButtonWidth;
        printAllBtn = new Button(buttonComp, SWT.PUSH);
        printAllBtn.setText("Print All");
        printAllBtn.setLayoutData(gd);
        printAllBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                printRecords();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, takeAvailableSpace, false);
        gd.widthHint = printSaveButtonWidth;
        saveToFileBtn = new Button(buttonComp, SWT.PUSH);
        saveToFileBtn.setText("Save to File");
        saveToFileBtn.setLayoutData(gd);
        saveToFileBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveToFile();
            }
        });

        if (fullControl == false) {
            okBtn.setVisible(false);
            applyBtn.setVisible(false);
            newBtn.setVisible(false);
            deleteBtn.setVisible(false);
            printAllBtn.setVisible(false);
            saveToFileBtn.setVisible(false);
        }
    }

    /**
     * Populate the month combo box.
     * 
     * @param combo
     *            Month combo control.
     */
    private void populateMonthCombo(Combo combo) {
        for (months m : months.values()) {
            combo.add(m.name());
        }
    }

    /**
     * Get the data list label text.
     * 
     * @return Label text.
     */
    private String getDataListLabelText() {
        String format = "%S      %S                %S        %S                         %S";

        String labelStr = String.format(format, "Impact Value", "Impact PE",
                "Begin", "End", "Tendency");

        return labelStr;
    }

    // ---------------------------------------------------------
    // Query the Floodstmt Table in the IHFS database using SQL.
    // ---------------------------------------------------------
    private void queryFloodstmt() {
        // ---------------------------------
        // Populate data list
        // ---------------------------------

        String fmtStr = "%8S %13S %25S %11S %29S";
        String myQuery = "select * from floodstmt where lid = '" + lid + "'";

        ArrayList<Object[]> data;
        try {
            data = (ArrayList<Object[]>) DirectDbQuery.executeQuery(myQuery,
                    HydroConstants.IHFS, QueryLanguage.SQL);
            for (Object[] rowData : data) {
                String risingIndicator;
                if (rowData[3].toString().equals("R")) {
                    risingIndicator = RISING;
                } else {
                    risingIndicator = FALLING;
                }

                String tmpStr = String.format(fmtStr, rowData[1].toString(),
                        rowData[6].toString(), rowData[4].toString(),
                        rowData[5].toString(), risingIndicator);
                dataList.add(tmpStr);
            }
        } catch (VizException e) {
            e.printStackTrace();
        }
    }

    // -----------------------------------------------
    // Populate characteristics of selected statement.
    // -----------------------------------------------
    private void populateCharacteristics(int index) {
        if ((index >= 0) && (dataList.getItemCount() > 0)) {
            FloodStatementData currData = statementData.get(index);
            impactValTF.setText(HydroDataUtils.getDisplayString(currData
                    .getImpactValue()));
            impactPeTF.setText(currData.getImpactPE());

            String temp = currData.getDateStart();
            String[] beginMmDd = temp.split("/");
            beginMonthCbo.select(Integer.parseInt(beginMmDd[0]) - 1);
            beginDayTF.setText(beginMmDd[1]);

            temp = currData.getDateEnd();
            String[] endMmDd = temp.split("/");
            endMonthCbo.select(Integer.parseInt(endMmDd[0]) - 1);
            endDayTF.setText(endMmDd[1]);

            if (currData.getRiseFall().equals("R")) {
                risingRdo.setSelection(true);
                fallingRdo.setSelection(false);
            } else {
                risingRdo.setSelection(false);
                fallingRdo.setSelection(true);
            }

            // Populate the Impact Statement
            textEditor.setText(statementData.get(index).getStatement());
        }
    }

    private void getDialogData() {
        FloodStatementData seedData = new FloodStatementData();
        seedData.setLid(lid);

        try {
            statementData = HydroDBDataManager.getInstance().getData(seedData);
        } catch (VizException e) {
            e.printStackTrace();
        }

        updateDisplay();
    }

    private void updateDisplay() {
        dataList.removeAll();

        clearInformation();

        for (FloodStatementData currStatement : statementData) {
            dataList.add(getStatementDisplayString(currStatement));
        }

        if (statementData.size() > 0) {
            updateDialogState(DialogStates.RECORDS_AVAILABLE);
        } else {
            updateDialogState(DialogStates.NO_RECORDS);
        }
    }

    private String getStatementDisplayString(FloodStatementData currStatement) {
        String fmtStr = "%8S %13S %25S %11S %29S";

        return String.format(fmtStr, currStatement.getImpactValue(),
                currStatement.getImpactPE(), currStatement.getDateStart(),
                currStatement.getDateEnd(), (currStatement.getRiseFall()
                        .equals("R")) ? RISING : FALLING);
    }

    private void clearInformation() {
        // Set Start to 01/01 and End to 12/31
        beginMonthCbo.select(0);
        beginDayTF.setText("01");
        endMonthCbo.select(11);
        endDayTF.setText("31");

        impactPeTF.setText("");
        impactValTF.setText("");
        textEditor.setText("");
        risingRdo.setSelection(true);
        fallingRdo.setSelection(false);
    }

    private void newRecord() {
        // Clear Form
        clearInformation();
        updateDialogState(DialogStates.NEW_RECORD);
    }

    private void deleteRecord() {
        int selectedIndex = dataList.getSelectionIndex();

        if (selectedIndex >= 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                    | SWT.CANCEL);
            mb.setText("Delete Confirmation");
            mb.setMessage("Do you wish to delete this entry?");

            int result = mb.open();

            if (result == SWT.OK) {
                try {
                    // Delete the record
                    HydroDBDataManager.getInstance().deleteRecord(
                            statementData.get(selectedIndex));

                    // Refresh the data
                    getDialogData();
                } catch (VizException e) {
                    mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                    mb.setText("Unable to Delete");
                    mb
                            .setMessage("An error occurred while trying to delete the record.");
                    mb.open();

                    e.printStackTrace();
                }
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_WARNING | SWT.OK);
            mb.setText("Invalid Selection");
            mb.setMessage("Please select a statement first.");
            mb.open();
        }
    }

    private boolean saveRecord() {
        boolean successful = false;

        if (checkFKConstraintsMet()) {
            Double impactVal = HydroDataUtils.getDoubleFromTF(shell,
                    impactValTF, "Impact Value", true);
            if (impactVal == null) {
                return successful;
            }

            FloodStatementData newData = new FloodStatementData();

            newData.setLid(lid);
            newData.setImpactValue(impactVal);
            newData.setStatement(textEditor.getText());
            newData.setRiseFall(risingRdo.getSelection() ? "R" : "F");

            SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd");
            dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));

            // Create the start date
            String beginDate = (beginMonthCbo.getSelectionIndex() + 1) + "/"
                    + beginDayTF.getText();

            try {
                newData.setDateStart(dateFormat.format(dateFormat
                        .parse(beginDate)));
            } catch (Exception e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                mb.setMessage("Invalid Begin Date");
                mb.open();

                return successful;
            }

            // Create the end Date
            String endDate = (endMonthCbo.getSelectionIndex() + 1) + "/"
                    + endDayTF.getText();

            try {
                newData
                        .setDateEnd(dateFormat
                                .format(dateFormat.parse(endDate)));
            } catch (Exception e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                mb.setMessage("Invalid End Date");
                mb.open();

                return successful;
            }

            newData.setImpactPE(impactPeTF.getText());

            try {
                HydroDBDataManager.getInstance().putData(newData);

                successful = true;

                // Refresh the data
                getDialogData();
            } catch (VizException e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                mb.setMessage("An error occurred while trying to save.");
                mb.open();

                e.printStackTrace();
            }
        }

        return successful;
    }

    private void updateDialogState(DialogStates currState) {
        switch (currState) {
        case NEW_RECORD:
            deleteBtn.setEnabled(false);
            dataList.setEnabled(false);

            okBtn.setEnabled(true);
            applyBtn.setEnabled(true);
            break;
        case RECORDS_AVAILABLE:
            deleteBtn.setEnabled(true);
            dataList.setEnabled(true);

            okBtn.setEnabled(true);
            applyBtn.setEnabled(true);

            dataList.select(0);
            populateCharacteristics(0);
            break;
        case NO_RECORDS:
            deleteBtn.setEnabled(false);
            dataList.setEnabled(true);

            okBtn.setEnabled(false);
            applyBtn.setEnabled(false);
            break;
        default:
            break;
        }
    }

    private void printRecords() {
        final String text = createSaveFileText();

        if (text != null) {
            PrintDialog dialog = new PrintDialog(shell, SWT.NONE);
            PrinterData data = dialog.open();

            if (data == null) {
                return;
            }

            printer = new Printer(data);

            /*
             * Do the printing in a background thread so that spooling does not
             * freeze the UI.
             */
            Thread printingThread = new Thread("PrintTable") {
                @Override
                public void run() {
                    print(printer, text);
                    printer.dispose();
                }
            };
            printingThread.start();
        }
    }

    private void saveToFile() {
        String text;
        try {
            text = createSaveFileText();
            FileDialog dialog = new FileDialog(shell, SWT.SAVE);
            String filename = dialog.open();
            if (filename == null) {
                return;
            }

            BufferedWriter out = new BufferedWriter(new FileWriter(filename));
            out.write(text);
            out.close();
        } catch (IOException e) {
            e.printStackTrace();
            // TODO Log error here
        }
    }

    /**
     * @return The formatted text containing the Impact Statement data.
     * @throws VizException
     */
    private String createSaveFileText() {
        StringBuffer outputStr = new StringBuffer();

        outputStr.append("FLOOD IMPACT STATEMENT LISTING FOR\n");

        /**********************************************************************
         * Retrieve the station's name, county, and state from the Location
         * table and output to file
         **********************************************************************/

        LocationData seedData = new LocationData();
        seedData.setLid(lid);
        ArrayList<LocationData> locData = null;
        try {
            locData = HydroDBDataManager.getInstance().getData(seedData);
        } catch (VizException e) {
            e.printStackTrace();
        }

        if ((locData != null) && (locData.size() > 0)) {
            // Should only be one record for the LID
            LocationData currLoc = locData.get(0);

            outputStr.append(String.format("%s %s - %s COUNTY, %s\n", currLoc
                    .getLid(), currLoc.getName(), currLoc.getCounty(), currLoc
                    .getState()));
        } else {
            outputStr
                    .append(String
                            .format(
                                    "The name, county, and state for station %s are not available.\n",
                                    lid));
        }

        outputStr.append("--------------------------\n");

        // Put in current time for Generated time
        SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm");
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
        Date now = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();
        outputStr.append(String.format("GENERATED %s\n\n\n", dateFormat
                .format(now)));

        for (FloodStatementData currStatement : statementData) {
            outputStr.append(String.format("IMPACT PE: %s  ", currStatement
                    .getImpactPE()));

            if (currStatement.getImpactPE().equals("QR")) {
                outputStr.append(String.format("IMPACT VALUE:  %9.2f CFS. ",
                        currStatement.getImpactValue()));
            } else {
                outputStr.append(String.format("IMPACT VALUE:  %9.2f FT. ",
                        currStatement.getImpactValue()));
            }

            // Rise/Fall
            if (currStatement.getRiseFall().equals("F")) {
                outputStr.append("(FALLING)     ");
            } else {
                outputStr.append("(RISING)     ");
            }

            // Start/End Dates
            outputStr.append(String.format("APPLIES TO PERIOD:  %s - %s\n\n",
                    currStatement.getDateStart(), currStatement.getDateEnd()));

            // Actual Statement
            if (currStatement.getStatement().equals("")) {
                outputStr
                        .append("AN IMPACT STATEMENT FOR THIS STAGE WAS NOT AVAILABLE.");
            } else {
                outputStr.append(wrapStatement(currStatement.getStatement()));
            }

            outputStr.append("\n\n");
        }

        return outputStr.toString();
    }

    /**
     * Wraps the Impact Statement so there isn't more than 80 characters per
     * line.
     * 
     * @param statement
     * @return
     */
    private String wrapStatement(String statement) {
        StringBuffer formattedStatement = new StringBuffer();

        String[] words = statement.split(" ");
        int lineWidth = 0;

        for (String currWord : words) {
            // if current line width is > 80, insert \n
            if ((lineWidth + currWord.length()) > 80) {
                formattedStatement.append("\n");

                lineWidth = 0;
            }

            // Append current word
            formattedStatement.append(currWord + " ");

            // Set the Current line width
            lineWidth += currWord.length() + 1;
        }

        return formattedStatement.toString();
    }

    /**
     * Send the text to the printer
     * 
     * @param printer
     *            The printer
     * @param text
     *            The text to print
     */
    private void print(Printer printer, String text) {
        if (printer.startJob("Text")) {
            Rectangle clientArea = printer.getClientArea();
            Rectangle trim = printer.computeTrim(0, 0, 0, 0);
            Point dpi = printer.getDPI();
            leftMargin = dpi.x + trim.x; // one inch from left side of paper
            rightMargin = clientArea.width - dpi.x + trim.x + trim.width; // one
            // inch
            // from
            // right
            // side
            // of
            // paper
            topMargin = dpi.y + trim.y; // one inch from top edge of paper
            bottomMargin = clientArea.height - dpi.y + trim.y + trim.height; // one
            // inch
            // from
            // bottom
            // edge
            // of
            // paper

            /* Create a buffer for computing tab width. */
            int tabSize = 4; // is tab width a user setting in your UI?
            StringBuffer tabBuffer = new StringBuffer(tabSize);
            for (int i = 0; i < tabSize; i++)
                tabBuffer.append(' ');
            String tabs = tabBuffer.toString();

            /*
             * Create printer GC, and create and set the printer font &
             * foreground color.
             */
            gc = new GC(printer);

            Font printerFont = new Font(printer, "Monospace", 8, SWT.NORMAL);

            Color printerForegroundColor = new Color(printer, new RGB(0, 0, 0));
            Color printerBackgroundColor = new Color(printer, new RGB(255, 255,
                    255));

            gc.setFont(printerFont);
            gc.setForeground(printerForegroundColor);
            gc.setBackground(printerBackgroundColor);
            tabWidth = gc.stringExtent(tabs).x;
            lineHeight = gc.getFontMetrics().getHeight();

            /* Print text to current gc using word wrap */
            printText(text);

            printer.endJob();

            /* Cleanup graphics resources used in printing */
            printerFont.dispose();
            printerForegroundColor.dispose();
            printerBackgroundColor.dispose();
            gc.dispose();
        }
    }

    /**
     * Print the text
     * 
     * @param text
     *            The text to be printed
     */
    private void printText(String text) {
        printer.startPage();
        wordBuffer = new StringBuffer();
        x = leftMargin;
        y = topMargin;
        index = 0;
        end = text.length();
        while (index < end) {
            char c = text.charAt(index);
            index++;
            if (c != 0) {
                if ((c == 0x0a) || (c == 0x0d)) {
                    if ((c == 0x0d) && (index < end)
                            && (text.charAt(index) == 0x0a)) {
                        index++; // if this is cr-lf, skip the lf
                    }
                    printWordBuffer();
                    newline();
                } else {
                    if (c != '\t') {
                        wordBuffer.append(c);
                    }
                    if (Character.isWhitespace(c)) {
                        printWordBuffer();
                        if (c == '\t') {
                            x += tabWidth;
                        }
                    }
                }
            }
        }
        if (y + lineHeight <= bottomMargin) {
            printer.endPage();
        }
    }

    /**
     * Word buffer for formating lines on the printed page
     */
    private void printWordBuffer() {
        if (wordBuffer.length() > 0) {
            String word = wordBuffer.toString();
            int wordWidth = gc.stringExtent(word).x;
            if (x + wordWidth > rightMargin) {
                /* word doesn't fit on current line, so wrap */
                newline();
            }
            gc.drawString(word, x, y, false);
            x += wordWidth;
            wordBuffer = new StringBuffer();
        }
    }

    /**
     * New line on the printed page
     */
    private void newline() {
        x = leftMargin;
        y += lineHeight;
        if (y + lineHeight > bottomMargin) {
            printer.endPage();
            if (index + 1 < end) {
                y = topMargin;
                printer.startPage();
            }
        }
    }

    /**
     * @return True if the lid meets the db constraints.
     */
    private boolean checkFKConstraintsMet() {
        boolean rval = false;

        // Needs to be in riverstat table
        String query = "SELECT lid FROM riverstat WHERE lid='" + lid + "'";

        try {
            QueryResult data = HydroDBDataManager.getInstance().runMappedQuery(
                    query);

            if (data.getResultCount() > 0) {
                rval = true;
            } else {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                mb
                        .setMessage("Data for the location must be add via the River Gauge dialog first.");
                mb.open();
            }
        } catch (VizException e) {
            e.printStackTrace();
        }

        return rval;
    }
}
