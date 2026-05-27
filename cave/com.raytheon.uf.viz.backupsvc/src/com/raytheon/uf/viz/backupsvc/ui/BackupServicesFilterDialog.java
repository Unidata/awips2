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
package com.raytheon.uf.viz.backupsvc.ui;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CCombo;
import org.eclipse.swt.custom.TableEditor;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.backupsvc.constants.BSJFilterData;
import com.raytheon.uf.viz.backupsvc.constants.BSJFilterMemo;
import com.raytheon.uf.viz.backupsvc.constants.VizBackupServicesConstants;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Filter Dialog interface is for user to setup the filtering conditions when
 * retrieving backup service job data from outgoing or incoming DB tables.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- -----------  --------------------------
 * May 25, 2021 84476      Gang Chen       Initial creation
 * Jul 01, 2021 93517      Amanuel Challa  Added a new field that is an instance of
 *                                         AbstractBackupServicesDialogTab in the Constructor
 *                                         and added handleRefresh() in saveListener
 * Jul 13, 2021 93058      Amanuel Challa  Modified deleteListener() to Replace the condition String of
 *                                         the first item when deleted from (and) to (" ") and added inputValidation()
 * Aug 05, 2021 94872      Lisa Singh      Fixed dialog height. Standardized column and operand names.
 * Aug 12, 2021 84654      Robert.Blum     Cleaned up constant files.
 * Aug 17, 2021 95214      Amanuel.Challa  Modified inputValidation() to handle inputs with leading and trailing spaces
 * Aug 31, 2021 95283      Amanuel Challa  Fixed dialog to remain open when user inputs are not valid
 * Spt 21, 2021 96163      Amanuel.Challa  Added filter applied texts when apply/save button is pressed
 * May 01, 2023 2033487    Lisa.Singh      Renamed Constants.
 * </pre>
 *
 * @author Gang Chen
 */
public class BackupServicesFilterDialog extends CaveSWTDialog {
    private Composite filterTableComp;

    private Table filterTable;

    private boolean isOutgoing; // true: outgoing; false: incoming

    public static final String SELECT_COLUMN = "Select";

    public static final String FIELD_COLUMN = "Field Name";

    public static final String OPERAND_COLUMN = "Operand";

    public static final String VALUE_COLUMN = "Values";

    String[] filterTableColumns = { SELECT_COLUMN, FIELD_COLUMN, OPERAND_COLUMN,
            VALUE_COLUMN };

    public static final String EQUALS_OPERAND = "=";

    public static final String NOT_EQUALS_OPERAND = "!=";

    public static final String GREATER_THAN_OPERAND = ">";

    public static final String GREATER_THAN_EQUALS_OPERAND = ">=";

    public static final String LESS_THAN_OPERAND = "<";

    public static final String LESS_THAN_EQUALS_OPERAND = "<=";

    public static final String IN_OPERAND = "in";

    String[] filterTableColumOperands = { EQUALS_OPERAND, NOT_EQUALS_OPERAND,
            GREATER_THAN_OPERAND, GREATER_THAN_EQUALS_OPERAND,
            LESS_THAN_OPERAND, LESS_THAN_EQUALS_OPERAND, IN_OPERAND };

    protected final IUFStatusHandler statusLogger = UFStatus
            .getHandler(BackupServicesFilterDialog.class);

    AbstractBackupServicesDialogTab inOutDialog;

    protected BackupServicesFilterDialog(Shell parent, String filterDialogTitle,
            boolean isOutgoing, AbstractBackupServicesDialogTab inOutDialog) {
        super(parent, SWT.SHELL_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK
                | CAVE.PERSPECTIVE_INDEPENDENT | CAVE.MODE_INDEPENDENT);
        setText(filterDialogTitle);
        this.isOutgoing = isOutgoing;
        if (inOutDialog instanceof OutgoingBackupServicesTab) {
            this.inOutDialog = inOutDialog;
        } else {
            this.inOutDialog = inOutDialog;
        }
    }

    @Override
    protected void initializeComponents(Shell shell) {
        shell.setLayout(new GridLayout(1, false));
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, false, false);
        shell.setLayoutData(gridData);
        // Create filter table
        filterTableComp = new Composite(shell, SWT.BORDER);
        filterTableComp
                .setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        filterTableComp.setLayout(new GridLayout(1, false));
        createFilterTables(filterTableComp);
        loadSavedFilterData(BSJFilterMemo.getFilterDataArray(isOutgoing));
        // Create buttons
        Composite buttonPlaceComp = new Composite(shell, SWT.BORDER);
        buttonPlaceComp.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        buttonPlaceComp.setLayout(new GridLayout(1, false));
        createButtons(buttonPlaceComp);
    }

    /**
     * Create the filters for outgoing|incoming backup service jobs
     *
     * @param parent
     *            The parent Composite
     *
     */
    private void createFilterTables(Composite parent) {
        filterTable = new Table(parent, SWT.CHECK | SWT.BORDER | SWT.V_SCROLL
                | SWT.H_SCROLL | SWT.MULTI);
        filterTable.setHeaderVisible(true);
        filterTable.setLinesVisible(true);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.heightHint = 150;
        gridData.horizontalSpan = 1;
        filterTable.setLayoutData(gridData);
        // Set filter table columns
        for (String title : filterTableColumns) {
            TableColumn filterTableColumn = new TableColumn(filterTable,
                    SWT.NULL);
            filterTableColumn.setText(title);
            switch (title) {
            case SELECT_COLUMN:
            case OPERAND_COLUMN:
                filterTableColumn.pack();
                break;
            default:
                filterTableColumn.setWidth(200);
                break;
            }
        }
    }

    private void loadSavedFilterData(
            ArrayList<BSJFilterData> theFilterDataArray) {
        if ((theFilterDataArray == null) || (theFilterDataArray.size() == 0)) {
            return;
        }
        // Load the saved filter conditions at runtime
        for (BSJFilterData filterDataItem : theFilterDataArray) {
            TableItem filterItem = new TableItem(filterTable, SWT.NULL);
            // Condition column
            buildCondition(filterItem, 0, filterDataItem.getCondition());
            // Field Name column
            buildFieldCombo(filterItem, 1, filterDataItem.getFieldName());
            // Operand column
            buildOperandCombo(filterItem, 2, filterDataItem.getOperand());
            // Values column
            buildValuesText(filterItem, 3, filterDataItem.getValues());
        }
    }

    /**
     * Create action buttons to add or delete filtering conditions
     *
     * @param parent
     *            The parent Composite
     */
    private void createButtons(Composite parent) {
        Composite buttonsComp = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout(4, true);
        layout.horizontalSpacing = 15;
        buttonsComp.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        buttonsComp.setLayout(layout);
        buildButton(buttonsComp, SWT.PUSH, "Add", addListener);
        buildButton(buttonsComp, SWT.PUSH, "Delete", deleteListener);
        buildButton(buttonsComp, SWT.PUSH, "Apply and Save", saveListener);
        buildButton(buttonsComp, SWT.PUSH, "Cancel", cancelListener);
    }

    // Build button function for Add, Delete, Save, Cancel buttons
    private Button buildButton(Composite parent, int style, String label,
            SelectionListener listener) {
        Button customizedButton = new Button(parent, style);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        customizedButton.setLayoutData(gridData);
        customizedButton.setText(label);
        customizedButton.addSelectionListener(listener);
        return customizedButton;
    }

    // Build condition table item field
    private void buildCondition(TableItem filterItem, int filterItemIdx) {
        buildCondition(filterItem, filterItemIdx, null);
    }

    private void buildCondition(TableItem filterItem, int filterItemIdx,
            String filterItemVal) {
        if (filterItemVal != null) {
            filterItem.setText(filterItemIdx, filterItemVal);
            return;
        }
        if (filterTable.getItemCount() > 1) {
            filterItem.setText(filterItemIdx, " and ");
        } else {
            filterItem.setText(filterItemIdx, " ");
        }
    }

    // Build fieldCombo table item field and listener
    private CCombo buildFieldCombo(TableItem filterItem, int filterItemIdx) {
        return buildFieldCombo(filterItem, filterItemIdx, null);
    }

    private CCombo buildFieldCombo(TableItem filterItem, int filterItemIdx,
            String filterItemVal) {
        CCombo customizedCombo = new CCombo(filterTable,
                SWT.NONE | SWT.READ_ONLY);
        if (isOutgoing) {
            customizedCombo.setItems(
                    VizBackupServicesConstants.OUTGOING_TABLE_COLUMNS);
        } else {
            customizedCombo.setItems(
                    VizBackupServicesConstants.INCOMING_TABLE_COLUMNS);
        }
        if (filterItemVal == null) {
            customizedCombo.setText(customizedCombo.getItem(0));
        } else {
            customizedCombo.setText(filterItemVal);
        }
        TableEditor fieldEditor = new TableEditor(filterTable);
        filterItem.setText(filterItemIdx, customizedCombo.getText());
        fieldEditor.grabHorizontal = true;
        fieldEditor.setEditor(customizedCombo, filterItem, filterItemIdx);
        customizedCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                filterItem.setText(filterItemIdx, customizedCombo.getText());
                // TODO hint text for Last Update Field Value
                /*
                 * if (customizedCombo.getText().equals("Last Update")) {
                 * customizedText.setText("HH:mm dd-MMM-yyyy");
                 * filterItem.setText(3, customizedText.getText());
                 *
                 * }
                 */
            }
        });
        return customizedCombo;
    }

    // Build operandCombo table item field and listener
    private CCombo buildOperandCombo(TableItem filterItem, int filterItemIdx) {
        return buildOperandCombo(filterItem, filterItemIdx, null);
    }

    private CCombo buildOperandCombo(TableItem filterItem, int filterItemIdx,
            String filterItemVal) {
        CCombo customizedCombo = new CCombo(filterTable,
                SWT.NONE | SWT.READ_ONLY);
        customizedCombo.setItems(filterTableColumOperands);
        if (filterItemVal == null) {
            customizedCombo.setText(customizedCombo.getItem(0));
        } else {
            customizedCombo.setText(filterItemVal);
        }
        TableEditor fieldEditor = new TableEditor(filterTable);
        filterItem.setText(filterItemIdx, customizedCombo.getText());
        fieldEditor.grabHorizontal = true;
        fieldEditor.setEditor(customizedCombo, filterItem, filterItemIdx);
        customizedCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                filterItem.setText(filterItemIdx, customizedCombo.getText());
            }
        });
        return customizedCombo;
    }

    // Build valueText table item field and listener
    private Text buildValuesText(TableItem filterItem, int filterItemIdx) {
        return buildValuesText(filterItem, filterItemIdx, null);
    }

    private Text buildValuesText(TableItem filterItem, int filterItemIdx,
            String filterItemVal) {
        Text customizedText = new Text(filterTable, SWT.NONE);
        if (filterItemVal == null) {
            customizedText.setText("null");
        } else {
            customizedText.setText(filterItemVal);
        }
        TableEditor valuesEditor = new TableEditor(filterTable);
        filterItem.setText(filterItemIdx, customizedText.getText());
        valuesEditor.grabHorizontal = true;
        valuesEditor.setEditor(customizedText, filterItem, filterItemIdx);
        customizedText.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(ModifyEvent e) {
                filterItem.setText(filterItemIdx, customizedText.getText());
            }
        });
        return customizedText;
    }

    /*
     * Button listener functions
     */
    private SelectionListener addListener = new SelectionAdapter() {
        @Override
        public void widgetSelected(SelectionEvent e) {
            TableItem filterItem = new TableItem(filterTable, SWT.NULL);
            // Condition column
            buildCondition(filterItem, 0);
            // Field Name column
            buildFieldCombo(filterItem, 1);
            // Operand column
            buildOperandCombo(filterItem, 2);
            // Values column
            buildValuesText(filterItem, 3);
        }
    };

    private SelectionListener deleteListener = new SelectionAdapter() {
        @Override
        public void widgetSelected(SelectionEvent e) {
            boolean isNoChecked = true;
            ArrayList<BSJFilterData> uncheckedFilterItems = new ArrayList<>();
            for (TableItem filterItem : filterTable.getItems()) {
                if (!filterItem.getChecked()) {
                    BSJFilterData filterData = new BSJFilterData(
                            filterItem.getText(0), filterItem.getText(1),
                            filterItem.getText(2), filterItem.getText(3));
                    uncheckedFilterItems.add(filterData);
                } else {
                    isNoChecked = false;
                    statusLogger.info(
                            "Removed filter condition: " + filterItem.getText(0)
                                    + " " + filterItem.getText(1) + " "
                                    + filterItem.getText(2) + " "
                                    + filterItem.getText(3));
                }
            }
            if (isNoChecked) {
                return;
            }
            // Clean all children controls from filterTable
            int count = filterTable.getChildren().length;
            while (count > 0) {
                filterTable.getChildren()[count - 1].dispose();
                count = filterTable.getChildren().length;
            }
            filterTable.clearAll();
            filterTable.removeAll();
            // Replace condition String of the first item from (and) to (" ")
            if (uncheckedFilterItems.size() >= 1) {
                String newCondition = " ";
                String fieldName = uncheckedFilterItems.get(0).getFieldName();
                String operand = uncheckedFilterItems.get(0).getOperand();
                String value = uncheckedFilterItems.get(0).getValues();
                uncheckedFilterItems.set(0, new BSJFilterData(newCondition,
                        fieldName, operand, value));
            }
            // Refresh to load unchecked filter items
            loadSavedFilterData(uncheckedFilterItems);
        }
    };

    private SelectionListener saveListener = new SelectionAdapter() {
        @Override
        public void widgetSelected(SelectionEvent e) {
            ArrayList<BSJFilterData> filterDataArray = new ArrayList<>();
            boolean exitFilter = false;
            for (TableItem filterTableItem : filterTable.getItems()) {
                // Verify user inputs
                boolean isValid = inputValidation(filterTableItem.getText(1),
                        filterTableItem.getText(3));
                if (!isValid) {
                    statusLogger.error("Invalid Input for Field Name: "
                            + filterTableItem.getText(1));
                    exitFilter = true;
                }
                BSJFilterData filterData = new BSJFilterData(
                        filterTableItem.getText(0), filterTableItem.getText(1),
                        filterTableItem.getText(2), filterTableItem.getText(3));
                filterDataArray.add(filterData);
            }
            // Don't make a call to DB if all inputs are not valid
            if (!exitFilter) {
                BSJFilterMemo.setFilterDataArray(filterDataArray, isOutgoing);
                statusLogger.info(BSJFilterMemo.toSQLStatement(isOutgoing));
                // call handleRefresh from AbstractBackupServicesDialogTab
                inOutDialog.handleRefresh();
                close();
            }

            // Set the text for filter applied Label indicator
            if (isOutgoing) {
                inOutDialog.tab.setText(BSJFilterMemo.getOutgoingTabName());
            } else {
                inOutDialog.tab.setText(BSJFilterMemo.getIncomingTabName());
            }

        }
    };

    /**
     * User input validation in Filter Dialog
     *
     * @param fieldName
     * @param value
     * @return isValid boolean value to indicate pass/fail
     */
    private boolean inputValidation(String fieldName, String value) {
        boolean isValid = true;

        if (value == null) {
            return false;
        }
        // Split all input values into and array
        String valueArray[] = value.trim().split("\\s*,\\s*");
        for (String element : valueArray) {
            // Input for ID that are Digit only
            if (fieldName
                    .equalsIgnoreCase(VizBackupServicesConstants.VIZ_COLUMN_ID)) {
                String pattern = "\\d+(\\d+)?";

                isValid = element.matches(pattern);
            } /*
               * Validate alphanumeric inputs Ex:OAX, OAX_01, OAX-01, 24.1.2.0,
               * Undefined
               */
            else if (fieldName
                    .equalsIgnoreCase(VizBackupServicesConstants.VIZ_COLUMN_DOMAIN)
                    || fieldName.equalsIgnoreCase(
                            VizBackupServicesConstants.VIZ_COLUMN_SENDER)
                    || fieldName.equalsIgnoreCase(
                            VizBackupServicesConstants.VIZ_COLUMN_SYSTEM_VERSION)
                    || fieldName.equalsIgnoreCase(
                            VizBackupServicesConstants.VIZ_COLUMN_RECIPIENT_VERSION)) {
                String pattern = "^[a-zA-Z0-9\\._-]*$";

                isValid = element.matches(pattern);
                // Validate Character inputs
            } else if (fieldName.equalsIgnoreCase(
                    VizBackupServicesConstants.VIZ_COLUMN_STATUS)) {
                String pattern = "^[a-zA-Z]+";

                isValid = element.matches(pattern);
            }
            // Validate alphanumeric for Job File Location, Job File Name
            else if (fieldName.equalsIgnoreCase(
                    VizBackupServicesConstants.VIZ_COLUMN_JOB_FILE_PATH)
                    || fieldName.equalsIgnoreCase(
                            VizBackupServicesConstants.VIZ_COLUMN_JOB_FILE_LOCATION)) {
                String pattern = "^[a-zA-Z0-9\\./_:-]+$";

                isValid = element.matches(pattern);
            }
            // Input for Time/Date fields verify using REGEX_TimeDateMap list
            else if (fieldName.equalsIgnoreCase(
                    VizBackupServicesConstants.VIZ_COLUMN_LAST_UPDATE)) {
                isValid = false;

                for (String key : VizBackupServicesConstants.REGEX_TIME_DATE_MAP
                        .keySet()) {
                    if (element.toLowerCase().matches(key)) {
                        isValid = true;
                        break;
                    }
                }
            }
        }
        return isValid;
    }

    private SelectionListener cancelListener = new SelectionAdapter() {
        @Override
        public void widgetSelected(SelectionEvent e) {
            // Close the filtering dialog UI
            close();
        }
    };

    @Override
    protected void preOpened() {
        super.preOpened();
        shell.setMinimumSize(shell.getSize());
    }
}
