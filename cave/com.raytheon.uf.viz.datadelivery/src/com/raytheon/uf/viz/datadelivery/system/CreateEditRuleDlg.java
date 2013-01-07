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
package com.raytheon.uf.viz.datadelivery.system;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.datadelivery.subscription.xml.SubscriptionRuleXML;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils.NameOperationItems;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils.SubscriptionPriority;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils.TypeOperationItems;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Create and Edit Rules Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 17, 2012   730       jpiatt     Initial creation.
 * Oct  5, 2012  1103       jpiatt     Use util class for pattern match.
 * Nov 09, 2012  1286       djohnson   Consolidate duplicate subscription handling.
 * Dec 18, 2012  1419       bgonzale   Fixed overwrite of values in updatSelectionFields().
 * Dec 18, 2012  1439       mpduff     Redo rule name validation.
 * Dec 18, 2012  1417       bgonzale   Changed value initialization in handleSave().
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class CreateEditRuleDlg extends CaveSWTDialog {

    /** Enumeration to use for Dataset Frequency */
    public static enum FreqUnitOptions {
        /** Operation Minutes */
        MIN("Mins"),
        /** Operation Hours */
        HOURS("Hrs");

        /** Datatype operation */
        private final String frequencyUnitOptions;

        private FreqUnitOptions(String frequencyUnitOptions) {
            this.frequencyUnitOptions = frequencyUnitOptions;
        }

        /**
         * Get frequency unit options.
         * 
         * @return frequencyUnitOptions
         */
        public String getOperation() {
            return frequencyUnitOptions;
        }

        @Override
        public String toString() {
            return frequencyUnitOptions;
        }
    }

    /** Enumeration to use for Dataset Size units */
    public static enum SizeUnitOptions {
        /** Kilobytes */
        KB("KB"),
        /** Megabytes */
        MB("MB"),
        /** Gigabytes */
        GB("GB");

        /** Dataset Size units */
        private final String sizeUnitOptions;

        private SizeUnitOptions(String sizeUnitOptions) {
            this.sizeUnitOptions = sizeUnitOptions;
        }

        /**
         * Get size unit options.
         * 
         * @return sizeUnitOptions
         */
        public String getOperation() {
            return sizeUnitOptions;
        }

        @Override
        public String toString() {
            return sizeUnitOptions;
        }
    }

    /** Instance of SystemRuleManager */
    private final SystemRuleManager srm = SystemRuleManager.getInstance();

    /** Composite for the rule field, operation & value */
    private Composite ruleDefinitionComp;

    /** Group for the rule field, operation & value */
    private Group ruleDefinitionGroup;

    /** SubscriptionRuleXML object */
    private SubscriptionRuleXML ruleXml;

    /** Save button */
    private Button saveBtn;

    /** Field Combination box */
    private Combo fieldCombo;

    /** Operation Combination box */
    private Combo operationCombo;

    /** Priority Combination box */
    private Combo priorityCombo;

    /** Units Combination box */
    private Combo unitsCombo;

    /** Name of the rule selected */
    private String ruleName;

    /** Type of the rule selected */
    private final String ruleType;

    /** Rule name entered */
    private Text ruleNameText;

    /** Rule text value entered */
    private Text ruleValue;

    /** Latency maximum value entered */
    private Text latencyMax;

    /** Create or edit flag */
    private final boolean create;

    /** Dataset Name selected flag */
    private boolean nameFlag = false;

    /** Dataset Type selected flag */
    private boolean typeFlag = false;

    /** Dataset Size selected flag */
    private boolean sizeFlag = false;

    /** Dataset Frequency selected flag */
    private boolean frequencyFlag = false;

    /** Flag indicating initial set up */
    private boolean initial = false;

    /** Priority constant */
    private final String PRIORITY_TYPE = "priority";

    /** Name constant */
    private final String DATASET_NAME = OpsNetFieldNames.NAME.toString();

    /** Datatype constant */
    private final String DATATYPE = OpsNetFieldNames.TYPE.toString();

    /** Size constant */
    private final String DATASET_SIZE = OpsNetFieldNames.SIZE.toString();

    /** Frequency constant */
    private final String DATASET_FREQ = OpsNetFieldNames.FREQUENCY.toString();

    private final IRulesNeedApplying rulesNeedApplying;

    /**
     * Constructor.
     * 
     * @param parent
     *            The parent shell
     * @param create
     *            Create or edit indicator
     * @param ruleName
     *            Name of the rule
     * @param ruleType
     *            type of rule
     * @param rulesNeedApplying
     */
    public CreateEditRuleDlg(Shell parent, boolean create, String ruleName,
            String ruleType, IRulesNeedApplying rulesNeedApplying) {
        super(parent, SWT.RESIZE | SWT.DIALOG_TRIM, CAVE.NONE);
        this.create = create;
        this.ruleName = ruleName;
        this.ruleType = ruleType;
        this.rulesNeedApplying = rulesNeedApplying;

        createRuleHeader();

    }

    /**
     * Constructor.
     * 
     * @param parent
     *            The parent shell
     * @param create
     *            create or edit indicator
     * @param ruleType
     *            type of rule
     */
    public CreateEditRuleDlg(Shell parent, boolean create, String ruleType,
            IRulesNeedApplying rulesNeedApplying) {
        super(parent, SWT.RESIZE | SWT.DIALOG_TRIM, CAVE.NONE);
        this.create = create;
        this.ruleType = ruleType;
        this.rulesNeedApplying = rulesNeedApplying;

        createRuleHeader();

    }

    private void createRuleHeader() {
        if (create) {
            if (PRIORITY_TYPE.equals(ruleType)) {
                setText("Create Priority Rule");
            } else {
                setText("Create Latency Rule");
            }

        } else {
            if (PRIORITY_TYPE.equals(ruleType)) {
                setText("Edit Priority Rule");
            } else {
                setText("Edit Latency Rule");
            }
        }

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
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;

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

        initial = true;
        createRuleNameArea();
        createRuleDefinitionGroup();
        createBottomButtons();
        initial = false;

        this.setReturnValue(false);

    }

    /**
     * Create top bar route information.
     */
    private void createRuleNameArea() {

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, true);

        // Create Rule
        if (create) {

            gl = new GridLayout(2, false);

            Composite textComp = new Composite(shell, SWT.NONE);
            textComp.setLayoutData(gd);
            textComp.setLayout(gl);

            gd = new GridData(115, SWT.DEFAULT);
            Label ruleLbl = new Label(textComp, SWT.NONE);
            ruleLbl.setLayoutData(gd);
            ruleLbl.setText("Enter Rule Name: ");

            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            ruleNameText = new Text(textComp, SWT.BORDER);
            ruleNameText.setLayoutData(gd);
            ruleNameText.setTextLimit(32);
            ruleNameText.setToolTipText("Enter rule text");

            // Edit Rule
        } else {

            Label ruleLbl = new Label(shell, SWT.NONE);
            ruleLbl.setText("Rule Name: " + ruleName);

            ruleXml = srm.loadRule(ruleName);

            if (DATASET_SIZE.equals(ruleXml.getRuleField())) {
                sizeFlag = true;
            } else if (DATASET_FREQ.equals(ruleXml.getRuleField())) {
                frequencyFlag = true;
            }

        }

    }

    /**
     * Dynamic update to the layout when the data field changes.
     */
    private void updateLayout() {

        GridData gd = new GridData(315, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(4, false);

        if (sizeFlag || frequencyFlag) {
            gd = new GridData(315, SWT.DEFAULT, true, false);
            gl = new GridLayout(4, false);
        } else {
            gd = new GridData(315, SWT.DEFAULT, true, false);
            gl = new GridLayout(3, false);
        }

        ruleDefinitionComp.setLayoutData(gd);
        ruleDefinitionComp.setLayout(gl);

        if ((typeFlag || nameFlag) && unitsCombo != null) {
            unitsCombo.dispose();
        }

        if (sizeFlag || frequencyFlag) {
            if (unitsCombo == null || unitsCombo.isDisposed()) {
                gd = new GridData(65, SWT.DEFAULT);
                unitsCombo = new Combo(ruleDefinitionComp, SWT.READ_ONLY);
                unitsCombo.setLayoutData(gd);
                unitsCombo.setLayout(gl);
                unitsCombo.setToolTipText("Select the units of size");
            }

        }

        if (unitsCombo != null && !unitsCombo.isDisposed()) {

            unitsCombo.removeAll();
            if (sizeFlag) {
                createSizeUnitItems();

            } else {
                createFreqUnitItems();
            }
            unitsCombo.select(0);
        }

        shell.layout();
        shell.pack();

    }

    /**
     * Create the rule definition grouping.
     */
    private void createRuleDefinitionGroup() {

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, true);

        ruleDefinitionGroup = new Group(shell, SWT.NONE);
        ruleDefinitionGroup.setLayout(gl);
        ruleDefinitionGroup.setLayoutData(gd);
        ruleDefinitionGroup.setText("Rule Definition");

        if (frequencyFlag || sizeFlag) {
            gd = new GridData(315, SWT.DEFAULT, true, false);
            gl = new GridLayout(4, false);
        } else {
            gd = new GridData(315, SWT.DEFAULT, true, false);
            gl = new GridLayout(3, false);
        }

        ruleDefinitionComp = new Composite(ruleDefinitionGroup, SWT.NONE);
        ruleDefinitionComp.setLayoutData(gd);
        ruleDefinitionComp.setLayout(gl);

        // Field combo
        gd = new GridData(160, SWT.DEFAULT);
        fieldCombo = new Combo(ruleDefinitionComp, SWT.NONE);
        fieldCombo.setLayoutData(gd);
        fieldCombo.setToolTipText("Select a field");

        fieldCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int index = fieldCombo.getSelectionIndex();
                String item = fieldCombo.getItem(index);
                updateSelectionFields(item);
            }

        });

        OpsNetFieldNames[] fieldItems = OpsNetFieldNames.values();
        for (OpsNetFieldNames fn : fieldItems) {
            fieldCombo.add(fn.getFieldName());
        }

        // Operation combo
        gd = new GridData(100, SWT.DEFAULT);
        operationCombo = new Combo(ruleDefinitionComp, SWT.NONE);
        operationCombo.setLayoutData(gd);
        operationCombo.setLayout(gl);
        operationCombo.setToolTipText("Select an operation");

        if (initial) {
            fieldCombo.select(0);
            createNameOpItems();
            operationCombo.select(0);
        }

        gd = new GridData(100, SWT.DEFAULT);
        ruleValue = new Text(ruleDefinitionComp, SWT.BORDER);
        ruleValue.setTextLimit(20);
        ruleValue.setLayoutData(gd);
        ruleValue.setToolTipText("Enter rule text");

        if (sizeFlag || frequencyFlag) {
            gd = new GridData(65, SWT.DEFAULT);
            unitsCombo = new Combo(ruleDefinitionComp, SWT.READ_ONLY);
            unitsCombo.setLayoutData(gd);
            unitsCombo.setLayout(gl);
            unitsCombo.setToolTipText("Select the units of size");
            if (sizeFlag) {
                createSizeUnitItems();
            } else {
                createFreqUnitItems();
            }
            unitsCombo.select(0);

        }

        if (PRIORITY_TYPE.equals(ruleType)) {
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            gl = new GridLayout(2, false);

            Composite prioritySelectionComp = new Composite(
                    ruleDefinitionGroup, SWT.NONE);
            prioritySelectionComp.setLayoutData(gd);
            prioritySelectionComp.setLayout(gl);

            // Priority combo
            Label operation = new Label(prioritySelectionComp, SWT.NONE);
            operation.setText("Priority:");

            gd = new GridData(80, SWT.DEFAULT);
            priorityCombo = new Combo(prioritySelectionComp, SWT.READ_ONLY);
            priorityCombo.setLayoutData(gd);
            priorityCombo.setLayout(gl);
            priorityCombo.setToolTipText("Select the priority of the rule");

            priorityCombo.removeAll();
            SubscriptionPriority[] priorityOptions = SubscriptionPriority
                    .values();

            for (SubscriptionPriority opt : priorityOptions) {
                priorityCombo.add(opt.getPriorityName());
            }

            priorityCombo.select(0);

        } else {
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            gl = new GridLayout(3, false);

            Composite latencySelectionComp = new Composite(ruleDefinitionGroup,
                    SWT.NONE);
            latencySelectionComp.setLayoutData(gd);
            latencySelectionComp.setLayout(gl);

            // Latency combo
            gd = new GridData(120, SWT.DEFAULT);
            Label latencyLbl = new Label(latencySelectionComp, SWT.NONE);
            latencyLbl.setLayoutData(gd);
            latencyLbl.setText("Maximum Latency: ");

            gd = new GridData(45, SWT.DEFAULT);
            latencyMax = new Text(latencySelectionComp, SWT.BORDER);
            latencyMax.setTextLimit(5);
            latencyMax.setLayoutData(gd);
            latencyMax.setToolTipText("Enter latency in minutes");

            gd = new GridData(75, SWT.DEFAULT);
            Label minutesLbl = new Label(latencySelectionComp, SWT.NONE);
            minutesLbl.setLayoutData(gd);
            minutesLbl.setText("Minutes");

        }

        populateFields();
        ruleDefinitionGroup.pack();

    }

    /**
     * Upon edit, populate the fields.
     */
    private void populateFields() {

        if (!create) {
            String field = ruleXml.getRuleField();
            if (!field.isEmpty()) {
                fieldCombo.select(fieldCombo.indexOf(field));
            } else {
                fieldCombo.select(0);
            }

            updateSelectionFields(field);

            String operator = ruleXml.getRuleOperator();
            if (!operator.isEmpty()) {
                operationCombo.select(operationCombo.indexOf(operator));
            } else {
                operationCombo.select(0);
            }

            String value = ruleXml.getRuleValue();
            if (!value.isEmpty()) {
                ruleValue.setText(value);
            }

            String unit = ruleXml.getRuleUnit();
            if (unit != null) {
                unitsCombo.select(unitsCombo.indexOf(unit));
            }

            Integer priority = ruleXml.getRulePriority();
            if (PRIORITY_TYPE.equals(ruleType)) {
                int o = 0;
                SubscriptionPriority[] priorityOptions = SubscriptionPriority
                        .values();
                for (SubscriptionPriority item : priorityOptions) {
                    if (priority == item.getPriorityValue()) {
                        priorityCombo.select(o);
                        break;
                    }
                    o++;
                }
            } else {
                Integer latency = ruleXml.getRuleLatency();
                if (latency != null) {
                    latencyMax.setText(latency.toString());
                }
            }
        }
    }

    /**
     * Create the bottom buttons on the Find Dialog pop up.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(2, false);

        Composite bottomComp = new Composite(shell, SWT.NONE);
        bottomComp.setLayout(gl);
        bottomComp.setLayoutData(gd);

        // OK Button
        int btnWidth = 100;
        GridData btnData = new GridData(btnWidth, SWT.DEFAULT);
        saveBtn = new Button(bottomComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(btnData);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (handleSave()) {
                    close();
                }
            }
        });

        // Cancel Button
        btnData = new GridData(btnWidth, SWT.DEFAULT);
        Button cancelBtn = new Button(bottomComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(btnData);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });

    }

    /**
     * Update the selections.
     */
    private void updateSelectionFields(String fieldName) {
        nameFlag = false;
        typeFlag = false;
        sizeFlag = false;
        frequencyFlag = false;

        if (DATASET_NAME.equals(fieldName)) {
            nameFlag = true;
            createNameOpItems();
        } else if (DATATYPE.equals(fieldName)) {
            typeFlag = true;
            operationCombo.removeAll();
            TypeOperationItems[] typeOperation = TypeOperationItems.values();
            for (TypeOperationItems top : typeOperation) {
                operationCombo.add(top.getOperation());
            }
        } else if (DATASET_SIZE.equals(fieldName)) {
            sizeFlag = true;
        } else if (DATASET_FREQ.equals(fieldName)) {
            frequencyFlag = true;
        }

        if (sizeFlag || frequencyFlag) {
            createSizeOpItems();
        }

        operationCombo.select(0);

        if (!initial) {
            updateLayout();
        }
    }

    /**
     * Check validity and save the rule.
     */
    private boolean handleSave() {

        boolean valid = false;

        if (create) {
            valid = DataDeliveryGUIUtils.hasText(ruleNameText);

            if (!valid) {
                DataDeliveryUtils
                        .showMessage(shell, SWT.ERROR,
                                DataDeliveryGUIUtils.NAME_REQUIRED_TITLE,
                                "Name required. The Rule Name needs to contain between 1 and 32 characters.");
                return false;
            }

            ruleName = ruleNameText.getText();
        }

        if (DataDeliveryGUIUtils.INVALID_CHAR_PATTERN.matcher(ruleName.trim())
                .find()) {
            DataDeliveryUtils.showMessage(getShell(), SWT.ERROR,
                    DataDeliveryGUIUtils.INVALID_CHARS_TITLE,
                    DataDeliveryGUIUtils.INVALID_CHARS_MESSAGE);
            return false;
        }

        SubscriptionPriority priorityVal = null;
        Integer priority = null;
        Integer latency = null;
        String unit = null;
        String fieldName = fieldCombo.getItem(fieldCombo.getSelectionIndex());
        String operator = operationCombo.getItem(operationCombo
                .getSelectionIndex());

        if (frequencyFlag || sizeFlag) {
            unit = unitsCombo.getItem(unitsCombo.getSelectionIndex());
        }

        if (priorityCombo != null) {
            priorityVal = SubscriptionPriority.valueOf(priorityCombo.getText()
                    .toUpperCase());
            for (SubscriptionPriority pri : SubscriptionPriority.values()) {
                if (pri.equals(priorityVal)) {
                    priority = pri.getPriorityValue();
                    break;
                }
            }

        }

        if (latencyMax != null) {
            if (!DataDeliveryGUIUtils.hasText(latencyMax)) {
                DataDeliveryUtils
                        .showMessage(shell, SWT.ERROR,
                                "Invalid Maximum Latency",
                                "Maximum Latency invalid. Please enter a value for Maximum Latency.");
                return false;
            }

            String latencyString = latencyMax.getText().trim();

            // Don't allow if text contains non-digit characters
            if (!DataDeliveryGUIUtils.DIGIT_PATTERN.matcher(latencyString)
                    .matches()) {
                DataDeliveryUtils
                        .showMessage(shell, SWT.ERROR,
                                "Invalid Maximum Latency",
                                "Maximum Latency invalid. Maximum Latency must be entered as digits only.");
                return false;
            }
            latency = Integer.parseInt(latencyString);
        }

        SubscriptionRuleXML rule = new SubscriptionRuleXML();
        if (DataDeliveryGUIUtils.hasText(ruleValue)) {

            String value = ruleValue.getText();
            if (DATASET_SIZE.equals(fieldName)
                    || DATASET_FREQ.equals(fieldName)) {

                // Don't allow if text contains non-digit characters
                if (!DataDeliveryGUIUtils.DIGIT_PATTERN.matcher(value.trim())
                        .matches()) {
                    DataDeliveryUtils
                            .showMessage(
                                    shell,
                                    SWT.ERROR,
                                    "Invalid Value",
                                    "Invalid value. Value is invalid for "
                                            + fieldName
                                            + ". Value must be entered as digits only.");
                    return false;
                }
            }
            rule.setRuleValue(value);

        } else {
            DataDeliveryUtils.showMessage(getShell(), SWT.ERROR,
                    "Invalid Value",
                    "Invalid value. Please enter a value for the " + fieldName
                            + ".");
            return false;
        }

        rule.setRuleName(ruleName);
        rule.setRuleField(fieldName);
        rule.setRuleOperator(operator);
        rule.setRuleUnit(unit);
        rule.setRulePriority(priority);
        rule.setRuleLatency(latency);

        // Save the rule file
        boolean saved = srm.saveRule(rule, shell);

        if (saved) {
            setReturnValue(true);
            rulesNeedApplying.flagRulesAsNeedApplying();
        }

        return true;

    }

    private void createSizeUnitItems() {
        unitsCombo.removeAll();
        SizeUnitOptions[] sizeUnits = SizeUnitOptions.values();
        for (SizeUnitOptions suo : sizeUnits) {
            unitsCombo.add(suo.getOperation());
        }

    }

    private void createSizeOpItems() {
        operationCombo.removeAll();
        OperatorTypes[] sizeOps = OperatorTypes.values();
        for (OperatorTypes suo : sizeOps) {
            operationCombo.add(suo.getOperation());
        }

    }

    private void createFreqUnitItems() {
        FreqUnitOptions[] freqUnits = FreqUnitOptions.values();
        for (FreqUnitOptions fuo : freqUnits) {
            unitsCombo.add(fuo.getOperation());
        }

    }

    private void createNameOpItems() {
        operationCombo.removeAll();
        NameOperationItems[] nameOperation = NameOperationItems.values();
        for (NameOperationItems nop : nameOperation) {
            operationCombo.add(nop.getOperation());
        }

    }

}
