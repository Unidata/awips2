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
package com.raytheon.viz.avnconfig;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;

import javax.xml.bind.JAXB;

import org.apache.commons.configuration.ConfigurationException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.viz.avncommon.SyntaxMonitorCfg;
import com.raytheon.viz.avnconfig.AvnConfigConstants.DataSource;
import com.raytheon.viz.avnconfig.AvnConfigConstants.RuleType;

/**
 * This is a Composite containing controls that will be displayed in the
 * TabFolder on the main display.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 22 MAY 2008  1119       lvenable    Initial creation
 * 29 OCT 2010  7262       rferrel     Realigned columns for wider method
 *                                     names.
 * 09 NOV 2011  8865       rferrel     Fixed selection of Edit Buttons.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class DataSourceTabComp extends Composite implements
        IAvailMethodSelected {
    /**
     * Parent composite.
     */
    private Composite parent;

    /**
     * Data list control.
     */
    private List dataList;

    /**
     * Font for the data list.
     */
    private Font listFont;

    /**
     * Scrolled composite used for scrolling the method buttons.
     */
    private ScrolledComposite buttonScrolledComp;

    /**
     * Scroll composite width.
     */
    private final int SCROLLED_COMP_WIDTH = 170;

    /**
     * Scrolled composite height.
     */
    private final int SCROLLED_COMP_HEIGHT = 300;

    /**
     * Add button.
     */
    private Button addBtn;

    /**
     * Replace button.
     */
    private Button replaceBtn;

    /**
     * Remove button.
     */
    private Button removeBtn;

    /**
     * Selected method label.
     */
    private Label selectedMethodLbl;

    /**
     * Severity combo control.
     */
    private Combo severityCbo;

    /**
     * Type combo control.
     */
    private Combo typeCbo;

    /**
     * Unique check box.
     */
    private Button uniqueChk;

    /**
     * Message text control.
     */
    private Text messageTF;

    /**
     * Page data object.
     */
    private PageData pageData;

    /**
     * Comment label.
     */
    private Label commentLbl;

    /**
     * Array of label & text controls.
     */
    private ArrayList<LabelTextControls> labelTextArray;

    /**
     * Label & text composite.
     */
    private Composite labelTextComp;

    /**
     * Array of all active monitoring rules.
     */
    private ArrayList<MethodData> activeRules;

    /**
     * Array of the severity colors.
     */
    private String[] severityColors;

    /**
     * SiteID
     */
    private String siteID;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param pageData
     *            Page data.
     * @throws LocalizationException
     * @throws SerializationException
     * @throws ConfigurationException
     */
    public DataSourceTabComp(Composite parent, final PageData pageData,
            String siteID) throws ConfigurationException,
            SerializationException, LocalizationException {
        super(parent, SWT.NONE);

        this.parent = parent;
        this.pageData = pageData;
        this.siteID = siteID;

        initComponents();

    }

    /**
     * Initialize the components on the display.
     * 
     * @throws SerializationException
     * @throws LocalizationException
     * @throws ConfigurationException
     * 
     */
    private void initComponents() throws SerializationException,
            ConfigurationException, LocalizationException {
        listFont = new Font(parent.getDisplay(), "Monospace", 10, SWT.NORMAL);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        this.setLayout(gl);
        this.setLayoutData(gd);

        SyntaxMonitorCfg syntaxMonCfg = null;
        IPathManager pm = PathManagerFactory.getPathManager();
        File path = pm
                .getStaticFile("aviation/config/gui/SyntaxMonitorCfg.xml");
        syntaxMonCfg = JAXB.unmarshal(path, SyntaxMonitorCfg.class);
        String colors = syntaxMonCfg.getMonitorColors();
        severityColors = StringUtil.split(colors, ",");
        if (severityColors.length < 3) {
            throw new ConfigurationException(
                    String.format(
                            "File \"%s\"'s tag <MonitorColors> must contain at least 3 colors",
                            path.getAbsoluteFile()));
        }
        createListLabels();
        createDataList();
        createBottomControls();

        this.addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent e) {
                listFont.dispose();
            }
        });

        loadActiveRulesList();
        populateList();
    }

    /**
     * Create the labels located above the data list.
     */
    private void createListLabels() {
        Composite labelComp = new Composite(this, SWT.NONE);
        labelComp.setLayout(new GridLayout(3, false));
        labelComp
                .setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        GridData gd = new GridData(101, SWT.DEFAULT);
        Label colorLbl = new Label(labelComp, SWT.NONE);
        colorLbl.setText("Color");
        colorLbl.setLayoutData(gd);

        gd = new GridData(160, SWT.DEFAULT);
        Label methodLbl = new Label(labelComp, SWT.NONE);
        methodLbl.setText("Method");
        methodLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        Label messageLbl = new Label(labelComp, SWT.NONE);
        messageLbl.setText("Message");
        messageLbl.setLayoutData(gd);
    }

    /**
     * Create the data list control.
     */
    private void createDataList() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 900;
        gd.heightHint = 220;
        gd.horizontalSpan = 3;
        dataList = new List(this, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL
                | SWT.H_SCROLL);
        dataList.setLayoutData(gd);
        dataList.setFont(listFont);
        dataList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                selectionAction();
            }
        });
    }

    /**
     * Determine button activation after change in rule selection.
     */
    private void selectionAction() {
        if (dataList.getItemCount() == 0) {
            replaceBtn.setEnabled(false);
            removeBtn.setEnabled(false);
        } else if (dataList.getSelectionIndex() < 0) {
            replaceBtn.setEnabled(false);
            removeBtn.setEnabled(false);
        } else {
            replaceBtn.setEnabled(true);
            removeBtn.setEnabled(true);
            addBtn.setEnabled(true);
            int selectedRule = dataList.getSelectionIndex();
            MethodData rule = activeRules.get(selectedRule);
            selectedMethodLbl.setText(rule.getMethodName());
            uniqueChk.setSelection(rule.getUnique());
            typeCbo.select(rule.getType().ordinal());
            severityCbo.select(Integer.valueOf(rule.getSeverity() - 2));
            messageTF.setText(rule.getMessage());
            updateArgLabelTextControls(rule);
        }
    }

    /**
     * Create the controls at the bottom half of the dialog.
     */
    private void createBottomControls() {
        Composite mainBottomComp = new Composite(this, SWT.NONE);
        mainBottomComp.setLayout(new GridLayout(2, false));
        mainBottomComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                true));

        createButtonScrolledComp(mainBottomComp);

        createRuleEditorGroup(mainBottomComp);
    }

    /**
     * Create the scrolled composite that will contain the available method
     * buttons.
     * 
     * @param mainBottomComp
     *            Parent composite.
     */
    private void createButtonScrolledComp(Composite mainBottomComp) {
        buttonScrolledComp = new ScrolledComposite(mainBottomComp, SWT.V_SCROLL
                | SWT.BORDER);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        buttonScrolledComp.setLayout(gl);
        GridData gd = new GridData(SCROLLED_COMP_WIDTH, SCROLLED_COMP_HEIGHT);
        gd.heightHint = SCROLLED_COMP_HEIGHT;
        buttonScrolledComp.setLayoutData(gd);

        gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        final AvailMethodsComp availMethodComp = new AvailMethodsComp(
                buttonScrolledComp, pageData.getMethodArray(), this);
        availMethodComp.setLayout(gl);

        availMethodComp.layout();

        buttonScrolledComp.setContent(availMethodComp);
        buttonScrolledComp.setExpandHorizontal(true);
        buttonScrolledComp.setExpandVertical(true);

        buttonScrolledComp.addControlListener(new ControlAdapter() {
            @Override
            public void controlResized(ControlEvent e) {
                Rectangle r = buttonScrolledComp.getClientArea();
                buttonScrolledComp.setMinSize(availMethodComp.computeSize(
                        r.width, SWT.DEFAULT));
            }
        });

        buttonScrolledComp.layout();
    }

    /**
     * Create the rule editor group composite.
     * 
     * @param mainBottomComp
     */
    private void createRuleEditorGroup(Composite mainBottomComp) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group ruleEditorGroup = new Group(mainBottomComp, SWT.NONE);
        ruleEditorGroup.setText(" Rule Editor ");
        GridLayout gl = new GridLayout(1, false);
        ruleEditorGroup.setLayout(gl);
        ruleEditorGroup.setLayoutData(gd);

        // -------------------------------------------
        // Add the Add, Replace, and Remove buttons
        // -------------------------------------------
        Composite buttonComp = new Composite(ruleEditorGroup, SWT.BORDER);
        buttonComp.setLayout(new GridLayout(3, false));
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        buttonComp.setLayoutData(gd);

        int buttonWidth = 100;

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        addBtn = new Button(buttonComp, SWT.PUSH);
        addBtn.setText("Add");
        addBtn.setEnabled(false);
        addBtn.setLayoutData(gd);
        addBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                addNewRule();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        replaceBtn = new Button(buttonComp, SWT.PUSH);
        replaceBtn.setText("Replace");
        replaceBtn.setEnabled(false);
        replaceBtn.setLayoutData(gd);
        replaceBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int idx = dataList.getSelectionIndex();
                removeRule(idx);
                addNewRule();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        removeBtn = new Button(buttonComp, SWT.PUSH);
        removeBtn.setText("Remove");
        removeBtn.setEnabled(false);
        removeBtn.setLayoutData(gd);
        removeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int idx = dataList.getSelectionIndex();
                removeRule(idx);
            }
        });

        // ---------------------------------------------------
        // Add the selected method, severity, type, and
        // unique controls
        // ---------------------------------------------------
        Composite middleComp = new Composite(ruleEditorGroup, SWT.NONE);
        middleComp.setLayout(new GridLayout(7, false));
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        middleComp.setLayoutData(gd);

        Label methodLbl = new Label(middleComp, SWT.NONE);
        methodLbl.setText("Method: ");

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        selectedMethodLbl = new Label(middleComp, SWT.NONE);
        selectedMethodLbl.setText("NONE");
        selectedMethodLbl.setLayoutData(gd);

        Label severityLbl = new Label(middleComp, SWT.NONE);
        severityLbl.setText("Severity: ");

        severityCbo = new Combo(middleComp, SWT.DROP_DOWN | SWT.READ_ONLY);

        // The first two colors in the list are never used; but to maintain
        // comparability with AWIPS I monitoring rule files must keep them.
        for (int i = 2; i < severityColors.length; i++) {
            severityCbo.add(severityColors[i]);
        }
        severityCbo.select(0);

        Label typeLbl = new Label(middleComp, SWT.NONE);
        typeLbl.setText("Type: ");

        typeCbo = new Combo(middleComp, SWT.DROP_DOWN | SWT.READ_ONLY);

        for (RuleType type : RuleType.values()) {
            typeCbo.add(type.name());
        }
        typeCbo.select(0);

        uniqueChk = new Button(middleComp, SWT.CHECK);
        uniqueChk.setText("Unique");

        // -----------------------------------------
        // Add message label and text control.
        // -----------------------------------------
        Label messageLbl = new Label(ruleEditorGroup, SWT.NONE);
        messageLbl.setText("Message");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        messageTF = new Text(ruleEditorGroup, SWT.BORDER);
        messageTF.setLayoutData(gd);

        // ---------------------------------------------------
        // Add the selected method, severity, type, and
        // unique controls
        // ---------------------------------------------------
        labelTextComp = new Composite(ruleEditorGroup, SWT.NONE);
        labelTextComp.setLayout(new GridLayout(11, false));
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        labelTextComp.setLayoutData(gd);

        labelTextArray = new ArrayList<LabelTextControls>();

        Label tmpLbl;
        Text tmpText;

        for (int i = 0; i < 4; i++) {
            gd = new GridData(115, SWT.DEFAULT);
            tmpLbl = new Label(labelTextComp, SWT.NONE);
            tmpText = new Text(labelTextComp, SWT.BORDER);
            tmpText.setLayoutData(gd);

            if (i < 3) {
                gd = new GridData(10, SWT.DEFAULT);
                Label filler = new Label(labelTextComp, SWT.NONE);
                filler.setLayoutData(gd);
            }

            labelTextArray.add(new LabelTextControls(tmpLbl, tmpText));
        }

        hideArgLabelTextControls();

        // -----------------------------------------
        // Add comment label
        // -----------------------------------------
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        commentLbl = new Label(ruleEditorGroup, SWT.NONE);
    }

    /**
     * Callback method to update the display with the selected method
     * information. Implemented from the IAvailMethodSelected interface.
     */
    public void methodSelected(String methodName) {
        selectedMethodLbl.setText(methodName);

        ArrayList<MethodData> methodArray = pageData.getMethodArray();
        MethodData methodData;

        for (int i = 0; i < methodArray.size(); i++) {
            methodData = methodArray.get(i);

            if (methodName.compareTo(methodData.getMethodName()) == 0) {
                updateArgLabelTextControls(methodData);
                severityCbo.select(0);
                uniqueChk.setSelection(methodData.getUnique());
                typeCbo.select(methodData.getType().ordinal());
                messageTF.setText(methodData.getMessage());

                break;
            }
        }

        addBtn.setEnabled(true);
    }

    /**
     * Update the argument labels and text controls.
     * 
     * @param methodData
     *            Method data information.
     */
    public void updateArgLabelTextControls(MethodData methodData) {
        hideArgLabelTextControls();

        ArrayList<MethodArgData> methodArgDataArray = methodData
                .getMethodArgsArray();
        MethodArgData methodArgData;
        LabelTextControls labelTextControls;

        for (int i = 0; i < methodArgDataArray.size(); i++) {
            methodArgData = methodArgDataArray.get(i);
            labelTextControls = labelTextArray.get(i);

            labelTextControls.labelControl.setText(methodArgData.getArgName());
            labelTextControls.labelControl.setVisible(true);
            labelTextControls.labelControl.pack();

            labelTextControls.textControl.setText(methodArgData.getArgValue());
            labelTextControls.textControl.setVisible(true);
        }

        labelTextComp.layout();

        commentLbl.setText(methodData.getComment());
        commentLbl.pack();
    }

    /**
     * Hide the argument label and text controls.
     */
    private void hideArgLabelTextControls() {
        LabelTextControls labelTextControls;

        for (int i = 0; i < labelTextArray.size(); i++) {
            labelTextControls = labelTextArray.get(i);

            labelTextControls.labelControl.setVisible(false);
            labelTextControls.textControl.setVisible(false);
        }
    }

    /**
     * Populate the list of active rules.
     */
    private void populateList() {
        dataList.removeAll();
        Collections.sort(activeRules);

        for (MethodData rule : activeRules) {
            String color = severityColors[rule.getSeverity()];
            String method = rule.getMethodName();
            String message = rule.getMessage();
            if (message.trim().length() == 0) {
                message = "System generated";
            }
            dataList.add(String
                    .format("%-12s %-20s %s", color, method, message));
        }
    }

    /**
     * Convert an integer type value to a RuleType enum value.
     * 
     * @param i
     *            Int
     * 
     * @return RuleType enum value.
     */
    private RuleType intToType(int i) {
        switch (i) {
        case 0:
            return RuleType.vsby;
        case 1:
            return RuleType.wind;
        case 2:
            return RuleType.sky;
        case 3:
            return RuleType.wx;
        case 4:
            return RuleType.cat;
        default:
            return null;
        }
    }

    /**
     * Add a new rule to the list of active rules and select it.
     * 
     */
    private void addNewRule() {
        String methodName = selectedMethodLbl.getText();
        String comment = commentLbl.getText();
        String message = messageTF.getText();
        RuleType type = intToType(typeCbo.getSelectionIndex());
        boolean unique = uniqueChk.getSelection();
        MethodData method = new MethodData(methodName, comment, message, type,
                unique);
        method.setSeverity(severityCbo.getSelectionIndex() + 2);
        if (pageData.getDataSource().equalsIgnoreCase("mtrs")
                || pageData.getDataSource().equalsIgnoreCase("grids")) {
            method.setMsgFromFile(true);
        }

        for (LabelTextControls ltc : labelTextArray) {
            if (!ltc.labelControl.getText().equals("")) {
                method.addArgument(new MethodArgData(
                        ltc.labelControl.getText(), ltc.textControl.getText()));
            }
        }

        // Determine where to insert method rule so it can be selected.
        int index = 0;
        for (index = 0; index < activeRules.size(); ++index) {
            if (method.compareTo(activeRules.get(index)) < 0) {
                break;
            }
        }

        activeRules.add(index, method);
        populateList();
        dataList.select(index);
        selectionAction();
    }

    /**
     * Remove a rule from the list of active rules.
     * 
     * @param idx
     *            index of the rule to remove.
     */
    private void removeRule(int idx) {
        activeRules.remove(idx);
        populateList();
        selectionAction();
    }

    /**
     * Permanently delete all the rules associated with a given site ID, doing
     * so by clearing the activeRules list and calling the saveRules method.
     * 
     * @param siteID
     * @throws LocalizationException
     * @throws ConfigurationException
     * @throws IOException
     */
    public void deleteRules(String siteID) throws ConfigurationException,
            LocalizationException, IOException {
        this.siteID = siteID;

        if (!this.siteID.equals("XXXX")) {
            MessageBox questionMB = new MessageBox(parent.getShell(),
                    SWT.ICON_QUESTION | SWT.OK | SWT.CANCEL);
            questionMB.setText("Confirm Delete");
            questionMB.setMessage("Delete " + pageData.getDataSource()
                    + " rules for " + siteID + ".");
            int result = questionMB.open();

            if (result == SWT.OK) {
                activeRules.clear();

                saveRules(siteID);

                // TODO The site's rules are removed and replaced with the
                // current XXXXs rules.
                // Should any monitoring rules configuration files for the
                // site be deleted so it will pick up any changes made to the
                // default rules?
            }
        }
    }

    /**
     * Save to file all rules associated with a given site ID. This method will
     * do nothing if given the default site ID "XXXX"
     * 
     * @param siteID
     * @throws LocalizationException
     * @throws ConfigurationException
     * @throws IOException
     */
    public void saveRules(String siteID) throws LocalizationException,
            ConfigurationException, IOException {
        this.siteID = siteID;
        AvnConfiguration config = AvnConfiguration.load(false);
        config.setRules(siteID, DataSource.valueOf(pageData.getDataSource()),
                activeRules);
    }

    /**
     * Get the rules that apply for a site and display them.
     * 
     * @param siteID
     * @throws LocalizationException
     * @throws ConfigurationException
     */
    public void reloadRules(String siteID) throws LocalizationException,
            ConfigurationException {
        this.siteID = siteID;
        loadActiveRulesList();
        populateList();
    }

    /**
     * Load active rules for the tab.
     * 
     * @throws LocalizationException
     * @throws ConfigurationException
     */
    private void loadActiveRulesList() throws LocalizationException,
            ConfigurationException {
        AvnConfiguration config = AvnConfiguration.load(false);
        try {
            activeRules = config.getRules(siteID,
                    DataSource.valueOf(pageData.getDataSource()),
                    severityColors.length - 1);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /**
     * The DataSource type associated with this tab.
     * 
     * @return dataSource
     */
    public String getDataSource() {
        return pageData.getDataSource();
    }
}
