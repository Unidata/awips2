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

import java.io.IOException;
import java.util.ArrayList;

import org.apache.commons.configuration.ConfigurationException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.viz.avncommon.AvnMessageMgr.StatusMessageType;
import com.raytheon.viz.avnconfig.AvnConfigConstants.DataSource;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog displaying the monitoring criteria controls. The dialog has a tab
 * folder that contains 'page' data (each tab is a page).
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 22 MAY 2008  1119       lvenable    Initial creation
 * 16 Mar 2011  8599       rferrel     Create msgStatusComp then load tabs.
 * 27 Sep 2011  10958      rferrel     Display more details when handling
 *                                     ConfigurationException.
 * 12 Oct 2012  1229       rferrel     Convert to subclass of CaveSWTDialog
 *                                      and made non-blocking.
 * 15 OCT 2012  1229       rferrel     Changes for non-blocking HelpUsageDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class MonitoringCriteriaDlg extends CaveSWTDialog {

    /**
     * Composite containing message status controls.
     */
    private MessageStatusComp msgStatusComp;

    /**
     * Site ID text control.
     */
    private Text siteIdTF;

    /**
     * Site ID initially set to the default id.
     */
    private String siteId = "XXXX";

    /**
     * Data source tab folder containing the 'pages' (tab items) of data.
     */
    private TabFolder dataSourceTabFolder;

    /**
     * Spacer added to the tab item text to make the tabs wider.
     */
    private final String spacer = "      ";

    /**
     * Default rule data object that contain all of the 'page' names and the
     * default available method data.
     */
    private DefaultRuleData defaultRuleData;

    private HelpUsageDlg usageDlg;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public MonitoringCriteriaDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("AvnFPS Monitoring Criteria");
    }

    @Override
    protected void initializeComponents(Shell shell) {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        mainLayout.verticalSpacing = 5;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        defaultRuleData = new DefaultRuleData();

        createDefaultRuleData();

        createTopControls();

        createTabFolderControl();

        createBottomMessageControls();

        populateTabFolderControl();
    }

    /**
     * Create the controls at the top of the display.
     */
    private void createTopControls() {
        // ------------------------------------------
        // Create the composite for the controls.
        // ------------------------------------------
        Composite controlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(7, false);
        controlComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        controlComp.setLayoutData(gd);

        Label siteIdLbl = new Label(controlComp, SWT.NONE);
        siteIdLbl.setText("Site ID:");

        gd = new GridData(40, SWT.DEFAULT);
        siteIdTF = new Text(controlComp, SWT.BORDER);
        siteIdTF.setLayoutData(gd);
        siteIdTF.setText(siteId);
        siteIdTF.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent e) {
                if (e.character == SWT.CR) {
                    loadSite();
                }
            }
        });

        siteIdTF.addFocusListener(new FocusListener() {

            @Override
            public void focusLost(FocusEvent e) {
                String value = siteIdTF.getText().trim().toUpperCase();
                siteIdTF.setText(value);
            }

            @Override
            public void focusGained(FocusEvent e) {
            }
        });

        int buttonWidth = 80;

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button loadBtn = new Button(controlComp, SWT.PUSH);
        loadBtn.setText("Load");
        loadBtn.setToolTipText("Retrieves rule set for selected site");
        loadBtn.setLayoutData(gd);
        loadBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                loadSite();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button saveBtn = new Button(controlComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setToolTipText("Saves rule set to a file");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                siteId = siteIdTF.getText();
                TabItem item = dataSourceTabFolder.getItem(dataSourceTabFolder
                        .getSelectionIndex());
                DataSourceTabComp tab = (DataSourceTabComp) item.getControl();
                String dataSource = tab.getDataSource().toUpperCase();

                try {
                    tab.saveRules(siteId);

                    msgStatusComp.setMessageText(dataSource
                            + " rules saved for site " + siteId + ".", new RGB(
                            0, 255, 0));
                } catch (ConfigurationException e) {
                    msgStatusComp.setMessageText(
                            "An error occured while saving rules for site "
                                    + siteId + ".", new RGB(255, 0, 0));
                } catch (LocalizationException e) {
                    msgStatusComp.setMessageText(
                            "An error occured while saving rules for site "
                                    + siteId + ".", new RGB(255, 0, 0));
                } catch (IOException e) {
                    msgStatusComp.setMessageText(
                            "An error occured while saving rules for site "
                                    + siteId + ".", new RGB(255, 0, 0));
                }
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button closeBtn = new Button(controlComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setToolTipText("Closes this dialog");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button deleteBtn = new Button(controlComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setToolTipText("Deletes site-specific rules");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                try {
                    siteId = siteIdTF.getText();

                    if (!siteId.equals("XXXX")) {
                        TabItem item = dataSourceTabFolder
                                .getItem(dataSourceTabFolder
                                        .getSelectionIndex());
                        DataSourceTabComp tab = (DataSourceTabComp) item
                                .getControl();
                        tab.deleteRules(siteId);
                        siteId = "XXXX";
                        siteIdTF.setText(siteId);
                        tab = (DataSourceTabComp) item.getControl();
                        tab.reloadRules(siteId);
                        String dataSource = tab.getDataSource().toUpperCase();

                        msgStatusComp.setMessageText(dataSource
                                + " rules deleted for site " + siteId + ".",
                                new RGB(0, 255, 0));
                    } else {
                        msgStatusComp.setMessageText(
                                "Cannot delete default rules.", new RGB(255, 0,
                                        0));
                    }
                } catch (LocalizationException e) {
                    msgStatusComp.setMessageText(
                            "An error occured when deleting rules for site"
                                    + siteId + ".", new RGB(255, 0, 0));
                } catch (ConfigurationException e) {
                    msgStatusComp.setMessageText(
                            "An error occured while deleting rules for site "
                                    + siteId + ".", new RGB(255, 0, 0));
                } catch (IOException e) {
                    msgStatusComp.setMessageText(
                            "An error occured while deleting rules for site "
                                    + siteId + ".", new RGB(255, 0, 0));
                }
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button helpBtn = new Button(controlComp, SWT.PUSH);
        helpBtn.setText("Help");
        helpBtn.setToolTipText("Shows help");
        helpBtn.setLayoutData(gd);
        helpBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(usageDlg)) {
                    String description = "AvnFPS - Monitoring Criteria Help";

                    String helpText = "This dialog is used to define and configure TAF monitoring\nrules.\n\nTo load current configuration, enter site id and press\n<Enter> or use the \"Load\" button. Site id XXXX loads\ndefault rules.\n\nSelect a tab for the monitoring module you wish to modify\nthe rules. The top list displays current rules: severity\nlevels (colors) and associated messages. The list is sorted\nwith respect to the severity level.\n\nTo view detailed rule description, select an item on this\nlist. All rule parameters will be displayed in the \"Rule\nEditor\" window. You may modify editable parameters. Press \n\"Replace\" when finished. To remove a rule from the list,\npress \"Remove\". To add a new rule, first select one of\nthe available methods, then modify rule parameters as\ndesired. You must enter a message. Press \"Add\" to add the\nrule to the list.\n\nNOTE: \n1. argument types and values are not verified by the\n   editor.\n2. if an argument is a list, the separators are commas.\n   \nPress 'Save' when finished.\n\nTo restore default rules for a TAF Site and a currently\nselected monitoring module, use the \"Delete\" button.";
                    usageDlg = new HelpUsageDlg(shell, description, helpText);
                    usageDlg.open();
                } else {
                    usageDlg.bringToTop();
                }
            }
        });
    }

    /**
     * Create tab folder that will contain all of the 'pages'.
     */
    private void createTabFolderControl() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite tabComp = new Composite(shell, SWT.NONE);
        tabComp.setLayout(new GridLayout(1, false));
        tabComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        dataSourceTabFolder = new TabFolder(tabComp, SWT.NONE);
        dataSourceTabFolder.setLayoutData(gd);
    }

    /**
     * Set up tabs and populate with default rules.
     */
    private void populateTabFolderControl() {
        ArrayList<PageData> pageArray = defaultRuleData.getPageArray();

        try {
            for (PageData tmpPageData : pageArray) {
                TabItem tmpTab = new TabItem(dataSourceTabFolder, SWT.NONE);
                tmpTab.setData(tmpPageData.getDataSource());
                tmpTab.setText(spacer + tmpPageData.getDataSource() + spacer);

                tmpTab.setControl(new DataSourceTabComp(dataSourceTabFolder,
                        tmpPageData, siteIdTF.getText().trim()));
            }
            msgStatusComp.setMessageText(siteId
                    + " monitoring rules loaded successfuly.", new RGB(0, 255,
                    0));
        } catch (ConfigurationException e) {
            StringBuilder msg = new StringBuilder(
                    "Error loading default (XXXX) rules");
            if (e.getLocalizedMessage() != null) {
                msg.append(": ").append(e.getLocalizedMessage());
            }
            msgStatusComp.setMessageText(msg.toString(), new RGB(255, 0, 0));
        } catch (Exception e) {
            msgStatusComp.setMessageText("Error loading default rule data.",
                    new RGB(255, 0, 0));
        }
    }

    /**
     * Create the message status composite.
     */
    private void createBottomMessageControls() {
        msgStatusComp = new MessageStatusComp(shell,
                StatusMessageType.MonitoringCriteria, null, null);
    }

    /**
     * Create the rule methods for all pages.
     */
    private void createDefaultRuleData() {
        for (DataSource source : DataSource.values()) {
            defaultRuleData.addPageData(createPageData(source));
        }
    }

    /**
     * Get the rule methods for a given page
     * 
     * @param dataSource
     *            source associated with the page
     * @return pageData
     */
    private PageData createPageData(DataSource dataSource) {
        PageData pageData = new PageData(dataSource.toString());
        AvnConfiguration configData = AvnConfiguration.load(false);
        ArrayList<MethodData> rules = configData.getRules(dataSource);

        for (MethodData rule : rules) {
            pageData.addMethodData(createMethodData(rule));
        }

        return pageData;
    }

    /**
     * Obtain the data information and comment for a give rule.
     * 
     * @param rule
     * @return methodData
     */
    private MethodData createMethodData(MethodData rule) {
        MethodData methodData = new MethodData(rule.getMethodName(),
                rule.getComment(), rule.getMessage(), rule.getType(),
                rule.getUnique());

        ArrayList<String> args = rule.getArgs();
        ArrayList<String> argValues = rule.getArgValues();

        for (String arg : args) {
            methodData.addMethodArgData(createMethodArgData(arg,
                    argValues.get(args.indexOf(arg))));
        }

        return methodData;
    }

    /**
     * Create a Method Data Argument.
     * 
     * @param argName
     *            Name of the argument to be displayed
     * @param argValue
     *            Default value for the argument
     * @return methodArgData
     */
    private MethodArgData createMethodArgData(String argName, String argValue) {
        return new MethodArgData(argName, argValue);
    }

    /**
     * Load the monitoring rules currently in use for site selected by the user.
     */
    private void loadSite() {
        siteId = siteIdTF.getText().trim().toUpperCase();
        siteIdTF.setText(siteId);
        TabItem[] tabs = dataSourceTabFolder.getItems();

        try {
            for (TabItem item : tabs) {
                DataSourceTabComp tab = (DataSourceTabComp) item.getControl();
                tab.reloadRules(siteId);
            }
            msgStatusComp.setMessageText(siteId
                    + " monitoring rules loaded successfuly.", new RGB(0, 255,
                    0));

        } catch (LocalizationException e) {
            msgStatusComp.setMessageText("Error reloading rule data for site "
                    + siteId + ".", new RGB(255, 0, 0));
        } catch (ConfigurationException e) {
            StringBuilder msg = new StringBuilder(
                    "Error loading rule data for site ");
            msg.append(siteId);
            if (e.getLocalizedMessage() != null) {
                msg.append(": ").append(e.getLocalizedMessage());
            }
            msgStatusComp.setMessageText(msg.toString(), new RGB(255, 0, 0));
        }
    }
}