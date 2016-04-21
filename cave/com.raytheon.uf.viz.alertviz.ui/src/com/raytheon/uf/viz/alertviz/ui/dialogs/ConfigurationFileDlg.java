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
package com.raytheon.uf.viz.alertviz.ui.dialogs;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.alertviz.ConfigContext;
import com.raytheon.uf.viz.alertviz.ConfigurationManager;

/**
 * Dialog for saving, deleting and loading configurations from alertviz
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 14, 2010            mschenke     Initial creation
 * Apr 29, 2011 9069       cjeanbap     Config file list selection: Save As
 * May 31, 2011 9785       cjeanbap     Prevent selection of 0 when list is empty.
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ConfigurationFileDlg extends Dialog {

    public static enum Function {
        SAVE, RETRIEVE, RETRIEVE_WITH_UNSAVED_CHANGES, DELETE;
    }

    /** Title for dialog */
    private static final String TITLE = "Configuration List";

    private static final String CHANGING_DEFAULT_MSG = "It is suggested that "
            + "the default settings be changed only by the "
            + "AlertViz Manager. Do you really wish to %1$s "
            + "the %2$s configuration?";

    private static final String CHANGING_DEFAULT_TITLE = "AlertViz: Default Config Action ";

    private static final String INVALID_FILE_NAME_TITLE = "AlertViz: Invalid File Name ";

    private static final String NO_DELIVERED_IN_NAME_MSG = "You may not Save or Delete a "
            + "Configuration file with \"delivered\" in the name.  This is a reserved term.";

    private static final String CONFIRM_MSG = "Are you sure you want to %1$s the configuration, %2$s?";

    private static final String CONFIRM_TITLE = "Confirmation";

    private static final String UNSAVED_TITLE = "AlertViz: Unsaved Config Changes";

    private static final String UNSAVED_MSG = "You have unsaved config changes for the current "
            + "Configuration.  If you retrieve another configuration these changes will be lost.  "
            + "Continue?";

    private static final String CLOSE_UNSAVED_MSG = "You have unsaved config changes for the current "
            + "Configuration.  If you exit the Configuration Dialog these changes will be lost.  "
            + "Continue?";

    /** Dialog shell. */
    private Shell shell;

    /** The display control. */
    private Display display;

    /** The functionality dialog */
    private Function function;

    /** UI selectable List for configurations */
    private List configurationList;

    /** List for configurations */
    private java.util.List<ConfigContext> configurations;

    /** Input for name of configuration */
    private Text configurationText;

    /** Input for level of configuration */
    private Combo configurationLevel;

    private ConfigContext rval = null;

    private ConfigContext currentConfig;

    public ConfigurationFileDlg(Shell parent, Function function,
            ConfigContext current) {
        super(parent, SWT.NONE);
        this.function = function;
        this.currentConfig = current;
    }

    public ConfigContext open() {
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.MIN | SWT.RESIZE
                | SWT.MAX);
        shell.setText(TITLE);

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();

        shell.pack();

        shell.open();

        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        return rval;
    }

    /**
     * Initialize all the dialog controls
     */
    private void initializeComponents() {
        int selection = createConfigurationList();

        if (function == Function.SAVE) {
            createTextInput();
        }

        createBottomButtons();

        selection = (selection == -1) ? 0 : selection;
        if (selection > 0) {
            this.configurationList.select(selection);
            updateSelectionInfo(selection);
        }
    }

    private void updateSelectionInfo(int index) {
        if (index > -1) {
            ConfigContext selected = configurations.get(index);
            if (configurationText != null) {
                configurationText.setText(selected.getName());
            }
            if (configurationLevel != null) {
                setSelectedLevel(selected);
            }
        }
    }

    /**
     * Create the configuration list
     * 
     * @return
     */
    private int createConfigurationList() {
        configurationList = new List(shell, SWT.BORDER | SWT.V_SCROLL
                | SWT.SINGLE);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 250;
        gd.heightHint = 200;
        configurationList.setLayoutData(gd);
        configurationList.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                int idx = configurationList.getSelectionIndex();
                if (idx >= 0) {
                    updateSelectionInfo(idx);
                }
            }
        });

        java.util.List<ConfigContext> tmpConfigurations = Arrays
                .asList(ConfigurationManager.getInstance().getConfigurations());
        Collections.sort(tmpConfigurations);
        int index = 0;
        int selection = -1;
        this.configurations = new ArrayList<ConfigContext>();
        for (ConfigContext config : tmpConfigurations) {
            if (!(config.isBaseOrConfiguredLevel() && (this.function == Function.SAVE || this.function == Function.DELETE))) {
                this.configurations.add(config);
                this.configurationList.add(config.toString());
                if (config.equals(this.currentConfig)) {
                    selection = index;
                }
                ++index;
            }
        }
        return selection;
    }

    protected void setSelectedLevel(ConfigContext selected) {
        int levelIndex = 0;
        for (String levelItem : configurationLevel.getItems()) {
            if (levelItem.equals(selected.getLevel().name())) {
                configurationLevel.select(levelIndex);
                break;
            }
            levelIndex++;
        }
    }

    private void createTextInput() {
        Composite textComp = new Composite(shell, SWT.NONE);
        textComp.setLayout(new GridLayout(2, false));

        Label label = new Label(textComp, SWT.NONE);
        label.setText("Configuration:");
        GridData gd = new GridData(SWT.LEFT, SWT.CENTER, true, false);
        label.setLayoutData(gd);

        configurationText = new Text(textComp, SWT.BORDER);
        gd = new GridData(SWT.RIGHT, SWT.CENTER, true, false);
        gd.widthHint = 185;
        configurationText.setLayoutData(gd);

        label = new Label(textComp, SWT.NONE);
        label.setText("Level:");
        gd = new GridData(SWT.LEFT, SWT.CENTER, true, false);
        label.setLayoutData(gd);

        configurationLevel = new Combo(textComp, SWT.DROP_DOWN | SWT.BORDER);
        gd = new GridData(SWT.LEFT, SWT.CENTER, true, false);
        gd.widthHint = 185;
        configurationLevel.setLayoutData(gd);

        for (LocalizationLevel level : PathManagerFactory.getPathManager()
                .getAvailableLevels()) {
            if (level.isSystemLevel() == false) {
                configurationLevel.add(level.name());
            }
        }

        Collections.sort(configurations);
        int selectionIndex = configurationList.getSelectionIndex();
        if (selectionIndex > -1) {
            ConfigContext selected = configurations.get(selectionIndex);
            configurationText.setText(selected.getName());
            setSelectedLevel(selected);
        }
    }

    /** Create the bottom buttons */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 100;

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, false, false);
        gd.widthHint = buttonWidth;
        Function theFunction = function
                .equals(Function.RETRIEVE_WITH_UNSAVED_CHANGES) ? Function.RETRIEVE
                : function;
        String functionName = theFunction.toString().toUpperCase();
        Button function = new Button(buttonComp, SWT.PUSH);
        function.setText(functionName);
        function.setLayoutData(gd);
        function.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                executeFunction();
            }
        });

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, false, false);
        gd.widthHint = buttonWidth;
        Button save = new Button(buttonComp, SWT.PUSH);
        save.setText("Cancel");
        save.setLayoutData(gd);
        save.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                rval = null;
                shell.dispose();
            }
        });
    }

    private void executeFunction() {
        switch (function) {
        case DELETE:
        case RETRIEVE:
        case RETRIEVE_WITH_UNSAVED_CHANGES: {
            // set the return value and dispose the shell
            int selection = configurationList.getSelectionIndex();
            if (selection >= 0) {
                rval = configurations
                        .get(configurationList.getSelectionIndex());

                if (function == Function.DELETE) {
                    if (validateNotDelivered(shell, rval.getName())) {
                        if (ConfigurationManager.isDefaultConfig(rval)) {
                            if (!confirmDefaultChange(shell, "delete",
                                    rval.toString())) {
                                rval = null;
                            }
                        } else {
                            if (!confirm(shell, "delete", rval.toString())) {
                                rval = null;
                            }
                        }
                    } else {
                        rval = null;
                    }
                } else if (function == Function.RETRIEVE_WITH_UNSAVED_CHANGES) {
                    if (!confirmLeaveWithUnsavedChanges(shell, rval.toString())) {
                        rval = null;
                    }
                }

            } else {
                rval = null;
            }
            break;
        }
        case SAVE: {
            String name = configurationText.getText();
            String level = configurationLevel.getText();

            if (validateNotDelivered(shell, name)) {
                rval = ("".equals(name)) ? null : new ConfigContext(name,
                        LocalizationLevel.valueOf(level));
                LocalizationLevel enteredLevel = LocalizationLevel
                        .valueOf(level);
                boolean isFound = false;
                for (ConfigContext config : configurations) {
                    isFound = config.getName().equals(name)
                            && enteredLevel.equals(config.getLevel());
                    if (isFound) {
                        break;
                    }
                }
                if (rval != null && isFound) {
                    // we are overwriting a current configuration
                    if (ConfigurationManager.isDefaultConfig(rval)) {
                        if (!confirmDefaultChange(shell, "overwrite",
                                rval.toString())) {
                            rval = null;
                        }
                    } else {
                        if (!confirm(shell, "overwrite", rval.toString())) {
                            rval = null;
                        }
                    }
                }
            }
            break;
        }
        }
        if (rval != null) {
            shell.dispose();
        }
    }

    public static boolean validateNotDelivered(Shell shell, String name) {
        if (name.contains("delivered")) {
            MessageDialog.openInformation(shell, INVALID_FILE_NAME_TITLE,
                    NO_DELIVERED_IN_NAME_MSG);
            return false;
        } else {
            return true;
        }
    }

    public static boolean confirmDefaultChange(Shell shell, String action,
            String contextName) {
        return MessageDialog.openConfirm(shell, CHANGING_DEFAULT_TITLE,
                String.format(CHANGING_DEFAULT_MSG, action, contextName));
    }

    public static boolean confirm(Shell shell, String action, String contextName) {
        return MessageDialog.openConfirm(shell, CONFIRM_TITLE,
                String.format(CONFIRM_MSG, action, contextName));
    }

    public static boolean confirmLeaveWithUnsavedChanges(Shell shell,
            String contextName) {
        return MessageDialog.openConfirm(shell, UNSAVED_TITLE,
                String.format(UNSAVED_MSG, contextName));
    }

    public static boolean confirmCloseWithUnsavedChanges(Shell shell,
            String contextName) {
        return MessageDialog.openConfirm(shell, UNSAVED_TITLE,
                String.format(CLOSE_UNSAVED_MSG, contextName));
    }
}
