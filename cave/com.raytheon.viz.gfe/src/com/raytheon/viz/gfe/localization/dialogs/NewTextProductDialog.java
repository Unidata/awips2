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
package com.raytheon.viz.gfe.localization.dialogs;

import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.localization.perspective.service.LocalizationPerspectiveUtils;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.localization.util.AbstractScriptUtil;
import com.raytheon.viz.gfe.localization.util.AbstractScriptUtil.Overwrite;
import com.raytheon.viz.gfe.localization.util.TextProductTableUtil;
import com.raytheon.viz.gfe.localization.util.TextProductUtil;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Dialog for creating a new Text Product
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- ---------------------------------------
 * Sep 25, 2008  1562     askripsky  Initial creation.
 * Nov 12, 2012  1298     rferrel    Changes for non-blocking dialog.
 * Mar 17, 2015  17217    ryu        Set smart product as the default type.
 * Aug 11, 2016  5816     randerso   Moved to gfe.localization.dialogs
 * 
 * </pre>
 * 
 * @author askripsky
 */
public class NewTextProductDialog extends CaveJFACEDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NewTextProductDialog.class);

    private String title;

    private Label typeSelectionLabel;

    private Label nameSelectionLabel;

    private Composite comp;

    @SuppressWarnings("unused")
    private Button cancelButton;

    @SuppressWarnings("unused")
    private Button okButton;

    private Button tableTypeButton;

    private Button smartTypeButton;

    private Text nameSelectionText;

    private IInputValidator validator;

    /**
     * Constructor
     * 
     * @param parentShell
     * @param title
     * @param validator
     */
    public NewTextProductDialog(Shell parentShell, String title,
            IInputValidator validator) {
        super(parentShell);

        this.title = title;
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL);
        this.validator = validator;
    }

    @Override
    protected void okPressed() {
        String name = nameSelectionText.getText();
        boolean isSmart = smartTypeButton.getSelection();

        if (!isInputValid()) {
            String error = "'Name' must be a valid identifier: ";
            error += "No special chracters (except underscore) are allowed.";
            MessageDialog.openWarning(getShell(), "Invalid Name", error);
            return;
        }

        AbstractScriptUtil util = null;
        if (isSmart) {
            util = new TextProductUtil();
        } else {
            util = new TextProductTableUtil();
        }

        // See if there's already a script with the name chosen
        LocalizationFile lfile = null;
        try {
            lfile = util.find(name, null);
        } catch (GFEException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error finding " + util.getScriptType() + " " + name, e);
        }

        boolean create = false;
        if (lfile == null) {
            create = true;
        } else {
            LocalizationLevel level = lfile.getContext().getLocalizationLevel();
            if (LocalizationLevel.USER == level) {
                // confirm overwrite
                String message = String
                        .format("The %s '%s' already exists at USER level!\nAre you sure you want to overwrite it?",
                                util.getScriptType(), name);
                create = MessageDialog.openConfirm(getParentShell(),
                        "Confirm Replacement", message);
            } else {
                // confirm SITE or BASE override
                String message = String
                        .format("The %s '%s' exists at the %s level and cannot be modified.\n"
                                + "Do you want to make a version that can be modified at USER level?",
                                util.getScriptType(), name, level.toString());
                create = MessageDialog.openConfirm(getParentShell(),
                        "Confirm Override", message);
            }
        }

        if (create) {
            try {
                lfile = util.createNew(name, Overwrite.ALLOW);
                LocalizationPerspectiveUtils.openLocalizationFile(lfile);
            } catch (GFEException e) {
                String message = String.format("Error creating %s '%s': %s",
                        util.getScriptType(), name, e.getLocalizedMessage());
                statusHandler.error(message, e);
            }
            close();
        }
    }

    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        if (title != null) {
            shell.setText(title);
        }
    }

    @Override
    protected Control createDialogArea(final Composite parent) {

        Composite composite = (Composite) super.createDialogArea(parent);

        comp = new Composite(composite, SWT.NONE);

        comp.setLayout(new GridLayout(2, false));
        comp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        // Setup new name
        initNameSelection();

        // Setup type table
        initTypeSelection();

        applyDialogFont(composite);

        return composite;
    }

    private void initTypeSelection() {
        initTypeSelectionLabel();
        initTypeSelectionButtons();
    }

    private void initTypeSelectionLabel() {
        typeSelectionLabel = new Label(comp, SWT.NONE);
        typeSelectionLabel.setText("Type");
        typeSelectionLabel.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER,
                false, false));
    }

    private void initTypeSelectionButtons() {
        Composite buttonComp = new Composite(comp, SWT.NONE);
        buttonComp.setLayout(new GridLayout());
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, true));

        smartTypeButton = new Button(buttonComp, SWT.RADIO);
        smartTypeButton.setText("Smart");
        tableTypeButton = new Button(buttonComp, SWT.RADIO);
        tableTypeButton.setText("Table");

        // Default to smart type
        smartTypeButton.setSelection(true);
    }

    private void initNameSelection() {
        initNameSelectionLabel();
        initNameSelectionEntry();
    }

    private void initNameSelectionLabel() {
        nameSelectionLabel = new Label(comp, SWT.NONE);
        nameSelectionLabel.setText("Name");
        nameSelectionLabel.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER,
                false, false));
    }

    private void initNameSelectionEntry() {
        nameSelectionText = new Text(comp, SWT.BORDER);
        nameSelectionText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER,
                true, true));
    }

    private boolean isInputValid() {
        return (validator.isValid(nameSelectionText.getText()) == null);
    }
}
