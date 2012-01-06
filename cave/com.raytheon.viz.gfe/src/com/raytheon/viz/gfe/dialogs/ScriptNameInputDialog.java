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
/**
 * 
 */
package com.raytheon.viz.gfe.dialogs;

import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.core.script.ExistMode;
import com.raytheon.viz.gfe.core.script.IScriptUtil;

/**
 * A dialog for the user to input the name of a script.
 * 
 * @author wldougher
 * 
 */
public class ScriptNameInputDialog extends InputDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(ScriptNameInputDialog.class);

    protected IScriptUtil util;

    protected ExistMode mode;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            The shell in which the dialog should appear.
     * @param dialogTitle
     *            The title to display on the dialog header.
     * @param dialogMessage
     *            The message to display next to the input field.
     * @param initialValue
     *            The initial value of the input field.
     * @param validator
     *            The validator to apply to the input field. This is called for
     *            each character typed.
     */
    public ScriptNameInputDialog(Shell parentShell, String dialogTitle,
            String dialogMessage, String initialValue,
            IInputValidator validator, IScriptUtil util) {
        super(parentShell, dialogTitle, dialogMessage, initialValue, validator);
        this.util = util;
        mode = ExistMode.NONE;
    }

    /**
     * @see org.eclipse.jface.dialogs.Dialog#okPressed()
     */
    @Override
    protected void okPressed() {
        String value = getValue();
        String stype = util.getScriptType();
        try {
            LocalizationFile file = util.find(value, null);
            if (file == null && ExistMode.ERR_NOTEXISTS == mode) {
                setErrorMessage(stype + " " + value + " does not exist.");
            } else if (file != null && ExistMode.ERR_EXISTS == mode) {
                setErrorMessage(stype + " " + value + " already exists.");
            } else {
                super.okPressed();
            }
        } catch (GFEException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error validating name", e);
        }
    }

    /**
     * Get the existence mode of the dialog. If the mode is ERR_EXISTS, an error
     * message will be shown if the user types in a script name that already
     * exists and clicks OK. If the mode is ERR_NOTEXISTS, an error message will
     * be shown if the user types in the name of a script that does NOT exist.
     * If the mode is NONE, the name will not be checked.
     * 
     * @return the mode
     */
    public ExistMode getMode() {
        return mode;
    }

    /**
     * Set the existence mode of the dialog. If the mode is ERR_EXISTS, an error
     * message will be shown if the user types in a script name that already
     * exists and clicks OK. If the mode is ERR_NOTEXISTS, an error message will
     * be shown if the user types in the name of a script that does NOT exist.
     * If the mode is NONE, the name will not be checked.
     * 
     * @param mode
     *            the mode to set
     */
    public void setMode(ExistMode mode) {
        this.mode = mode;
    }
}
