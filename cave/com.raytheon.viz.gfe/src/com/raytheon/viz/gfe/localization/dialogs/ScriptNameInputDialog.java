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
package com.raytheon.viz.gfe.localization.dialogs;

import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.localization.util.AbstractScriptUtil;

/**
 * A dialog for the user to input the name of a script.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * ???                    wldougher  Initial creation
 * Aug 11, 2016  5816     randerso   Moved to gfe.localization.dialogs.
 *                                   Code cleanup
 * 
 * </pre>
 * 
 * @author wldougher
 */
public class ScriptNameInputDialog extends InputDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ScriptNameInputDialog.class);

    protected AbstractScriptUtil util;

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
     * @param util
     */
    public ScriptNameInputDialog(Shell parentShell, String dialogTitle,
            String dialogMessage, String initialValue,
            IInputValidator validator, AbstractScriptUtil util) {
        super(parentShell, dialogTitle, dialogMessage, initialValue, validator);
        this.util = util;
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
            if (file != null) {
                setErrorMessage(stype + " " + value + " already exists.");
            } else {
                super.okPressed();
            }
        } catch (GFEException e) {
            statusHandler.handle(Priority.PROBLEM, "Error validating name", e);
        }
    }
}
