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
package com.raytheon.viz.gfe.core.script.action;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.PythonUtil;
import com.raytheon.viz.gfe.core.script.ExistMode;
import com.raytheon.viz.gfe.core.script.IScriptUtil;
import com.raytheon.viz.gfe.core.script.IScriptUtil.Overwrite;
import com.raytheon.viz.gfe.core.script.NewInputValidator;
import com.raytheon.viz.gfe.dialogs.ScriptNameInputDialog;

/**
 * An Action for creating a new instance of a script.
 * 
 * @author wldougher
 * 
 */
public class NewAction extends Action {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NewAction.class);

    protected IScriptUtil util;

    /**
     * @param selected
     * @param util
     */
    public NewAction(IScriptUtil util) {
        super("New...");
        this.util = util;
    }

    /**
     * 
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        Shell parent = Display.getCurrent().getActiveShell();
        String type = util.getScriptType();
        IInputValidator validator = new NewInputValidator();
        ScriptNameInputDialog dialog = new ScriptNameInputDialog(parent, "My "
                + type, "Name", "My" + type, validator, util);
        dialog.setMode(ExistMode.ERR_EXISTS);
        int rtnCode = dialog.open();
        if (rtnCode == Dialog.OK) {
            String script = dialog.getValue().trim();
            try {
                // since createNew() will only return if a LocalizationFile is
                // properly created, no null check is necessary here
                LocalizationFile fileToEdit = util.createNew(script,
                        LocalizationLevel.USER, Overwrite.SAFE);
                statusHandler.handle(Priority.VERBOSE, type + " " + script
                        + " created.");
                PythonUtil.openPythonFile(fileToEdit);
            } catch (GFEException e) {
                statusHandler.handle(Priority.PROBLEM, "Error creating " + type
                        + " " + script + ".", e);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, "Error creating " + type
                        + " " + script + ".", e);
            }
        }
    }
}
