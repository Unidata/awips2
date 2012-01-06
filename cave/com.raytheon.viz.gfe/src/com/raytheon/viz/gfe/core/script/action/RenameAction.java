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
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.core.script.CopyInputValidator;
import com.raytheon.viz.gfe.core.script.ExistMode;
import com.raytheon.viz.gfe.core.script.IScriptUtil;
import com.raytheon.viz.gfe.core.script.IScriptUtil.Overwrite;
import com.raytheon.viz.gfe.dialogs.ScriptNameInputDialog;

/**
 * An Action that, when invoked, prompts the user for a new script name and
 * changes the name of &quot;script&quot; to the new name.
 * 
 * @author wldougher
 * 
 */
public class RenameAction extends Action {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(RenameAction.class);

    protected String script;

    protected IScriptUtil util;

    /**
     * Constructor.
     * 
     * @param script
     *            The name of the script to be renamed.
     * @param util
     *            The utility class that does the real work.
     */
    public RenameAction(String script, IScriptUtil util) {
        super("Rename...");
        this.script = script;
        this.util = util;
    }

    /**
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        // Set up a dialog to get the new name
        Shell parent = Display.getCurrent().getActiveShell();
        String type = util.getScriptType();
        String title = "Rename " + type + " " + script;
        IInputValidator validator = new CopyInputValidator(script, util);
        ScriptNameInputDialog renameDialog = new ScriptNameInputDialog(parent,
                title, "Name", script, validator, util);
        renameDialog.setMode(ExistMode.ERR_EXISTS);
        // Get the new name from the user
        int code = renameDialog.open();
        if (Window.OK == code) {
            String dest = renameDialog.getValue();
            try {
                // Rename the script
                util.rename(script, dest, LocalizationLevel.USER,
                        Overwrite.SAFE);
            } catch (GFEException e) {
                statusHandler.handle(Priority.PROBLEM, "Error renaming "
                                + type + " " + script + " to " + dest + ".", e);
            }
        }
    }
}
