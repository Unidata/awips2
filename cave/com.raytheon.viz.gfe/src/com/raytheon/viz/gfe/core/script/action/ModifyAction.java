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
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.PythonUtil;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.core.script.IScriptUtil;
import com.raytheon.viz.gfe.core.script.IScriptUtil.Overwrite;

/**
 * @author wldougher
 * 
 */
public class ModifyAction extends Action {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(ModifyAction.class);

    String scriptName;

    IScriptUtil util;

    public ModifyAction(String scriptName, IScriptUtil util) {
        super("Modify...", Action.AS_PUSH_BUTTON);
        this.scriptName = scriptName;
        this.util = util;
    }

    @Override
    public void run() {
        LocalizationFile fileToEdit = null;

        // get the localization file
        try {
            fileToEdit = util.find(scriptName, null);
        } catch (GFEException e1) {
            e1.printStackTrace();
        }
        String scriptClass = util.getScriptType();

        if ((fileToEdit == null) || !fileToEdit.exists()) {
            // Script has been deleted since we listed it.?
            String message = String.format(
                    "%s \"%s\" does not exist.\nWould you like to create it?",
                    scriptClass, scriptName);
            boolean response = MessageDialog.openConfirm(Display.getCurrent()
                    .getActiveShell(), "No Such Script", message);
            if (response) {
                try {
                    fileToEdit = util.createNew(scriptName,
                            LocalizationLevel.USER, Overwrite.SAFE);
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error creating " + scriptClass + " " + scriptName,
                            e);
                }
            } else {
                return;
            }
        } else {
            LocalizationLevel level = fileToEdit.getContext()
                    .getLocalizationLevel();
            if (LocalizationLevel.USER != level) {
                String message = String
                        .format(
                                "%s \"%s\" is at %s level and cannot be modified.\n"
                                        + "Would you like to create an editable copy at USER level?",
                                scriptClass, scriptName, level);
                boolean response = MessageDialog
                        .openConfirm(Display.getCurrent().getActiveShell(),
                                "Copy to USER", message);
                if (response) {
                    try {
                        fileToEdit = util.copy(scriptName, scriptName,
                                LocalizationLevel.USER, Overwrite.SAFE);
                        if (fileToEdit == null) {
                            boolean confirmed = false;
                            confirmed = MessageDialog.openConfirm(Display
                                    .getCurrent().getActiveShell(), scriptClass
                                    + " Already Exists", scriptClass + " "
                                    + scriptName
                                    + " already exists at USER level!\n"
                                    + "Are you sure you want to overwrite it?");
                            if (confirmed) {
                                fileToEdit = util.copy(scriptName, scriptName,
                                        LocalizationLevel.USER,
                                        Overwrite.OVERWRITE);
                            } else {
                                return;
                            }
                        }
                    } catch (GFEException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error copying " + scriptClass + " "
                                        + scriptName + " to USER level", e);
                    }
                } else {
                    return;
                }
            }
        }

        PythonUtil.openPythonFile(fileToEdit);
    }
}
