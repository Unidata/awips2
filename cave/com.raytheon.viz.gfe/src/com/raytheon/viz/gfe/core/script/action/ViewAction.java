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
package com.raytheon.viz.gfe.core.script.action;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.PythonUtil;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.core.script.IScriptUtil;

/**
 * Action to view a file (read only)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 5, 2010       #4956 randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ViewAction extends Action {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(ViewAction.class);

    String scriptName;

    IScriptUtil util;

    public ViewAction(String scriptName, IScriptUtil util) {
        super("View...", Action.AS_PUSH_BUTTON);
        this.scriptName = scriptName;
        this.util = util;
    }

    @Override
    public void run() {
        LocalizationFile fileToView = null;

        // get the localization file
        try {
            fileToView = util.find(scriptName, null);
        } catch (GFEException e) {
            statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
        String scriptClass = util.getScriptType();

        if ((fileToView == null) || !fileToView.exists()) {
            // Script has been deleted since we listed it.?
            String message = String.format("%s \"%s\" does not exist.",
                    scriptClass, scriptName);
            MessageDialog.openError(Display.getCurrent().getActiveShell(),
                    "No Such Script", message);
            return;
        }

        PythonUtil.openPythonFile(fileToView);
    }
}
