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
package com.raytheon.viz.gfe.localization.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.localization.perspective.service.LocalizationPerspectiveUtils;
import com.raytheon.viz.gfe.localization.dialogs.NewInputValidator;
import com.raytheon.viz.gfe.localization.dialogs.ScriptNameInputDialog;
import com.raytheon.viz.gfe.localization.util.AbstractScriptUtil;
import com.raytheon.viz.gfe.localization.util.AbstractScriptUtil.Overwrite;

/**
 * An Action for creating a new instance of a script.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * ???                    wldougher  Initial creation
 * Jan 19, 2016  4834     njensen    Cleaned up warnings
 * Aug 11, 2016  5816     randerso   Moved to gfe.localization.actions Code
 *                                   refactored and cleaned up
 * 
 * </pre>
 * 
 * @author wldougher
 * 
 */
public class NewAction extends Action {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NewAction.class);

    protected AbstractScriptUtil util;

    /**
     * @param util
     */
    public NewAction(AbstractScriptUtil util) {
        super("New...");
        this.util = util;
    }

    @Override
    public void run() {
        Shell parent = Display.getCurrent().getActiveShell();
        String type = util.getScriptType();
        IInputValidator validator = new NewInputValidator();
        ScriptNameInputDialog dialog = new ScriptNameInputDialog(parent, "My "
                + type, "Name", "My" + type, validator, util);
        int rtnCode = dialog.open();
        if (rtnCode == Window.OK) {
            String script = dialog.getValue().trim();
            try {
                // since createNew() will only return if a LocalizationFile is
                // properly created, no null check is necessary here
                LocalizationFile fileToEdit = util.createNew(script,
                        Overwrite.DISALLOW);
                statusHandler.handle(Priority.VERBOSE, type + " " + script
                        + " created.");
                LocalizationPerspectiveUtils.openLocalizationFile(fileToEdit);
            } catch (Exception e) {
                String message = String.format("Error creating %s '%s': %s",
                        util.getScriptType(), script, e.getLocalizedMessage());
                statusHandler.error(message, e);
            }
        }
    }
}
