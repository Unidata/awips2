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
package com.raytheon.viz.gfe.textproduct.action;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.core.script.IScriptUtil;
import com.raytheon.viz.gfe.core.script.IScriptUtil.Overwrite;

/**
 * An action to upgrade a text product to the SITE localization level.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 29, 2010            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class TextProductSiteAction extends Action {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(TextProductSiteAction.class);

    private static final Pattern OVR_PATTERN = Pattern.compile(
            "(.+?)_?overrides", Pattern.CASE_INSENSITIVE);

    String script;

    IScriptUtil util;

    /**
     * @param script
     *            The simple name of the script, i.e., "OAX_APD"
     * @param util
     *            The script utility instance that provides low-level
     *            script-manipulation methods
     */
    public TextProductSiteAction(String script, IScriptUtil util) {
        super("Make SITE");
        this.script = script;
        this.util = util;
    }

    /**
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        String scriptType = util.getScriptType();
        LocalizationFile source = null;
        try {
            source = util.find(script, null);
        } catch (GFEException e) {
            String message = String.format(
                    "An error occurred while finding '%s'", script);
            statusHandler.handle(Priority.PROBLEM, message, e);
            return;
        }

        // Make sure the source exists
        if (source == null) {
            String message = String.format("%s '%s' does not exist",
                    util.getScriptType(), script);
            MessageDialog.openError(Display.getCurrent().getActiveShell(),
                    "Does Not Exist", message);
            statusHandler.handle(Priority.PROBLEM, message);
            return;
        }

        LocalizationLevel srcLevel = source.getContext().getLocalizationLevel();

        String dest = script;
        Matcher ovrMatch = OVR_PATTERN.matcher(script);
        boolean isOverride = ovrMatch.matches();

        if (isOverride) {
            // strip "overrides" or "_overrides" from destination name
            dest = ovrMatch.group(1);
        }

        if (LocalizationLevel.BASE == srcLevel) {
            // source is BASE, we can't downgrade it
            String message = String
                    .format("%s '%s' is at BASE level and cannot be moved to SITE level.",
                            scriptType, script);
            MessageDialog.openInformation(
                    Display.getCurrent().getActiveShell(), "Cannot Move Base",
                    message);
            return;
        }

        if (LocalizationLevel.SITE == srcLevel && !isOverride) {
            // nothing to do
            String message = String.format("%s '%s' is already at SITE level.",
                    scriptType, script);
            MessageDialog.openInformation(
                    Display.getCurrent().getActiveShell(), message, message);
            return;
        }

        LocalizationFile destlf = null;
        try {
            destlf = util.find(dest, LocalizationLevel.SITE);
        } catch (GFEException e1) {
            String message = String.format("Error during search for %s '%s'",
                    scriptType, dest);
            statusHandler.handle(Priority.PROBLEM, message);
            return;
        }

        if (destlf != null
                && destlf.getFile().getPath()
                        .contains(GfePyIncludeUtil.TEXT_PRODUCTS)) {
            // destination already exists. Confirm overwrite.
            String message = String
                    .format("%s '%s' already exists at SITE level!\n"
                            + "Confirm that you want to overwrite it from %s at %s level:",
                            scriptType, dest, script, srcLevel.toString());
            boolean confirmed = MessageDialog.openConfirm(Display.getCurrent()
                    .getActiveShell(), "Confirm Overwrite", message);
            if (confirmed) {
                try {
                    destlf.delete();
                } catch (Exception e) {
                    String errMessage = String.format("Error deleting %s '%s'",
                            scriptType, dest);
                    statusHandler.handle(Priority.PROBLEM, errMessage, e);
                }
            } else {
                return;
            }
        }

        try {
            util.copy(script, dest, LocalizationLevel.SITE, Overwrite.SAFE);
            source.delete();
            String message = String.format("%s '%s' set as SITE", scriptType,
                    dest);
            statusHandler.handle(Priority.VERBOSE, message);
        } catch (Exception e) {
            String message = String.format("Error moving %s to SITE", script);
            statusHandler.handle(Priority.PROBLEM, message, e);
        }
    }
}
