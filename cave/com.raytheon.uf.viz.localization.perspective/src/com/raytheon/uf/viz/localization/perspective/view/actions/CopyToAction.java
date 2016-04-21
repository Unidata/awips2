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
package com.raytheon.uf.viz.localization.perspective.view.actions;

import java.io.File;
import java.io.IOException;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.localization.service.ILocalizationService;

/**
 * Action to copy a localization file to a different level, also adds option to
 * rename file
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 3, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CopyToAction extends AbstractToAction {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CopyToAction.class);

    protected ILocalizationService service;

    public CopyToAction(LocalizationFile file, ILocalizationService service) {
        super("Copy To", file);
        this.service = service;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.localization.perspective.view.actions.AbstractToAction
     * #isLevelEnabled(com.raytheon.uf.common.localization.LocalizationContext.
     * LocalizationLevel)
     */
    @Override
    protected boolean isLevelEnabled(LocalizationLevel level) {
        boolean enabled = super.isLevelEnabled(level);
        if (enabled && file.isProtected()) {
            // Ensure protected level is greater than copy to level
            enabled = file.getProtectedLevel().compareTo(level) >= 0;
        }
        return enabled;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.localization.filetreeview.actions.AbstractToAction
     * #run
     * (com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel
     * )
     */
    @Override
    protected void run(LocalizationLevel level) {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile newFile = pm.getLocalizationFile(
                pm.getContext(file.getContext().getLocalizationType(), level),
                file.getName());
        copyFile(newFile);
    }

    protected boolean copyFile(LocalizationFile newFile) {
        File copyTo = newFile.getFile();
        File copyFrom = file.getFile();

        // Delete local copy of existing contents
        copyTo.delete();
        // Make sure parent directories exist
        if (copyTo.getParentFile().exists() == false) {
            copyTo.getParentFile().mkdirs();
        }

        try {
            // Copy file contents locally
            FileUtil.copyFile(copyFrom, copyTo);
        } catch (IOException e) {
            statusHandler
                    .handle(Priority.PROBLEM, "Error copying file contents of "
                            + file + " to " + newFile, e);
            return false;
        }

        try {
            // Save localization file with new contents
            boolean rval = newFile.save();
            if (!rval) {
                // If failed, make sure we get the latest file
                newFile.getFile();
            }
            return rval;
        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.PROBLEM, "Error saving " + newFile
                    + ": " + e.getLocalizedMessage(), e);
        }
        return false;
    }

    @Override
    protected void fillMenu(Menu menu) {
        super.fillMenu(menu);
        new Separator().fill(menu, -1);
        Action rename = getRenameAction();
        rename.setText("New File...");
        new ActionContributionItem(rename).fill(menu, -1);
    }

    protected Action getRenameAction() {
        return new RenameAction(file, service, false);
    }

}
