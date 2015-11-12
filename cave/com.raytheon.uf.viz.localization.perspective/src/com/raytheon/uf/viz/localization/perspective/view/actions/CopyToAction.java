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

import java.io.IOException;
import java.io.InputStream;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.localization.filetreeview.LocalizationFileEntryData;
import com.raytheon.uf.viz.localization.filetreeview.PathData;
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
 * Nov 3, 2010             mschenke    Initial creation
 * Oct 13, 2015 4410       bsteffen    Allow localization perspective to mix
 *                                     files for multiple Localization Types.
 * Nov 12, 2015 4834       njensen     Changed LocalizationOpFailedException to LocalizationException
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CopyToAction extends AbstractToAction {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CopyToAction.class);

    protected final ILocalizationService service;

    protected final PathData pathData;

    public CopyToAction(LocalizationFile file, PathData pathData,
            ILocalizationService service) {
        super("Copy To", file);
        this.service = service;
        this.pathData = pathData;
    }

    public CopyToAction(LocalizationFileEntryData data,
            ILocalizationService service) {
        this(data.getFile(), data.getPathData(), service);
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
        ILocalizationFile newFile = pm.getLocalizationFile(
                pm.getContext(file.getContext().getLocalizationType(), level),
                file.getName());
        removeAlternateTypeFiles(level);
        copyFile(newFile);
    }

    /**
     * If it is possible for the target to exist for multiple localization types
     * then delete any others that exist at the selected level so there are not
     * multiple files at the same level after the operation completes.
     * 
     * @param level
     */
    protected void removeAlternateTypeFiles(LocalizationLevel level) {
        IPathManager pm = PathManagerFactory.getPathManager();

        for (LocalizationType type : pathData.getTypes()) {
            if (type != file.getContext().getLocalizationType()) {
                LocalizationFile altFile = pm.getLocalizationFile(
                        pm.getContext(type, level), file.getName());
                if (altFile.exists()) {
                    try {
                        altFile.delete();
                    } catch (LocalizationException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Unable to delete existing " + type.name()
                                        + " " + level + " file.", e);
                    }
                }
            }
        }
    }

    protected boolean copyFile(ILocalizationFile newFile) {
        try (InputStream is = file.openInputStream();
                SaveableOutputStream os = newFile.openOutputStream()) {
            FileUtil.copy(is, os);
            os.save();
            return true;
        } catch (LocalizationException | IOException e) {
            statusHandler
                    .handle(Priority.PROBLEM, "Error copying file contents of "
                            + file + " to " + newFile, e);
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
