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

import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.IWorkbenchPage;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.FileUpdatedMessage.FileChangeType;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.localization.service.ILocalizationService;

/**
 * Changes file's context, then deletes original file, also add's option to
 * rename file in process
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class MoveFileAction extends CopyToAction {

    private DeleteAction delete;

    private IWorkbenchPage page;

    /**
     * @param file
     */
    public MoveFileAction(IWorkbenchPage page, LocalizationFile file,
            ILocalizationService service) {
        super(file, service);
        setText("Move To");
        this.page = page;
        delete = new DeleteAction(page, new LocalizationFile[] { file }, false);
        setEnabled(delete.isEnabled());
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

    @Override
    protected void run(LocalizationLevel level) {
        boolean choice = MessageDialog
                .openQuestion(
                        page.getWorkbenchWindow().getShell(),
                        "Move Confirmation",
                        "Are you sure you want to move "
                                + LocalizationUtil.extractName(file.getName())
                                + " to "
                                + level
                                + " replacing any existing file and deleting this file?");
        if (choice) {
            IPathManager pm = PathManagerFactory.getPathManager();
            final LocalizationFile newFile = pm.getLocalizationFile(
                    pm.getContext(file.getContext().getLocalizationType(),
                            level), file.getName());

            // Make sure we select the file after the drop
            final ILocalizationFileObserver[] observers = new ILocalizationFileObserver[1];
            ILocalizationFileObserver observer = new ILocalizationFileObserver() {
                @Override
                public void fileUpdated(FileUpdatedMessage message) {
                    if (message.getContext().equals(newFile.getContext())
                            && message.getFileName().equals(newFile.getName())
                            && message.getChangeType() != FileChangeType.DELETED) {
                        service.fileUpdated(message);
                        VizApp.runAsync(new Runnable() {
                            @Override
                            public void run() {
                                service.selectFile(newFile);
                            }
                        });
                    }
                    newFile.removeFileUpdatedObserver(observers[0]);
                }
            };
            observers[0] = observer;
            newFile.addFileUpdatedObserver(observer);
            if (copyFile(newFile)) {
                delete.run();
            } else {
                newFile.removeFileUpdatedObserver(observer);
            }
        }
    }

    @Override
    protected Action getRenameAction() {
        return new RenameAction(file, service);
    }

}
