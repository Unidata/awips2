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

import java.util.regex.Pattern;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.FileUpdatedMessage.FileChangeType;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.localization.service.ILocalizationService;
import com.raytheon.viz.ui.VizWorkbenchManager;

/**
 * Action to rename a localization file, prompts user for new name
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 27, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class RenameAction extends Action {

    public static class NewFileInputValidator implements IInputValidator {

        private static final String VALID_FILE_REGEX = "^[a-zA-Z0-9_]+[a-zA-Z0-9_\\-.]*$";

        private static final Pattern VALID_FILE_PATTERN = Pattern
                .compile(VALID_FILE_REGEX);

        private String initialValue;

        public NewFileInputValidator(String initialValue) {
            this.initialValue = initialValue;
        }

        @Override
        public String isValid(String newText) {
            if (initialValue != null && initialValue.equals(newText.trim())) {
                return "File name is not different";
            } else if (newText.trim().isEmpty()) {
                return "New name must not be empty";
            } else {
                String[] parts = newText.split("[" + IPathManager.SEPARATOR
                        + "]");
                if (VALID_FILE_PATTERN.matcher(parts[parts.length - 1])
                        .matches() == false) {
                    return "File name must only contain "
                            + FileUtil.VALID_FILENAME_CHARS;
                }
                for (int i = 0; i < parts.length - 1; ++i) {
                    if (parts[i].isEmpty() == false
                            && VALID_FILE_PATTERN.matcher(parts[i]).matches() == false) {
                        return "Directory path: " + parts[i]
                                + " must only contain "
                                + FileUtil.VALID_FILENAME_CHARS;
                    }
                }
            }
            return null;
        }
    }

    private LocalizationFile file;

    private ILocalizationService service;

    private boolean deleteOld;

    public RenameAction(LocalizationFile file, ILocalizationService service,
            boolean deleteOld) {
        super("Rename...", IAction.AS_PUSH_BUTTON);
        // Only set not enabled if we are deleting the old file and level is a
        // system level
        setEnabled(!(deleteOld && file.getContext().getLocalizationLevel()
                .isSystemLevel()));
        this.file = file;
        this.service = service;
        this.deleteOld = deleteOld;
    }

    public RenameAction(LocalizationFile file, ILocalizationService service) {
        this(file, service, true);
    }

    @Override
    public void run() {
        String path = file.getName();
        String[] pathParts = LocalizationUtil.splitUnique(path);
        final String name = pathParts[pathParts.length - 1];
        Shell parent = VizWorkbenchManager.getInstance().getCurrentWindow()
                .getShell();

        boolean canceled = false;
        boolean done = false;
        while (!canceled && !done) {
            InputDialog dialog = new InputDialog(parent, "Rename file",
                    "File name:", name, new NewFileInputValidator(name));
            if (InputDialog.OK == dialog.open()) {
                pathParts[pathParts.length - 1] = dialog.getValue();
                String newPath = pathParts[0];
                for (int i = 1; i < pathParts.length; ++i) {
                    newPath += (IPathManager.SEPARATOR + pathParts[i]);
                }

                IPathManager pm = PathManagerFactory.getPathManager();
                final LocalizationFile newFile = pm.getLocalizationFile(pm
                        .getContext(file.getContext().getLocalizationType(),
                                LocalizationLevel.USER), newPath);
                boolean rename = true;
                if (newFile.exists()) {
                    rename = MessageDialog.openConfirm(parent, "Override file",
                            "The file: " + newFile.toString() + " exists, "
                                    + "are you sure you want to override it?");
                }

                if (rename) {
                    done = true;
                    try {
                        if (service != null) {
                            final Runnable select = new Runnable() {
                                @Override
                                public void run() {
                                    service.selectFile(newFile);
                                }
                            };
                            // Make sure we select the file after the drop
                            if (newFile.exists() == false) {
                                final ILocalizationFileObserver[] observers = new ILocalizationFileObserver[1];
                                ILocalizationFileObserver observer = new ILocalizationFileObserver() {
                                    @Override
                                    public void fileUpdated(
                                            FileUpdatedMessage message) {
                                        if (message.getChangeType() != FileChangeType.DELETED) {
                                            service.fileUpdated(message);
                                            VizApp.runAsync(select);
                                        }
                                        newFile.removeFileUpdatedObserver(observers[0]);
                                    }
                                };
                                observers[0] = observer;
                                newFile.addFileUpdatedObserver(observer);
                            } else {
                                VizApp.runAsync(select);
                            }
                        }

                        FileUtil.copyFile(file.getFile(), newFile.getFile());
                        newFile.save();
                        if (deleteOld) {
                            file.delete();
                        }
                    } catch (Exception e) {
                        UFStatus.getHandler(RenameAction.class).handle(
                                Priority.PROBLEM,
                                "Error renaming file: "
                                        + e.getLocalizedMessage(), e);
                    }
                }
            } else {
                canceled = true;
            }
        }
    }
}
