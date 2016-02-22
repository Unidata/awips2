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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.Action;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.localization.perspective.editor.LocalizationEditorInput;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.dialogs.SWTMessageBox;

/**
 * Deletes the selected localization file
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 3, 2010            mschenke     Initial creation
 * Feb 18, 2015 4132      mapeters     Fixed issue with deleting overrides.
 * Jun 29, 2015 946       rferrel      Do not allow delete of a protected level file.
 * Nov 13, 2015 4946      mapeters     Use SWTMessageBox instead of MessageDialog.
 * Jan 15, 2016 5242      kbisanz      Replaced LocalizationFile with
 *                                     ILocalizationFile where possible
 * Jan 27, 2016 5054      randerso     Cleaned up SWTMessageBox
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DeleteAction extends Action {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DeleteAction.class);

    private LocalizationFile[] toDelete;

    private IWorkbenchPage page;

    private boolean prompt;

    /**
     * Map of extensions associated with the key extension.
     */
    private Map<String, String> associatedExtensions = new HashMap<String, String>();

    public DeleteAction(IWorkbenchPage page, LocalizationFile[] toDelete) {
        this(page, toDelete, true);
    }

    public DeleteAction(IWorkbenchPage page, LocalizationFile[] toDelete,
            boolean prompt) {
        super("Delete");
        this.page = page;
        this.toDelete = toDelete;
        this.prompt = prompt;
        populateAssociatedExtensions();
    }

    @Override
    public void run() {
        StringBuilder listOfFiles = new StringBuilder();
        for (int i = 0; i < toDelete.length; ++i) {
            listOfFiles.append(LocalizationUtil.extractName(toDelete[i]
                    .getPath()));
            listOfFiles.append("\n");
        }

        Shell shell = page.getWorkbenchWindow().getShell();

        if (prompt) {
            StringBuilder msg = new StringBuilder();
            msg.append("Are you sure you want to delete ");
            if (toDelete.length > 1) {
                msg.append("these " + toDelete.length + " items");
            } else {
                msg.append("this file");
            }
            msg.append("?\n\n").append(listOfFiles);

            SWTMessageBox messageDialog = new SWTMessageBox(shell,
                    "Delete Confirmation", msg.toString(), SWT.OK | SWT.CANCEL
                            | SWT.ICON_QUESTION);

            messageDialog.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof Integer) {
                        if ((int) returnValue == SWT.OK) {
                            deleteFiles();
                        }
                    }
                }
            });

            messageDialog.open();
        }
    }

    /**
     * Delete the selected files and all associated file extension variations.
     */
    private void deleteFiles() {
        List<IEditorReference> toClose = new ArrayList<>();
        // check for open editors and close them
        for (IEditorReference ref : page.getEditorReferences()) {
            IEditorInput input = null;
            try {
                input = ref.getEditorInput();
            } catch (PartInitException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Failed to check if an editor for the deleted "
                                + "file was open (in order to close it)", e);
            }
            if (input instanceof LocalizationEditorInput) {
                ILocalizationFile editorFile = ((LocalizationEditorInput) input)
                        .getLocalizationFile();
                String editorFilePath = editorFile.getPath();
                for (ILocalizationFile file : toDelete) {
                    if ((editorFilePath.equals(file.getPath()))
                            && editorFile.getContext()
                                    .equals(file.getContext())) {
                        toClose.add(ref);
                        break;
                    }
                }
            }
        }

        if (toClose.size() > 0) {
            page.closeEditors(
                    toClose.toArray(new IEditorReference[toClose.size()]),
                    false);
        }

        for (ILocalizationFile file : toDelete) {
            try {
                deleteFile(file);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, "Error deleting file: "
                        + file.toString(), e);
            }
        }
    }

    /**
     * Delete the file and all associated file extension variations.
     * 
     * @param file
     *            The file to delete
     * @throws Exception
     */
    private void deleteFile(ILocalizationFile file) throws Exception {
        if (file.isDirectory() == false) {
            // Check for file extension
            String name = LocalizationUtil.extractName(file.getPath());
            String[] parts = name.split("[.]");

            if (parts.length > 1) {
                // file has an extension, delete associated extensions if any
                String ext = parts[parts.length - 1];
                String associated = associatedExtensions.get(ext);

                if (associated != null) {
                    String[] extensions = associated.split(",");
                    String path = file.getPath().substring(0,
                            file.getPath().lastIndexOf(name));

                    String prefix = "";
                    for (int i = 0; i < (parts.length - 1); ++i) {
                        if (i > 0) {
                            prefix += ".";
                        }
                        prefix += parts[i];
                    }

                    path += prefix;

                    LocalizationContext ctx = file.getContext();
                    IPathManager pathManager = PathManagerFactory
                            .getPathManager();

                    for (String extension : extensions) {
                        String deletePath = path + "." + extension;
                        ILocalizationFile result = pathManager
                                .getLocalizationFile(ctx, deletePath);
                        if (result != null) {
                            result.delete();
                        }
                    }
                }
            }
        }

        // Didn't delete based on extensions, just delete the file
        file.delete();
    }

    @Override
    public boolean isEnabled() {
        boolean canDelete = true;
        for (LocalizationFile file : toDelete) {
            LocalizationContext ctx = file.getContext();
            LocalizationLevel level = ctx.getLocalizationLevel();
            if (level.isSystemLevel()) {
                canDelete = false;
                break;
            }
            if (file.isProtected() && file.getProtectedLevel().equals(level)) {
                canDelete = false;
                break;
            }
        }
        return canDelete;
    }

    /**
     * Fill the associatedExtensions Map with associated extensions.
     */
    private void populateAssociatedExtensions() {
        // Python: .py = .pyo, .pyc
        associatedExtensions.put("py", "pyo, pyc");
    }
}
