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
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbenchPage;

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

/**
 * Deletes the selected localation file
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
        String listOfFiles = "";
        for (int i = 0; i < toDelete.length; ++i) {
            listOfFiles += LocalizationUtil.extractName(toDelete[i].getName())
                    + "\n";
        }

        Shell shell = page.getWorkbenchWindow().getShell();

        if (prompt) {
            boolean choice = MessageDialog.openConfirm(
                    shell,
                    "Delete Confirmation",
                    listOfFiles
                            + String.format(
                                    "\n\nAre you sure you want to delete %s?",
                                    toDelete.length > 1 ? "these items"
                                            : "this file"));
            if (!choice) {
                return;
            }
        }
        List<IEditorReference> toClose = new ArrayList<IEditorReference>();
        // check for open editors and close them
        for (IEditorReference ref : page.getEditorReferences()) {
            IEditorPart part = ref.getEditor(false);
            IEditorInput input = part.getEditorInput();
            if (input instanceof LocalizationEditorInput) {
                LocalizationFile editorFile = ((LocalizationEditorInput) input)
                        .getLocalizationFile();
                for (LocalizationFile file : toDelete) {
                    if ((editorFile.compareTo(file) == 0)
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

        for (LocalizationFile file : toDelete) {
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
    private void deleteFile(LocalizationFile file) throws Exception {
        if (file.isDirectory() == false) {
            // Check for file extension
            String name = LocalizationUtil.extractName(file.getName());
            String[] parts = name.split("[.]");

            if (parts.length > 1) {
                // file has an extension, delete associated extensions if any
                String ext = parts[parts.length - 1];
                String associated = associatedExtensions.get(ext);

                if (associated != null) {
                    String[] extensions = associated.split(",");
                    String path = file.getName().substring(0,
                            file.getName().lastIndexOf(name));

                    String prefix = "";
                    for (int i = 0; i < parts.length - 1; ++i) {
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
                        LocalizationFile result = pathManager
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
