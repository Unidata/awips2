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
package com.raytheon.uf.viz.localization.perspective.ui;

import java.io.File;

import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.eclipse.ui.ide.FileStoreEditorInput;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.DocumentProviderRegistry;
import org.eclipse.ui.texteditor.IDocumentProvider;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.FileUpdatedMessage.FileChangeType;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.localization.LocalizationPerspectiveUtils;
import com.raytheon.uf.viz.localization.perspective.editor.LocalizationEditorInput;
import com.raytheon.uf.viz.localization.service.ILocalizationService;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Dynamically populates levels that can be saved as
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 24, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class LocalizationSaveAsPopulator extends CompoundContributionItem {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LocalizationSaveAsPopulator.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.actions.CompoundContributionItem#getContributionItems()
     */
    @Override
    protected IContributionItem[] getContributionItems() {
        MenuManager menuMgr = new MenuManager("SaveAs", "FileSaveAs");
        final boolean enabled;
        final LocalizationLevel protectedLevel;
        final IEditorPart active = EditorUtil.getActiveEditor();
        if (active != null
                && active.getEditorInput() instanceof LocalizationEditorInput) {
            enabled = active.isDirty();
            protectedLevel = ((LocalizationEditorInput) active.getEditorInput())
                    .getLocalizationFile().getProtectedLevel();
        } else {
            enabled = false;
            protectedLevel = null;
        }

        LocalizationLevel[] levels = PathManagerFactory.getPathManager()
                .getAvailableLevels();
        for (final LocalizationLevel level : levels) {
            if (level.isSystemLevel() == false) {
                String displayName = LocalizationUtil.getProperName(level);
                final String contextName = LocalizationManager
                        .getContextName(level);
                if (contextName != null) {
                    displayName += " (" + contextName + ")";
                }

                menuMgr.add(new Action(displayName) {

                    @Override
                    public void run() {
                        saveEditorAs(active, level);
                    }

                    @Override
                    public boolean isEnabled() {
                        return enabled
                                && (protectedLevel == null || level
                                        .compareTo(protectedLevel) <= 0);
                    }

                });
            }
        }

        menuMgr.add(new Separator());
        menuMgr.add(new Action("Local File...") {

            /*
             * (non-Javadoc)
             * 
             * @see org.eclipse.jface.action.Action#isEnabled()
             */
            @Override
            public boolean isEnabled() {
                return active.isDirty();
            }

            /*
             * (non-Javadoc)
             * 
             * @see org.eclipse.jface.action.Action#run()
             */
            @Override
            public void run() {
                saveEditorAs(active);
            }

        });

        return menuMgr.getItems();
    }

    private static void saveEditorAs(IEditorPart editor,
            LocalizationLevel newLevel) {
        LocalizationEditorInput input = (LocalizationEditorInput) editor
                .getEditorInput();
        LocalizationFile fileToSave = input.getLocalizationFile();

        if (newLevel == fileToSave.getContext().getLocalizationLevel()) {
            String myContext = LocalizationManager.getContextName(newLevel);
            String existingContext = fileToSave.getContext().getContextName();

            if ((myContext != null && myContext.equals(existingContext))
                    || (myContext == null && existingContext == null)) {
                // Easy case, just save
                editor.doSave(null);
                return;
            }
        }

        // get new level file
        IPathManager pm = PathManagerFactory.getPathManager();

        String name = fileToSave.getName();
        LocalizationContext newCtx = pm.getContext(fileToSave.getContext()
                .getLocalizationType(), newLevel);
        final LocalizationFile newFile = pm.getLocalizationFile(newCtx, name);
        boolean proceed = true;
        if (newFile.exists()) {
            proceed = MessageDialog
                    .openConfirm(
                            editor.getSite().getShell(),
                            "Override File",
                            "The file, "
                                    + name
                                    + ", already exists in this directory at the USER level. "
                                    + "Are you sure you want to override it?");
        }

        if (proceed) {
            try {
                IFileStore store = saveToFile(editor.getEditorInput(),
                        newFile.getFile(false));
                if (store != null) {
                    final IWorkbenchPage page = editor.getSite().getPage();
                    newFile.addFileUpdatedObserver(new ILocalizationFileObserver() {

                        @Override
                        public void fileUpdated(FileUpdatedMessage message) {
                            if (message.getChangeType() != FileChangeType.DELETED) {
                                final ILocalizationService service = LocalizationPerspectiveUtils
                                        .getService(page);
                                VizApp.runAsync(new Runnable() {
                                    @Override
                                    public void run() {
                                        service.openFile(newFile);
                                    }
                                });
                            }
                        }
                    });
                    newFile.save();
                    page.closeEditor(editor, false);
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, "Error saving file as "
                        + newLevel + ": " + e.getLocalizedMessage(), e);
            }
        }
    }

    private static IFileStore saveToFile(IEditorInput input, File fileToSaveTo)
            throws CoreException {
        IDocumentProvider provider = DocumentProviderRegistry.getDefault()
                .getDocumentProvider(input);
        if (provider != null) {
            IFileStore store = EFS.getLocalFileSystem().getStore(
                    new Path(fileToSaveTo.getAbsolutePath()));
            FileStoreEditorInput newInput = new FileStoreEditorInput(store);
            provider.saveDocument(null, newInput, provider.getDocument(input),
                    true);
            return store;
        }
        return null;
    }

    private static void saveEditorAs(IEditorPart editor) {
        LocalizationEditorInput input = (LocalizationEditorInput) editor
                .getEditorInput();
        LocalizationFile fileToSave = input.getLocalizationFile();
        FileDialog fd = new FileDialog(editor.getSite().getShell(), SWT.SAVE);
        fd.setFileName(LocalizationUtil.extractName(fileToSave.getName()));
        fd.setOverwrite(true);
        fd.setFilterPath(System.getProperty("user.home"));

        String pathToSaveTo = fd.open();
        if (pathToSaveTo != null) {
            File newFile = new File(pathToSaveTo);
            try {
                IFileStore store = saveToFile(input, newFile);
                if (store != null) {
                    IWorkbenchPage page = editor.getSite().getPage();
                    page.closeEditor(editor, false);
                    IDE.openEditorOnFileStore(page, store);
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, "Error saving file to: "
                        + newFile, e);
            }
        }
    }
}
