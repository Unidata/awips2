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
package com.raytheon.uf.viz.localization.perspective;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.ui.IElementFactory;
import org.eclipse.ui.IMemento;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.localization.perspective.editor.LocalizationEditorInput;

/**
 * Constructs LocalizationEditorInput objects saved to an IMemento
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 28, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class LocalizationFileInputFactory implements IElementFactory {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IElementFactory#createElement(org.eclipse.ui.IMemento)
     */
    @Override
    public IAdaptable createElement(IMemento memento) {
        LocalizationEditorInput input = null;
        String fileName = memento
                .getString(LocalizationEditorInput.FILE_NAME_ID);
        LocalizationContext ctx = new LocalizationContext(
                LocalizationType.valueOf(memento
                        .getString(LocalizationEditorInput.CONTEXT_TYPE_ID)),
                LocalizationLevel.valueOf(memento
                        .getString(LocalizationEditorInput.CONTEXT_LEVEL_ID)),
                memento.getString(LocalizationEditorInput.CONTEXT_NAME_ID));
        String resourcePath = memento
                .getString(LocalizationEditorInput.RESOURCE_PATH_ID);
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile file = pm.getLocalizationFile(ctx, fileName);
        if (file.exists()) {
            IPath path = Path.fromPortableString(resourcePath);
            IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
            IFile fi = root.getFile(path);
            input = new LocalizationEditorInput(file, fi);
            if (fi.exists() == false) {
                IFolder parent = (IFolder) fi.getParent();
                if (parent.exists() == false) {
                    recursivelyBuildPath(parent.getParent(), parent);
                }
                try {
                    fi.createLink(file.getFile(false).toURI(), IResource.NONE,
                            null);
                } catch (Exception e) {
                    UFStatus.getHandler().handle(Priority.PROBLEM,
                            "Error restoring file link: " + fi, e);
                }
            }
        }
        return input;
    }

    /**
     * @param root
     * @param parent
     */
    private void recursivelyBuildPath(IContainer parent, IFolder create) {
        if (parent.exists() == false && parent instanceof IFolder) {
            recursivelyBuildPath(parent.getParent(), (IFolder) parent);
        }

        if (parent.exists()) {
            try {
                create.create(true, true, null);
            } catch (CoreException e) {
                e.printStackTrace();
            }
        }
    }
}
