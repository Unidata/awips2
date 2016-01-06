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
package com.raytheon.uf.viz.localization.perspective.editor;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IPathEditorInput;
import org.eclipse.ui.IPersistableElement;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 *
 * Editor input for localization files
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 3,  2010            mschenke    Initial creation
 * Nov 27, 2013            mschenke    Moved into localization.perspective project
 * Feb 11, 2015  4108      randerso    Implmented hashCode() and equals()
 * Jan 06, 2016  4834      nabowle     add refreshLocalizationFile().
 *
 * </pre>
 *
 * @author mschenke
 * @version 1.0
 */
public class LocalizationEditorInput implements IFileEditorInput,
        IPathEditorInput, IPersistableElement {

    public static final String FACTORY_ID = "com.raytheon.uf.viz.localizationInputFactoryId";

    public static final String FILE_NAME_ID = "fileName";

    public static final String CONTEXT_LEVEL_ID = "contextLevel";

    public static final String CONTEXT_TYPE_ID = "contextType";

    public static final String CONTEXT_NAME_ID = "contextName";

    public static final String RESOURCE_PATH_ID = "resourcePath";

    private LocalizationFile localizationFile;

    private String name;

    private IFile file = null;

    public LocalizationEditorInput(LocalizationFile file, IFile rscFile) {
        setLocalizationFile(file);
        file.getFile();
        this.file = rscFile;
    }

    public void setLocalizationFile(LocalizationFile localizationFile) {
        this.localizationFile = localizationFile;
        name = LocalizationUtil.extractName(localizationFile.getName());
        file = null;
    }

    /**
     * Refreshes the localizationFile reference.
     */
    public void refreshLocalizationFile() {
        IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationFile latestFile = pathManager.getLocalizationFile(
                this.localizationFile.getContext(),
                this.localizationFile.getPath());
        this.localizationFile = latestFile;
    }

    /**
     * @return the localizationFile
     */
    public LocalizationFile getLocalizationFile() {
        return localizationFile;
    }

    @Override
    public boolean exists() {
        return localizationFile.exists();
    }

    @Override
    public ImageDescriptor getImageDescriptor() {
        return null;
    }

    @Override
    public String getName() {
        return name + " - "
                + localizationFile.getContext().getLocalizationLevel();
    }

    @Override
    public IPersistableElement getPersistable() {
        return this;
    }

    @Override
    public String getToolTipText() {
        String tip = localizationFile.getName();
        if (localizationFile.isProtected()) {
            tip += " (Protected @ " + localizationFile.getProtectedLevel()
                    + ")";
        }
        return tip;
    }

    @SuppressWarnings("rawtypes")
    @Override
    public Object getAdapter(Class adapter) {
        return Platform.getAdapterManager().getAdapter(this, adapter);
    }

    @Override
    public IStorage getStorage() throws CoreException {
        return getFile();
    }

    @Override
    public IFile getFile() {
        if (file.exists() == false) {
            try {
                file.createLink(localizationFile.getFile().toURI(),
                        IResource.NONE, null);
            } catch (CoreException e) {
                UFStatus.getHandler().handle(Priority.PROBLEM,
                        "Error creating file link for: " + localizationFile, e);
            }
        }
        return file;
    }

    @Override
    public IPath getPath() {
        return getFile().getRawLocation();
    }

    @Override
    public void saveState(IMemento memento) {
        LocalizationContext ctx = localizationFile.getContext();
        memento.putString(FILE_NAME_ID, localizationFile.getName());
        memento.putString(CONTEXT_NAME_ID, ctx.getContextName());
        memento.putString(CONTEXT_TYPE_ID, ctx.getLocalizationType().name());
        memento.putString(CONTEXT_LEVEL_ID, ctx.getLocalizationLevel().name());
        memento.putString(RESOURCE_PATH_ID, file.getFullPath()
                .toPortableString());
    }

    @Override
    public String getFactoryId() {
        return FACTORY_ID;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = (prime * result)
                + ((localizationFile == null) ? 0 : localizationFile.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        LocalizationEditorInput other = (LocalizationEditorInput) obj;
        if (localizationFile == null) {
            if (other.localizationFile != null) {
                return false;
            }
        } else if (!localizationFile.equals(other.localizationFile)) {
            return false;
        }
        return true;
    }

}
