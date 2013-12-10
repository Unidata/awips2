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

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
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
     * @return the localizationFile
     */
    public LocalizationFile getLocalizationFile() {
        return localizationFile;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IEditorInput#exists()
     */
    @Override
    public boolean exists() {
        return localizationFile.exists();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IEditorInput#getImageDescriptor()
     */
    @Override
    public ImageDescriptor getImageDescriptor() {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IEditorInput#getName()
     */
    @Override
    public String getName() {
        return name + " - "
                + localizationFile.getContext().getLocalizationLevel();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IEditorInput#getPersistable()
     */
    @Override
    public IPersistableElement getPersistable() {
        return this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IEditorInput#getToolTipText()
     */
    @Override
    public String getToolTipText() {
        String tip = localizationFile.getName();
        if (localizationFile.isProtected()) {
            tip += " (Protected @ " + localizationFile.getProtectedLevel()
                    + ")";
        }
        return tip;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
     */
    @SuppressWarnings("rawtypes")
    @Override
    public Object getAdapter(Class adapter) {
        return Platform.getAdapterManager().getAdapter(this, adapter);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IStorageEditorInput#getStorage()
     */
    @Override
    public IStorage getStorage() throws CoreException {
        return getFile();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IFileEditorInput#getFile()
     */
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

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPathEditorInput#getPath()
     */
    @Override
    public IPath getPath() {
        return getFile().getRawLocation();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPersistable#saveState(org.eclipse.ui.IMemento)
     */
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

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPersistableElement#getFactoryId()
     */
    @Override
    public String getFactoryId() {
        return FACTORY_ID;
    }
}
