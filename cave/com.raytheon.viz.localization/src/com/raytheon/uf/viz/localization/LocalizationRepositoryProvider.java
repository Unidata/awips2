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
package com.raytheon.uf.viz.localization;

import org.eclipse.core.resources.IFileModificationValidator;
import org.eclipse.core.resources.team.FileModificationValidator;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.team.core.RepositoryProvider;

/**
 * Class used to inject our LocalizationFileModifactionValidor
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 3, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

@SuppressWarnings("deprecation")
public class LocalizationRepositoryProvider extends RepositoryProvider {

    public static final String ID = "com.raytheon.uf.viz.localization.localizationRepositoryProvider";

    @Override
    public IFileModificationValidator getFileModificationValidator() {
        return getFileModificationValidator2();
    }

    @Override
    public FileModificationValidator getFileModificationValidator2() {
        return new LocalizationFileModificationValidator();
    }

    @Override
    public boolean canHandleLinkedResources() {
        return true;
    }

    @Override
    public boolean canHandleLinkedResourceURI() {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.resources.IProjectNature#deconfigure()
     */
    @Override
    public void deconfigure() throws CoreException {

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.team.core.RepositoryProvider#configureProject()
     */
    @Override
    public void configureProject() throws CoreException {

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.team.core.RepositoryProvider#getID()
     */
    @Override
    public String getID() {
        return ID;
    }

}
