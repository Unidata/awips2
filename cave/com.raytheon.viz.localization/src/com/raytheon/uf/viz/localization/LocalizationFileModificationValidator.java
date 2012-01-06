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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.team.FileModificationValidationContext;
import org.eclipse.core.resources.team.FileModificationValidator;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

/**
 * Class used to verify we can modify localziation files. Used so user is not
 * prompted to make file writable when set to read only
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

public class LocalizationFileModificationValidator extends
        FileModificationValidator {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.resources.team.FileModificationValidator#validateEdit
     * (org.eclipse.core.resources.IFile[],
     * org.eclipse.core.resources.team.FileModificationValidationContext)
     */
    @Override
    public IStatus validateEdit(IFile[] files,
            FileModificationValidationContext context) {
        boolean allOk = true;
        for (IFile file : files) {
            if (file.isReadOnly()) {
                allOk = false;
                break;
            }
        }
        return allOk ? Status.OK_STATUS : Status.CANCEL_STATUS;
    }

}
