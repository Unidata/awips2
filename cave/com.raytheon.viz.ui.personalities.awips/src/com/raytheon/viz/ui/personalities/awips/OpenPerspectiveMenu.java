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
package com.raytheon.viz.ui.personalities.awips;

import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.WorkbenchException;
import org.eclipse.ui.actions.PerspectiveMenu;

import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Menu to show perspective list for opening perspectives
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class OpenPerspectiveMenu extends PerspectiveMenu {

    public OpenPerspectiveMenu() {
        super(PlatformUI.getWorkbench().getActiveWorkbenchWindow(),
                "Perspective");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.actions.PerspectiveMenu#run(org.eclipse.ui.
     * IPerspectiveDescriptor)
     */
    @Override
    protected void run(IPerspectiveDescriptor desc) {
        if (desc != null) {
            try {
                PlatformUI.getWorkbench().showPerspective(desc.getId(),
                        getWindow());
            } catch (WorkbenchException e) {
                UFStatus.getHandler().handle(
                        Priority.PROBLEM,
                        "Error opening perspective (" + desc.getId() + "): "
                                + e.getLocalizedMessage(), e);
            }
        }
    }

}
