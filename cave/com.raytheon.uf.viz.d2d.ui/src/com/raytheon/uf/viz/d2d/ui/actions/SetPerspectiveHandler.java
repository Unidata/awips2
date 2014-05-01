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
/**
 * 
 */
package com.raytheon.uf.viz.d2d.ui.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;

/**
 * @author randerso
 * 
 */
public class SetPerspectiveHandler extends AbstractHandler {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        String perspectiveId = arg0.getParameter("perspective");
        IWorkbenchPage activePage = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();

        String oldId = null;
        IPerspectiveDescriptor oldDesc = null;
        if (activePage != null) {
            oldDesc = activePage.getPerspective();
            if (oldDesc != null) {
                oldId = oldDesc.getId();
            }
        }

        if (!perspectiveId.equals(oldId)) {

            IPerspectiveDescriptor newPerspective = PlatformUI.getWorkbench()
                    .getPerspectiveRegistry().findPerspectiveWithId(
                            perspectiveId);

            PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getActivePage().setPerspective(newPerspective);

            if (activePage != null) {
                PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                        .getActivePage()
                        .closePerspective(oldDesc, false, false);
            }
        }

        return null;
    }
}
