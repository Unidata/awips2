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
package com.raytheon.viz.gfe.perspective;

import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.gfe.GridManagerView;
import com.raytheon.viz.gfe.core.GFEMapRenderableDisplay;
import com.raytheon.viz.ui.perspectives.AbstractWorkbenchPartContextActivator;

/**
 * Context activator for GFE perspective parts
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 21, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GFEContextActivator extends AbstractWorkbenchPartContextActivator {

    /**
     * @param page
     */
    GFEContextActivator(IWorkbenchPage page) {
        super(page);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.perspectives.AbstractPerspectiveContextActivator#
     * isPerspectivePart(org.eclipse.ui.IWorkbenchPartReference)
     */
    @Override
    protected boolean isPerspectivePart(IWorkbenchPartReference partRef) {
        if (GridManagerView.ID.equals(partRef.getId())) {
            return true;
        } else {
            IWorkbenchPart part = partRef.getPart(false);
            if (part instanceof IDisplayPaneContainer) {
                IDisplayPaneContainer container = (IDisplayPaneContainer) part;
                for (IDisplayPane pane : container.getDisplayPanes()) {
                    if (pane.getRenderableDisplay() instanceof GFEMapRenderableDisplay) {
                        return true;
                    }
                }
            }
        }
        return false;
    }
}
