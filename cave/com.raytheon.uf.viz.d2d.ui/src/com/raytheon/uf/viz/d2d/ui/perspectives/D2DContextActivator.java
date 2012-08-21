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
package com.raytheon.uf.viz.d2d.ui.perspectives;

import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.d2d.core.ID2DRenderableDisplay;
import com.raytheon.viz.ui.perspectives.AbstractWorkbenchPartContextActivator;

/**
 * Activates context for D2D UI for IDisplayPaneContainers with
 * ID2DRenderableDisplays
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 5, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class D2DContextActivator extends AbstractWorkbenchPartContextActivator {

    D2DContextActivator(IWorkbenchPage page) {
        super(page);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.perspectives.AbstractPerspectiveContextManager#
     * isPerspectivePart(org.eclipse.ui.IWorkbenchPartReference)
     */
    @Override
    protected boolean isPerspectivePart(IWorkbenchPartReference partRef) {
        if (partRef != null) {
            IWorkbenchPart part = partRef.getPart(false);
            if (part instanceof IDisplayPaneContainer) {
                for (IDisplayPane pane : ((IDisplayPaneContainer) part)
                        .getDisplayPanes()) {
                    if (pane.getRenderableDisplay() instanceof ID2DRenderableDisplay) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

}
