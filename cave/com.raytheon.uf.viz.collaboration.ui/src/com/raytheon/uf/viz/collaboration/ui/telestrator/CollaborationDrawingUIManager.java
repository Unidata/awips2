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
package com.raytheon.uf.viz.collaboration.ui.telestrator;

import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.collaboration.ui.session.CollaborationSessionView;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.drawing.DrawingToolUIManager;

/**
 * UI Manager for the CollaborationDrawingResource, handles mouse input and
 * opens the toolbar
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CollaborationDrawingUIManager extends DrawingToolUIManager {

    private CollaborationDrawingResource resource;

    private CollaborationSessionView view;

    public CollaborationDrawingUIManager(CollaborationDrawingResource resource) {
        super(resource.getDrawingLayerFor(resource.getMyUser()), resource
                .getResourceContainer());
        this.resource = resource;
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                CollaborationDrawingResource resource = CollaborationDrawingUIManager.this.resource;
                IWorkbenchPage page = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getActivePage();
                view = (CollaborationSessionView) page.findViewReference(
                        CollaborationSessionView.ID,
                        resource.getContainer().getSessionId()).getPart(false);
                view.updateToolItems();
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingToolUIManager#dispose()
     */
    @Override
    public void dispose() {
        super.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingToolUIManager#canTellestrate(int)
     */
    @Override
    protected boolean canTellestrate(int mouseButton) {
        return super.canTellestrate(mouseButton) && resource.canTellestrate();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingToolUIManager#handleMouseUp(int,
     * int, int)
     */
    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
        boolean rval = super.handleMouseUp(x, y, mouseButton);
        if (rval) {
            view.updateToolItems();
        }
        return rval;
    }

}
