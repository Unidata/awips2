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

    private CollaborationDrawingToolbar toolbar;

    public CollaborationDrawingUIManager(CollaborationDrawingResource resource) {
        super(resource.getDrawingLayerFor(resource.getMyUser()), resource
                .getResourceContainer());
        this.resource = resource;
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                CollaborationDrawingResource resource = CollaborationDrawingUIManager.this.resource;
                toolbar = CollaborationDrawingToolbar.openToolbar(resource);
                toolbar.setCurrentDrawingLayer(resource
                        .getDrawingLayerFor(resource.getMyUser()));
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
        if (toolbar != null) {
            // Incase disposed before async exec can run in constructor
            toolbar.disposed(resource);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.drawing.DrawingToolUIManager#handleMouseDown(int,
     * int, int)
     */
    @Override
    public boolean handleMouseDown(int x, int y, int mouseButton) {
        if (resource.canTellestrate() == false) {
            return false;
        }
        return super.handleMouseDown(x, y, mouseButton);
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
            toolbar.resourceChanged(resource);
        }
        return rval;
    }

}
