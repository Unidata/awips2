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

package com.raytheon.viz.drawing.collaboration;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.drawing.DrawingLayer;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.tools.map.AbstractMapTool;

/**
 * 
 * Start/Stop Collaboration Tool
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Nov 21, 2006 66          chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */

public class StartCollaborationTool extends AbstractMapTool {

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        super.execute(arg0);
        CollaborationManager mgr = CollaborationManager.getInstance();

        if (mgr != null && mgr.isConnected()) {
            MessageBox mb = new MessageBox(VizWorkbenchManager.getInstance()
                    .getCurrentWindow().getShell(), SWT.ICON_QUESTION | SWT.YES
                    | SWT.NO);
            mb.setMessage("Disconnect from Collaboration?");

            int id = mb.open();

            if (id == SWT.YES) {
                mgr.disconnect();
                ChatWindow wind = ChatWindow.getInstance();
                if (wind != null)
                    wind.clear();
            }

            return null;
        }

        DrawingLayer theDrawingLayer = null;
        IDescriptor desc = editor.getActiveDisplayPane().getDescriptor();
        ResourceList rscList = desc.getResourceList();
        for (ResourcePair rp : rscList) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            // find a drawable layer
            if (rsc instanceof DrawingLayer) {
                theDrawingLayer = (DrawingLayer) rsc;
                break;
            }
        }

        if (theDrawingLayer == null) {
            theDrawingLayer = new DrawingLayer();
            desc.getResourceList().add(theDrawingLayer);
        }

        ConnectDialog cd = new ConnectDialog(VizWorkbenchManager.getInstance()
                .getCurrentWindow().getShell(), theDrawingLayer);
        cd.open();

        return null;
    }

}
