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

package com.raytheon.viz.drawing;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.tools.map.AbstractMapTool;

/**
 * 
 * Reset the drawing tool (remove any previously drawn shapes)
 * 
 * @author chammack
 * 
 */
public class ResetDrawingTool extends AbstractMapTool {

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        super.execute(arg0);

        DrawingLayer drawingLayer = null;
        IDescriptor desc = editor.getActiveDisplayPane().getDescriptor();
        ResourceList rscList = desc.getResourceList();
        for (ResourcePair rp : rscList) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            // find a drawable layer
            if (rsc instanceof DrawingLayer) {
                drawingLayer = (DrawingLayer) rsc;
                break;
            }
        }

        // if one found, reset and refresh
        if (drawingLayer != null) {
            drawingLayer.reset();
            editor.refresh();
        }

        return null;
    }

}
