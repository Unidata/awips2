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

package com.raytheon.uf.viz.drawing;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.tools.AbstractModalTool;

/**
 * Describes a basic drawing tool.
 * 
 * All subclasses implement abstract method getMouseHandler() which returns how
 * the drawing is handled
 * 
 * 
 * @author chammack
 * 
 */
public abstract class AbstractDrawingTool extends AbstractModalTool {

    /** The drawing layer */
    protected DrawingLayer theDrawingLayer;

    /** The handler (if any) that has been registered */
    protected IInputHandler handlerRegistered;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractModalTool#deactivateTool()
     */
    @Override
    protected void deactivateTool() {
        editor.unregisterMouseHandler(this.handlerRegistered);
    }

    @Override
    protected void activateTool() {
        theDrawingLayer = null;
        IDescriptor desc = getActiveDescriptor();
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
            PathDrawingResourceData resourceData = new PathDrawingResourceData();
            try {
                theDrawingLayer = resourceData.construct(new LoadProperties(),
                        desc);
            } catch (VizException e1) {
                e1.printStackTrace();
            }
            try {
                desc.getResourceList().add(theDrawingLayer);

                // TODO probably need to make this so that it can't be the time
                // match basis
                theDrawingLayer.initInternal(editor.getActiveDisplayPane()
                        .getTarget());
            } catch (Exception e) {
                // ignore
            }
        }

        if (this.handlerRegistered != null) {
            editor.unregisterMouseHandler(this.handlerRegistered);
        }

        this.handlerRegistered = getMouseHandler();
        editor.registerMouseHandler(this.handlerRegistered);
    }

    /**
     * This method returns a mousehandler that will be registered/unregistered
     * on the editor when appropriate.
     * 
     * The handler should interact with the drawing layer to implement drawing
     * functions.
     * 
     * @return the handler
     */
    public abstract IInputHandler getMouseHandler();

}
