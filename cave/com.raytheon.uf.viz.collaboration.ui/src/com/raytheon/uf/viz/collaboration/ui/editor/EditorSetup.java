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
package com.raytheon.uf.viz.collaboration.ui.editor;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.collaboration.display.editor.CollaborationEditor;
import com.raytheon.uf.viz.collaboration.display.editor.SharedEditorData;
import com.raytheon.uf.viz.collaboration.ui.rsc.CollaborationWrapperResource;
import com.raytheon.uf.viz.collaboration.ui.rsc.CollaborationWrapperResourceData;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Utilities for setting up collaboration editors.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 19, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class EditorSetup {

    /**
     * Extracts a SharedEditorData object from the editor passed in.
     * 
     * @param editor
     *            the editor to extract a shared editor for.
     * @return
     */
    public static SharedEditorData extractSharedEditorData(AbstractEditor editor) {
        SharedEditorData se = new SharedEditorData();

        IRenderableDisplay display = editor.getActiveDisplayPane()
                .getRenderableDisplay();
        if (display instanceof AbstractRenderableDisplay) {
            AbstractRenderableDisplay clonedDisplay = ((AbstractRenderableDisplay) display)
                    .cloneDisplay();
            List<ResourcePair> toKeep = new ArrayList<ResourcePair>();
            for (ResourcePair rp : display.getDescriptor().getResourceList()) {
                if (rp.getResource() instanceof CollaborationWrapperResource) {
                    CollaborationWrapperResource resource = (CollaborationWrapperResource) rp
                            .getResource();
                    ResourcePair copy = new ResourcePair();
                    copy.setLoadProperties(rp.getLoadProperties());
                    copy.setProperties(rp.getProperties());
                    copy.setResourceData(resource.getWrapperResourceData());
                    toKeep.add(copy);
                }
            }
            ResourceList list = clonedDisplay.getDescriptor().getResourceList();
            list.clear();
            list.addAll(toKeep);
            se.setDisplay(clonedDisplay);
        }
        // extract grid geometry
        IDescriptor desc = display.getDescriptor();
        se.setGeometry(desc.getGridGeometry());

        // extract extent to get the proper zoom/pan
        IExtent extent = editor.getActiveDisplayPane().getRenderableDisplay()
                .getExtent();
        se.setEnvelope(new Envelope(extent.getMinX(), extent.getMaxX(), extent
                .getMinY(), extent.getMaxY()));

        // Set current size
        Rectangle bounds = display.getBounds();
        se.setWidth(bounds.width);
        se.setHeight(bounds.height);

        return se;
    }

    /**
     * Creates and opens a CollaborationEditor based on the SharedEditorData.
     * 
     * @param sharedEditor
     *            the data necessary to create the editor
     * @return
     */
    public static CollaborationEditor createEditor(SharedEditorData sharedEditor) {
        CollaborationEditor editor = null;
        AbstractRenderableDisplay[] displays = new AbstractRenderableDisplay[1];
        AbstractRenderableDisplay disp = sharedEditor.getDisplay();
        PixelExtent extent = new PixelExtent(sharedEditor.getEnvelope()
                .getMinX(), sharedEditor.getEnvelope().getMaxX(), sharedEditor
                .getEnvelope().getMinY(), sharedEditor.getEnvelope().getMaxY());
        disp.setExtent(extent);

        List<ResourcePair> toRemove = new ArrayList<ResourcePair>();
        ResourceList list = disp.getDescriptor().getResourceList();
        for (ResourcePair rp : list) {
            if (rp.getResourceData() instanceof CollaborationWrapperResourceData) {
                rp.setResourceData(((CollaborationWrapperResourceData) rp
                        .getResourceData()).getWrappedResourceData());
            } else {
                toRemove.add(rp);
            }
        }
        list.removeAll(toRemove);

        displays[0] = disp;
        editor = (CollaborationEditor) UiUtil.createEditor(
                CollaborationEditor.EDITOR_ID, displays);
        editor.setCanvasSize(new Rectangle(0, 0, sharedEditor.getWidth(),
                sharedEditor.getHeight()));
        return editor;
    }

}
