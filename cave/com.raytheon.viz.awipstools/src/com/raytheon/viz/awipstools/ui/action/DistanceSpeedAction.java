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
package com.raytheon.viz.awipstools.ui.action;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IEditorPart;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.maps.actions.NewMapEditor;
import com.raytheon.uf.viz.core.maps.display.VizMapEditor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.uf.viz.core.rsc.tools.action.AbstractGenericToolAction;
import com.raytheon.viz.awipstools.ui.layer.DistanceSpeedLayer;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Loads a {@link DistanceSpeedLayer} to a {@link VizMapEditor}.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 17, 2007  495      ebabin      Initial Creation.
 * Feb 15, 2011  7975     bkowal      Restore the DistanceSpeedLayer
 *                                    associated with the Display Pane.
 * Aug 30, 2013  2310     bsteffen    Ensure tool is used on a map editor.
 * Sep 30, 2013  2400     njensen     Ensure tool opens on the active map editor if applicable
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class DistanceSpeedAction extends
        AbstractGenericToolAction<DistanceSpeedLayer> {

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        IEditorPart editorPart = EditorUtil.getActiveEditor();
        if (editorPart != null && editorPart instanceof VizMapEditor) {
            // if the current editor is a map, use that one
            editorPart.getSite().getPage().bringToTop(editorPart);
        } else {
            // find any map editor that's open
            editorPart = EditorUtil.findEditor(VizMapEditor.EDITOR_ID);
            if (editorPart == null) {
                // no map editor open, make a new one
                new NewMapEditor().execute(arg0);
            } else {
                editorPart.getSite().getPage().bringToTop(editorPart);
            }
        }
        return super.execute(arg0);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.awipstools.ui.action.MapToolAction#getResourceData()
     */
    @Override
    protected GenericToolsResourceData<DistanceSpeedLayer> getResourceData() {
        return new GenericToolsResourceData<DistanceSpeedLayer>(
                DistanceSpeedLayer.NAME, DistanceSpeedLayer.class);
    }

    @Override
    protected DistanceSpeedLayer getResource(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {

        DistanceSpeedLayer layer = null;

        for (IDisplayPane pane : getSelectedPanes()) {
            for (ResourcePair rp : pane.getDescriptor().getResourceList()) {
                if (rp.getResource() instanceof DistanceSpeedLayer) {
                    layer = (DistanceSpeedLayer) rp.getResource();
                    layer.reopenDialog();
                    return layer;
                }
            }
        }

        layer = super.getResource(loadProperties, descriptor);
        return layer;
    }

}
