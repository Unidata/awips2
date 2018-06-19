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
package com.raytheon.viz.volumebrowser.loader;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IEditorPart;

import com.raytheon.uf.viz.core.DescriptorMap;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.rsc.GridLoadProperties;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.maps.actions.NewMapEditor;
import com.raytheon.uf.viz.core.maps.display.MapRenderableDisplay;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;
import com.raytheon.viz.volumebrowser.vbui.VolumeBrowserDialogSettings;

/**
 * 
 * Contains logic for creating a {@link MapRenderableDisplay} appropriate for
 * the current perspective.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 03, 2015  3861     bsteffen  Extracted from ProductTableComp
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public abstract class AbstractMapProductCreator extends AbstractProductCreator {

    @Override
    protected MapRenderableDisplay createNewRenderableDisplay(VolumeBrowserDialogSettings dialogSettings, SelectedData selectedData) {
        String editorId = DescriptorMap.getEditorId(MapDescriptor.class
                .getName());

        IEditorPart editorPart = EditorUtil.findEditor(editorId);
        try {
            if (editorPart == null) {
                new NewMapEditor().execute(null);
                editorPart = EditorUtil.findEditor(editorId);
            }
            AbstractEditor editor = (AbstractEditor) editorPart;
            MapRenderableDisplay display = (MapRenderableDisplay) editor
                    .getActiveDisplayPane().getRenderableDisplay()
                    .createNewDisplay();
            display.setDescriptor(new MapDescriptor());
            return display;
        } catch (ExecutionException | VizException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    protected LoadProperties createNewLoadProperties(DisplayType displayType) {
        return new GridLoadProperties(displayType);
    }

}
