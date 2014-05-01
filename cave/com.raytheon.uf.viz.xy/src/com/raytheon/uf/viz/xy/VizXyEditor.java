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
package com.raytheon.uf.viz.xy;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.xy.graph.AbstractXyRenderableDisplay;
import com.raytheon.uf.viz.xy.map.IInsetMapContainer.InsetMapUtil;
import com.raytheon.viz.ui.editor.EditorInput;
import com.raytheon.viz.ui.editor.VizMultiPaneEditor;
import com.raytheon.viz.ui.panes.PaneManager;

/**
 * Default xy editor, contains a graphPane and insetMap, the insetMap can be
 * disregarded if the renderable display is not of type IInsetMapContainer or
 * the function getInsetMapLocation returns null
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 8, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VizXyEditor extends VizMultiPaneEditor implements
        IRenderableDisplayChangedListener {

    protected String name;

    public VizXyEditor() {
        addRenderableDisplayChangedListener(this);
    }

    @Override
    protected void validateEditorInput(EditorInput input)
            throws PartInitException {
        super.validateEditorInput(input);
        if (input.getPaneManager() != null
                && input.getPaneManager() instanceof XyPaneManager == false) {
            throw new PartInitException("Expected pane manager of type: "
                    + XyPaneManager.class);
        }

        for (IRenderableDisplay display : input.getRenderableDisplays()) {
            if (display instanceof AbstractXyRenderableDisplay == false) {
                throw new PartInitException("Expected display of type: "
                        + AbstractXyRenderableDisplay.class + ", got "
                        + display.getClass());
            } else {
                name = ((AbstractXyRenderableDisplay) display).getTabTitle();
            }
        }
    }

    private XyPaneManager getPaneManager() {
        return (XyPaneManager) editorInput.getPaneManager();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.editor.AbstractEditor#getNewPaneManager()
     */
    @Override
    protected PaneManager getNewPaneManager() {
        return new XyPaneManager();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IRenderableDisplayChangedListener#
     * renderableDisplayChanged(com.raytheon.uf.viz.core.IDisplayPane,
     * com.raytheon.uf.viz.core.drawables.IRenderableDisplay,
     * com.raytheon.uf.viz
     * .core.IRenderableDisplayChangedListener.DisplayChangeType)
     */
    @Override
    public void renderableDisplayChanged(IDisplayPane pane,
            IRenderableDisplay newRenderableDisplay, DisplayChangeType type) {
        if (type == DisplayChangeType.ADD) {
            XyPaneManager pm = getPaneManager();
            if (pm.isInsetMapDisplay(newRenderableDisplay)) {
                IDisplayPane insetMap = pm.getInsetMapForPane(pane);
                if (insetMap != null) {
                    IRenderableDisplay display = InsetMapUtil
                            .loadInsetMap(newRenderableDisplay);
                    insetMap.setRenderableDisplay(display);
                    display.refresh();
                }
            }

            if (newRenderableDisplay instanceof AbstractXyRenderableDisplay) {
                String title = ((AbstractXyRenderableDisplay) newRenderableDisplay)
                        .getTabTitle();
                if (title != null && "".equals(title) == false) {
                    setTabTitle(title);
                }
            }
        }
    }

    @Override
    protected String getEditorName() {
        if (name == null) {
            IEditorDescriptor desc = PlatformUI.getWorkbench()
                    .getEditorRegistry().findEditor(getSite().getId());
            if (desc != null) {
                name = desc.getLabel();
            } else {
                name = editorInput != null ? editorInput.getName()
                        : "Unknown Graph Editor";
            }
        }
        return name;
    }

    @Override
    public void setColor(BGColorMode mode, RGB newColor) {
        // Graphs must set color for their pane and their inset pane
        super.setColor(mode, newColor);
        setColor(getPaneManager().getInsetPanes(), newColor);
    }

}
