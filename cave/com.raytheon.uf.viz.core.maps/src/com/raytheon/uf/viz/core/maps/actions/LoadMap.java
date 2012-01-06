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
package com.raytheon.uf.viz.core.maps.actions;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.maps.MapManager;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * 
 * Load Map Handler
 * 
 * Loads a map from the Maps menu
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 12, 2009            randerso    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class LoadMap extends AbstractHandler implements IElementUpdater {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(final ExecutionEvent arg0) throws ExecutionException {

        if (arg0.getCommand() == null) {
            return null;
        }

        IDisplayPaneContainer cont = EditorUtil.getActiveVizContainer();
        if (cont != null) {
            for (IDisplayPane pane : getSelectedPanes(cont)) {
                IDescriptor descriptor = pane.getDescriptor();
                if (descriptor instanceof IMapDescriptor) {
                    MapManager.getInstance((IMapDescriptor) descriptor)
                            .toggleMap(arg0.getParameter("mapName"),
                                    arg0.getParameter("mapPath"), cont);
                }
            }
        }

        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.commands.IElementUpdater#updateElement(org.eclipse.ui.
     * menus.UIElement, java.util.Map)
     */
    @SuppressWarnings("unchecked")
    public void updateElement(UIElement element, Map parameters) {
        IDisplayPaneContainer cont = EditorUtil.getActiveVizContainer();
        if (cont != null) {
            for (IDisplayPane pane : getSelectedPanes(cont)) {
                IDescriptor descriptor = pane.getDescriptor();
                if (descriptor instanceof IMapDescriptor) {
                    element.setChecked(MapManager.getInstance(
                            (IMapDescriptor) descriptor).isMapLoaded(
                            (String) parameters.get("mapName")));
                }
            }
        }
    }

    protected IDisplayPane[] getSelectedPanes(IDisplayPaneContainer editor) {
        IDisplayPane[] displayPanes = editor.getDisplayPanes();

        if (editor instanceof IMultiPaneEditor) {
            IDisplayPane selected = ((IMultiPaneEditor) editor)
                    .getSelectedPane(IMultiPaneEditor.LOAD_ACTION);
            if (selected != null) {
                displayPanes = new IDisplayPane[] { selected };
            }
        }
        return displayPanes;
    }

}
