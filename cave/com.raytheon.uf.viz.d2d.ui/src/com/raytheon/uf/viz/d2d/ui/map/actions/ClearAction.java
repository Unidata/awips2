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

package com.raytheon.uf.viz.d2d.ui.map.actions;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IEditorPart;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.maps.actions.NewMapEditor;
import com.raytheon.uf.viz.core.maps.display.VizMapEditor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Action to reset or close an editor. With no editors, it will create a new
 * map. With a map editor, it will reset. With any other editor, it will close
 * the editor.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Mar 10, 2007             chammack    Initial Creation.
 * Oct 23, 2007             ebabin      Added pan as default action, so clear works
 *                                       properly.
 * Jul 10, 2008             chammack    Properly clear the resource list on the old 
 *                                       descriptor to facilitate better cleanup
 * Oct 21, 2008   #1450     randerso    Fixed to support multipane editors
 * May 24, 2015              mjames@ucar Moved UnloadAllProductsAction here to replace map reload
 * Mar 16, 2016    5023     njensen     Fix clear reseting editor name to Map
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ClearAction extends AbstractHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ClearAction.class);

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        IDisplayPaneContainer cont = EditorUtil.getActiveVizContainer();

    	IDisplayPane[] panes = cont.getDisplayPanes();

        for (IDisplayPane displayPane : panes) {
            IDescriptor desc = displayPane.getDescriptor();
            ResourceList rl = desc.getResourceList();

            Iterator<ResourcePair> iterator = rl.iterator();
            List<AbstractVizResource<?, ?>> rscsToRemove = new ArrayList<AbstractVizResource<?, ?>>();

            while (iterator.hasNext()) {
                ResourcePair rp = iterator.next();
                if (!rp.getProperties().isMapLayer()
                        && !rp.getProperties().isSystemResource()) {
                    rscsToRemove.add(rp.getResource());
        }
            }

            for (AbstractVizResource<?, ?> rsc : rscsToRemove) {
                rl.removeRsc(rsc);
            }
        }
        cont.refresh();

        return null;
    }

    public static void clear(IEditorPart part) {
        if (part instanceof VizMapEditor) {
            VizMapEditor mapEditor = (VizMapEditor) part;
            mapEditor.setPartName(mapEditor.getEditorInput().getName());
            mapEditor.clear();
        } else if (part instanceof AbstractEditor) {
            // AbstractEditor asks the user if they are sure if we pass in
            // save=true, if the user clicked clear than they must be sure so
            // pass in save=false.
            part.getSite().getPage().closeEditor(part, false);
        } else {
            // Give other editors a chance to save.
            part.getSite().getPage().closeEditor(part, true);
        }

        if (EditorUtil.getActiveEditor() == null) {
            try {
                new NewMapEditor().execute(null);
            } catch (ExecutionException e) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Error loading default map", e);
            }
        }
        VizGlobalsManager.getCurrentInstance().updateUI(
                EditorUtil.getActiveVizContainer());
    }
}
