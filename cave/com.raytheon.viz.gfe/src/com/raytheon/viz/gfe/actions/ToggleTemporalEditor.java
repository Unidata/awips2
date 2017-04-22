package com.raytheon.viz.gfe.actions;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.gridmanager.IGridManager;
import com.raytheon.viz.gfe.gridmanager.IGridManager.GridManagerState;

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

/**
 * Toggles the temporal editor
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 30, 2009 2159       rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class ToggleTemporalEditor extends AbstractHandler implements
        IElementUpdater {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        DataManager.getCurrentInstance().getGridManager()
                .toggleTemporalEditor();
        ICommandService service = (ICommandService) PlatformUI.getWorkbench()
                .getService(ICommandService.class);
        service.refreshElements(
                "com.raytheon.viz.gfe.actions.toggleTemporalEditor", null);
        return null;
    }

    @Override
    public void updateElement(UIElement element, Map parameters) {
        DataManager dm = DataManager.getCurrentInstance();

        if (dm != null) {
            IGridManager gm = dm.getGridManager();

            if (gm != null) {
                GridManagerState gmTEState = gm.getState();
                ImageDescriptor imageDesc = null;

                if (GridManagerState.GridManager.equals(gmTEState)) {
                    imageDesc = AbstractUIPlugin.imageDescriptorFromPlugin(
                            Activator.PLUGIN_ID, "icons/gmTe0.gif");
                } else {
                    imageDesc = AbstractUIPlugin.imageDescriptorFromPlugin(
                            Activator.PLUGIN_ID, "icons/gmTe1.gif");
                }

                element.setIcon(imageDesc);
            }
        }
    }
}
