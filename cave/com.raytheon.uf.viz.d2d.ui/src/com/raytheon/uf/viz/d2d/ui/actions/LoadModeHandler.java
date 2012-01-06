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
 * 
 */
package com.raytheon.uf.viz.d2d.ui.actions;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.d2d.core.time.D2DTimeMatcher;
import com.raytheon.uf.viz.d2d.core.time.LoadMode;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.VizWorkbenchManager;

/**
 * Handles a set load mode command
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2007            randerso    Initial Creation.
 * 
 * </pre>
 * 
 * @author randerso
 * 
 */
public class LoadModeHandler extends AbstractHandler implements IElementUpdater {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        IWorkbenchWindow window = HandlerUtil.getActiveWorkbenchWindow(arg0);
        if (window == null) {
            window = VizWorkbenchManager.getInstance().getCurrentWindow();
        }
        IDisplayPaneContainer editor = EditorUtil.getActiveVizContainer(window);
        VizGlobalsManager mgr = VizGlobalsManager.getInstance(window);
        try {
            LoadMode loadMode = LoadMode.valueOf(arg0.getParameter("loadMode"));

            if (editor != null) {
                IDisplayPane[] displayPanes = editor.getDisplayPanes();
                for (IDisplayPane pane : displayPanes) {
                    ((D2DTimeMatcher) pane.getRenderableDisplay()
                            .getDescriptor().getTimeMatcher())
                            .setLoadMode(loadMode);
                }
                mgr.updateUI(editor);
            } else {
                mgr.updateChange(VizConstants.LOADMODE_ID, loadMode);
            }

            final ICommandService service = (ICommandService) PlatformUI
                    .getWorkbench().getService(ICommandService.class);

            service.refreshElements(
                    "com.raytheon.viz.ui.actions.loadmodebutton", null);

        } catch (IllegalArgumentException e) {
            throw (new ExecutionException("Invalid load mode", e));
        }

        return null;
    }

    @SuppressWarnings("unchecked")
    public void updateElement(UIElement element, Map parameters) {
        String text = parameters.get("loadMode").toString();
        try {
            LoadMode loadMode = LoadMode.valueOf(text);
            text = loadMode.getLabel();
        } catch (IllegalArgumentException e) {
            text += " <Invalid>";
        }
        element.setText(text);
    }

}
