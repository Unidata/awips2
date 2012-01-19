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
package com.raytheon.viz.ui.actions;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.time.TimeMatchingJob;
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
public class FramesHandler extends AbstractHandler implements IElementUpdater {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        IWorkbenchWindow window = HandlerUtil.getActiveWorkbenchWindow(event);
        if (window == null) {
            window = VizWorkbenchManager.getInstance().getCurrentWindow();
        }
        IDisplayPaneContainer container = EditorUtil
                .getActiveVizContainer(window);
        VizGlobalsManager mgr = VizGlobalsManager.getInstance(window);
        try {
            int frameCount = Integer
                    .parseInt((event.getParameter("frameCount")));
            boolean dontUpdatePanes = Boolean.parseBoolean(event
                    .getParameter("dontUpdatePanes"));
            if (!dontUpdatePanes) {
                if (container != null) {
                    IDisplayPane[] panes = container.getDisplayPanes();
                    // first change the frames for everyone
                    for (IDisplayPane pane : panes) {
                        IDescriptor descriptor = pane.getDescriptor();
                        if (descriptor != null) {
                            descriptor.setNumberOfFrames(frameCount);
                        }
                    }
                    // then redo time amtching for everyone.
                    for (IDisplayPane pane : panes) {
                        IDescriptor descriptor = pane.getDescriptor();
                        if (descriptor != null) {
                            TimeMatchingJob.scheduleTimeMatch(descriptor);
                        }
                    }
                }
            }
            mgr.updateChange(VizConstants.FRAMES_ID, frameCount);
        } catch (NumberFormatException e) {
            throw (new ExecutionException("Invalid frame count", e));
        }

        return null;
    }

    @SuppressWarnings("rawtypes")
    public void updateElement(UIElement element, Map parameters) {
        String text = parameters.get("frameCount").toString();
        try {
            int frameCount = Integer.parseInt(text);
            text = Integer.toString(frameCount);
        } catch (NumberFormatException e) {
            text += " <Invalid>";
        }
        element.setText(text);
    }

}
