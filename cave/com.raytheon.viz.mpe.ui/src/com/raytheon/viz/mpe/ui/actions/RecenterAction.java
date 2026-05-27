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
package com.raytheon.viz.mpe.ui.actions;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.input.InputAdapter;

/**
 * Recenters to a point selected on the map
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class RecenterAction extends AbstractHandler {

    private class RecenterHandler extends InputAdapter {

        IDisplayPaneContainer container;

        private Shell shell;

        private Cursor prev, hand;

        private RecenterHandler(IDisplayPaneContainer container) {
            this.container = container;
            shell = VizWorkbenchManager.getInstance().getCurrentWindow()
                    .getShell();
            container.registerMouseHandler(this);
        }

        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {
            return true;
        }

        @Override
        public boolean handleMouseDownMove(int x, int y, int mouseButton) {
            return true;
        }

        @Override
        public boolean handleMouseMove(int x, int y) {
            if (hand == null) {
                hand = shell.getDisplay().getSystemCursor(SWT.CURSOR_HAND);
                prev = shell.getCursor();
                shell.setCursor(hand);
            }
            return true;
        }

        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            for (IDisplayPane pane : container.getDisplayPanes()) {
                pane.getRenderableDisplay()
                        .getView()
                        .shiftExtent(
                                new double[] { pane.getBounds().width / 2,
                                        pane.getBounds().height / 2 },
                                new double[] { x, y }, pane.getTarget());
            }
            container.unregisterMouseHandler(this);
            map.remove(container);
            container.refresh();
            handleMouseExit(null);
            return true;
        }

        @Override
        public boolean handleMouseExit(Event event) {
            shell.setCursor(prev);
            hand = null;
            return false;
        }
    }

    private Map<IDisplayPaneContainer, IInputHandler> map = new HashMap<IDisplayPaneContainer, IInputHandler>();

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container != null) {
            IInputHandler handler = map.get(container);
            if (handler == null) {
                handler = new RecenterHandler(container);
                map.put(container, handler);
            }
        }
        return null;
    }

}
