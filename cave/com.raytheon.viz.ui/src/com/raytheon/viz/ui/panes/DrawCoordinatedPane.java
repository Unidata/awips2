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
package com.raytheon.viz.ui.panes;

import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.viz.ui.VizWorkbenchManager;

/**
 * Wrapper for an IDisplayPaneContainer and its associated GLDisplay panes which
 * is used by DrawCoordinatedJob to manage the IDisplayPaneContainer draw
 * executions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 5, 2009             bgonzale    Initial creation
 * Aug 2, 2013        2190 mschenke    Made displayPanes Set a LinkedHashSet to ensure
 *                                     rendered in order added
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class DrawCoordinatedPane {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DrawCoordinatedPane.class);

    private IDisplayPaneContainer container;

    private IWorkbenchWindow containerWindow;

    private VizWorkbenchManager manager = VizWorkbenchManager.getInstance();

    /**
     * Multiple gl display panes can be registered with each container.
     */
    private Set<VizDisplayPane> displayPanes = new LinkedHashSet<VizDisplayPane>();

    private boolean editor = false;

    private boolean dead = false;

    public DrawCoordinatedPane(IDisplayPaneContainer container) {
        this.container = container;
        editor = container instanceof IEditorPart;
        if (container instanceof IWorkbenchPart) {
            containerWindow = ((IWorkbenchPart) container).getSite()
                    .getWorkbenchWindow();
        } else {
            containerWindow = manager.getCurrentWindow();
        }
    }

    public synchronized void add(VizDisplayPane pane) {
        displayPanes.add(pane);
    }

    public synchronized void remove(VizDisplayPane pane) {
        displayPanes.remove(pane);
    }

    public synchronized Set<VizDisplayPane> getPanes() {
        return displayPanes;
    }

    public synchronized boolean needsRefresh() {
        if (dead) {
            return false;
        }
        try {
            if (editor == true
                    && !manager.isVisibleEditor(containerWindow, container)) {
                return false;
            }

            boolean hasMultipleFrames = false;
            if (container.getDisplayPanes() != null) {
                for (IDisplayPane pane : container.getDisplayPanes()) {
                    if (pane != null
                            && pane.getDescriptor() != null
                            && pane.getDescriptor().getFramesInfo()
                                    .getFrameCount() > 1) {
                        hasMultipleFrames = true;
                        break;
                    }
                }
            }

            LoopProperties loopProperties = container.getLoopProperties();

            loopProperties.setCurrentDrawTime(System.currentTimeMillis());
            for (IDisplayPane pane : displayPanes) {
                IRenderableDisplay disp = pane.getRenderableDisplay();
                if (disp == null) {
                    continue;
                }

                // Handle looping frame changing
                IDescriptor desc = disp.getDescriptor();
                desc.getFrameCoordinator().changeFrame(loopProperties);
            }

            // check all of the panes registered with the managed pane.
            for (IDisplayPane pane : displayPanes) {
                IRenderableDisplay disp = pane.getRenderableDisplay();
                if (disp == null) {
                    continue;
                }

                if ((disp.isBlinking() && disp.isBlinkStateChanged())
                        || pane.getTarget().isNeedsRefresh()
                        || (loopProperties.isShouldDraw() && hasMultipleFrames)) {
                    return true;
                }
            }

            return false;
        } catch (Exception e) {
            handleException(e);
            return false;
        }
    }

    public synchronized void draw(boolean actualDraw) {
        if (dead) {
            return;
        }
        try {
            long t0 = System.nanoTime();
            LoopProperties loopProperties = container.getLoopProperties();

            for (VizDisplayPane pane : displayPanes) {
                // no need to actually draw if we are invisible but we still
                // want to draw for things like looping and zooming
                pane.draw((actualDraw || loopProperties.isShouldDraw())
                        && pane.isVisible());
            }

            if (actualDraw || loopProperties.isShouldDraw()) {
                for (VizDisplayPane pane : displayPanes) {
                    if (pane.isVisible()) {
                        pane.drawEnd();
                    }
                }
            }

            // System.out.println("Time to draw = "
            // + ((System.nanoTime() - t0) / 1000000.0) + "ms");
        } catch (Exception e) {
            handleException(e);
            return;
        }
    }

    private void handleException(Exception e) {
        // TODO it might be nice to attempt clearing the pane here, although
        // then we need some complex mechanism to determine if errors are
        // occurring after clear to avoid infinite error looping
        dead = true;
        statusHandler.handle(Priority.CRITICAL,
                "A display pane has been disabled due to an unexpected error.",
                e);
        return;
    }
}
