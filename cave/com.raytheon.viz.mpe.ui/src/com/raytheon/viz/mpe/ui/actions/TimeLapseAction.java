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

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.timelapse.TimeLapseDlg;
import com.raytheon.viz.mpe.ui.rsc.TimeLapseResource;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Action class for MPE's Time Lapse Function.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2009            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class TimeLapseAction extends AbstractHandler {
    private static AbstractVizResource<?, ?> prevRsc = null;

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        MPEDisplayManager dman = MPEDisplayManager.getCurrent();
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container != null) {
            LoopProperties loopProps = container.getLoopProperties();
            int frameRate = Integer.parseInt(AppsDefaults.getInstance()
                    .getToken("hydroview_mpe_timelapse", "1000"));
            loopProps.setFwdFrameTime(frameRate);

            if (!dman.isTimeLapseMode()) {
                prevRsc = dman.getDisplayedResource();
            }

            String selection = event.getParameter("Hour");
            if (selection.equalsIgnoreCase("6")) {
                dman.setTimeLapseHours(6);
                dman.setTimeLapseMode(true);
                loopProps.setLooping(true);
                dman.displayTimeLapse();
            } else if (selection.equalsIgnoreCase("12")) {
                dman.setTimeLapseHours(12);
                dman.setTimeLapseMode(true);
                loopProps.setLooping(true);
                dman.displayTimeLapse();
            } else if (selection.equalsIgnoreCase("24")) {
                dman.setTimeLapseHours(24);
                dman.setTimeLapseMode(true);
                loopProps.setLooping(true);
                dman.displayTimeLapse();
            } else if (selection.equalsIgnoreCase("O")) {
                TimeLapseDlg tld = new TimeLapseDlg(shell);
                tld.open();
                dman.setTimeLapseMode(true);
                loopProps.setLooping(true);
                dman.displayTimeLapse();
            } else if (selection.equalsIgnoreCase("E")) {
                dman.setTimeLapseHours(0);
                dman.setTimeLapseMode(false);
                loopProps.setLooping(false);
                IRenderableDisplay display = MPEDisplayManager.getCurrent()
                        .getRenderableDisplay();
                IDescriptor descriptor = display.getDescriptor();
                TimeLapseResource timeLapseRsc = (TimeLapseResource) dman
                        .getDisplayedResource();
                if (descriptor.getResourceList().containsRsc(timeLapseRsc)) {
                    descriptor.getResourceList().removeRsc(timeLapseRsc);
                }
                if (timeLapseRsc != null) {
                    timeLapseRsc.dispose();
                }
                dman.setDisplayedResource(prevRsc);
                display.getContainer().refresh();
            }
        }

        return null;
    }
}
