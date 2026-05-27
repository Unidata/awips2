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
package com.raytheon.viz.ui.tools.looping;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator.FrameChangeMode;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator.FrameChangeOperation;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * CombinedFrameTool
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Dec 26, 2007             chammack    Initial Creation.
 * Mar 06, 2025 2038488     mapeters    Let Time Match Basis frame coordinator determine
 *                                      index for all panes, when possible
 *
 * </pre>
 *
 * @author chammack
 */
public class CombinedFrameTool extends AbstractTool {

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        this.editor = EditorUtil.getActiveVizContainer();
        if (editor != null) {
            String operationStr = arg0.getParameter("operation");
            String modeStr = arg0.getParameter("mode");

            this.editor.getLoopProperties().setLooping(false);
            LoopPropertiesDialog.setLooping(false);

            FrameChangeMode mode = FrameChangeMode.valueOf(modeStr);
            FrameChangeOperation operation = FrameChangeOperation
                    .valueOf(operationStr);
            // Get the Time Match Basis frame coordinator
            IDisplayPane[] mainCanvases = editor.getMainCanvases();
            IFrameCoordinator tmbFrameCoordinator = null;
            for (IDisplayPane canvas : mainCanvases) {
                IDescriptor desc = canvas.getDescriptor();
                AbstractVizResource<?, ?> timeMatchBasis = desc.getTimeMatcher()
                        .getTimeMatchBasis();
                if (timeMatchBasis != null) {
                    tmbFrameCoordinator = timeMatchBasis.getDescriptor()
                            .getFrameCoordinator();
                    break;
                }
            }

            if (tmbFrameCoordinator != null) {
                /*
                 * Let Time Match Basis process the frame change operation, and
                 * then have other frame coordinators update to match it. This
                 * ensures that they stay in sync, particularly when the
                 * descriptors are using different frame coordinator types (e.g.
                 * SAILS vs. default).
                 */
                tmbFrameCoordinator.changeFrame(operation, mode);
                for (IDisplayPane canvas : mainCanvases) {
                    canvas.getDescriptor().getFrameCoordinator()
                            .matchFrameChange(tmbFrameCoordinator);
                }
            } else {
                /*
                 * No Time Match Basis (e.g. not in D2D), just fall back to
                 * having each frame coordinator process the operation
                 * separately
                 */
                for (IDisplayPane canvas : mainCanvases) {
                    canvas.getDescriptor().getFrameCoordinator()
                            .changeFrame(operation, mode);
                }

            }
            editor.refresh();
        }

        return null;
    }
}
