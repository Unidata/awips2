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
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * CombinedFrameTool
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Dec 26, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class CombinedFrameTool extends AbstractTool {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.tools.AbstractTool#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        this.editor = EditorUtil.getActiveVizContainer();
        if (editor != null) {
            String operationStr = arg0.getParameter("operation");
            String modeStr = arg0.getParameter("mode");

            this.editor.getLoopProperties().setLooping(false);
            LoopPropertiesDialog.setLooping(false);

            IFrameCoordinator.FrameChangeMode mode = IFrameCoordinator.FrameChangeMode
                    .valueOf(modeStr);
            IFrameCoordinator.FrameChangeOperation operation = IFrameCoordinator.FrameChangeOperation
                    .valueOf(operationStr);

            IDisplayPane[] panes = editor.getDisplayPanes();
            for (IDisplayPane pane : panes) {
                IDescriptor desc = (IDescriptor) pane.getRenderableDisplay()
                        .getDescriptor();

                desc.getFrameCoordinator().changeFrame(operation, mode);
            }
            editor.refresh();
        }

        return null;

    }
}
