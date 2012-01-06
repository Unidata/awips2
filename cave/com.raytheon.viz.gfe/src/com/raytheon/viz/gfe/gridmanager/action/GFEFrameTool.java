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
package com.raytheon.viz.gfe.gridmanager.action;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.drawables.IDescriptor.FrameChangeOperation;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * Implements the frame tool events for GFE
 * 
 * GFE only defines single step frame operations in forward and reverse, the
 * rest of the operations are no-op.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/26/2008              chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class GFEFrameTool extends AbstractTool {
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
        if (this.editor == null) {
            return null;
        }

        this.editor.getLoopProperties().setLooping(false);

        String operationStr = arg0.getParameter("operation");
        FrameChangeOperation operation = FrameChangeOperation
                .valueOf(operationStr);

        DataManager dm = DataManager.getCurrentInstance();

        switch (operation) {
        case PREVIOUS:
            dm.getGridManager().previousSelectedGrid();
            break;
        case NEXT:
            dm.getGridManager().nextSelectedGrid();
            break;
        case FIRST:
            dm.getGridManager().firstSelectedGrid();
            break;
        case LAST:
            dm.getGridManager().lastSelectedGrid();
            break;
        }

        editor.refresh();
        dm.getGridManager().redraw();

        return null;

    }

}
