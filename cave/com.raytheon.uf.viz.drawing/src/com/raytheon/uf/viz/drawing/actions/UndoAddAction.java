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
package com.raytheon.uf.viz.drawing.actions;

import com.raytheon.uf.viz.drawing.DrawingLayer;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 23, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class UndoAddAction extends AbstractRightClickAction {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        ((DrawingLayer) getSelectedRsc()).undoAdd();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#isEnabled()
     */
    @Override
    public boolean isEnabled() {
        if (!((DrawingLayer) getSelectedRsc()).getWireframeShapes().isEmpty()) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public String getText() {
        return "Undo Draw";
    }

    @Override
    public boolean isHidden() {
        if (getSelectedRsc() instanceof DrawingLayer) {
            return false;
        } else {
            return true;
        }
    }
}
