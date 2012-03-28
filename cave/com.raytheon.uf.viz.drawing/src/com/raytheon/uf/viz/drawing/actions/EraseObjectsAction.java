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

import org.eclipse.jface.action.IAction;

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

public class EraseObjectsAction extends AbstractRightClickAction {

    /**
     * 
     */
    public EraseObjectsAction() {
        super("Eraser", IAction.AS_CHECK_BOX);
    }

    @Override
    public void run() {
        ((DrawingLayer) getSelectedRsc())
                .setErase(!((DrawingLayer) getSelectedRsc()).isErase());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.cmenu.AbstractRightClickAction#isHidden()
     */
    @Override
    public boolean isHidden() {
        if (true) {
            return true;
        }
        if (getSelectedRsc() instanceof DrawingLayer) {
            return false;
        } else {
            return true;
        }
    }
}
