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

import com.raytheon.viz.gfe.core.DataManager;

/**
 * Stores information right click menu action UndoAction.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/04/2008              dfitch      Initial creation.
 * 05/21/2009   #2159      rjpeter     Removed parm dependency.
 * </pre>
 * 
 * @author dfitch
 * @version 1.0
 */

public class UndoAction extends AbstractGridManagerAction {
    /**
     * The undo action.
     * 
     * @param text
     *            Title input.
     */
    public UndoAction() {
        super("Undo");
    }

    /**
     * Runs the undo action on the parmOp
     */
    @Override
    public void run() {
        DataManager dm = DataManager.getCurrentInstance();

        dm.getParmOp().undo();
        dm.getGridManager().redraw();
    }

}