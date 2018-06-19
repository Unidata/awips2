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

import java.util.Date;

import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * Stores information right click menu action CopyAction.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/17/2008              dfitch      Initial creation.
 * 
 * 
 * </pre>
 * 
 * @author dfitch
 * @version 1.0
 */

public class CopyAction extends AbstractGridManagerAction {

    Parm parm;

    Date copyAt;

    public CopyAction(Parm parm, Date copyAt) {
        super("Copy Grid");

        this.parm = parm;

        this.copyAt = copyAt;

    }

    @Override
    public void run() {
        // Copy the Grid
        DataManager.getCurrentInstance().getParmOp().copyGrid(parm, copyAt);
    }
}