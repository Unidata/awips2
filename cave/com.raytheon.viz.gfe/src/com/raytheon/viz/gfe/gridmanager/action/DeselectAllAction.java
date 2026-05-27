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

import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.actions.ClearSelection;
import com.raytheon.viz.gfe.constants.StatusConstants;

/**
 * Stores information right click menu action to Deselect All.
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

public class DeselectAllAction extends AbstractGridManagerAction {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(DeselectAllAction.class);

    public DeselectAllAction() {
        super("Deselect All");

    }

    @Override
    public void run() {
        ClearSelection tmp = new ClearSelection();
        try {
            tmp.execute(null);
        } catch (ExecutionException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error in DeselectAllAction.", e);
        }
    }

}
