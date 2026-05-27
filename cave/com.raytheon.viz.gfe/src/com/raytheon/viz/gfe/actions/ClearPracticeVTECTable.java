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

package com.raytheon.viz.gfe.actions;

/**
 * Action handler to clear practice active table.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2010            wkwock     Initial creation
 * Nov 17, 2015  #5129     dgilling   Support new IFPClient.
 *
 * </pre>
 *
 * @author wkwock
 * @version 1.0	
 */
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;

public class ClearPracticeVTECTable extends AbstractHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ClearPracticeVTECTable.class);

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        DataManager dm = DataManagerUIFactory.getCurrentInstance();
        if (dm != null) {
            ServerResponse<?> sr = dm.getClient().clearVTECTable(
                    ActiveTableMode.PRACTICE);
            if (!sr.isOkay()) {
                statusHandler
                        .error(String.format(
                                "Failed to clear practice VTEC table: %s",
                                sr.message()));
            } else {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Practice VTEC table has been cleared.");
            }
        } else {
            statusHandler.handle(Priority.PROBLEM,
                    "Failed to clear practice VTEC table");
        }

        return null;
    }
}
