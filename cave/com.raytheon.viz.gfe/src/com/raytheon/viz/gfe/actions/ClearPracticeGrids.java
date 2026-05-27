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

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.common.dataplugin.gfe.request.ClearPracticeGridsRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;

/**
 * Handler class for Clear Practice forecast grids.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 10, 2010  4475     randerso  Initial creation
 * May 02, 2013  1949     rjpeter   Change ServerResponse return type.
 * Nov 18, 2015  5129     dgilling  Support new IFPClient.
 * Dec 10, 2020  8302     randerso  Changed to use new ClearPracticeGridsRequest
 *
 * </pre>
 *
 * @author randerso
 */
public class ClearPracticeGrids extends AbstractHandler {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ClearPracticeGrids.class);

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        DataManager dataManager = DataManagerUIFactory.getCurrentInstance();
        if (dataManager == null) {
            return null;
        }

        ClearPracticeGridsRequest request = new ClearPracticeGridsRequest();
        ServerResponse<?> sr = dataManager.getClient().makeRequest(request);

        if (sr.isOkay()) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Prac_Fcst database has been cleared.");
        } else {
            statusHandler.error(sr.message());
        }

        return null;
    }
}
