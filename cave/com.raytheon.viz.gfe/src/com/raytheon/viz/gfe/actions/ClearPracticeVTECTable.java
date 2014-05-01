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
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2010            wkwock     Initial creation
 *
 * </pre>
 *
 * @author wkwock
 * @version 1.0	
 */
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.core.DataManager;

public class ClearPracticeVTECTable extends AbstractHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(ClearPracticeVTECTable.class);

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		try {
			DataManager dm = DataManager.getCurrentInstance();
			if (dm != null) {
				dm.getClient().clearPracticeTable(dm.getSiteID());

				statusHandler.handle(Priority.SIGNIFICANT,
						"Practice VTEC table has been cleared.");
			} else {
				statusHandler.handle(Priority.PROBLEM,
						"Failed to clear practice VTEC table");
			}

		} catch (VizException e) {
			statusHandler.handle(
							Priority.PROBLEM,
							"Unexpected exception while attempting to clear practice VTEC table",
							e);
		}

		return null;
	}

}
