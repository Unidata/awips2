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
package com.raytheon.uf.viz.d2d.ui.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.d2d.ui.dialogs.procedures.ProcedureComm;
import com.raytheon.uf.viz.d2d.ui.dialogs.procedures.ProcedureComm.BundlePair;
import com.raytheon.viz.ui.HistoryList;
import com.raytheon.viz.ui.actions.SaveBundle;

/**
 * CopyOutAWIPSProcedure
 * 
 * Copy out current display to any open procedures
 * 
 * <pre>
 *   
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Sep 13, 2007             chammack    Initial Creation.
 *   
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class CopyOutAWIPSProcedure extends AbstractHandler {

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
	 */
	@Override
	public Object execute(ExecutionEvent arg0) throws ExecutionException {

		try {
			Bundle b = SaveBundle.extractCurrentBundle();
			HistoryList.getInstance().refreshLatestBundle(b);
			String sb = HistoryList.getInstance().getBundleAsString(0, false);
			BundlePair bp = new BundlePair();
			bp.name = (HistoryList.getInstance().getLabels()[0]);
			bp.xml = sb;

			ProcedureComm.getInstance().copyOut(bp, this);
		} catch (VizException e) {
			throw new ExecutionException("Error creating bundle from display",
					e);
		}

		return null;
	}

}
