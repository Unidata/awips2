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
/**
 * 
 */
package com.raytheon.viz.hydro.timeseries;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.viz.hydrocommon.HydroDisplayManager;

/**
 * Action for unimplemented features. To be used temporarily until final
 * behavior is implemented.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 6/27/06                  lvenable    Initial Creation.
 * 02/05/2013   1578        rferrel     Changes for non-blocking singleton TimeSeriesDlg.
 * 6/8/2013     15980       wkwock      Fix selected station not update
 * 07/10/2013   2166        mpduff      Select the site but don't display the graph.
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class TimeSeriesAction extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        TimeSeriesDlg dlg = TimeSeriesDlg.getInstance();
        dlg.updateAndOpen(HydroDisplayManager.getInstance().getCurrentLid(),
                false);

    	return null;
    }
}
