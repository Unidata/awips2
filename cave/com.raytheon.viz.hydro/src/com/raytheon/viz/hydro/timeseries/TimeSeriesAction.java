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
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.hydro.resource.MultiPointResource;
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
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class TimeSeriesAction extends AbstractHandler {
    private TimeSeriesDlg dlg = null;
    
	@Override
	public Object execute(ExecutionEvent arg0) throws ExecutionException {
        AbstractVizResource<?,?> rsc = HydroDisplayManager.getInstance().getDisplayedResource();
        if (rsc instanceof MultiPointResource) {
            dlg = ((MultiPointResource) rsc).getTs();
        }
        if ((dlg != null) && (dlg.isDisposed() == false)) {
            dlg.setFocus();
        } else {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            dlg = new TimeSeriesDlg(shell);
            dlg.open();
        }
        
		return null;
	}
}
