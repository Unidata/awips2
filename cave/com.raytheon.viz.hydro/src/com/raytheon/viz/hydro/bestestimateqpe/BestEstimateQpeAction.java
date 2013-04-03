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
package com.raytheon.viz.hydro.bestestimateqpe;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

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
 * 6/27/06                  randerso    Initial Creation.
 * 8/24/2009    2258        mpduff      Removed not implemented message.
 * 12/05/1212   1363        rferrel     Changes for non-blocking BestEstimateQpeDlg.
 * 
 * </pre>
 * 
 * @author randerso
 * 
 */
public class BestEstimateQpeAction extends AbstractHandler {
    private BestEstimateQpeDlg bestEstimateDlg;

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        if (bestEstimateDlg == null) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            bestEstimateDlg = new BestEstimateQpeDlg(shell);
        }
        bestEstimateDlg.open();

        return null;
    }

}
