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

package com.raytheon.viz.hydro.impactstatement;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.impactstatement.ImpactStatementDlg;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Action for Impact Statement Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 6/27/08                  lvenable    Initial creation.
 * 10/20/2008   1617        grichard    Support impact statement.
 * 07/15/2013   2088        rferrel     Changes for non-blocking ImpactStatementDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class ImpactStatementAction extends AbstractHandler {
    /**
     * Allow one instance per station.
     */
    Map<String, ImpactStatementDlg> impactStatmentDlgMap = new HashMap<String, ImpactStatementDlg>();

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        HydroDisplayManager manager = HydroDisplayManager.getInstance();
        if (manager.isCurrentLidSelected(shell)) {
            String lid = manager.getCurrentLid();
            ImpactStatementDlg impactStatmentDlg = impactStatmentDlgMap
                    .get(lid);

            if (impactStatmentDlg == null || impactStatmentDlg.isDisposed()) {
                String name = manager.getCurrentData().getName();

                StringBuilder displayString = new StringBuilder(" - ");
                displayString.append(lid);

                if (name != null && name.length() > 0) {
                    displayString.append(" - ").append(name);
                }

                impactStatmentDlg = new ImpactStatementDlg(shell,
                        displayString.toString(), lid, false);
                impactStatmentDlg.setCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        if (returnValue instanceof String) {
                            String lid = returnValue.toString();
                            impactStatmentDlgMap.remove(lid);
                        }
                    }
                });
                impactStatmentDlg.open();
                impactStatmentDlgMap.put(lid, impactStatmentDlg);
            } else {
                impactStatmentDlg.bringToTop();
            }
        }

        return null;
    }
}
