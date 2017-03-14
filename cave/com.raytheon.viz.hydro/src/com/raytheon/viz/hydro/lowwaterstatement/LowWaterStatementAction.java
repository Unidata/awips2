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
package com.raytheon.viz.hydro.lowwaterstatement;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.lowwaterstatment.LowWaterStatementDlg;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

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
 * 07/15/2013   2088        rferrel     Changes for non-blocking LowWaterStatementDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class LowWaterStatementAction extends AbstractHandler {
    private Map<String, LowWaterStatementDlg> lowWaterStmntDlgMap = new HashMap<String, LowWaterStatementDlg>();

    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        HydroDisplayManager manager = HydroDisplayManager.getInstance();
        if (manager.isCurrentLidSelected(shell)) {
            String lid = manager.getCurrentLid();
            LowWaterStatementDlg lowWaterStmntDlg = lowWaterStmntDlgMap
                    .get(lid);

            if (lowWaterStmntDlg == null || lowWaterStmntDlg.isDisposed()) {
                String name = manager.getCurrentData().getName();
                StringBuilder displayString = new StringBuilder(" - ");
                displayString.append(lid);

                if (name != null && name.length() > 0) {
                    displayString.append(" - ").append(name);
                }

                lowWaterStmntDlg = new LowWaterStatementDlg(shell,
                        displayString.toString(), false, lid);
                lowWaterStmntDlg.setCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        if (returnValue instanceof String) {
                            String lid = returnValue.toString();
                            lowWaterStmntDlgMap.remove(lid);
                        }
                    }
                });
                lowWaterStmntDlg.open();
                lowWaterStmntDlgMap.put(lid, lowWaterStmntDlg);
            } else {
                lowWaterStmntDlg.bringToTop();
            }
        }

        return null;
    }
}
