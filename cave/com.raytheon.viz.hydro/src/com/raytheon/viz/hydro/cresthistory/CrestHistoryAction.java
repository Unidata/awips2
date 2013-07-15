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
package com.raytheon.viz.hydro.cresthistory;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.cresthistory.CrestHistoryDlg;
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
 * 20Nov2008      1628      dhladky     Updated.
 * 11Jul2013      2088      rferrel     Changes for non-blocking CrestHistoryDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class CrestHistoryAction extends AbstractHandler {
    /** Allow single instance of dialog per station. */
    private final Map<String, CrestHistoryDlg> crestHistoryDlgMap = new HashMap<String, CrestHistoryDlg>();

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        // get the name for this gage...gage has to be selected.
        HydroDisplayManager manager = HydroDisplayManager.getInstance();
        if (manager.isCurrentLidSelected(shell)) {
            String lid = manager.getCurrentLid();
            CrestHistoryDlg crestHistoryDlg = crestHistoryDlgMap.get(lid);

            if (crestHistoryDlg == null || crestHistoryDlg.isDisposed()) {
                String name = manager.getCurrentData().getName();
                StringBuilder displayString = new StringBuilder(" - ");
                displayString.append(lid);

                if (name != null && !("").equals(name)) {
                    displayString.append(" - ").append(name);
                }

                crestHistoryDlg = new CrestHistoryDlg(shell,
                        manager.getCurrentLid(), displayString.toString(),
                        false);
                crestHistoryDlg.setCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        if (returnValue instanceof String) {
                            String lid = returnValue.toString();
                            crestHistoryDlgMap.remove(lid);
                        }
                    }
                });
                crestHistoryDlg.open();
                crestHistoryDlgMap.put(lid, crestHistoryDlg);
            } else {
                crestHistoryDlg.bringToTop();
            }
        }

        return null;
    }

}
