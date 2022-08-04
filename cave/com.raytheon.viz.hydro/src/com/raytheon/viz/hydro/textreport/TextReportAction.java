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
package com.raytheon.viz.hydro.textreport;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.textreport.TextReportDlg;
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
 * 7/15/2013    2088        rferrel     Changes for non-blocking TextReportDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class TextReportAction extends AbstractHandler {
    private final Map<String, TextReportDlg> dataSourcesDlgMap = new HashMap<String, TextReportDlg>();

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        /* Check for selected lid */
        if (HydroDisplayManager.getInstance().isCurrentLidSelected(shell)) {

            /* If site selected get lid and launch the dialog */
            String lid = HydroDisplayManager.getInstance().getCurrentLid();
            TextReportDlg dataSourcesDlg = dataSourcesDlgMap.get(lid);
            if (dataSourcesDlg == null || dataSourcesDlg.isDisposed()) {
                dataSourcesDlg = new TextReportDlg(shell, lid);
                dataSourcesDlg.addCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        if (returnValue instanceof String) {
                            String lid = returnValue.toString();
                            dataSourcesDlgMap.remove(lid);
                        }
                    }
                });
                dataSourcesDlg.open();
                dataSourcesDlgMap.put(lid, dataSourcesDlg);
            } else {
                dataSourcesDlg.bringToTop();
            }
        }

        return null;
    }
}
