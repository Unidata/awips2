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
package com.raytheon.viz.hydro.stationprofile;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.hydrocommon.HydroDisplayManager;
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
 * 17 Nov 2008     1628     dhladky     Little update.
 * 29 Mar 2013     1790     rferrel     Changes for non-blocking StationProfileDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class StationProfileAction extends AbstractHandler {
    /**
     * Keep track of open dialogs.
     */
    Map<String, StationProfileDlg> dialogMap = new HashMap<String, StationProfileDlg>();

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

        // get the name for this gage...gage has to be selected.
        HydroDisplayManager manager = HydroDisplayManager.getInstance();
        if (manager.isCurrentLidSelected(shell)) {
            String currentLid = manager.getCurrentLid();
            StationProfileDlg stationProfileDlg = dialogMap.get(currentLid);
            if (stationProfileDlg == null) {
                stationProfileDlg = new StationProfileDlg(shell, currentLid);
                stationProfileDlg.setCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        if (returnValue instanceof String) {
                            String currentLid = returnValue.toString();
                            dialogMap.remove(currentLid);
                        }
                    }
                });

                Object o = stationProfileDlg.open();
                if (o != null) {
                    // Dialog found data and properly opened.
                    dialogMap.put(currentLid, stationProfileDlg);
                }
            } else {
                stationProfileDlg.setStation(currentLid);
                stationProfileDlg.bringToTop();
            }
        }
        // throws up dialog

        return null;
    }

}
