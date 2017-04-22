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
package com.raytheon.viz.hydro.stationreporting;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

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
 * 06/27/2006               lvenable    Initial Creation.
 * 04/16/2013   1790        rferrel     Changes for non-blocking stationReportingDlg.
 * 05/04/2016   5483        dgilling    Updates for updated 
 *                                      StationReportingStatusDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class StationReportingAction extends AbstractHandler {
    private StationReportingStatusDlg stationReportingDlg;

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        if (stationReportingDlg == null || stationReportingDlg.isDisposed()) {
            Shell shell = HandlerUtil.getActiveShellChecked(arg0);
            stationReportingDlg = new StationReportingStatusDlg(shell);
            stationReportingDlg.addCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    stationReportingDlg = null;
                }
            });
            stationReportingDlg.open();
        } else {
            stationReportingDlg.bringToTop();
        }

        return null;
    }
}
