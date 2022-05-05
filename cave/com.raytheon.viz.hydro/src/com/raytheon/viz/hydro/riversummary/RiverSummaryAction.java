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
package com.raytheon.viz.hydro.riversummary;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

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
 * 03/15/2013   1790        rferrel     Changes for non-blocking RiverSummaryDlg.
 * 04/08/2016   5483        dgilling    Code cleanup.
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class RiverSummaryAction extends AbstractHandler {
    private CaveJFACEDialog riverSummaryDlg;

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        if ((riverSummaryDlg == null) || (!riverSummaryDlg.isOpen())) {
            Shell shell = HandlerUtil.getActiveShellChecked(arg0);
            riverSummaryDlg = new RiverSummaryDlg(shell);
            riverSummaryDlg.open();
        } else {
            riverSummaryDlg.bringToTop();
        }

        return null;
    }
}
