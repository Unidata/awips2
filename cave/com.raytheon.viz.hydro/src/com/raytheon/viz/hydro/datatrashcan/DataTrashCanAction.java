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
package com.raytheon.viz.hydro.datatrashcan;

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
 * 6/27/06                  lvenable    Initial Creation.
 * 2/06/2013    1578        rferrel     Change for non-blocking DataTrashCanDlg.
 * 2/27/2013    1790        rferrel     Bug fix for non-blocking dialogs.
 * 4/15/2016    5483        dgilling    Support changes to DataTrashCanDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class DataTrashCanAction extends AbstractHandler {

    /** The dialog to display */
    private DataTrashCanDlg dataTrashCanDlg;

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        if (dataTrashCanDlg == null || dataTrashCanDlg.isDisposed()) {
            Shell shell = HandlerUtil.getActiveShellChecked(arg0);
            dataTrashCanDlg = new DataTrashCanDlg(shell);
            dataTrashCanDlg.addCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    dataTrashCanDlg = null;
                }
            });
            dataTrashCanDlg.open();
        } else {
            dataTrashCanDlg.bringToTop();
        }

        return null;
    }
}
