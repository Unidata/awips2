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
package com.raytheon.viz.hydro.questionabledata;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Action to launch {@link QuestionableBadDataDlg}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 6/27/06                  lvenable    Initial Creation.
 * 02/07/2013   1578        rferrel     Changes for non-blocking questionableDlg.
 * 03/27/2013   1790        rferrel     Bug fix for non-blocking dialogs.
 * 05/06/2016   5483        dgilling    Changes to support updated QuestionableBadDataDlg.
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class QuestionableDataAction extends AbstractHandler {
    private QuestionableBadDataDlg questionableDlg;

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        if (questionableDlg == null || questionableDlg.isDisposed()) {
            Shell shell = HandlerUtil.getActiveShellChecked(arg0);
            questionableDlg = new QuestionableBadDataDlg(shell);
            questionableDlg.addCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    questionableDlg = null;
                }
            });
            questionableDlg.open();
        } else {
            questionableDlg.bringToTop();
        }

        return null;
    }
}
