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
package com.raytheon.uf.viz.radarapps.rps;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

/**
 * An abstract handler for showing the RPS List Editor dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ??? ??, 20??            ????????     Initial creation
 * Mar 28, 2016  #5511     dgilling     Code cleanup.
 * 
 * </pre>
 * 
 * @author ????????
 * @version 1.0
 */
public class RPSListEditorCommandHandler extends AbstractHandler {

    protected RpsListEditorDlg dialog;

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        if (dialog == null) {
            Shell parent = HandlerUtil.getActiveShellChecked(event);
            dialog = new RpsListEditorDlg(parent, new RpsListRequestContainer());
            dialog.addListener(SWT.Dispose, new Listener() {

                @Override
                public void handleEvent(Event event) {
                    dialog = null;
                }
            });
        }
        dialog.open();

        return null;
    }
}