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

package com.raytheon.viz.ui.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.viz.ui.color.IBackgroundColorChangedListener.BGColorMode;
import com.raytheon.viz.ui.dialogs.colordialog.BackgroundColorDialog;

/**
 * Opens the singleton instance of the Set Time Dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 7,2008              Dan Fitch   Initial Creation
 * Oct 16, 2012 1229       rferrel     Changed for non-blocking BackgroundColorDialog.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class SetBackgroundColor extends AbstractHandler {

    private BackgroundColorDialog dialog = null;

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        if (dialog == null || dialog.getShell() == null || dialog.isDisposed()) {
            dialog = new BackgroundColorDialog(
                    HandlerUtil.getActiveShell(event), null, BGColorMode.GLOBAL);
            dialog.open();
        } else {
            dialog.bringToTop();
        }
        return null;
    }
}
