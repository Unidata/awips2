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
package com.raytheon.viz.hydro.gagedisplay;

import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Action for clicking on the contextual menu for color legend.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 02, 2008  1194      mpduff      Initial creation
 * Feb 07, 2013 1578       rferrel     Changes for non-blocking GageLegend.
 * Feb 27, 2013 1790       rferrel     Bug fix for non-blocking dialogs.
 * Jan 15, 2015 5054       randerso    Remove unnecessary new Shell
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GageColorLegendAction extends AbstractRightClickAction {

    /** Text for the action. */
    private final String displayText = "Display Gage Color Legend";

    /** The dialog with the legend in it. */
    private GageLegend dialog;

    /**
     * Returns the text for the action.
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return displayText;
    }

    /**
     * Launches the Gage Color Legend Dialog.
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        if (dialog == null || dialog.isDisposed()) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            dialog = new GageLegend(shell);
            dialog.open();
        } else {
            dialog.bringToTop();
        }
    }
}
