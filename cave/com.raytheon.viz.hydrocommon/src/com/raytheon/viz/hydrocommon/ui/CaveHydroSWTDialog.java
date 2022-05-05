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
package com.raytheon.viz.hydrocommon.ui;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * An abstract dialog with common elements, LID, TS and PE for Hydro dialogs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 24, 2008            mpduff      Initial creation
 * Feb 05, 2013 1578       rferrel     Made dialog non-blocking.
 * Jan 26, 2016 5054       randerso    Allow dialog to be parented to display
 * Jan 29, 2016 5289       tgurney     Add missing minimize button in trim.
 * Feb 16, 2016 5289       tgurney     Fix conflict with #5054 fix
 * Mar 17, 2016 5483       randerso    Added constructors to allow setting of SWT style
 * Sep 21, 2018 7379       mduff       Support PDC Refactor.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public abstract class CaveHydroSWTDialog extends CaveSWTDialog {
    /**
     * Currently selected lid.
     */
    protected String currentLid = null;

    /**
     * Currently selected type source.
     */
    protected String currentTs = null;

    /**
     * Currently selected physical element.
     */
    protected String currentPe = null;

    /**
     * Non-blocking Constructor.
     * 
     * @param parentShell
     */
    protected CaveHydroSWTDialog(Shell parentShell) {
        this(parentShell, CAVE.NONE);
    }

    /**
     * Construct to specify cave style and make it non-blocking.
     * 
     * @param parentShell
     * @param caveStyle
     */
    protected CaveHydroSWTDialog(Shell parentShell, int caveStyle) {
        this(parentShell, SWT.NONE, caveStyle | CAVE.DO_NOT_BLOCK);
    }

    /**
     * Construct to specify cave style and make it non-blocking.
     * 
     * @param parentShell
     * @param swtStyle
     * @param caveStyle
     */
    protected CaveHydroSWTDialog(Shell parentShell, int swtStyle,
            int caveStyle) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.MIN | swtStyle,
                caveStyle | CAVE.DO_NOT_BLOCK);
    }

    /**
     * Construct top level dialog specifying cave style and make it
     * non-blocking.
     * 
     * @param display
     * @param caveStyle
     */
    protected CaveHydroSWTDialog(Display display, int caveStyle) {
        this(display, SWT.NONE, caveStyle | CAVE.DO_NOT_BLOCK);
    }

    /**
     * Construct top level dialog specifying cave style and make it
     * non-blocking.
     * 
     * @param display
     * @param swtStyle
     * @param caveStyle
     */
    protected CaveHydroSWTDialog(Display display, int swtStyle, int caveStyle) {
        super(display, SWT.DIALOG_TRIM | swtStyle,
                caveStyle | CAVE.DO_NOT_BLOCK);
    }

    /**
     * Determine if there is a currently selected LID.
     * 
     * @return true if LID selected otherwise false
     */
    protected boolean isCurrentLidSelected() {
        HydroDisplayManager displayManager = HydroDisplayManager.getInstance();

        currentLid = displayManager.getCurrentLid();

        if (currentLid == null) {
            MessageDialog.openError(getParent(), "Invalid Selection",
                    "You must select a site first");
            return false;
        }

        return true;
    }

    /**
     * Get the currently selected lid.
     * 
     * @return The selected lid, or null if no selection has been made
     */
    protected String getCurrentLid() {
        return currentLid;
    }

    /**
     * The currently selected type source.
     * 
     * @return TS
     */
    protected String getTs() {
        return currentTs;
    }

    /**
     * The currently selected physical element.
     * 
     * @return PE
     */
    protected String getPe() {
        return currentPe;
    }
}
