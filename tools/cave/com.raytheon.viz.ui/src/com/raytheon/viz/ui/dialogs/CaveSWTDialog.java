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
package com.raytheon.viz.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.IPerspectiveSpecificDialog;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;

/**
 * CaveSWTDialog.
 * 
 * Extends CaveSWTDialogBase and allows for perspective dependent dialogs which
 * requires the workbench to be running. Always use this class over
 * CaveSWTDialogBase unless you have a standalone component that uses dialogs
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ----------	----------	-----------	--------------------------
 * 12/20/07     561         Dan Fitch    Initial Creation.
 * </pre>
 * 
 * @author Dan Fitch
 * @version 1
 */
public abstract class CaveSWTDialog extends CaveSWTDialogBase implements
        IPerspectiveSpecificDialog {

    protected AbstractVizPerspectiveManager perspectiveManager;

    /**
     * Construct default cave dialog
     * 
     * @param parentShell
     */
    protected CaveSWTDialog(Shell parentShell) {
        this(parentShell, SWT.DIALOG_TRIM, CAVE.NONE);
    }

    /**
     * Construct dialog with parent shell and specific swt style
     * 
     * @param parentShell
     * @param swtStyle
     */
    protected CaveSWTDialog(Shell parentShell, int swtStyle) {
        this(parentShell, swtStyle, CAVE.NONE);
    }

    /**
     * Construct dialog with parent shell and swt style and cave style
     * 
     * @param parentShell
     * @param style
     */
    protected CaveSWTDialog(Shell parentShell, int style, int caveStyle) {
        super(parentShell, style, caveStyle);
        perspectiveManager = VizPerspectiveListener
                .getCurrentPerspectiveManager();
        if (!hasAttribute(CAVE.PERSPECTIVE_INDEPENDENT)) {
            if (perspectiveManager != null) {
                perspectiveManager.addPerspectiveDialog(this);
            }
        }
    }

    @Override
    protected void preOpened() {
        super.preOpened();

        if (doesNotHaveAttribute(CAVE.PERSPECTIVE_INDEPENDENT)) {
            // add the dispose listener for perspective switching
            shell.addDisposeListener(new DisposeListener() {
                @Override
                public void widgetDisposed(DisposeEvent e) {
                    if (perspectiveManager != null) {
                        perspectiveManager
                                .removePespectiveDialog(CaveSWTDialog.this);
                    }
                }
            });
        }
    }

    /**
     * Show the dialog and set the location of the shell.
     */
    @Override
    public final void restore() {
        if (shell != null && shell.isDisposed() == false) {
            shell.setLocation(lastLocation);
            if (shell.isVisible() != wasVisible) {
                shell.setVisible(wasVisible);
            }
        }
    }

    /**
     * Hide the dialog and save the current location of the shell.
     */
    @Override
    public final void hide() {
        if (shell != null && shell.isDisposed() == false) {
            wasVisible = shell.isVisible();
            lastLocation = shell.getLocation();
            shell.setVisible(false);
        }
    }

    public AbstractVizPerspectiveManager getPerspectiveManager() {
        return perspectiveManager;
    }
}
