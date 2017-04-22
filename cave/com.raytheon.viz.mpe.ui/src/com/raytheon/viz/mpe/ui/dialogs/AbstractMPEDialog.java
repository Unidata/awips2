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
package com.raytheon.viz.mpe.ui.dialogs;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.IPerspectiveSpecificDialog;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;

/**
 * MPE Perspective specific dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 28, 2010            mschenke     Initial creation
 * Apr 20, 2016 5541       dgilling     Fix issues with hide/restore and perspective switching.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class AbstractMPEDialog extends Dialog implements
        IPerspectiveSpecificDialog {

    protected Shell shell;

    private Point lastLocation;

    private boolean wasVisible = true;

    public AbstractMPEDialog(Shell parent) {
        super(parent);
        AbstractVizPerspectiveManager mgr = VizPerspectiveListener
                .getCurrentPerspectiveManager();
        if (mgr != null) {
            mgr.addPerspectiveDialog(this);
        }
    }

    protected void removePerspectiveListener() {
        AbstractVizPerspectiveManager mgr = VizPerspectiveListener
                .getCurrentPerspectiveManager();
        if (mgr != null) {
            mgr.removePespectiveDialog(this);
        }
    }

    @Override
    public final boolean close() {
        if (shell != null && shell.isDisposed() == false) {
            shell.dispose();
        }
        return true;
    }

    /**
     * @return the shell
     */
    public final Shell getShell() {
        return shell;
    }

    @Override
    public final void hide() {
        hide(false);
    }

    @Override
    public final void hide(boolean isPerspectiveSwitch) {
        Shell shell = getShell();
        if ((shell != null) && (!shell.isDisposed())) {
            wasVisible = shell.isVisible() && isPerspectiveSwitch;
            lastLocation = shell.getLocation();
            shell.setVisible(false);
        }
    }

    @Override
    public final void restore() {
        restore(false);
    }

    @Override
    public final void restore(boolean isPerspectiveSwitch) {
        Shell shell = getShell();
        if ((shell != null) && (!shell.isDisposed())) {
            if ((isPerspectiveSwitch && wasVisible) || (!isPerspectiveSwitch)) {
                shell.setVisible(true);
                if (lastLocation != null) {
                    shell.setLocation(lastLocation);
                } else {
                    lastLocation = shell.getLocation();
                }
            }
        }
    }

}
