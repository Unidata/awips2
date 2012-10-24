/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package com.raytheon.uf.viz.points.ui.action;

import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.points.ui.dialog.PointsMgrDialog;
import com.raytheon.uf.viz.points.ui.layer.PointsToolLayer;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Aciton class for displaying the points manager dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 27, 2012 875        rferrel     Initial creation
 * 
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class PointEditAction extends AbstractRightClickAction {

    private PointsMgrDialog dialog;

    @Override
    public boolean isHidden() {
        if (getSelectedRsc() instanceof PointsToolLayer) {
            return false;
        }
        return true;
    }

    @Override
    public String getText() {
        return "Edit Points...";
    }

    @Override
    public void run() {
        if (dialog == null || dialog.getShell() == null || dialog.isDisposed()) {
            dialog = new PointsMgrDialog(Display.getCurrent().getShells()[0],
                    (PointsToolLayer) getSelectedRsc());
            dialog.setBlockOnOpen(false);
            dialog.open();
        } else {
            dialog.bringToTop();
        }
    }
}
