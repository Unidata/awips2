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
package com.raytheon.uf.viz.d2d.ui.map.actions;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.color.IBackgroundColorChangedListener.BGColorMode;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.dialogs.colordialog.BackgroundColorDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 1, 2009            mschenke     Initial creation
 * Oct 16, 2012 1229       rferrel     Changes for non-blocking BackgroundColorDialog.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class SetBackgroundColorAction extends AbstractRightClickAction {

    private static Map<IDisplayPaneContainer, BackgroundColorDialog> dialogMap = new HashMap<IDisplayPaneContainer, BackgroundColorDialog>();

    private BGColorMode mode = null;

    public void setMode(BGColorMode mode) {
        this.mode = mode;
    }

    public SetBackgroundColorAction() {
        super("Set Background Color...");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        BackgroundColorDialog dialog = dialogMap.get(container);
        if (dialog == null || dialog.getShell() == null || dialog.isDisposed()) {
            dialog = new BackgroundColorDialog(Display.getCurrent()
                    .getActiveShell(), container, mode);
            dialogMap.put(container, dialog);
            dialog.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    dialogMap.remove(container);
                }
            });
            dialog.open();
        } else {
            dialog.bringToTop();
        }
    }
}
