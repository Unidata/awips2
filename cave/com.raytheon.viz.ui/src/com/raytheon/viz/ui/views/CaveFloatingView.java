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
package com.raytheon.viz.ui.views;

import java.io.File;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;

import com.raytheon.viz.ui.UiPlugin;

/**
 * Creates the ability to have a dialog that can be minimized and maximized and
 * is separate from the original CAVE window from a view
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public abstract class CaveFloatingView extends ViewPart {

    protected FloatAction floatAction;

    /**
     * Constructs the view
     */
    public CaveFloatingView() {
        super();
    }

    @Override
    public void createPartControl(final Composite parent) {
        createToolbarButton();
    }

    public void setFloating(boolean floating) {
        floatAction.setChecked(floating);
        if (floating) {
            floatAction.setToolTipText("Dock");
        } else {
            floatAction.setToolTipText("Float");
        }
    }

    /**
     * Creates the "Float" toolbar button
     */
    protected void createToolbarButton() {
        IToolBarManager mgr = getViewSite().getActionBars().getToolBarManager();
        floatAction = new FloatAction();
        mgr.add(floatAction);

    }

    private class FloatAction extends Action {

        public FloatAction() {
            super("Float", SWT.TOGGLE);
            setImageDescriptor(UiPlugin.getImageDescriptor("icons"
                    + File.separator + "float.gif"));
        }

        @Override
        public void run() {
            if (isChecked()) {
                CaveWorkbenchPageManager manager = CaveWorkbenchPageManager
                        .getInstance(getSite().getPage());
                manager.floatView(CaveFloatingView.this);
            } else {
                CaveWorkbenchPageManager manager = CaveWorkbenchPageManager
                        .getInstance(getSite().getPage());
                manager.dockView(CaveFloatingView.this);
            }
        };

    }
}
