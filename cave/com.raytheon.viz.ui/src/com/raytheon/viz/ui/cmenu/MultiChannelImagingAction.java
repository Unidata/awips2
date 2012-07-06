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
package com.raytheon.viz.ui.cmenu;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IDisposeListener;
import com.raytheon.uf.viz.core.rsc.capabilities.MultiChannelCapability;
import com.raytheon.viz.ui.dialogs.MultiChannelDialog;

/**
 * Right click action that opens a dialog for modifying resources with
 * MultiChannelCapability
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 3, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class MultiChannelImagingAction extends AbstractRightClickAction {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return "Channel Options...";
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        final AbstractVizResource<?, ?> rsc = getSelectedRsc();
        MultiChannelCapability cap = rsc
                .getCapability(MultiChannelCapability.class);
        final MultiChannelDialog dialog = new MultiChannelDialog(shell, rsc,
                cap);
        final IDisposeListener listener = new IDisposeListener() {
            @Override
            public void disposed(AbstractVizResource<?, ?> rsc) {
                dialog.close();
            }
        };
        rsc.registerListener(listener);
        dialog.addListener(SWT.Dispose, new Listener() {
            @Override
            public void handleEvent(Event event) {
                rsc.unregisterListener(listener);
            }
        });
        dialog.open();
    }
}
