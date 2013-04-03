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
package com.raytheon.uf.viz.gisdatastore.actions;

import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.gisdatastore.rsc.DataStoreResource;
import com.raytheon.uf.viz.gisdatastore.ui.SelectAttributeDialog;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 13, 2012            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SetSampleAttributeAction extends AbstractRightClickAction {

    /**
     * 
     */
    public SetSampleAttributeAction() {
        super("Select Sample Attribute...", AS_PUSH_BUTTON);
    }

    @Override
    public boolean isHidden() {
        if (this.getSelectedRsc() instanceof DataStoreResource) {
            final DataStoreResource rsc = (DataStoreResource) this
                    .getSelectedRsc();
            return rsc.getProperties().isMapLayer();
        }
        return true;
    }

    @Override
    public void run() {
        if (this.getSelectedRsc() instanceof DataStoreResource) {
            final DataStoreResource rsc = (DataStoreResource) this
                    .getSelectedRsc();

            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            SelectAttributeDialog dlg = new SelectAttributeDialog(shell, rsc);
            dlg.setBlockOnOpen(false);
            dlg.open();
        }
    }
}
