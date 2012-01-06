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
package com.raytheon.viz.mpe.ui.rsc;

import java.util.List;

import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.mpe.ui.dialogs.AddPseudoGageDialog;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.input.EditableManager;

/**
 * Resource for adding a pseudo gage, is resource for editable exclusiveness
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 8, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class AddPseudoGageResource extends AbstractMPEInputResource {

    /**
     * @param resourceData
     * @param loadProperties
     */
    public AddPseudoGageResource(GenericResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.rsc.AbstractMPEInputResource#handleMouseUp(int,
     * int, int)
     */
    @Override
    protected boolean handleMouseUp(int x, int y, int mouseButton) {
        if (mouseButton == 1) {
            EditableManager.makeEditable(this, false);
            MPEGageResource rsc = null;
            List<MPEGageResource> rscs = descriptor.getResourceList()
                    .getResourcesByTypeAsType(MPEGageResource.class);
            for (MPEGageResource mgr : rscs) {
                rsc = mgr;
                break;
            }
            AddPseudoGageDialog pdlg = new AddPseudoGageDialog(
                    VizWorkbenchManager.getInstance().getCurrentWindow()
                            .getShell(), getResourceContainer().translateClick(
                            x, y), rsc);
            pdlg.open();
            return true;
        }
        return false;
    }
}
