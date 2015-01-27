package gov.noaa.nws.ncep.viz.resources.misc;

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

import gov.noaa.nws.ncep.viz.resources.groupresource.GroupResource;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.ResourceList.MoveOperation;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * 
 * Move down a resource in the legend list. It works for resources in a group
 * resource.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/14            ?        B. Yin    Initial Creation.
 * 
 * </pre>
 * 
 * @author byin
 * @version 1
 */
public class NCMoveDownAction extends AbstractRightClickAction {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        IDescriptor desc = getDescriptor();

        for (ResourcePair pair : desc.getResourceList()) {
            if (selectedRsc == pair) {
                desc.getResourceList().moveResource(
                        getTopMostSelectedResource(), MoveOperation.Down);
                break;
            } else if (pair.getResource() instanceof GroupResource
                    && ((GroupResource) pair.getResource()).getResourceList()
                            .contains(selectedRsc)) {
                ((GroupResource) pair.getResource()).getResourceList()
                        .moveResource(selectedRsc.getResource(),
                                MoveOperation.Down);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return "Move Down";
    }

}
