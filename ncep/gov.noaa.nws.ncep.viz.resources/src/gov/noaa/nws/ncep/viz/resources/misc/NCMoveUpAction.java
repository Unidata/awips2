/*
 * gov.noaa.nws.ncep.viz.resources.misc
 * 
 * 13 August 2014
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.resources.misc;

import gov.noaa.nws.ncep.viz.resources.groupresource.GroupResource;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.ResourceList.MoveOperation;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * 
 * Move up a resource in the legend list. It works for resources in a group
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
public class NCMoveUpAction extends AbstractRightClickAction {

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
                        getTopMostSelectedResource(), MoveOperation.Up);
                break;
            } else if (pair.getResource() instanceof GroupResource
                    && ((GroupResource) pair.getResource()).getResourceList()
                            .contains(selectedRsc)) {
                ((GroupResource) pair.getResource()).getResourceList()
                        .moveResource(selectedRsc.getResource(),
                                MoveOperation.Up);
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
        return "Move Up";
    }

}
