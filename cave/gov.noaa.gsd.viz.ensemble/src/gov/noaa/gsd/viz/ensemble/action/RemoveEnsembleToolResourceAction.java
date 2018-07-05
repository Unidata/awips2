package gov.noaa.gsd.viz.ensemble.action;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

import gov.noaa.gsd.viz.ensemble.control.EnsembleTool;

/**
 * This is a specialized Unload action for resources in the Ensemble Tool. The
 * action is only displayed as a menu item in the context-sensitive pop-up menu
 * associated with the right-click on a visible widget (e.g. legend, treeitem,
 * etc.). The action will only be visible-to and interactible-with the user when
 * the active ensemble tool layer contains the resource which has focus.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Mar 22, 2017  19325      polster    Initial Creation.
 * 
 * </pre>
 * 
 * @author polster
 * @version 1
 */

public class RemoveEnsembleToolResourceAction extends AbstractRightClickAction {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        if (EnsembleTool.getInstance() != null) {
            AbstractVizResource<?, ?> rsc = getSelectedRsc();
            ResourceList list = EnsembleTool.getInstance()
                    .getActiveResourceList();
            if (list != null) {
                rsc.unload(list);
            }
        }
    }

    @Override
    public String getText() {
        return "Unload";
    }

    /**
     * Only display this action if the resource is controlled by and contained
     * in the active ensemble tool layer and the resource is okay to unload.
     */
    @Override
    public boolean isHidden() {
        boolean hide = true;
        if (EnsembleTool.getInstance() != null) {
            if (EnsembleTool.getInstance()
                    .activeToolLayerContains(getSelectedRsc())) {
                hide = !getSelectedRsc().okToUnload();
            }
        }
        return hide;
    }

}
