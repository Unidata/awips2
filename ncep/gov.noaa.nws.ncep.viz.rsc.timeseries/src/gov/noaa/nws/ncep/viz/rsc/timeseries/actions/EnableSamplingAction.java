package gov.noaa.nws.ncep.viz.rsc.timeseries.actions;

import gov.noaa.nws.ncep.viz.rsc.timeseries.rsc.GeoMagResource;

import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * 
 * Enable/Disable Sampling for GeoMagResource
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 * 07/07/2014    R4079       qzhou     Initial Creation.
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1
 */

public class EnableSamplingAction extends AbstractRightClickAction {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        AbstractVizResource<?, ?> rsc = getSelectedRsc();
        if (rsc instanceof GeoMagResource) {
            ((GeoMagResource) rsc).reopenSamplingView();

        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.cmenu.AbstractRightClickAction#setSelectedRsc(com
     * .raytheon.uf.viz.core.rsc.AbstractVizResource)
     */
    @Override
    public void setSelectedRsc(ResourcePair selectedRsc) {
        super.setSelectedRsc(selectedRsc);
        AbstractVizResource<?, ?> rsc = getSelectedRsc();
        if (rsc instanceof GeoMagResource) {

        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return "Open Readout";
    }

}
