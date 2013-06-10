package gov.noaa.nws.ncep.viz.rsc.solarimage.actions;

import gov.noaa.nws.ncep.viz.rsc.solarimage.rsc.SolarImageResource;

import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * 
 * Enable/Disable Sampling for SolarImageResource
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 * Feb 13, 2013  958         sgurung     Initial Creation.
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1
 */

public class EnableDisableSamplingAction extends AbstractRightClickAction {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {    	    	  
    	AbstractVizResource<?, ?> rsc = getSelectedRsc();
    	if (rsc instanceof SolarImageResource) {
    		boolean isEnabled = ((SolarImageResource) rsc).isSampling();      		
    		((SolarImageResource) rsc).setSampling(!isEnabled);
    		((SolarImageResource) rsc).issueRefresh();
            this.setChecked(!isEnabled);
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
        if (rsc instanceof SolarImageResource) {
    		boolean isEnabled = ((SolarImageResource) rsc).isSampling();      		
    		this.setChecked(isEnabled);
    	} 
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return "Sample";
    }

}

