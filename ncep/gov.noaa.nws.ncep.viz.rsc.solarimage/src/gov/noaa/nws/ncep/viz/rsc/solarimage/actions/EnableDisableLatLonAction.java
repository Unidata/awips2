package gov.noaa.nws.ncep.viz.rsc.solarimage.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;

import gov.noaa.nws.ncep.viz.rsc.solarimage.rsc.SolarImageResource;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * 
 * Enable/Disable LatLon Overlay for SolarImageResource
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 * Feb 21, 2013  958         qzhou     Initial Creation.
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1
 */

public class EnableDisableLatLonAction extends AbstractRightClickAction 
	implements  IMenuCreator {
	
	private Menu menu;	
	 
	private static String[] latLonIntervals = {"No Overlay", "10", "15",  "30", "45", "60"};

    /**
     * Default constructor.
     */
    public EnableDisableLatLonAction() {
        super(Action.AS_DROP_DOWN_MENU);
        setMenuCreator(this);
    }

    @Override
    public IMenuCreator getMenuCreator() {
        return this;
    }

    
    @Override
    public void dispose() {
        if (menu != null) {
            menu.dispose();
            menu = null;
        }
    }

    @Override
    public Menu getMenu(Control parent) {
        if (menu != null) {
            menu.dispose();
        }
        menu = new Menu(parent);
        fillMenu(menu, parent.getDisplay());
        return menu;
    }

    @Override
    public Menu getMenu(Menu parent) {
        if (menu != null) {
            menu.dispose();
        }
        menu = new Menu(parent);
        fillMenu(menu, parent.getDisplay());
        return menu;
    }

    public void fillMenu(Menu menu, Display d) {
    	
    	AbstractVizResource<?, ?> rsc = getSelectedRsc();
    	if (rsc instanceof SolarImageResource) {
	    	
	        boolean found = false;
	        String currentInterval = getTopMostSelectedResource().getCapability(
	                LatLonIntervalCapability.class).getInterval();
	        
	        for (String intvl : latLonIntervals) {
	            boolean selected = intvl.equals(currentInterval);
	            found |= selected;
	            ActionContributionItem actionItem = new ActionContributionItem(
	                    new ChangeLatLonIntervalInternalAction(rsc, intvl, d, selected));
	           actionItem.fill(menu, -1);
	           
	        }
	
	        if (!found) {
	            ActionContributionItem actionItem = new ActionContributionItem(
	                    new ChangeLatLonIntervalInternalAction(rsc, currentInterval, d, true));
	            actionItem.fill(menu, -1);	            
	        }
    	}
    }

    private class ChangeLatLonIntervalInternalAction extends Action {
        private String interval;
        AbstractVizResource<?, ?> resource = null;

        public ChangeLatLonIntervalInternalAction(AbstractVizResource<?, ?> resource, String intvl, Display d, boolean selected) {
            super(intvl);
            this.resource = resource;
            this.interval = intvl;           
        	this.setChecked(false);
                  
            if (selected) {
               this.setChecked(true);
            } 
                       
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#getText()
         */
        @Override
        public String getText() {
            return super.getText();
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
        	
        	if (interval != null) {
            		
        		if (this.getText().equals("No Overlay")) {
            		((SolarImageResource) resource).setLatLonOverlay(false);
        		}
        		else {
        			((SolarImageResource) resource).setLatLonOverlay(true);
        		}

        		getTopMostSelectedResource().getCapability(LatLonIntervalCapability.class).setInterval(interval);
        			
        	}  
            getContainer().refresh();
        }
    } 

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return "Stonyhurst LatLon Overlay";
    }

}

