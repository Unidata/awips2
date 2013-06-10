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
 * Enable/Disable Carrington LatLon Overlay for SolarImageResource
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

public class CarrLatLonAction extends AbstractRightClickAction 
	implements  IMenuCreator {
	
	private Menu menu;	
	 
	private static String[] latLonIntervals = {"No Overlay", "10", "15",  "30", "45", "60"};

    /**
     * Default constructor.
     */
    public CarrLatLonAction() {
        super(Action.AS_DROP_DOWN_MENU);
        this.setChecked(false);
        setMenuCreator(this);
    }

    @Override
    public IMenuCreator getMenuCreator() {
    	this.setChecked(false);
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
	        		CarrLatLonCapability.class).getInterval();
	        
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
            		((SolarImageResource) resource).isCarrington = false;
        		}
        		else {
        			((SolarImageResource) resource).setLatLonOverlay(true);
        			((SolarImageResource) resource).isCarrington = true;
        		}

        		getTopMostSelectedResource().getCapability(CarrLatLonCapability.class).setInterval(interval);
        		getTopMostSelectedResource().getCapability(StonyLatLonCapability.class).setInterval("No Overlay");	
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
        return "Carrington LatLon Overlay";
    }

}

