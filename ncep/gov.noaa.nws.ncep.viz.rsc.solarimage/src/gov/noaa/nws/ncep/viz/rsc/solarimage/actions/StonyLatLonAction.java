package gov.noaa.nws.ncep.viz.rsc.solarimage.actions;

import gov.noaa.nws.ncep.viz.rsc.solarimage.rsc.SolarImageResource;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * 
 * Enable/Disable stonyhurst LatLon Overlay for SolarImageResource
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 * Feb 21, 2013  958         qzhou     Initial Creation.
 * 12/17/2013    958         qzhou     Fixed cylind and latlon context menu leftover problem
 * 01/22/2014    1046        qzhou     Move enum to MenuConstants
 * </pre>
 * 
 * @author qzhou
 * @version 1
 */

public class StonyLatLonAction extends AbstractRightClickAction implements
        IMenuCreator {

    private Menu menu;

    /**
     * Default constructor.
     */
    public StonyLatLonAction() {
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
            String currentInterval = getTopMostSelectedResource()
                    .getCapability(StonyLatLonCapability.class).getInterval();

            for (String intvl : MenuConstants.latLonIntervals) {
                boolean selected = intvl.equals(currentInterval);
                found |= selected;
                ActionContributionItem actionItem = new ActionContributionItem(
                        new ChangeLatLonIntervalInternalAction(rsc, intvl, d,
                                selected));
                actionItem.fill(menu, -1);

            }

            if (!found) {
                ActionContributionItem actionItem = new ActionContributionItem(
                        new ChangeLatLonIntervalInternalAction(rsc,
                                currentInterval, d, true));
                actionItem.fill(menu, -1);
            }
        }
    }

    private class ChangeLatLonIntervalInternalAction extends Action {
        private String interval;

        AbstractVizResource<?, ?> resource = null;

        public ChangeLatLonIntervalInternalAction(
                AbstractVizResource<?, ?> resource, String intvl, Display d,
                boolean selected) {
            super(intvl);
            this.resource = resource;
            this.interval = intvl;
            this.setChecked(false);

            if (selected) {
                this.setChecked(true);
            }
            // CarrLatLonAciton
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

                if (this.getText().equals(MenuConstants.latLonIntervals[0])) {
                    if (getTopMostSelectedResource()
                            .getCapability(CarrLatLonCapability.class)
                            .getInterval()
                            .equals(MenuConstants.latLonIntervals[0])) {
                        ((SolarImageResource) resource).setLatLonOverlay(false);
                        ((SolarImageResource) resource).isCarrington = false;
                        getTopMostSelectedResource().getCapability(
                                StonyLatLonCapability.class).setInterval(
                                interval);
                    }
                } else {
                    if (!getTopMostSelectedResource()
                            .getCapability(CylindricalCedCapability.class)
                            .getCylind().equals(MenuConstants.projections[2])) {
                        ((SolarImageResource) resource).setLatLonOverlay(true);
                        ((SolarImageResource) resource).isCarrington = false;
                        getTopMostSelectedResource().getCapability(
                                StonyLatLonCapability.class).setInterval(
                                interval);

                        getTopMostSelectedResource().getCapability(
                                CarrLatLonCapability.class).setInterval(
                                MenuConstants.latLonIntervals[0]);
                    }
                }

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
