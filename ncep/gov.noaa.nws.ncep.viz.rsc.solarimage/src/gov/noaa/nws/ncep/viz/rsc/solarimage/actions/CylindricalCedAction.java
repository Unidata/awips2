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
 * Enable/Disable Cylindrical Display for SolarImageResource
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 * 04/02/2013    958         qzhou     Initial Creation.
 * 12/17/2013    958         qzhou     Fixed cylind and latlon context menu leftover problem
 * 01/22/2014    1046        qzhou     Move enum to MenuConstants
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1
 */

public class CylindricalCedAction extends AbstractRightClickAction implements
        IMenuCreator {

    private Menu menu;

    /**
     * Default constructor.
     */
    public CylindricalCedAction() {
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
            String currentCylind = getTopMostSelectedResource().getCapability(
                    CylindricalCedCapability.class).getCylind();

            for (String proj : MenuConstants.projections) {
                boolean selected = proj.equals(currentCylind);
                found |= selected;
                ActionContributionItem actionItem = new ActionContributionItem(
                        new ChangeCylindricalCedAction(rsc, proj, d, selected));
                actionItem.fill(menu, -1);

            }

            if (!found) {
                ActionContributionItem actionItem = new ActionContributionItem(
                        new ChangeCylindricalCedAction(rsc, currentCylind, d,
                                true));
                actionItem.fill(menu, -1);
            }
        }
    }

    private class ChangeCylindricalCedAction extends Action {

        private String cylind;

        AbstractVizResource<?, ?> resource = null;

        public ChangeCylindricalCedAction(AbstractVizResource<?, ?> resource,
                String cylind, Display d, boolean selected) {
            super(cylind);
            this.resource = resource;
            this.cylind = cylind;
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

            if (cylind != null) {

                if (this.getText().equals(MenuConstants.projections[0])) {
                    ((SolarImageResource) resource).setCylindrical(0);

                } else if (this.getText().equals(MenuConstants.projections[1])) {
                    ((SolarImageResource) resource).setCylindrical(1);

                    if (!getTopMostSelectedResource()
                            .getCapability(CarrLatLonCapability.class)
                            .getInterval()
                            .equals(MenuConstants.latLonIntervals[0])) {

                        ((SolarImageResource) resource).setLatLonOverlay(true);
                        ((SolarImageResource) resource).isCarrington = false;
                        getTopMostSelectedResource().getCapability(
                                StonyLatLonCapability.class).setInterval(
                                getTopMostSelectedResource().getCapability(
                                        CarrLatLonCapability.class)
                                        .getInterval());

                        getTopMostSelectedResource().getCapability(
                                CarrLatLonCapability.class).setInterval(
                                MenuConstants.latLonIntervals[0]);

                    }
                } else if (this.getText().equals(MenuConstants.projections[2])) {
                    ((SolarImageResource) resource).setCylindrical(2);

                    if (!getTopMostSelectedResource()
                            .getCapability(StonyLatLonCapability.class)
                            .getInterval()
                            .equals(MenuConstants.latLonIntervals[0])) {

                        ((SolarImageResource) resource).setLatLonOverlay(true);
                        ((SolarImageResource) resource).isCarrington = true;
                        getTopMostSelectedResource().getCapability(
                                CarrLatLonCapability.class).setInterval(
                                getTopMostSelectedResource().getCapability(
                                        StonyLatLonCapability.class)
                                        .getInterval());

                        getTopMostSelectedResource().getCapability(
                                StonyLatLonCapability.class).setInterval(
                                MenuConstants.latLonIntervals[0]);

                    }
                }

                getTopMostSelectedResource().getCapability(
                        CylindricalCedCapability.class).setCylindrical(cylind);

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
        return "Cylindrical Display";
    }

}
