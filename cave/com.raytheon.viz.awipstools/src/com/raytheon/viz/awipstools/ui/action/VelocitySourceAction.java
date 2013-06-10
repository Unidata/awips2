package com.raytheon.viz.awipstools.ui.action;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.awipstools.ui.layer.VRShearLayer;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Displays a menu of resources that can be used with the VR Shear tool.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- ------------ --------------------------
 * 5/2/2013     DR 14587   D. Friedman  Initial revision
 * 
 * </pre>
 */
public class VelocitySourceAction extends AbstractRightClickAction implements IMenuCreator {
    private Menu menu;

    /**
     * Default constructor.
     */
    public VelocitySourceAction() {
        super(Action.AS_DROP_DOWN_MENU);
        setMenuCreator(this);
    }

    @Override
    public IMenuCreator getMenuCreator() {
        return this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return "Velocity Source";
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
        VRShearLayer layer = getVRShearLayer();
        if (layer != null) {
            AbstractVizResource<?, ?> selectedResource = layer.getSelectedVelocitySource();
            ActionContributionItem actionItem;

            actionItem = new ActionContributionItem(
                    new SetVelocitySourceAction(null, selectedResource == null));
            actionItem.fill(menu, -1);

            for (AbstractVizResource<?, ?> resource : layer.getEligibleResources(false)) {
                actionItem = new ActionContributionItem(
                        new SetVelocitySourceAction(resource, resource == selectedResource));
                actionItem.fill(menu, -1);
            }
        }
    }

    private class SetVelocitySourceAction extends Action {
        private AbstractVizResource<?, ?> resource;

        public SetVelocitySourceAction(AbstractVizResource<?, ?> resource, boolean selected) {
            super(resource != null ? resource.getName() : "(default)", Action.AS_RADIO_BUTTON);
            setChecked(selected);
            this.resource = resource;
        }

        @Override
        public void run() {
            VRShearLayer layer = getVRShearLayer();
            if (layer != null) {
                layer.setSelectedVelocitySource(resource);
            }
        }
    }

    private VRShearLayer getVRShearLayer() {
        if (getSelectedRsc() instanceof VRShearLayer) {
            return (VRShearLayer) getSelectedRsc();
        } else {
            return null;
        }
    }

}
