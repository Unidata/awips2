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

package com.raytheon.uf.viz.d2d.ui.actions;

import java.util.Arrays;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.d2d.core.map.D2DMapRenderableDisplay;
import com.raytheon.uf.viz.d2d.ui.Activator;
import com.raytheon.uf.viz.d2d.ui.dialogs.ImageBlinkDialog;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * 
 * Enable blinking on a resource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *   
 * Date          Ticket#   Engineer    Description
 * ------------- --------  ----------- --------------------------
 * Oct 15, 2007            chammack    Initial Creation.
 * Jan 09, 2014  2647      bsteffen    Get properties directly from resource.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class BlinkToggleAction extends AbstractRightClickAction implements
        IMenuCreator {

    private static final int MAX_SIZE = 15;

    public static final String NO_BLINK = "No Blink";

    public static final float NO_BLINK_VALUE = 0.0f;

    private enum InternalMode {
        IMAGE, IMAGE_BLENDED, MAP, OTHER;
    }

    private InternalMode mode = InternalMode.MAP;

    private Menu menu = null;

    private static float[] blinkRates;

    static {
        blinkRates = Activator.getDefault().getPreferenceStore()
                .getFloatArray("blinkRate");
        Arrays.sort(blinkRates);
    }

    public BlinkToggleAction() {
    }

    public BlinkToggleAction(IDisplayPaneContainer container) {
        super(IAction.AS_DROP_DOWN_MENU);
        setContainer(container);
    }

    @Override
    public int getStyle() {
        switch (mode) {
        case IMAGE: {
            return IAction.AS_PUSH_BUTTON;
        }
        case IMAGE_BLENDED:
        case MAP: {
            return IAction.AS_DROP_DOWN_MENU;
        }
        case OTHER:
        default: {
            return IAction.AS_CHECK_BOX;
        }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        switch (mode) {
        case OTHER: {
            ResourceProperties props = getTopMostSelectedResource()
                    .getProperties();
            boolean isEnabled = props.isBlinking();
            props.setBlinking(!isEnabled);
            this.setChecked(!isEnabled);
            break;
        }
        case IMAGE: {
            launchDialog(selectedRsc);
            break;
        }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.cmenu.AbstractRightClickAction#setSelectedRsc(com
     * .raytheon.viz.core.rsc.IVizResource)
     */
    @Override
    public void setSelectedRsc(ResourcePair selectedRsc) {
        super.setSelectedRsc(selectedRsc);
        AbstractVizResource<?, ?> topMost = getTopMostSelectedResource();
        if (topMost.hasCapability(ColorMapCapability.class)) {
            mode = InternalMode.IMAGE;
        } else if (topMost.hasCapability(BlendableCapability.class)) {
            mode = InternalMode.IMAGE_BLENDED;
        } else {
            mode = InternalMode.OTHER;

            ResourceProperties topMostProps = topMost.getProperties();
            boolean isEnabled = topMostProps.isBlinking();
            this.setChecked(isEnabled);
        }
    }

    @Override
    public boolean isHidden() {
        for (IDisplayPane pane : getContainer().getDisplayPanes()) {
            if (pane.getRenderableDisplay() instanceof D2DMapRenderableDisplay == false) {
                return true;
            }
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        switch (mode) {
        case IMAGE:
        case IMAGE_BLENDED: {
            return "Change blinking...";
        }
        case OTHER: {
            return "Blinking";
        }
        case MAP:
        default: {
            return "Blink Rate (seconds)";
        }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.IMenuCreator#dispose()
     */
    @Override
    public void dispose() {
        if (menu != null) {
            menu.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets
     * .Control)
     */
    @Override
    public Menu getMenu(Control parent) {
        if (menu != null) {
            menu.dispose();
        }

        menu = new Menu(parent);

        switch (mode) {
        case IMAGE_BLENDED: {
            fillBlendedMenu(menu);
            break;
        }
        case MAP: {
            fillRateMenu(menu);
            break;
        }
        }
        return menu;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets
     * .Menu)
     */
    @Override
    public Menu getMenu(Menu parent) {
        if (menu != null) {
            menu.dispose();
        }

        menu = new Menu(parent);

        switch (mode) {
        case IMAGE_BLENDED: {
            fillBlendedMenu(menu);
            break;
        }
        case MAP: {
            fillRateMenu(menu);
            break;
        }
        }
        return menu;
    }

    private void fillRateMenu(Menu menu) {
        float currRate = ((D2DMapRenderableDisplay) getContainer()
                .getActiveDisplayPane().getRenderableDisplay())
                .getBlinkInterval() / 1000.0f;

        for (float rate : blinkRates) {
            ActionContributionItem aci = new ActionContributionItem(
                    new BlinkRateAction(rate, rate == currRate));
            aci.fill(menu, -1);
        }

        ActionContributionItem aci = new ActionContributionItem(
                new BlinkRateAction(NO_BLINK_VALUE, currRate == NO_BLINK_VALUE));
        aci.fill(menu, -1);
    }

    private void fillBlendedMenu(Menu menu) {
        ResourceList list = getTopMostSelectedResource().getCapability(
                BlendableCapability.class).getResourceList();
        for (ResourcePair rp : list) {
            ActionContributionItem aci = new ActionContributionItem(
                    new BlinkBlendAction(rp));
            aci.fill(menu, -1);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getMenuCreator()
     */
    @Override
    public IMenuCreator getMenuCreator() {
        return this;
    }

    private void launchDialog(ResourcePair rsc) {
        AbstractVizResource<?, ?> resource = rsc != null ? rsc.getResource()
                : null;
        ResourceProperties props = rsc != null ? rsc.getProperties() : null;
        D2DMapRenderableDisplay[] displays = new D2DMapRenderableDisplay[getContainer()
                .getDisplayPanes().length];
        int i = 0;
        for (IDisplayPane pane : getContainer().getDisplayPanes()) {
            displays[i++] = (D2DMapRenderableDisplay) pane
                    .getRenderableDisplay();
        }
        ImageBlinkDialog.openDialog(blinkRates,
                displays[0].getBlinkInterval() / 1000.0f, resource, props,
                displays);
    }

    private class BlinkRateAction extends Action {

        private float rate;

        public BlinkRateAction(float rate, boolean selected) {
            super(rate == NO_BLINK_VALUE ? NO_BLINK : "" + rate,
                    Action.AS_RADIO_BUTTON);
            this.rate = rate;
            setChecked(selected);
        }

        @Override
        public void run() {
            for (IDisplayPane pane : getContainer().getDisplayPanes()) {
                ((D2DMapRenderableDisplay) pane.getRenderableDisplay())
                        .setBlinkInterval((long) (rate * 1000));
            }
        }
    }

    private class BlinkBlendAction extends Action {

        private ResourcePair rsc;

        public BlinkBlendAction(ResourcePair rsc) {
            super(getName(rsc), Action.AS_PUSH_BUTTON);
            this.rsc = rsc;
        }

        @Override
        public void run() {
            launchDialog(rsc);
        }
    }

    private static String getName(ResourcePair rsc) {
        String name = rsc != null ? rsc.getResource().getName() : "";
        if (name.length() > MAX_SIZE) {
            name = name.substring(0, 12) + "...";
        }
        return name;
    }
}
