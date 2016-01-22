package com.raytheon.viz.warngen.gui;

import org.eclipse.jface.action.IAction;

import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Action to toggle the display of the extension are in WarngenLayer
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- ------------ --------------------------
 * 12/21/2015   DCS 17942  D. Friedman  Initial revision
 * </pre>
 *
 */
public class ShowExtensionAreaToggleAction extends AbstractRightClickAction {

    WarngenLayer warngenLayer;

    public void setSelectedRsc(ResourcePair selectedRsc) {
        super.setSelectedRsc(selectedRsc);
        AbstractVizResource<?, ?> rsc = selectedRsc != null ? selectedRsc.getResource() : null;
        if (rsc instanceof WarngenLayer) {
            warngenLayer = (WarngenLayer) rsc;
            setChecked(warngenLayer.isExtensionAreaVisible());
        } else {
            warngenLayer = null;
        }
    }

    @Override
    public void run() {
        if (warngenLayer != null) {
            boolean checked = ! warngenLayer.isExtensionAreaVisible();
            warngenLayer.setExtensionAreaVisualized(checked);
            setChecked(checked);
        }
    }

    @Override
    public int getStyle() {
        return IAction.AS_CHECK_BOX;
    }

    @Override
    public String getText() {
        return "Show Extension Area";
    }

}
