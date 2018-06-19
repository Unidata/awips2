package com.raytheon.uf.viz.monitor.ffmp.ui.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPResource;
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.ChangeColorAction;

public class FFMPTraceColorAction extends ChangeColorAction {

    @Override
    public String getText() {
        return "Change Up/Down Stream Color...";
    }

    @Override
    public boolean isHidden() {
        if (getSelectedRsc() instanceof FFMPResource) {
            return false;
        }
        return true;
    }

    @Override
    public void fillMenu(Menu menu, Display d) {
        ActionContributionItem.setUseColorIconsInToolbars(true);
        RGB[] rgbPresets = ColorUtil.getResourceColorPresets();
        boolean found = false;
        FFMPResource resource = ((FFMPResource) getSelectedRsc());

        RGB currentColor = resource.getBasinTraceColor();
        for (RGB rgb : rgbPresets) {
            boolean selected = rgb.equals(currentColor);
            found |= selected;
            ActionContributionItem actionItem = new ActionContributionItem(
                    new ChangeColorInternalAction(rgb, d, selected));
            actionItem.fill(menu, -1);
        }

        if (!found && (currentColor != null)) {
            ActionContributionItem actionItem = new ActionContributionItem(
                    new ChangeColorInternalAction(currentColor, d, true));
            actionItem.fill(menu, -1);
        }

        ActionContributionItem actionItem = new ActionContributionItem(
                new ChooseColorInternalAction(this));
        actionItem.fill(menu, -1);
    }

    private class ChangeColorInternalAction extends Action {
        private RGB rgb;

        public ChangeColorInternalAction(RGB rgb, Display d, boolean selected) {
            super(RGBColors.getColorName(rgb));
            // super(new
            // StringBuilder("R:").append(rgb.red).append(" G:").append(
            // rgb.green).append(" B:").append(rgb.blue).toString());
            this.rgb = rgb;

            Image image = new Image(d, 20, 20);
            Rectangle bounds = image.getBounds();
            Color color = new Color(d, rgb);
            GC gc = new GC(image);

            gc.setBackground(color);
            gc.fillRectangle(bounds);
            gc.setForeground(d.getSystemColor(SWT.COLOR_BLACK));
            gc.drawRectangle(bounds.x, bounds.y, bounds.width - 1,
                    bounds.height - 1);

            if (selected) {
                gc.setLineWidth(2);
                gc.drawLine(bounds.x, bounds.y, bounds.x + bounds.width - 1,
                        bounds.y + bounds.height - 1);
                gc.drawLine(bounds.x + bounds.width - 1, bounds.y, bounds.x,
                        bounds.y + bounds.height - 1);
            }

            gc.dispose();
            color.dispose();

            setImageDescriptor(ImageDescriptor.createFromImage(image));
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
            FFMPResource resource = ((FFMPResource) getSelectedRsc());
            if (rgb != null) {
                resource.setBasinTraceColor(rgb);
            }
            resource.refresh();
        }
    }

    private class ChooseColorInternalAction extends Action {
        private AbstractRightClickAction parentAction;

        public ChooseColorInternalAction(AbstractRightClickAction parent) {
            super("Choose Color...");
            this.parentAction = parent;
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
            ColorDialog cd = new ColorDialog(VizWorkbenchManager.getInstance()
                    .getCurrentWindow().getShell());
            cd.setRGB(parentAction.getTopMostSelectedResource()
                    .getCapability(ColorableCapability.class).getColor());

            cd.setText(parentAction.getTopMostSelectedResource().getName());

            RGB result = cd.open();
            FFMPResource resource = ((FFMPResource) getSelectedRsc());
            if (result != null) {
                resource.setBasinTraceColor(result);
            }
            resource.refresh();
        }
    }
}
