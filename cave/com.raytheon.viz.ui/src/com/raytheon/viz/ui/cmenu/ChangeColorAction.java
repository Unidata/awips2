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

package com.raytheon.viz.ui.cmenu;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.core.ColorUtil;

/**
 * 
 * Allows a resource's color to be changed
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Nov 13, 2006             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ChangeColorAction extends AbstractRightClickAction implements
        IMenuCreator {

    private Menu menu;

    /**
     * Default constructor.
     */
    public ChangeColorAction() {
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
        return "Change Color...";
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
        ActionContributionItem.setUseColorIconsInToolbars(true);
        RGB[] rgbPresets = ColorUtil.getResourceColorPresets();
        boolean found = false;
        RGB currentColor = getTopMostSelectedResource().getCapability(
                ColorableCapability.class).getColor();
        for (RGB rgb : rgbPresets) {
            boolean selected = rgb.equals(currentColor);
            found |= selected;
            ActionContributionItem actionItem = new ActionContributionItem(
                    new ChangeColorInternalAction(rgb, d, selected));
            actionItem.fill(menu, -1);
        }

        if (!found) {
            ActionContributionItem actionItem = new ActionContributionItem(
                    new ChangeColorInternalAction(currentColor, d, true));
            actionItem.fill(menu, -1);
        }

        ActionContributionItem actionItem = new ActionContributionItem(
                new ChooseColorAction(this));
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
            if (rgb != null) {
                ColorableCapability capability = getTopMostSelectedResource()
                        .getCapability(ColorableCapability.class);
                capability.setColor(rgb);
            }
            getContainer().refresh();
        }

    }
}
