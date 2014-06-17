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
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.PointCapability;

/**
 * Action to change point rendering style on the map
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ???                     ???         Initial Creation
 * Jun 17, 2014  2903      bclement    added PIPE to PointStyle
 * 
 * </pre>
 * 
 * @version 1.0
 */
public class ChangePointStyleAction extends AbstractRightClickAction implements
        IMenuCreator {

    private Menu menu;

    /**
     * Constructor
     */
    public ChangePointStyleAction() {
        super(SWT.DROP_DOWN);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return "Point Style";
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
        createMenu(menu);

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

        createMenu(parent);

        return menu;
    }

    private void createMenu(Menu parent) {
        menu = new Menu(parent);

        for (PointStyle style : PointStyle.values()) {
            ActionContributionItem aci = new ActionContributionItem(
                    new SetPointStyleInternalAction(style, parent.getDisplay()));
            aci.fill(menu, -1);
        }
    }

    private class SetPointStyleInternalAction extends Action {
        private PointStyle style;

        public SetPointStyleInternalAction(PointStyle style, Display d) {
            super(style.toString());
            this.style = style;

            boolean selected = style == getTopMostSelectedResource()
                    .getCapability(PointCapability.class).getPointStyle();

            Image image = new Image(d, 20, 20);
            Rectangle bounds = image.getBounds();
            GC gc = new GC(image);

            if (selected) {
                gc.setForeground(d
                        .getSystemColor(SWT.COLOR_LIST_SELECTION_TEXT));
                gc.setBackground(d.getSystemColor(SWT.COLOR_LIST_SELECTION));
            } else {
                gc.setForeground(d.getSystemColor(SWT.COLOR_LIST_FOREGROUND));
                gc.setBackground(d.getSystemColor(SWT.COLOR_LIST_BACKGROUND));
            }
            gc.fillRectangle(bounds);

            int x = bounds.x + bounds.width / 2;
            int y = bounds.y + bounds.height / 2;
            int xTick = 3;
            int yTick = 3;
            switch (style) {
            case POINT:
                gc.drawLine(x, y, x + 1, y);
                break;

            case CROSS:
                gc.drawLine(x - xTick, y, x + xTick, y);
                gc.drawLine(x, y - yTick, x, y + yTick);
                break;

            case STAR:
                gc.drawLine(x, y - (int) (yTick * 1.5), x, y
                        + (int) (yTick * 1.5));
            case X:
                gc.drawLine(x - xTick, y - yTick, x + xTick, y + yTick);
                gc.drawLine(x - xTick, y + yTick, x + xTick, y - yTick);
                break;

            case BOX:
                gc.drawRectangle(x - xTick, y - yTick, xTick * 2, yTick * 2);
                break;

            case SQUARE:
                gc.setBackground(gc.getForeground());
                gc.fillRectangle(x - xTick, y - yTick, xTick * 2, yTick * 2);
                break;

            case CIRCLE:
                gc.drawOval(x - xTick, y - yTick, xTick * 2 + 1, yTick * 2 + 1);
                break;

            case DISC:
                gc.setBackground(gc.getForeground());
                gc.fillOval(x - xTick, y - yTick, xTick * 2 + 1, yTick * 2 + 1);
                break;

            case PIPE:
                gc.drawLine(x, y - yTick, x, y + yTick);
                break;

            case DASH:
                gc.drawLine(x - xTick, y, x + xTick, y);
                break;

            case NONE:
            default:
            }

            gc.dispose();

            setImageDescriptor(ImageDescriptor.createFromImage(image));
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            AbstractVizResource<?, ?> rsc = getTopMostSelectedResource();
            rsc.getCapability(PointCapability.class).setPointStyle(style);

            rsc.issueRefresh();
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

}
