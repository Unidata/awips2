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

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;

/**
 * 
 * ChangeLineStyleAction
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Oct 31, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ChangeLineStyleAction extends AbstractRightClickAction implements
        IMenuCreator {

    private Menu menu;

    private static LineStyle[] selectableStyles;

    static {
        // TODO: If need to, read in from HierarchicalPreferenceStore the styles
        // by name available to change to
        selectableStyles = new LineStyle[] { LineStyle.DEFAULT,
                LineStyle.SOLID, LineStyle.DASHED, LineStyle.DASHED_LARGE,
                LineStyle.DOTTED, LineStyle.DASH_DOTTED };
    }

    /**
     * Constructor
     */
    public ChangeLineStyleAction() {
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
        return "Line Style";
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
        for (LineStyle style : selectableStyles) {
            ActionContributionItem aci = new ActionContributionItem(
                    new SetLineStyleInternalAction(style, parent.getDisplay()));
            aci.fill(menu, -1);
        }
    }

    private class SetLineStyleInternalAction extends Action {
        private LineStyle style;

        public SetLineStyleInternalAction(LineStyle style, Display d) {
            super(style.toString());
            this.style = style;

            int[] dashes = style.getSWTLineStyle();
            // switch (style) {
            // case DASHED:
            // dashes = new int[] { 4, 4 };
            // break;
            // case DASHED_LARGE:
            // dashes = new int[] { 12, 4 };
            // break;
            // case DOTTED:
            // dashes = new int[] { 1, 1 };
            // break;
            // case DASH_DOTTED:
            // dashes = new int[] { 3, 2, 1, 2 };
            // ;
            // break;
            // case DEFAULT:
            // case SOLID:
            // default:
            // dashes = null;
            // break;
            // }

            boolean selected = style == getTopMostSelectedResource()
                    .getCapability(OutlineCapability.class).getLineStyle();

            Image image = new Image(d, 28, 20);
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
            gc.setLineDash(dashes);
            gc.drawLine(bounds.x, bounds.y + bounds.height / 2,
                    bounds.width - 1, bounds.y + bounds.height / 2);

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
            rsc.getCapability(OutlineCapability.class).setLineStyle(style);

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
