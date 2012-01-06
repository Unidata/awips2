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

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.map.IMapDescriptor;

/**
 * ZoomMenuAction
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Dec 14, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ZoomMenuAction extends AbstractRightClickAction implements
        IMenuCreator {

    private Menu menu;

    private int mapWidth;

    private int currentWidth;

    private int lastX, lastY;

    private IDisplayPane activePane;

    public static double[] ZOOM_LEVELS = new double[] { 1, Math.sqrt(2), 2,
            2 * Math.sqrt(2), 4, 6.3, 10, 16 };

    /**
     * Constructor
     */
    public ZoomMenuAction(IDisplayPaneContainer container) {
        super(SWT.DROP_DOWN);
        setContainer(container);
        this.activePane = container.getActiveDisplayPane();
        this.lastX = activePane.getLastMouseX();
        this.lastY = activePane.getLastMouseY();
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
        return "Zoom";
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
     * .Menu)
     */
    @Override
    public Menu getMenu(Menu parent) {
        if (menu != null) {
            menu.dispose();
        }

        menu = new Menu(parent);

        fillMenu(menu);
        return menu;
    }

    private class ZoomInternalAction extends Action {
        DecimalFormat df = new DecimalFormat("0.0x");

        int width;

        boolean preSelected;

        /**
         * Constructor
         * 
         * @param sz
         */
        public ZoomInternalAction(int width) {
            super("", Action.AS_RADIO_BUTTON);
            this.width = width;
            preSelected = width == currentWidth;
            this.setChecked(preSelected);
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            // Don't do anything if we were selected already
            if (preSelected) {
                return;
            }
            IDisplayPane[] panes = getContainer().getDisplayPanes();
            int mouseX = activePane.getLastClickX();
            int mouseY = activePane.getLastClickY();

            double[] c2 = activePane.screenToGrid(mouseX, mouseY, 0.0);
            c2 = activePane.getDescriptor().pixelToWorld(c2);
            double zoomLevel = (double) width / mapWidth;
            for (IDisplayPane pane : panes) {
                pane.getRenderableDisplay().getExtent().reset();
                if (zoomLevel < 1.0) {
                    pane.getRenderableDisplay().recenter(c2);
                    pane.getRenderableDisplay().zoom(zoomLevel);
                } else {
                    pane.getRenderableDisplay().scaleToClientArea(
                            pane.getBounds());
                }
                pane.refresh();
            }

            getContainer().refresh();
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#getText()
         */
        @Override
        public String getText() {
            IDescriptor descriptor = container.getActiveDisplayPane()
                    .getDescriptor();
            if (descriptor instanceof IMapDescriptor) {
                return "" + (width) + " km";
            } else {
                double value = (double) mapWidth / width;
                return df.format(value);
            }
        }

    }

    @Override
    public Menu getMenu(Control parent) {
        if (menu != null) {
            menu.dispose();
        }

        menu = new Menu(parent);

        fillMenu(menu);

        return menu;
    }

    /**
     * 
     */
    private void fillMenu(Menu menu) {
        IDescriptor descriptor = container.getActiveDisplayPane()
                .getDescriptor();
        if (descriptor instanceof IMapDescriptor) {
            mapWidth = ((IMapDescriptor) descriptor).getMapWidth() / 1000;
        } else {
            mapWidth = 10000;
        }

        IDisplayPane pane = getContainer().getActiveDisplayPane();
        currentWidth = (int) (mapWidth * pane.getRenderableDisplay().getZoom());

        // Create the basic list of zoom ratios
        List<Integer> widths = new ArrayList<Integer>();

        for (double d : ZOOM_LEVELS) {
            int width = (int) (mapWidth / d);
            // If this width is close enough to the current width(within 1%)
            // then substitute in current width to avoid near duplicates
            if (Math.abs(width - currentWidth) < mapWidth / 100) {
                width = currentWidth;
            }
            widths.add(width);
        }

        // Add in the current zoom ratio if necessary
        if (!widths.contains(currentWidth)) {
            widths.add(currentWidth);
            Collections.sort(widths);
            Collections.reverse(widths);
        }

        // Create the menu items
        for (int width : widths) {
            ActionContributionItem aci = new ActionContributionItem(
                    new ZoomInternalAction(width));
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
}
