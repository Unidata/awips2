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
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator.FrameChangeMode;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator.FrameChangeOperation;

/**
 * Allows the user to go to the first frame, the previous frame, the next frame,
 * or the last frame in the smaller D2D panes via a context sub-menu.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 25, 2010            bkowal     Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class StepFrameMenuAction extends AbstractRightClickAction implements
        IMenuCreator {
    public StepFrameMenuAction(IDisplayPaneContainer container) {
        super(SWT.DROP_DOWN);
        this.setContainer(container);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return "Step Frame";
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.IMenuCreator#dispose()
     */
    @Override
    public void dispose() {
        // TODO Auto-generated method stub
    }

    private class StepFrameInternalAction extends Action {
        private String text;

        protected FrameChangeOperation fco;

        public StepFrameInternalAction(String text, FrameChangeOperation fco) {
            super(null);
            this.text = text;
            this.fco = fco;
        }

        @Override
        public void run() {
            IDisplayPaneContainer container = getContainer();

            container.getActiveDisplayPane().getDescriptor()
                    .getFrameCoordinator()
                    .changeFrame(this.fco, FrameChangeMode.TIME_AND_SPACE);
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#getText()
         */
        @Override
        public String getText() {
            return this.text;
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
        Menu menu = new Menu(parent);
        this.fillMenu(menu);
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
        Menu menu = new Menu(parent);
        this.fillMenu(menu);
        return menu;
    }

    private void fillMenu(Menu menu) {
        final FrameChangeOperation[] frameNavigationAction = {
                FrameChangeOperation.FIRST, FrameChangeOperation.PREVIOUS,
                FrameChangeOperation.NEXT, FrameChangeOperation.LAST };
        final String[] frameNavigationText = { "First Frame", "Previous Frame",
                "Next Frame", "Last Frame" };

        for (int i = 0; i < frameNavigationText.length; i++) {
            ActionContributionItem aci = new ActionContributionItem(
                    new StepFrameInternalAction(frameNavigationText[i],
                            frameNavigationAction[i]));
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
